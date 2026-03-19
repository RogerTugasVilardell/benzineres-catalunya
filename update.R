#!/usr/bin/env Rscript
# update.R — Descarrega preus de benzineres a Catalunya i genera preus.json
# Executat per GitHub Actions cada 6 hores

library(jsonlite)
library(dplyr)

cat("Iniciant descàrrega de preus...\n")
cat("Data/hora:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n")

# ── 1. Descarregar dades de l'API del MINETUR ──────────────────────────────────
url_api <- "https://energia.serviciosmin.gob.es/ServiciosRestCarburantes/PreciosCarburantes/EstacionesTerrestres/FiltroCCAA/09"

resp <- tryCatch(
  fromJSON(url_api, flatten = TRUE),
  error = function(e) {
    cat("ERROR en la descàrrega:", conditionMessage(e), "\n")
    quit(status = 1)
  }
)

cat("Dades descarregades. Data oficial:", resp$Fecha, "\n")
cat("Estacions a l'API:", nrow(resp$ListaEESSPrecio), "\n")

# ── 2. Netejar i transformar ───────────────────────────────────────────────────
preus <- resp$ListaEESSPrecio

# Funció per convertir preus (coma decimal → punt, string → numeric)
to_num <- function(x) {
  x <- gsub(",", ".", trimws(x))
  x[x == ""] <- NA
  as.numeric(x)
}

df <- preus |>
  mutate(
    IDEESS                = trimws(IDEESS),
    Rotulo                = trimws(`Rótulo`),
    Gasolina95            = to_num(`Precio Gasolina 95 E5`),
    Gasolina95Premium     = to_num(`Precio Gasolina 95 E5 Premium`),
    Gasolina98            = to_num(`Precio Gasolina 98 E5`),
    GasoleoA              = to_num(`Precio Gasoleo A`),
    GasoloePremium        = to_num(`Precio Gasoleo Premium`),
    GasoleoB              = to_num(`Precio Gasoleo B`),
    GLP                   = to_num(`Precio Gases licuados del petróleo`),
    GNC                   = to_num(`Precio Gas Natural Comprimido`),
    GNL                   = to_num(`Precio Gas Natural Licuado`),
    Hidrogen               = to_num(`Precio Hidrogeno`),
    Adblue                = to_num(`Precio Adblue`),
    Provincia             = trimws(Provincia),
    Municipio             = trimws(Municipio),
  ) |>
  select(
    IDEESS, Rotulo, Provincia, Municipio,
    Gasolina95, Gasolina95Premium, Gasolina98,
    GasoleoA, GasoloePremium, GasoleoB,
    GLP, GNC, GNL, Hidrogen, Adblue
  )

cat("Estacions processades:", nrow(df), "\n")

# ── 3. Estadístiques de resum (per al mapa) ────────────────────────────────────
# Mediana per província (per a la llegenda de colors)
stats <- df |>
  summarise(
    mediana_g95    = median(Gasolina95,  na.rm = TRUE),
    mediana_diesel = median(GasoleoA,    na.rm = TRUE),
    mediana_g98    = median(Gasolina98,  na.rm = TRUE),
    n_total        = n()
  )

cat("Mediana Gasolina 95:", round(stats$mediana_g95, 3), "€/L\n")
cat("Mediana Gasoleo A:  ", round(stats$mediana_diesel, 3), "€/L\n")

# ── 4. Guardar JSON de sortida ─────────────────────────────────────────────────
output <- list(
  fecha_actualizacion = resp$Fecha,
  timestamp_utc       = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  n_estaciones        = nrow(df),
  estadisticas        = as.list(stats),
  precios             = df
)

output_path <- "data/preus.json"
dir.create("data", showWarnings = FALSE)
write_json(output, output_path, na = "null", digits = 4, auto_unbox = TRUE)

cat("JSON guardat a:", output_path, "\n")
cat("Mida fitxer:", file.size(output_path), "bytes\n")
cat("Fet!\n")
