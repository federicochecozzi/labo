# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022")

# Cargamos el dataset
dataset <- fread("./datasets/competencia2_2022.csv.gz")

#Nuevos feature

mis_variables <- c("ctrx_quarter",
                   "mprestamos_personales",
                   "mcuentas_saldo",
                   "mactivos_margen",
                   "mcaja_ahorro",
                   "mcuenta_corriente")

prefix <- "r_"
for (var in mis_variables) {
  dataset[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]#get permite obtener la variable a partir de su nombre
}

dataset[, cociente_mrentabilidad := mrentabilidad_annual/mrentabilidad] #sin cambios debería ser alrededor de 12

dataset[, cuentas_totales := rowSums(.SD, na.rm = TRUE), .SDcols = c("ccuenta_corriente", "ccaja_ahorro")]

dataset[, mprestamos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios")]

dataset[, cociente_mplazo_fijo := mplazo_fijo_dolares/mplazo_fijo_pesos]

dataset[, suma_mplazo_fijo := rowSums(.SD, na.rm = TRUE), .SDcols = c("mplazo_fijo_dolares", "mplazo_fijo_pesos")]

dataset[, suma_minversion := rowSums(.SD, na.rm = TRUE), .SDcols = c("minversion1_pesos", "minversion2")]

dataset[, cseguro := rowSums(.SD, na.rm = TRUE), .SDcols = c("cseguro_vida", "cseguro_auto", "cseguro_vivienda","cseguro_accidentes_personales")]

dataset[, suma_mpayroll := rowSums(.SD, na.rm = TRUE), .SDcols = c("mpayroll", "mpayroll2")]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/FE777002b" )

fwrite( dataset, #solo los campos para Kaggle
        file= "./exp/FE777002b/dataset_con_features.csv",
        sep=  "," )