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
# Poner sus semillas
semillas <- c(671017, 273107, 827251, 967693, 247591)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

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

replaceNAwith0 <- function(col){
  ifelse(
    is.na(col),
    0,
    col
  )
}

dataset[, cociente_mrentabilidad := mrentabilidad_annual/mrentabilidad] #sin cambios debería ser alrededor de 12
dataset[, cuentas_totales := ifelse(
            is.na(ccuenta_corriente),
            0,
            ccuenta_corriente
          ) + 
          ifelse(
            is.na(ccaja_ahorro),
            0,
            ccaja_ahorro
          )] 

dataset[, mprestamos := ifelse(
                          is.na(mprestamos_personales),
                          0,
                          mprestamos_personales
                        ) + 
                        ifelse(
                          is.na(mprestamos_prendarios),
                          0,
                          mprestamos_prendarios
                        ) + 
                        ifelse(
                          is.na(mprestamos_hipotecarios),
                          0,
                          mprestamos_hipotecarios
          )] 

dataset[, cociente_mplazo_fijo := mplazo_fijo_dolares/mplazo_fijo_pesos]

dataset[, suma_mplazo_fijo := ifelse(
  is.na(mplazo_fijo_dolares),
  0,
  mplazo_fijo_dolares
) + 
  ifelse(
    is.na(mplazo_fijo_pesos),
    0,
    mplazo_fijo_pesos
  )] 

dataset[, suma_minversion := ifelse(
  is.na(minversion1_pesos),
  0,
  minversion1_pesos
) + 
  ifelse(
    is.na(minversion2),
    0,
    minversion2
  )] 

dataset[, cseguro := replaceNAwith0(cseguro_vida) + replaceNAwith0(cseguro_auto) + replaceNAwith0(cseguro_vivienda) + replaceNAwith0(cseguro_accidentes_personales)]
dataset[, suma_mpayroll := replaceNAwith0(mpayroll) + replaceNAwith0(mpayroll2)]


#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/FE777002" )

fwrite( dataset, #solo los campos para Kaggle
        file= "./exp/FE777002/dataset_con_features.csv",
        sep=  "," )