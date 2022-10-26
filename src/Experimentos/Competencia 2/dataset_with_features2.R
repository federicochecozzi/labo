# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")
require("xgboost")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022")

# Cargamos el dataset
dataset <- fread("./datasets/competencia2_2022.csv.gz")

#Nuevos feature

dataset[, cociente_mrentabilidad := mrentabilidad_annual/(mrentabilidad + 1)] #sin cambios debería ser alrededor de 12

dataset[, cuentas_totales := rowSums(.SD, na.rm = TRUE), .SDcols = c("ccuenta_corriente", "ccaja_ahorro")]

dataset[, mprestamos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios")]

dataset[, cociente_mplazo_fijo := mplazo_fijo_dolares/(mplazo_fijo_pesos + 1)]

dataset[, suma_mplazo_fijo := rowSums(.SD, na.rm = TRUE), .SDcols = c("mplazo_fijo_dolares", "mplazo_fijo_pesos")]

dataset[, suma_minversion := rowSums(.SD, na.rm = TRUE), .SDcols = c("minversion1_pesos", "minversion2")]

dataset[, cseguro := rowSums(.SD, na.rm = TRUE), .SDcols = c("cseguro_vida", "cseguro_auto", "cseguro_vivienda","cseguro_accidentes_personales")]

dataset[, suma_mpayroll := rowSums(.SD, na.rm = TRUE), .SDcols = c("mpayroll", "mpayroll2")]

#magia de xgboost
marzo <- dataset[foto_mes == 202103]

clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
marzo$clase_ternaria <- NULL

dtrain <- xgb.DMatrix(
  data = data.matrix(marzo),
  label = clase_binaria, missing = NA)

# Empecemos con algo muy básico
param_fe <- list(
  max_depth = 2,
  eta = 0.1,
  objective = "binary:logistic")
nrounds <- 5

xgb_model <- xgb.train(params = param_fe, data = dtrain, nrounds = nrounds)

clase_real <- dataset$clase_ternaria
dataset$clase_ternaria <- NULL

new_features <- xgb.create.features(model = xgb_model, data.matrix(dataset))

final_dataset <- as.data.table(as.matrix(new_features))

final_dataset[,clase_ternaria := clase_real]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/FE777005b" )

fwrite( final_dataset, #solo los campos para Kaggle
        file= "./exp/FE777005b/dataset_con_features2.csv",
        sep=  "," )