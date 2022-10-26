#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022")  #Establezco el Working Directory
# Poner sus semillas
semillas <- c(671017, 273107, 827251, 967693, 247591)

# Cargamos el dataset
dataset <- fread("./exp/FE777002/dataset_con_features.csv")
dataset  <- fread( "./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

#Variables a eliminar
dataset[, c("numero_de_cliente","Master_Fvencimiento","Master_Finiciomora","Master_fultimo_cierre","Master_fechaalta","Visa_Fvencimiento","Visa_Finiciomora","Visa_fultimo_cierre","Visa_fechaalta") := NULL]

ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}

modelo_rpart2 <- function(train, test, cp =  -1, ms = 1000, mb = 405, md = 5) {
  modelo <- rpart(clase_binaria ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  
  ganancia(test_prediccion[, "evento"], test$clase_binaria) / 0.3
}

experimento_rpart2 <- function(ds, semillas, cp =  -1, ms = 1000, mb = 405, md = 5) {
  gain <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    r <- modelo_rpart2(train, test, 
                       cp = cp, ms = ms, mb = mb, md = md)
    gain <- c(gain, r)
  }
  print(sd(gain))
  mean(gain)
}

experimento_rpart2(dataset, semillas)
