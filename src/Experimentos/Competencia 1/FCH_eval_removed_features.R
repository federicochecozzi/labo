#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022")  #Establezco el Working Directory

#cargo el dataset
dataset <- fread("./exp/FE777002/dataset_con_features.csv")

dataset[, c("Master_Fvencimiento","Master_Finiciomora","Master_fultimo_cierre","Master_fechaalta","Visa_Fvencimiento","Visa_Finiciomora","Visa_fultimo_cierre","Visa_fechaalta") := NULL]

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

# Creamos una clase binaria, importante no confundirla el diseño alternativo de Denicolay
dtrain[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria - numero_de_cliente",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.975,
                 minsplit=  647,   
                 minbucket=  1,   
                 maxdepth=     10 )  


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOs columnas, llamadas "evento" y "no evento"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de "evento"
dapply[ , prob_baja2 := prediccion[, "evento"] ]

#solo le envio estimulo a los registros con probabilidad de "evento" mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA777006" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA777006/KA777006_002.csv",
        sep=  "," )