#Necesita para correr en Google Cloud
# 256 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#Parametros del script
PARAM  <- list()
PARAM$experimento <- "FE777001z" #carpeta para datasets

PARAM$exp_input  <- "DR9141"

PARAM$lag1    <- TRUE
PARAM$lag2    <- FALSE
PARAM$delta1  <- FALSE
PARAM$delta2  <- FALSE
#Pendiente: agregar solo tendencia si queda tiempo
PARAM$bsofd   <- FALSE #backward second order finite difference
# FIN Parametros del script

#------------------------------------------------------------------------------

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/" )

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", PARAM$exp_input, "/dataset.csv.gz" )
dataset  <- fread( dataset_input )



#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#--------------------------------------
#estas son las columnas a las que se puede agregar lags o media moviles ( todas menos las obvias )
cols_lagueables  <- copy(  setdiff( colnames(dataset), c("numero_de_cliente", "foto_mes", "clase_ternaria")  ) )

#ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
#  es MUY  importante esta linea
setorder( dataset, numero_de_cliente, foto_mes )


if( PARAM$lag1 )
{
  #creo los campos lags de orden 1
  dataset[ , paste0( cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]
}

if( PARAM$delta1 )
{
  #creo los campos lags de orden 1
  dataset[ , paste0( cols_lagueables, "_delta1") := .SD - shift(.SD, 1, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]
}

if( PARAM$lag2 )
{
  #creo los campos lags de orden 2
  dataset[ , paste0( cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]
}

if( PARAM$delta2 )
{
  #creo los campos lags de orden 1
  dataset[ , paste0( cols_lagueables, "_delta2") := .SD - shift(.SD, 2, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]
}

if( PARAM$bsofd )
{
  #creo los campos lags de orden 1
  dataset[ , paste0( cols_lagueables, "_bsofd") := .SD - 2*shift(.SD, 1, NA, "lag") + shift(.SD, 2, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]
}
#------------------------------------------------------------------------------
#grabo el dataset
fwrite( dataset,
        "dataset.csv.gz",
        logical01= TRUE,
        sep= "," )