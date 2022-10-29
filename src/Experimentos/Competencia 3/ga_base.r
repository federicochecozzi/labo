#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# ZZ final que necesita de UNDERSAMPLING

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("primes")
require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "GA9410"
PARAM$exp_input  <- "HT9410"

PARAM$modelos  <- 5
# FIN Parametros del script

ksemilla  <- 671017
PARAM$semilla1  <- 671017
PARAM$semillas_modelo    <- 5

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_training.csv.gz" )
dataset  <- fread( arch_dataset )

#Verificaciones
if( ! ("fold_train"    %in% colnames(dataset) ) ) stop("Error, el dataset no tiene el campo fold_train \n")
if( ! ("fold_validate" %in% colnames(dataset) ) ) stop("Error, el dataset no tiene el campo fold_validate \n")
if( ! ("fold_test"     %in% colnames(dataset) ) ) stop("Error, el dataset no tiene el campo fold_test  \n")
if( dataset[ fold_train==1, .N ] == 0 ) stop("Error, en el dataset no hay fold_train==1 \n")


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------
dataset[  , ganancia :=  ifelse( clase_ternaria == "BAJA+2", 78000, -2000 ) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "ganancia","fold_train", "fold_validate", "fold_test") )

#--------------------------------------

dfuture  <- dataset[ fold_test== 1 ]

#genero dos vectores de semillas
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
set.seed( PARAM$semilla1) #seteo la semilla que controla al sample de los primos
ksemillas_modelo  <- sample(primos)[ 1:PARAM$semillas_modelo ]   #me quedo con PARAM$semillerio primos al azar


tb_cortes  <- data.table(  semilla= integer(),
                           corte= integer(),
                           ganancia= numeric() )

#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( i in  1:PARAM$modelos )
{
  parametros  <- as.list( copy( tb_log[ i ] ) )
  #iteracion_bayesiana  <- parametros$iteracion_bayesiana
  
  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ fold_train==1, campos_buenos, with=FALSE] ),
                          label=   dataset[ fold_train==1, clase01 ],
                          free_raw_data= FALSE
  )
  
  ganancia  <- parametros$ganancia
  
  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL
  
  for( semilla_modelo  in  ksemillas_modelo )
  {
    #Utilizo la semilla definida en este script
    parametros$seed  <- semilla_modelo
  
    #genero el modelo entrenando en los datos finales
    set.seed( parametros$seed )
    modelo_final  <- lightgbm( data= dtrain,
                             param=  parametros,
                             verbose= -100 )
  
    #genero la prediccion, Scoring
    prediccion  <- predict( modelo_final,
                            data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
    
    #genero la tabla de entrega
    tb_entrega  <- dfuture[ , lista(ganancia) ]
    tb_entrega[  , prob := prediccion ]
    
    #ordeno por probabilidad descendente
    setorder( tb_entrega, -prob )
    
    tb_entrega[ , x := .I ]
    tb_entrega[ , gan_acum := cumsum( ganancia ) ]
    
    
    #genero los archivos para Kaggle
    cortes  <- seq( from=  5000,
                    to=   13000,
                    by=     500 )
    
    for( corte in cortes )
    {
      total    <- tb_entrega[ x <= corte,  sum( ganancia,    na.rm=TRUE ) ]
      
      tb_cortes  <-  rbind( tb_cortes,
                            list( semilla_modelo,
                                  corte,
                                  total) )
      
    }
    
    arch_seed  <- paste0( "modelo_" ,
                            sprintf( "%02d", i ),
                            "_semilla",
                            sprintf( "%03d", semilla_modelo ),
                            ".txt" )
    
    fwrite( tb_cortes,
            file= arch_seed,
            sep= "\t" )
    
    #borro y limpio la memoria para la vuelta siguiente del for
    rm(tb_entrega)
    rm(tb_cortes)
    gc()
  
  }
  rm( parametros )
  rm( dtrain )
  gc()
}