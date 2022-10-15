# para correr el Google Cloud
#   8 vCPU
#  32 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("primes")
require("lightgbm")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "DA777001zalt"

PARAM$input$dataset       <- "./exp/FE777001z/dataset.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$semilla1  <- 200177
PARAM$semillas_modelo    <- 50

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.005036461
PARAM$finalmodel$num_iterations    <-    684
PARAM$finalmodel$num_leaves        <-    1019
PARAM$finalmodel$min_data_in_leaf  <-   673
PARAM$finalmodel$feature_fraction  <-      0.4408681
PARAM$finalmodel$semilla           <- 102191

#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "~/buckets/b1" )


#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

dataset[  , ganancia :=  ifelse( clase_ternaria == "BAJA+2", 78000, -2000 ) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "ganancia") )

#--------------------------------------

#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO


dfuturo  <- dataset[ foto_mes== PARAM$input$future ]

#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero dos vectores de semillas
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
set.seed( PARAM$semilla1) #seteo la semilla que controla al sample de los primos
ksemillas_modelo  <- sample(primos)[ 1:PARAM$semillas_modelo ]   #me quedo con PARAM$semillerio primos al azar


tb_optimos  <- data.table(  semilla= integer(),
                            corte= numeric(),
                            gan=  numeric() )

for( semilla_modelo  in  ksemillas_modelo )
  {
    #los campos que se van a utilizar
    campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "ganancia", "train") )


    #genero el modelo
    #estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
    modelo  <- lgb.train( data= dtrain,
                          param= list( objective=          "binary",
                                       max_bin=            PARAM$finalmodel$max_bin,
                                       learning_rate=      PARAM$finalmodel$learning_rate,
                                       num_iterations=     PARAM$finalmodel$num_iterations,
                                       num_leaves=         PARAM$finalmodel$num_leaves,
                                       min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                       feature_fraction=   PARAM$finalmodel$feature_fraction,
                                       seed=               semilla_modelo
                                      )
                        )


    #aplico el modelo a los datos nuevos
    prediccion  <- predict( modelo, 
                            data.matrix( dfuturo[, campos_buenos, with=FALSE ]) )

    #genero la tabla de entrega
    tb_entrega  <- dfuturo[ , list(ganancia ) ]
    tb_entrega[  , prob := prediccion ]
    
    #ordeno por probabilidad descendente
    setorder( tb_entrega, -prob )

    tb_entrega[ , x := .I ]
    tb_entrega[ , gan_acum := cumsum( ganancia ) ]

    #Tabla de optimos
    gan_mejor <- tb_entrega[ , max(gan_acum, na.rm=TRUE)  ]
    x_mejor   <- tb_entrega[ gan_acum==gan_mejor,  mean(x) ]

    tb_optimos  <- rbind( tb_optimos,
                         list( semilla_modelo,
                               x_mejor,
                               gan_mejor ) )

    fwrite( tb_optimos,
            file= "tb_optimos.txt",
            sep= "\t" )
}

