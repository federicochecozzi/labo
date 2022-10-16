#Necesita para correr en Google Cloud
# 256 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")

#Parametros del script
PARAM  <- list()
PARAM$experimento <- "FE777002z" #carpeta para datasets

PARAM$exp_input  <- "DR9141"

PARAM$lag1    <- FALSE
PARAM$lag2    <- FALSE
PARAM$delta1  <- FALSE
PARAM$delta2  <- FALSE
PARAM$Tendencias <- FALSE
PARAM$bsofd   <- TRUE #backward second order finite difference
# FIN Parametros del script

#------------------------------------------------------------------------------

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------
#se calculan para los 6 meses previos ls tendencia calculada con cuadrados minimos

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( n );

  for(int i = 0; i < n; i++)
  {
    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
    }
    else
    {
      out[ i ]  =  NA_REAL ; 
    }
  }

  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
#La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

Tendencia  <- function( dataset, cols, ventana=6)
{
  gc()
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- ventana
  
  last  <- nrow( dataset )
  
  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente
  
  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1
  
  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }
  
  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 
    dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ 1:last ]  ]
    #if(tendencia)  dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ (0*last +1):(1*last) ]  ]
  }
  
}
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
  #creo los campos deltas de orden 1
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
  #creo los campos deltas de orden 1
  dataset[ , paste0( cols_lagueables, "_delta2") := .SD - shift(.SD, 2, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]
}

#--------------------------------------
#agrego las tendencias
if( PARAM$Tendencias )
{
  Tendencia( dataset, 
              cols= cols_lagueables,
              ventana=   6)
}


if( PARAM$bsofd )
{
  #creo los campos bsofd
  #shift devuelve una lista así que no se puede multiplicar por 2, en ese caso es más fácil multiplicar .SD primero
  dataset[ , paste0( cols_lagueables, "_bsofd") := .SD - shift(2*.SD, 1, NA, "lag") + shift(.SD, 2, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]
}
#------------------------------------------------------------------------------
#grabo el dataset
fwrite( dataset,
        "dataset.csv.gz",
        logical01= TRUE,
        sep= "," )