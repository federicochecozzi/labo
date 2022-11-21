# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "hib2"
PARAM$ensemble_folder <- "ensembles_test"
PARAM$exp_input <- c("TE777003d_m2","TE777003d_m4")
PARAM$month <- 202103

#PARAM$corte <- 11000 # cantidad de envios
PARAM$cortes  <- seq( from=  7000,
                      to=    14000,
                      by=        250 )
# FIN Parametros del script

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

base_dir <- "C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022\\"#"~/buckets/b1/"

# creo la carpeta donde va el experimento
dir.create(paste0(base_dir, "exp/", PARAM$ensemble_folder,"/",PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/", PARAM$ensemble_folder,"/",PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

# Levantar dataset C4
# leo el dataset a partir del cual voy a calcular las ganancias
arch_dataset <- paste0(base_dir, "exp/BA777001d/dataset_basic_info.csv.gz")
dataset <- fread(arch_dataset)

dataset <- dataset[foto_mes == PARAM$month]

# Tabla que contendrá los rankings de todos los clientes para todas las semillas
tb_ranking_semillerio <- data.table(numero_de_cliente = dataset[, numero_de_cliente])
tb_prob_semillerio <- data.table(numero_de_cliente = dataset[, numero_de_cliente])

for (model in PARAM$exp_input) {
  file <- paste0(base_dir, "exp/", PARAM$ensemble_folder,"/",model,"_ranking.csv")
  tb_rank <- fread(file)
  tb_ranking_semillerio <- tb_ranking_semillerio[tb_rank,on = "numero_de_cliente", ]
  file <- paste0(base_dir, "exp/", PARAM$ensemble_folder,"/",model,"_prob.csv")
  tb_prob <- fread(file)
  tb_prob_semillerio <- tb_prob_semillerio[tb_prob,on = "numero_de_cliente", ]
}

tb_avg_rank <- data.table(
   tb_ranking_semillerio[, list(numero_de_cliente)],
   prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
)

tb_avg_prob <- data.table(
  tb_prob_semillerio[, list(numero_de_cliente)],
  prediccion = rowMeans(tb_prob_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
)


setorder(tb_avg_rank, prediccion)
setorder(tb_avg_prob, -prediccion)


for (corte in PARAM$cortes)
{
  tb_avg_rank[, Predicted := 0]
  tb_avg_rank[1:corte, Predicted := 1L]
  nombre_arch_pred_rank <- paste0(
    PARAM$experimento,
    "_rank_",
    sprintf("%05d", corte),
    ".csv"
  )
  
  fwrite(
    tb_avg_rank[,.(numero_de_cliente,Predicted)],
    file = nombre_arch_pred_rank,
    sep = ","
  )
  
  tb_avg_prob[, Predicted := 0]
  tb_avg_prob[1:corte, Predicted := 1L]
  nombre_arch_pred_prob <- paste0(
    PARAM$experimento,
    "_prob_",
    sprintf("%05d", corte),
    ".csv"
  )
  
  fwrite(
    tb_avg_prob[,.(numero_de_cliente,Predicted)],
    file = nombre_arch_pred_prob,
    sep = ","
  )
}