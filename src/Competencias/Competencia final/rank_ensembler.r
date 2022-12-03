# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ensembles"
PARAM$exp_input <- "ZZ777005d_m2"
PARAM$month <- 202105

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
dir.create(paste0(base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

# Cargar las semillas usadas para levantar las ganancias en el orden que fueron calculadas
#arch_future <- paste0(base_dir, "exp/", PARAM$exp_input, "/ksemillas.csv")
#ksemillas <- read.csv(arch_future, header = TRUE)$x

path_experimento_semillerio <- paste0(base_dir, "exp/", PARAM$exp_input)
archivos <- list.files(path = path_experimento_semillerio, pattern = "_resultados.csv")

# Esto es MUY dependiente del formato del nombre de los experimentos, se puede romper muy facil
ksemillas <- strtoi(sapply(strsplit(archivos, "_"), "[", 3))

# Levantar dataset C4
# leo el dataset a partir del cual voy a calcular las ganancias
arch_dataset <- paste0(base_dir, "exp/BA777001d/dataset_basic_info.csv.gz")
dataset <- fread(arch_dataset)

dataset <- dataset[foto_mes == PARAM$month]
#rm(dataset)

#dataset[, clase_real := ifelse(clase_ternaria == "BAJA+2", 1, 0)]
# Nos quedamos con las 2 columnas que nos resultan relevantes
#dataset <- dataset[, .(numero_de_cliente, clase_real)]

# Tabla que contendrá los rankings de todos los clientes para todas las semillas
tb_ranking_semillerio <- data.table(numero_de_cliente = dataset[, numero_de_cliente])
tb_prob_semillerio <- data.table(numero_de_cliente = dataset[, numero_de_cliente])

for (archivo in archivos) {
  
  ksemilla <- strtoi(sapply(strsplit(archivo, "_"), "[", 3))
  
  # cols: numero_de_cliente,foto_mes,prob,rank
  tb_prediccion <- fread(paste0(path_experimento_semillerio, '/', archivo))
  setorder(tb_prediccion, numero_de_cliente)
  setorder(tb_ranking_semillerio, numero_de_cliente)
  
  # Generamos predicción del semillerio
  tb_ranking_semillerio[, paste0("rank_",PARAM$exp_input,"_", ksemilla) := tb_prediccion$rank]
  tb_prob_semillerio[, paste0("prob_",PARAM$exp_input,"_", ksemilla) := tb_prediccion$prob]
  
  
  # Esta es la predicción del semillerio para la semilla i-esima
  # tb_prediccion_semillerio <- data.table(
  #   tb_ranking_semillerio[, list(numero_de_cliente)],
  #   prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
  # )
  # setorder(tb_prediccion_semillerio, prediccion) # Esto es un ranking, entonces de menor a mayor
  # 
  # 
  # for (corte in PARAM$cortes)
  # {
  #   nom_col_ind = paste0("individual_",sprintf("%d", corte))
  #   nom_col_sem = paste0("semillerio_",sprintf("%d", corte))
  #   tb_prediccion_semillerio[, Predicted := 0]
  #   tb_prediccion_semillerio[1:corte, Predicted := 1L]
  # }
  
  
}

nombre_arch_ranking <- paste0(
  PARAM$exp_input,
  "_",
  "ranking",
  ".csv"
)

fwrite(
  tb_ranking_semillerio,
  file = nombre_arch_ranking,
  sep = ","
)

nombre_arch_prob <- paste0(
  PARAM$exp_input,
  "_",
  "prob",
  ".csv"
)

fwrite(
  tb_prob_semillerio,
  file = nombre_arch_prob,
  sep = ","
)

tb_avg_rank <- data.table(
     tb_ranking_semillerio[, list(numero_de_cliente)],
     prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
   )

setorder(tb_avg_rank, prediccion)

nombre_arch_avg_rank <- paste0(
  PARAM$exp_input,
  "_",
  "avg_rank",
  ".csv"
)

fwrite(
  tb_avg_rank,
  file = nombre_arch_avg_rank,
  sep = ","
)

tb_avg_prob <- data.table(
  tb_prob_semillerio[, list(numero_de_cliente)],
  prediccion = rowMeans(tb_prob_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
)

setorder(tb_avg_prob, -prediccion)

nombre_arch_avg_prob <- paste0(
  PARAM$exp_input,
  "_",
  "avg_prob",
  ".csv"
)

fwrite(
  tb_avg_prob,
  file = nombre_arch_avg_prob,
  sep = ","
)

dir.create(paste0(base_dir, "exp/", PARAM$experimento, "/",PARAM$exp_input), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/", PARAM$experimento, "/",PARAM$exp_input)) # Establezco el Working Directory DEL EXPERIMENTO

for (corte in PARAM$cortes)
  {
    tb_avg_rank[, Predicted := 0]
    tb_avg_rank[1:corte, Predicted := 1L]
    nombre_arch_pred_rank <- paste0(
      PARAM$exp_input,
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
      PARAM$exp_input,
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