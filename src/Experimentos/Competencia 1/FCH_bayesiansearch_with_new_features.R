# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")
require("ROCR")
require("ggplot2")
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022")
# Poner sus semillas
semillas <- c(671017, 273107, 827251, 967693, 247591)

# Cargamos el dataset
dataset <- fread("./exp/FE777002/dataset_con_features.csv")

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

ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}

modelo_rpart2 <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  
  ganancia(test_prediccion[, "evento"], test$clase_binaria) / 0.3
}

experimento_rpart2 <- function(ds, semillas, cp = 0, ms = 20, mb = 1, md = 10) {
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
  mean(gain)
}

obj_fun_md_ms_mb_cp <- function(x) {
  experimento_rpart2(dataset, semillas
                     , md = x$maxdepth
                     , ms = x$minsplit
                     , mb = floor(x$minbucket * x$minsplit)
                     , cp = x$cp)
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms_mb_cp,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 4L, upper = 20L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 2000L),
    makeNumericParam("minbucket", lower = 0L, upper = 0.5),
    makeNumericParam("cp", lower = -0.1, upper = 0.1)
  ),
  # noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 100L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms_mb_cp <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms_mb_cp)