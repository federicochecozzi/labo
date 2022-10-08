# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022")

# Cargamos el dataset
dataset <- fread("./datasets/competencia2_2022.csv.gz")

ARScols <- c(
"mrentabilidad",
"mrentabilidad_annual",
"mcomisiones",
"mactivos_margen",
"mpasivos_margen",
"mcuenta_corriente_adicional",
"mcuenta_corriente",
"mcaja_ahorro",
"mcaja_ahorro_adicional",
"mcaja_ahorro_dolares",
"mcuentas_saldo",
"mautoservicio",
"mtarjeta_visa_consumo",
"mtarjeta_master_consumo",
"mprestamos_personales",
"mprestamos_prendarios",
"mprestamos_hipotecarios",
"mplazo_fijo_dolares",
"mplazo_fijo_pesos",
"minversion1_pesos",
"minversion1_dolares",
"minversion2",
"mpayroll",
"mpayroll2",
"mcuenta_debitos_automaticos",
"mttarjeta_visa_debitos_automaticos",
"mttarjeta_master_debitos_automaticos",
"mpagodeservicios",
"mpagomiscuentas",
"mcajeros_propios_descuentos",
"mtarjeta_visa_descuentos",
"mtarjeta_master_descuentos",
"mcomisiones_mantenimiento",
"mcomisiones_otras",
"mforex_buy",
"mforex_sell",
"mtransferencias_recibidas",
"mtransferencias_emitidas",
"mextraccion_autoservicio",
"mcheques_depositados",
"mcheques_emitidos",
"mcheques_depositados_rechazados",
"mcheques_emitidos_rechazados",
"matm",
"matm_other",
"Master_mfinanciacion_limite",
"Master_msaldototal",
"Master_msaldopesos",
"Master_msaldodolares",
"Master_mconsumospesos",
"Master_mconsumosdolares",
"Master_mlimitecompra",
"Master_madelantopesos",
"Master_madelantodolares",
"Master_mpagado",
"Master_mpagospesos",
"Master_mpagosdolares",
"Master_mconsumototal",
"Master_mpagominimo",
"Visa_mfinanciacion_limite",
"Visa_msaldototal",
"Visa_msaldopesos",
"Visa_msaldodolares",
"Visa_mconsumospesos",
"Visa_mconsumosdolares",
"Visa_mlimitecompra",
"Visa_madelantopesos",
"Visa_madelantodolares",
"Visa_mpagado",
"Visa_mpagospesos",
"Visa_mpagosdolares",
"Visa_mconsumototal",
"Visa_mpagominimo"
)

#ajuste por inflación, nivel base es en marzo para no tener que reentrenar los modelos previos
#recordar que el snapshot de un mes se hace el último día de este mismo
#solo se ajusta enero y mayo
dataset[foto_mes == 202101, (ARScols):= lapply(.SD,"*",1.048*1.036), .SDcols = ARScols]
dataset[foto_mes == 202105, (ARScols):= lapply(.SD,"*",1/(1.033*1.041)), .SDcols = ARScols]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/FE777000b" )

fwrite( dataset, #solo los campos para Kaggle
        file= "./exp/FE777000b/inflation_adjusted_dataset.csv",
        sep=  "," )