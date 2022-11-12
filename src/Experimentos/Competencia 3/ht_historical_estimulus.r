library("tidyverse")
setwd( "C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022\\exp\\HT777001c" )

bo_log <- read.delim("BO_log.txt")

bo_log %>% ggplot(aes(x = iteracion_bayesiana,y = estimulos)) +
  geom_point()
