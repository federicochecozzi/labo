library("tidyverse")

#setwd( "~/buckets/b1" )
setwd( "C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022" )


tb_opt1 <- read.delim("./exp/DA777001z/tb_optimos.txt")
tb_opt2 <- read.delim("./exp/DA777002z/tb_optimos.txt")
tb_opt3 <- read.delim("./exp/DA777003z/tb_optimos.txt")

tb_opt1alt <- read.delim("./exp/DA777001zalt/tb_optimos.txt")
tb_opt2alt <- read.delim("./exp/DA777002zalt/tb_optimos.txt")
tb_opt3alt <- read.delim("./exp/DA777003zalt/tb_optimos.txt")

dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/GA77700xz/" , showWarnings = FALSE )
setwd( "./exp/GA77700xz/" )   #Establezco el Working Directory DEL EXPERIMENTO

png( "Comparación de ganancias 2019.jpg" )

bind_rows("delta1" = tb_opt1,"bsofd" = tb_opt2, "tend6" = tb_opt3,.id = "Feature") %>%
  ggplot(aes(x = gan, color = Feature, fill = Feature)) +
  geom_density(alpha = 0.2) +
  ggtitle("Comparación de ganancias, entrenamiento = 201904, prueba = 201906") + 
  theme(legend.position="bottom")

dev.off()

png( "Comparación de ganancias 2021.jpg" )

bind_rows("delta1" = tb_opt1alt,"bsofd" = tb_opt2alt, "tend6" = tb_opt3alt,.id = "Feature") %>%
  ggplot(aes(x = gan, color = Feature, fill = Feature)) +
  geom_density(alpha = 0.2) +
  ggtitle("Comparación de ganancias, entrenamiento = 202103, prueba = 202105") + 
  theme(legend.position="bottom")

dev.off()