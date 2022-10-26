library("tidyverse")
setwd( "C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022" )

tb_opt1 <- read.delim("./exp/DA777008b/tb_optimos.txt")
tb_opt2 <- read.delim("./exp/DA777004balt/tb_optimos.txt")

summary(tb_opt1)
summary(tb_opt2)

bind_rows("BO con varias semillas" = tb_opt1,"BO con una semilla, ajustado por inflación" = tb_opt2, .id = "Modelo") %>%
  ggplot(aes(x = public_gan, y = private_gan, color = Modelo)) +
  geom_point() +
  theme(legend.position="bottom")