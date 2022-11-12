library("tidyverse")
setwd( "C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022\\exp\\GA777001c" )

df1_modelo1<- grep("modelo_01",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows() 
df1_modelo2<- grep("modelo_02",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows()
df1_modelo3<- grep("modelo_03",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows() 
df1_modelo4<- grep("modelo_04",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows()
df1_modelo5<- grep("modelo_05",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows()

bind_rows("h1_m1" = df1_modelo1,"h1_m2" = df1_modelo2,"h1_m3" = df1_modelo3,"h1_m4" = df1_modelo4,"h1_m5" = df1_modelo5,.id ="modelo") %>%
  ggplot(aes(x = corte,y = ganancia, color = modelo)) +
  geom_point(alpha = 0.3) +
  stat_summary(
    geom = "line",
    fun.y = "mean",
    size = 3,
  )

df1_modelo3 %>% ggplot(aes(x = corte,y = ganancia)) +
  geom_point(alpha = 0.3) +
  stat_summary(
    geom = "line",
    fun.y = "mean",
    size = 3,
  )

setwd( "C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022\\exp\\GA777002c" )

df2_modelo1<- grep("modelo_01",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows() 
df2_modelo2<- grep("modelo_02",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows()
df2_modelo3<- grep("modelo_03",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows() 
df2_modelo4<- grep("modelo_04",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows()
df2_modelo5<- grep("modelo_05",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows()

bind_rows("h2_m1" = df2_modelo1,"h2_m2" = df2_modelo2,"h2_m3" = df2_modelo3,"h2_m4" = df2_modelo4,"h2_m5" = df2_modelo5,.id ="modelo") %>%
  ggplot(aes(x = corte,y = ganancia, color = modelo)) +
  geom_point(alpha = 0.3) +
  stat_summary(
    geom = "line",
    fun.y = "mean",
    size = 3,
  )

setwd( "C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022\\exp\\GA9410" )

df3_modelo1<- grep("modelo_01",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows() 
df3_modelo2<- grep("modelo_02",list.files(path = "."),value=TRUE) %>% 
  lapply(read.delim) %>% 
  bind_rows()

bind_rows("h3_m1" = df3_modelo1,"h3_m2" = df3_modelo2,.id ="modelo") %>%
  ggplot(aes(x = corte,y = ganancia, color = modelo)) +
  geom_point(alpha = 0.3) +
  stat_summary(
    geom = "line",
    fun.y = "mean",
    size = 3,
  )

#ronda final
bind_rows("h1_m3" = df1_modelo3,"h2_m3" = df2_modelo3,"h3_m1" = df3_modelo1,.id ="modelo") %>%
  ggplot(aes(x = corte,y = ganancia, color = modelo)) +
  geom_point(alpha = 0.3) +
  stat_summary(
    geom = "line",
    fun.y = "mean",
    size = 3,
  )


#no son significativamente diferentes
wilcox.test(df1_modelo3[df1_modelo3$corte == 9000,"ganancia"],df2_modelo3[df1_modelo3$corte == 9500,"ganancia"])
