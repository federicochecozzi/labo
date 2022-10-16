setwd( "C:\\Users\\tiama\\OneDrive\\Documentos\\DMEyF_2022" )

tb_1    <- read.delim("./exp/HT777001z/HT777001z.txt")
tb_1alt <- read.delim("./exp/HT777001zalt/HT777001zalt.txt")
tb_2    <- read.delim("./exp/HT777002z/HT777002z.txt")
tb_2alt <- read.delim("./exp/HT777002zalt/HT777002zalt.txt")
tb_3    <- read.delim("./exp/HT777003z/HT777003z.txt")
tb_3alt <- read.delim("./exp/HT777003zalt/HT777003zalt.txt")

print("1")

tb_1[tb_1$ganancia == max(tb_1$ganancia),]

print("1alt")

tb_1alt[tb_1alt$ganancia == max(tb_1alt$ganancia),]

print("2")

tb_2[tb_2$ganancia == max(tb_2$ganancia),]

print("2alt")

tb_2alt[tb_2alt$ganancia == max(tb_2alt$ganancia),]

print("3")

tb_3[tb_3$ganancia == max(tb_3$ganancia),]

print("3alt")

tb_3alt[tb_3alt$ganancia == max(tb_3alt$ganancia),]