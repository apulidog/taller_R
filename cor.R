library(reshape2)
library(dplyr)
marg<- marg[-c(2), ]  
#Cargo bases 
colnames(hom)[colnames(hom) == 'X__1'] <- 'ano'
hom_2010 <- subset(hom, ano==2010)
#Cambio la estructura de mi base homicidios 
hom_melt<- melt(hom_2010, id.vars=c("ano"))
colnames(hom_melt)[colnames(hom_melt) == 'Entidad Federativa'] <- 'Entidad federativa'
colnames(hom_melt)[colnames(hom_melt) == 'value'] <- 'hom'
marg_hom <- merge(hom_melt, marg,by=("Entidad federativa"))
colnames(marg)[colnames(marg) == 'Índice de marginación'] <- 'marg'
colnames(marg_hom)[colnames(marg_hom) == 'Índice de marginación'] <- 'marg'
#Simple plot de datos 
plot(marg_hom$hom,  marg_hom$marg)
cor(marg_hom)
cor.test(marg_hom$hom, marg_hom$marg) 
#Paquetes para analizar correlación
install.packages("corrgram")
library(corrgram)
cor(marg_hom$hom,marg_hom$marg)
#Método Kendall: encontar pares dispares en datos cualitativos a los que tenemos que asignar valores (ej. fumar y ejercicio) 
cor(marg_hom$hom,marg_hom$marg, method="kendall")
install.packages("ggpubr")
library("ggpubr")
ggscatter(marg_hom, x = "marg", y = "hom", 
          , conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          )
