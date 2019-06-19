install.packages("data.table")
install.packages("dplyr")
install.packages("tidyr")
install.packages("formattable")
install.packages("ggplot2")
install.packages("readr")
install.packages("reshape")
install.packages("ggthemes")
install.packages("ggplot2")
library(data.table)
library(formattable)
library(dplyr)
library(reshape)
library(tidyr)
library(formattable)
library(ggthemes)
library(plyr)
library(readr)
library(ggplot2)
robo_vandalismo_new<- separate(robo_vandalismo, "fecha", c("año", "mes", "dia"), sep = "-")
robo_estados<- subset(robo_vandalismo_new, subset=categoria=="Robo a Tren")

estados<- robo_estados %>%
   group_by( año, estado) %>%
     summarise(no_obs, count= n())%>% 
     arrange(año, no_obs)
estados <- count(robo_estados, c('año','estado'))


formattable(estados$año=="2016")
t<- cast(estados, estado~año)
t<- reshape(estados,idvar="estado", timevar=(c("año")), direction = "wide")
estados_wide <- spread(estados, año, freq)
formattable(estados_wide)
estados_wide[order("2017"),]
sort(estados_wide$`2016`)
estados_2016<- sort(estados_wide$`2016`)
estados_mean<-estados %>%
  group_by(estado) %>%
  summarise_at(vars(freq), 
               funs(mean(., na.rm=TRUE)))

estados_2016<- subset(estados, subset= año=="2016")
estado_id<- c("CM", "CO", "GT", "HG", "EM", "MI", "NL", "QT", "SLP", "TB",
              "TM", "VE")
factor_estado_id<- factor(estado_id)
estados_2016$edo_id  <- c("CM", "CO", "GT", "HG", "EM", "MI", "NL", "QT", "SLP", "TB",
                          "TM", "VE")
estados_2016_graph<- ggplot(estados_2016, aes(x=factor(edo_id), y=freq,fill=freq))+
  ggtitle("Número de Incidencias sobre Robo en el Sistema Ferroviario Mexicano", subtitle = "2016")+
  geom_col(aes(colour=freq))+
  xlab("Estados")+
  ylab("Número de Incidencias")+
  labs(caption="Elaboración propia con datos oficiales")+
  theme(plot.title = element_text(size=10),
        axis.text.x = element_text(size=8, angle=90),
        plot.subtitle = element_text(size=8, color="black", hjust=0.5),
        legend.position = "none")
estados_2016_graph
estados_2017<- subset(estados, subset= año=="2017")
estados_2017_graph<- ggplot(estados_2017, aes(x=factor(estado), y=freq,fill=freq))+
  ggtitle("Número de Incidencias sobre Robo en el Sistema Ferroviario Mexicano", subtitle = "2017")+
  geom_col( aes(colour=freq))+
  xlab("Estados")+
  ylab("Número de Incidencias")+
   theme(plot.title = element_text(size=10),
        axis.text.x = element_text(size=8, angle=90),
        plot.subtitle = element_text(size=8, color="black", hjust=0.5),
        legend.position = "none")

estados_2018<- subset(estados, subset= año=="2018")
estados_2018$edo_id  <- c("OA", "DG", "BC", "NA",  "MI", "TM", "CH","CM", 
                          "NL", "SL", "SI", "VE", "ZA","QT", "AG",  "HG", "TL", 
                          "EM", "SO", "CO",  "JC", "GT", "PU")
estados_2018_graph<- ggplot(estados_2018, aes(x=factor(estado), y=freq,fill=freq))+
  ggtitle("Número de Incidencias sobre Robo en el Sistema Ferroviario Mexicano", subtitle = "2018")+
  geom_col(aes(colour=freq))+
  xlab("Estados")+
  ylab("Número de Incidencias")+
  theme(plot.title = element_text(size=10),
        axis.text.x = element_text(size=8, angle=90),
        plot.subtitle = element_text(size=8, color="black", hjust=0.5),
        legend.position = "none")
#Gráficos con temas 
estados_2018_graph +  theme(axis.text.x=element_text(face="bold", size=8, angle=90))  
estados_2018_graph + theme_economist()

#Mostrar todos los gráficos en un mismo plot 
grid.arrange(estados_2016_graph, estados_2017_graph, estados_2018_graph, nrow = 1)

#Gráfica general 
graph_total<- ggplot(estados, aes(x=factor(estado), y=freq, group=año))+
  geom_line(aes(color=año), size=0.8)
graph_total+ theme(axis.text.x=element_text(face="bold", size=8, angle=90))
graph_total+ scale_color_economist()

#Gráficas a partir de una propia df 
cargo<-c("dip_nac", "dip_dist", "diputado_parlacen", "diputado_parlacen_sup", "alcalde",
         "sindico_tit", "sindico_supl","concejal_tit", "concejal_supl")
candidatas<-c(90, 536, 44 , 45, 209, 798, 473, 3078, 1230)
candidatos<-c(251, 1441,61, 67, 2794, 4971, 1899,11023, 3530)
candidatos_df<-data.frame(cargo, candidatas, candidatos)
candidatos_df$total<-candidatos_df$candidatas + candidatos_df$candidatos
candidatos_df$por_mujeres<-candidatos_df$candidatas/candidatos_df$total *100
candidatos_df$por_hombres<-candidatos_df$candidatos/candidatos_df$total *100
cand_por<-subset(candidatos_df, select=c(cargo, por_hombres, por_mujeres))

cand_long<- cand_por %>% gather( expre, value, -cargo)
por_graph<- ggplot(cand_long, aes(x=factor(cargo), y=value, group=factor(expre)))+
  geom_line(aes( color=expre), size=1.2)+
  scale_color_manual(values = c("blue",
                                "purple"))+
  geom_text(aes(label = round(value, 1)),
            vjust = "inward", hjust = "inward",
            show.legend = FALSE)+
  scale_x_discrete(labels=c( "alcalde", "con_sup", "conc_tit", "dip_dist", "dip_nac", "dip_cen",
                             "dip_cen_sup" , "sin_supl", "sin_tit"))+
  theme(axis.text.x = element_text(face="bold",  
                                   size=8, angle=45))
por_graph
por_graph + 
  ggtitle("Porcentaje de candidatas por puesto de elección", subtitle = "Elecciones 2019") +
  labs(x="Cargo", y="Porcentaje") 



