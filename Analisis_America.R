#El presente proyecto tiene como objetivo explorar los
#Resultados del Club América del año 2013 al 2023

library(data.table)
library (tidyverse)
library(ggplot2)

setwd("~/Documentos/GitHub/America")

america <- read.csv("America_Data.csv", header= TRUE, sep=",")
#Las siguientes lineas extraen la data del america se pueden usar para
#Extraer la data de otros equipos del CSV MEX
#Data <- read.csv("MEX.csv", header= TRUE, sep=",")
#america <- filter(Data, Home == "Club America" | Away == "Club America")
#america <- select(america, Season, Date, Home, Away, HG, AG, Res )                  
#write.csv(america, "America_Data.csv", row.names = F)

#La siguinete linea regresa si el club jugó local o visitante
#america <- america %>% 
  #mutate(HorA=as.character(ifelse(america$Home=="Club America", "H", "A")))

#Esta linea regresa si ganó, perdió o empató el juego 

#america <- america %>%
 # mutate(Resultado=as.character(ifelse(america$HorA==america$Res,"G",
  #                                   ifelse(america$Res=="D","E", "P"))))

#Las siguientes lineas regresan columnas señalando los juesgos G, E, P

#america <- america %>%
 # mutate(Ganados=as.numeric(ifelse(america$Resultado=="G", 1, 0)))
#america <- america %>%
 # mutate(Perdidos=as.numeric(ifelse(america$Resultado=="P", 1, 0)))
#america <- america %>%
 # mutate(Empatados=as.numeric(ifelse(america$Resultado=="E", 1, 0)))

#Las siguinete linea regresa una columna con el rival 

#america <- america %>%
 # mutate(Rival=as.character(ifelse(america$Home=="Club America", 
  #                                 america$Away, america$Home)))
#Las siguientes lineas crean una variable con los partidos ganados
#por año futbolistico 

america <- america %>%
  group_by(Season) %>% 
  mutate(JuGan=sum(Ganados)) 

#Las siguientes lineas crean un grafico de los partidos ganados 
#en cada año futbolistico.
Plot_JuGan <- ggplot(america, aes(x=Season, y=JuGan, group=1))+
  ggtitle("Partidos Ganados por Año Futbolístico")+
  theme(plot.title=element_text(size=20, face='bold', color='Black',hjust=0.5),
        axis.title.x=element_text(size=15, face='bold', color='Black'),
        axis.title.y=element_text(size=15, face='bold', color='Black'))+
  xlab('Año Futbolístico') +
  ylab('Partidos Ganados')+
  geom_line()+
  geom_point()

Plot_JuGan
ggsave(filename = "Plot_JuGan.pdf",
      width =40 , height = 20, units = "cm", dpi = 600)

