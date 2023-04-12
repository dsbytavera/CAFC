#El presente proyecto tiene como objetivo explorar los
#Resultados del Club América del año 2013 al 2023

library(data.table)
library (tidyverse)
library(ggplot2)

setwd("~/Documentos/GitHub/America")

america <- read.csv("America_Data.csv", header= TRUE, sep=",")
#Las siguientes lineas extraen la data del america se pueden usar para
#Extraer la data de otros equipos del CSV MEX
#america <- filter(Data, Home == "Club America" | Away == "Club America")
#america <- select(america, Season, Date, Home, Away, HG, AG, Res )                  
#write.csv(america, "America_Data.csv", row.names = F)

#La siguinete linea regresa si el club jugo local o visitante
america <- america %>% 
  mutate(HorA=as.character(ifelse(america$Home=="Club America", "H", "A")))

#Esta linea regresa si ganó, perdió o empató el juego 

america <- america %>%
  mutate(Resultado=as.character(ifelse(america$HorA==america$Res,"G",
                                     ifelse(america$Res=="D","E", "P"))))

#Las siguientes lineas regresan columnas señalando los juesgos G, E, P

america <- america %>%
  mutate(Ganados=as.numeric(ifelse(america$Resultado=="G", 1, 0)))
america <- america %>%
  mutate(Perdidos=as.numeric(ifelse(america$Resultado=="P", 1, 0)))
america <- america %>%
  mutate(Empatados=as.numeric(ifelse(america$Resultado=="E", 1, 0)))

#Las siguinete linea regresa una columna con el rival 

america <- america %>%
  mutate(Rival=as.character(ifelse(america$Home=="Club America", 
                                   america$Away, america$Home)))
ganados_af<- america %>% group_by(Season) %>% summarise(jg=sum(Ganados))

gaf_plot <- ggplot(ganados_af,
                   aes(x=Season, y=jg))+
  geom_point()
gaf_plot
