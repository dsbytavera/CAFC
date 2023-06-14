#El presente proyecto tiene como objetivo explorar los
#Resultados del Club América del año 2013 al 2023
#install.packages("wesanderson")

library(data.table)
library (tidyverse)
library(wesanderson)

setwd("~/GitHub/CAFC")

#america <- read.csv("America_Data.csv", header= TRUE, sep=",")
#Las siguientes lineas extraen la data del america se pueden usar para
#Extraer la data de otros equipos del CSV MEX
Data <- read.csv("MEX.csv", header= TRUE, sep=",")
america <- filter(Data, Home == "Club America" | Away == "Club America")
america <- select(america, Season, Date, Home, Away, HG, AG, Res )                  
write.csv(america, "America_Data.csv", row.names = F)

#La siguinete linea regresa si el club jugó local o visitante
america <- america %>% 
  mutate(HorA=as.character(ifelse(america$Home=="Club America", "H", "A")))

#Esta linea regresa si ganó, perdió o empató el juego 

america <- america %>%
  mutate(Resultado=as.character(ifelse(america$HorA==america$Res,"G",
                                     ifelse(america$Res=="D","E", "P"))))

#Las siguientes lineas regresan columnas señalando los juegos G, E, P

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
#Las siguientes lineas crean una variable con los partidos ganados
#por año futbolistico 

america <- america %>%
  group_by(Season) %>% 
  mutate(JuGan=sum(Ganados)) 

#Las siguientes lineas crean una variable con los partidos ganados
#por rival 

america <- america %>%
  group_by(Rival) %>% 
  mutate(JuGanRival=sum(Ganados)) 

#Las siguientes lineas crean una variable con los partidos Perdidos
#por rival 

america <- america %>%
  group_by(Rival) %>% 
  mutate(JuPerRival=sum(Perdidos)) 

#Las siguientes lineas crean una variable con los partidos Perdidos
#por rival 

america <- america %>%
  group_by(Rival) %>% 
  mutate(JuEmpRival=sum(Empatados)) 

#Las siguientes lineas crean una variable con partidos jugados por rival

america <- america %>%
  group_by(Rival) %>% 
  mutate(JuePorRiv= n ()) 

#Las siguientes lineas crean una variable con el total de juegos jugados por 
#año futbolistico 
america <- america %>%
  group_by(Season) %>% 
  mutate(JueJu = n()) 
#La siguiente linea calcula la proporcion de juegos ganados 

america <- america %>%
  group_by(Season) %>% 
  mutate(PPganados = JuGan / JueJu)


#Creando funcion para reemplazar un valor en un a columna. 

reemplazar_valor <- function(data, variable, valor_original, valor_reemplazo) {
  data[[variable]] <- ifelse(data[[variable]] == valor_original, 
                             valor_reemplazo, data[[variable]])
  return(data)
}

#A continuacion se reemplaza el nombre de los equipos por uno mas corto.

america <- reemplazar_valor(america, "Rival", "Atl. San Luis", "San Luis")
america <- reemplazar_valor(america, "Rival", "Club Leon", "Leon")
america <- reemplazar_valor(america, "Rival", "Club Tijuana", "Tijuana")
america <- reemplazar_valor(america, "Rival", "Dorados de Sinaloa", "Dorados")
america <- reemplazar_valor(america, "Rival", "Guadalajara Chivas", "Chivas")
america <- reemplazar_valor(america, "Rival", "Lobos BUAP", "BUAP")
america <- reemplazar_valor(america, "Rival", "Mazatlan FC", "Mazatlan")
america <- reemplazar_valor(america, "Rival", "Santos Laguna", "Santos")
america <- reemplazar_valor(america, "Rival", "U.A.N.L.- Tigres", "Tigres")
america <- reemplazar_valor(america, "Rival", "U.N.A.M.- Pumas", "Pumas")
america <- reemplazar_valor(america, "Rival", "Leones Negros", "UDG")



#------------------------------------------------------------------------------#
#############--En Esta sección se encuentran los graficos--##################### 

#Las siguientes lineas crean un grafico de la proporcion de partidos ganados 
#en cada año futbolistico.
Plot_PPganados <- ggplot(america, aes(x=Season, y=PPganados, group=1))+
  geom_line(color = wes_palette(n=1, name = "Zissou1"))+
  geom_point(color = wes_palette(n=1, name = "Zissou1"))+
  ggtitle("Proporción de Partidos Ganados por Año Futbolístico")+
  xlab('Año Futbolístico') +
  ylab('Proporción Partidos Ganados')+
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        panel.grid = element_line(color="#272829"),
        plot.title=element_text(hjust=0.5, color="#f0f8ff", family='mono', size=20),
        axis.title.x=element_text(color="#f0f8ff", family='mono', size=15),
        axis.title.y=element_text(color="#f0f8ff", family='mono', size=15),
        axis.text = element_text(color="#f0f8ff", family='mono', size=10))
 

Plot_PPganados
ggsave(filename = "Plot_PPganados.pdf",
       width =40 , height = 20, units = "cm", dpi = 600)


#Las siguientes lineas crean un grafico de los partidos ganados 
#en cada año futbolistico.
Plot_JuGan <- ggplot(america, aes(x=Season, y=JuGan, group=1))+
  geom_line(color = wes_palette(n=1, name = "Moonrise1"))+
  geom_point(color = wes_palette(n=1, name = "Moonrise1"))+
  ggtitle("Partidos Ganados por Año Futbolístico")+
  xlab('Año Futbolístico') +
  ylab('Partidos Ganados')+
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        panel.grid = element_line(color="#272829"),
        plot.title=element_text(hjust=0.5, color="#f0f8ff", family='mono', size=20),
        axis.title.x=element_text(color="#f0f8ff", family='mono', size=15),
        axis.title.y=element_text(color="#f0f8ff", family='mono', size=15),
        axis.text = element_text(color="#f0f8ff", family='mono', size=10))
  
Plot_JuGan

ggsave(filename = "Plot_JuGan.pdf",
      width =40 , height = 20, units = "cm", dpi = 600)

#Las siguientes lineas crean un grafico de los partidos ganados 
#en cada año futbolistico. 

# Ordenar el conjunto de datos por la columna Rival
america <- america[order(america$Rival),]

# Graficar utilizando ggplot2

Plot_Equipos <- ggplot(america, aes(x = Rival, group = 1)) +
  geom_point(aes(x = Rival, y = JuGanRival, color = "Ganados")) +
  geom_point(aes(x = Rival, y = JuPerRival, color = "Perdidos")) +
  geom_point(aes(x = Rival, y = JuEmpRival, color = "Empatados")) +
  geom_line(aes(y = JuGanRival, color = "Ganados")) +
  geom_line(aes(y = JuPerRival, color = "Perdidos")) +
  geom_line(aes(y = JuEmpRival, color = "Empatados")) +
  labs(x = "Rivales", y = "Resultados", color = " ", 
       title='Resultados Club América Últimos 10 Años') +
  scale_color_manual(values = wes_palette(n = 3, name = "Darjeeling1"))+
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        panel.grid = element_line(color="#272829"),
        axis.title.x = element_text(color="#f0f8ff", family='mono', size=15),
        axis.title.y = element_text(color="#f0f8ff", family='mono', size=15),
        plot.title=element_text(hjust=0.5, color="#f0f8ff", family='mono', size=20),
        axis.text = element_text(color="#f0f8ff", family='mono', size=10),
        legend.text = element_text(color="#f0f8ff", family='mono', size = 11),
        legend.background = element_rect (fill= "#000000"))

Plot_Equipos

ggsave(filename = "Plot_Equipos.pdf",
       width =40 , height = 20, units = "cm", dpi = 600)