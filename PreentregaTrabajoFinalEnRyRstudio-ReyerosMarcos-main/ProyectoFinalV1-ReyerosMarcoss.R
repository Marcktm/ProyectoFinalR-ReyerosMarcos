#install.packages(“tidyverse”)
library(tidyverse)

data <- read.csv("./Mlbb_Heroes+ReyerosMarcos.csv", header = TRUE, sep = ",")

select(data, c(Primary_Role, Secondary_Role))

mutate(data, Lane = as.factor(Lane), Mitad_De_Vida = Hp/2)

transmute(data, Mitad_De_vida = Hp/2)

rename(data, Nombre = Name, Rol_Primario = Primary_Role)

filter(data, Hp > 2400 & Hp <= 2500)


#Utilizando pipe
#Muestra que personajes son los del año de Lanzamiento del juego
data %>% 
  select(Name, Release_Date) %>% 
  filter(Release_Date == 2016) %>% 
  mutate(Relevancia = "LANZAMIENTO") %>% 
  select(Name, Release_Date, Relevancia )

#Muestra Que rol tiene mas resistencia En este caso el rol "Tank"
data %>% 
  group_by(Primary_Role) %>%
  summarise(meanHp = mean(Hp)) %>% 
  ungroup()
  
data %>% 
  select(Name, Secondary_Role) %>% 
  filter(Secondary_Role != "Tank" & 
           Secondary_Role != "Mage" & Secondary_Role != "Support" & 
           Secondary_Role != "Fighter" & Secondary_Role != "Assassin" & 
           Secondary_Role != "Marksman" ) %>% 
  mutate(Secondary_Role = "NoTieneOtroRol") %>% 
  select(Name, Secondary_Role)

