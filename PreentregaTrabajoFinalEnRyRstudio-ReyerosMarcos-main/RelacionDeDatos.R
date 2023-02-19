install.packages("arules")
install.packages("arulesViz")
install.packages("lubridate")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("magrittr")
install.packages("e1071")
install.packages("caTools")
install_github("vqv/ggbiplot")
install.packages('ggplot2', dependencies = TRUE)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(psych)
library(devtools)
library(ggbiplot)
library(arules)
library(arulesViz)
library(lubridate)
library(gridExtra)
library(devtools)
library(e1071)
library(caTools)
library(class)
library(ggplot2)

data_inicial <- read.csv("Mlbb_Heroes+ReyerosMarcos.csv" , sep = ",")
summary(data_inicial)

data_inicial <- data_inicial %>% 
  mutate(
    Primary_Role = factor(Primary_Role),
    Release_Date = as_date(Release_Date, format= "%d/%m/%Y")
  ) 
rownames(data_inicial) <- data_inicial$Name
borrar2 <- c("Title", "Voice_Line", "Secondary_Role", "Lane", "Hp", 
             "Hp_Regen", "Mana", "Mana_Regen", "Phy_Damage", "Mag_Damage",
             "Phy_Defence", "Mag_Defence", "Mov_Speed", "Esport_Wins",
             "Esport_Loss")

data_inicial <- data_inicial[ , !names(data_inicial) %in% borrar2]

data_cvs <- data_inicial %>% 
  arrange(Name) %>% 
  group_by(Release_Date, Name) %>% 
  mutate(
    rolList = glue::glue_collapse(glue::glue("{Primary_Role}"), sep = ",")
  )

data_cvs$Name <- NULL
data_cvs$Release_Date <- NULL
data_cvs$Primary_Role <- NULL
data_cvs$rolList <- as.character(data_cvs$rolList)

write.csv(data_cvs, "RolList.csv", quote = FALSE, row.names = TRUE)
  

lanzamientos <- read.transactions(
  "RolList.csv",
  rm.duplicates = TRUE,
  format= "basket",
  sep=",",
  cols=1
)

lanzamientos@itemInfo$labels <- gsub("\"", "" , lanzamientos@itemInfo$labels)

itemFrequencyPlot(
  lanzamientos,
  topN = 6, 
  type = "absolute",
  main = "Frecuencia Absoluta"
)

itemFrequencyPlot(
  lanzamientos,
  topN = 7, 
  type = "relative",
  ylab="Frecuency (relative)",
  main = "Frecuencia Relativa"
)

data_inicial <- read.csv("Mlbb_Heroes+ReyerosMarcos.csv" , sep = ",")

data_inicial %>% 
  mutate(
    Month = as.factor(month(Release_Date))
  ) %>% 
  group_by(Month) %>% 
  dplyr::summarize(Lanzamientos = n()) %>% 
  ggplot(aes(x = Month, y = Lanzamientos)) +
  geom_bar(stat = 'identity', fill = "mistyrose1", show.legend = FALSE, color = "black")+
  ylab("Lanzamientos")+
  xlab("Mes")+
  ggtitle("Lanzamientos por Mes")+
  theme_bw()

data_inicial %>% 
  mutate(
    Year = as.factor(year(Release_Date))
  ) %>% 
  group_by(Year) %>% 
  dplyr::summarize(Lanzamientos = n()) %>% 
  ggplot(aes(x = Year, y = Lanzamientos)) +
  geom_bar(stat = 'identity', fill = "mistyrose1", show.legend = FALSE, color = "black")+
  ylab("Lanzamientos")+
  xlab("Año")+
  ggtitle("Lanzamientos por Año")+
  theme_bw()


supportLevels <- c(0.1, 0.05, 0.01, 0.005, 0.0001 )
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05)

rules_sup10 <- integer(length = 10)
rules_sup5 <- integer(length = 10)
rules_sup1 <- integer(length = 10)
rules_sup0.5 <- integer(length = 10)
rules_sup0.1 <- integer(length = 10)

for(i in 1:length(confidenceLevels)){
  rules_sup10[i] <- length(
    apriori(
      lanzamientos,
      parameter = list(
        minlen = 1,
        sup = supportLevels[1],
        conf = confidenceLevels[i],
        target = "rules"
        
      )
    )
  )
}

for(i in 1:length(confidenceLevels)){
  rules_sup5[i] <- length(
    apriori(
      lanzamientos,
      parameter = list(
        minlen = 1,
        sup = supportLevels[1],
        conf = confidenceLevels[i],
        target = "rules"
        
      )
    )
  )
}

for(i in 1:length(confidenceLevels)){
  rules_sup1[i] <- length(
    apriori(
      lanzamientos,
      parameter = list(
        minlen = 1,
        sup = supportLevels[1],
        conf = confidenceLevels[i],
        target = "rules"
        
      )
    )
  )
}

for(i in 1:length(confidenceLevels)){
  rules_sup0.5[i] <- length(
    apriori(
      lanzamientos,
      parameter = list(
        minlen = 1,
        sup = supportLevels[1],
        conf = confidenceLevels[i],
        target = "rules"
        
      )
    )
  )
}

for(i in 1:length(confidenceLevels)){
  rules_sup0.1[i] <- length(
    apriori(
      lanzamientos,
      parameter = list(
        minlen = 1,
        sup = supportLevels[1],
        conf = confidenceLevels[i],
        target = "rules"
        
      )
    )
  )
}

iteracion_parametros <- purrr::map_dfr(
  .x = supportLevels,
  .f = function(x) {
    
    purrr::map(
      .x = confidenceLevels,
      .f = function(.x) {
        
        data.frame(
          support = x,
          confidence = .x,
          rules = length(
            apriori(lanzamientos, parameter = list(minlen = 1, sup = x, conf = .x, target = "rules")
            )
          )
        )
      }
    )
  }
)


plot1 <- gplot(
  confidenceLevels,
  rules_sup10,
  geom = c("point", "line"),
  xlab = "Confidence level", ylab = "Number of rules found",
  main = "Apriori whit a support level of 10%"
) +
  theme_bw()

