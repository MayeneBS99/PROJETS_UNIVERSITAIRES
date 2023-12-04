
###### PROJET PROFESSIONNEL
##### MAYENE Bienvenue Schekina


library("ggplot2")  # Visualisation des données
library("dplyr")    # Manipulation des données

library(tidyverse)
install.packages("ggforce")
library(ggforce)
library(scales)

## Observer le pourcentage d'individu s'étant fait vacciner au moins une fois

count.data <- data.frame(
  vaccin1 = c("Oui", "Non"),
  n = c(54638435, 13225135),
  prop = c(80.5, 19.5)
)
count.data


# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(vaccin1)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
count.data


mycols <- c("#00FF00", "#008000")

ggplot(count.data, aes(x = "", y = prop, fill = vaccin1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()

### L’ÉVOLUTION DU NOMBRE DE PERSONNE AYANT REÇU LA PREMIÈRE DOSE 
### DE VACCIN DEPUIS LE DÉBUT DE LA VACCINATION JUSQU’AU 13/01
 
data <- read_delim("Desktop/MASTER II MIASHS/PROJET_PRO/BASES_ACT/vacsi-fra-2023-01-13-19h01.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

data

plot(data$jour, data$n_dose1, type = "l" , col = "blue", 
     main = "Evolution du nombre de personne ayant pris la 1ère dose de vaccin",
     xlab= "Nombre de personne ")


ggplot(data   = data,              # spécifier les données
       mapping = aes(x = jour,    # mapper 'displ' à l'axe des x
                     y = n_dose1)) +   # mapper 'hwy' à l'axe des y
  geom_line(color = "blue", linetype = 1) + labs(
    title    = "Evolution du nombre de personne ayant pris la 1ère dose de vaccin",
    subtitle = "Période allant du 27/12/2020 - 13/01/23",
    x        = "Jours",
    y        = "Personnes"
    ) + theme_bw()


#ggplot(data, aes(y = n_dose1)) + geom_density()


### Couverture vaccinale homme vs femme 


library(readr)
data2 <- read_delim("Desktop/MASTER II MIASHS/PROJET_PRO/BASES_ACT/vacsi-s-fra-2023-01-13-19h02.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)


maxis = c()

data2_bis1 = subset(data2, sexe ==1)
data2_bis2 = subset(data2, sexe ==2)
plot(data2_bis1$jour, data2_bis1$couv_dose1, type = "l" , col = "blue", 
     main = "Evolution du nombre de personne ayant pris la 1ère dose de vaccin",
     xlab= "Nombre de personne ")
lines(data2_bis2$jour, data2_bis2$couv_dose1, type = "l", col = "red")

### Amélioration du graphique

ggplot(data   = data2_bis1,              # spécifier les données
       mapping = aes(x = jour,    # mapper 'displ' à l'axe des x
                     y = couv_dose1)) +   # mapper 'hwy' à l'axe des y
  geom_line(color = "blue", linetype = 1) +
  geom_line(mapping = aes(x = jour, y = couv_dose1), data = data2_bis2, col = "orange") +
  labs(
    title    = "Evolution de la prise de la 1ère dose selon le sexe",
    subtitle = "Période allant du 27/12/2020 - 13/01/23",
    x        = "Jours",
    y        = "Personnes"
  ) + theme_bw()



### Vaccination selon l'indice de défavorisation et la tranche d'age

library(readr)
data3 <- read_delim("Desktop/MASTER II MIASHS/PROJET_PRO/BASES/donnees-de-vaccination-par-indice-de-defavorisation-par-tranche-dage.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

### Exemple pour comprendre
x = c(1,2,3) 
y = c(2,4,2) 
type = c("test a","test b","test c") 
moyennes = c(x,y) 
moyennes = matrix(moyennes,nc=3, nr=2, byrow=T) # nc : nombre de tests - nr : nombre de barres accolées (ici par paire) 
colnames(moyennes) = type 
barplot(moyennes,beside=T) ; box() 

### Réalisation du barplot
indice <- read_delim("donnees-de-vaccination-par-indice-de-defavorisation-par-tranche-dage.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(indice)
variable.names(indice)

ourdata<-subset(indice[,c(7,8,10)])
ourdata<-na.omit(ourdata)

ourdata2 <- pivot_wider(ourdata, names_from = classe_age, values_from = effectif_1_inj, values_fn = sum)
typeof(ourdata2)  
as.matrix(ourdata2)
ourdata3<- ourdata2[c(1,6,2),c(1:length(ourdata2)-1)]

ourdata3 <- t(ourdata3)
rownames(ourdata3) <- ourdata3[,1]
ourdata3 <- ourdata3[,-1]
ourdata4<- ourdata3[,-1]
barplot(as.matrix(ourdata3),beside=T) 

barplot(as.matrix(ourdata4), beside=T, col=c("red","blue","green"), legend=(ourdata3$decile_defavorisation), ylim =c(0,6000000), xlab = " Classe d'âge",
        ylab = " Nombre de première injection")
title("Effectif de première injection de vaccin par déclie de défavorisation")



library(tibble)
library(dplyr)

# convert the row names to new column
df <- rownames_to_column(df, var = "new_rows")

# remove the original column
df <- select(df,-1)

library(ggplot2)

# Create a dataframe from the matrix
df <- data.frame(ourdata3)

# Plot the barplot using ggplot2
ggplot(df, aes(x=rownames(df), y=ourdata3, fill=rownames(df))) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_brewer(palette="Paired") +
  labs(x="Categories", y="Values", fill="Categories") +
  theme_classic() +
  theme(legend.position = "top")




#### Evolution du nombre dce décès du au covid-19

library(readr)
data4 <- read_delim("Desktop/MASTER II MIASHS/PROJET_PRO/BASES_ACT/covid-hosp-txad-age-fra-2023-01-16-19h02.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
data4_bis = subset(data4, clage_90 == 0)
data4_bis= subset(data4_bis, PourAvec == 0)



ggplot(data   = data4_bis,              # spécifier les données
       mapping = aes(x = jour,    # mapper 'displ' à l'axe des x
                     y = tx_indic_7J_DC)) +   # mapper 'hwy' à l'axe des y
  geom_line(color = "black", linetype = 1) +
  geom_line(mapping = aes(x = jour, y = tx_indic_7J_hosp), data = data4_bis, col = "orange") +
  geom_line(mapping = aes(x = jour, y = tx_indic_7J_SC), data = data4_bis, col = "red")+
  labs(
    title    = "Evolution Taux de personnes décédées durant les 7 derniers jours (pour 100 000 hab.)",
    subtitle = "Période allant du 07/03/2020 - 12/01/23",
    x        = "Jours",
    y        = "Taux"
  ) + theme_bw()

data4_bis2 = subset(data4, PourAvec == 0)
data4_bis2$clage_90 = as.integer(data4_bis2$clage_90)

new_classe = c()
for(i in 1:length(data4_bis2$clage_90)){
  if(data4_bis2$clage_90[i]>0 & data4_bis2$clage_90[i] <20 ){new_classe[i] = 1}
  else if(data4_bis2$clage_90[i] >20 & data4_bis2$clage_90[i] <40 ){new_classe[i] = 2}
  else if(data4_bis2$clage_90[i] >40 & data4_bis2$clage_90[i] <60 ){new_classe[i] = 3}
  else if(data4_bis2$clage_90[i] >60 ){new_classe[i] = 4}
  
}
data4_bis2$new_classe = new_classe

dataa = subset(data4_bis2, new_classe == 1)
datab = subset(data4_bis2, new_classe == 2)
datac = subset(data4_bis2, new_classe == 3)
datad = subset(data4_bis2, new_classe == 4)


ggplot(data   = dataa,              
       mapping = aes(x = jour,   
                     y = tx_indic_7J_DC)) +  
  geom_line(color = "black", linetype = 1) +
  labs(
    title    = "Evolution Taux de personnes décédées durant les 7 derniers jours (pour 100 000 hab.)",
    subtitle = "Période allant du 07/03/2020 - 12/01/23",
    x        = "Jours",
    y        = "Taux"
  ) + theme_bw()

plot(datad$jour, datad$tx_indic_7J_DC, type = "l")

### Amélioration du grapgique avec spécification des dates d'apparition des variants

library(readr)
library(ggplot2)
data4 <- read_delim("covid-hosp-txad-age-fra-2023-01-16-19h02.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(hosp)
data4_bis = subset(data4, clage_90 == 0)
data4_bis= subset(data4_bis, PourAvec == 0)



ggplot(data   = data4_bis,              # spécifier les données
       mapping = aes(x = jour,    # mapper 'displ' à l'axe des x
                     y = tx_indic_7J_DC)) +   # mapper 'hwy' à l'axe des y
  geom_line(color = "blue", linetype = 1) +
  geom_line(mapping = aes(x = jour, y = tx_indic_7J_hosp), data = data4_bis, col = "orange") +
  labs(
    title    = "Evolution Taux de personnes décédées durant les 7 derniers jours (pour 100 000 hab.)",
    subtitle = "Période allant du 07/03/2020 - 12/01/23",
    x        = "Jours",
    y        = "Taux"
  ) + theme_bw()

# specify the dates of the variants' appearances
Première_Vague <- as.Date("2020-03-07")
Variant_Alpha <- as.Date("2020-09-15")
Variant_Beta_Gamma <- as.Date("2021-01-20")
Variant_Delta <- as.Date("2021-07-05")
Variant_Omicro <- as.Date("2021-12-01")


ggplot(data   = data4_bis,              
       mapping = aes(x = jour,    
                     y = tx_indic_7J_DC)) +   
  geom_line(color = "blue", linetype = 1) +
  geom_line(mapping = aes(x = jour, y = tx_indic_7J_hosp), data = data4_bis, col = "orange") +
  geom_line(mapping = aes(x = jour, y = tx_indic_7J_SC), data = data4_bis, col = "red") +
  geom_text(x = 0, y = max(data4_bis$tx_indic_7J_DC), label = "Décédées", color = "blue", hjust = -0.1) +
  geom_text(x = 0, y = max(data4_bis$tx_indic_7J_hosp), label = "Hospitalisées", color = "orange", hjust = -0.1) +
  geom_text(x = 0, y = max(data4_bis$tx_indic_7J_SC), label = "Soins Critiques", color = "red", hjust = -0.1) +
  geom_vline(xintercept = Première_Vague, linetype = "dashed", color = "black", size = 1) +
  geom_text(x = Première_Vague, y = max(data4_bis$tx_indic_7J_DC), label = "", angle = 90, vjust = 1, color = "black") +
  geom_vline(xintercept = Variant_Alpha, linetype = "dashed", color = "black", size = 1) +
  geom_text(x = Variant_Alpha, y = max(data4_bis$tx_indic_7J_DC), label = "", angle = 90, vjust = 1, color = "black") +
  geom_vline(xintercept = Variant_Beta_Gamma, linetype = "dashed", color = "black", size = 1) +
  geom_text(x = Variant_Beta_Gamma, y = max(data4_bis$tx_indic_7J_DC), label = "", angle = 90, vjust = 1, color = "black") +
  geom_vline(xintercept = Variant_Delta, linetype = "dashed", color = "black", size = 1) +
  geom_text(x = Variant_Delta, y = max(data4_bis$tx_indic_7J_DC), label = "", angle = 90, vjust = 1, color = "black") +
  geom_vline(xintercept = Variant_Omicro, linetype = "dashed", color = "black", size = 1) +
  geom_text(x = Variant_Omicro, y = max(data4_bis$tx_indic_7J_DC), label = "", angle = 90, vjust = 1, color = "black") +
  labs(
    title    = "Evolution Taux de personnes hospitalisées, en soins critiques ou \ndécédées durant les 7 derniers jours (pour 100 000 hab.)",
    subtitle = "Période allant du 07/03/2020 - 12/01/23",
    x        = "Jours",
    y        = "Taux"
  ) + theme_bw()











####----------------------------------- Autre étude
library(readxl)
data <- read_excel("Desktop/MASTER II MIASHS/PROJET_PRO/BASES_ACT/data.xlsx")
View(data) 


##------------------------------------- Nombre de doses de vaccin reçues pour les 3 pays 

# France
base_bis = subset(data, ReportingCountry== "FR")
base_bis = subset(base_bis, TargetGroup== "ALL")
base_bis$Vaccine = as.factor(base_bis$Vaccine)

for(i in 1:length(base_bis$NumberDosesReceived)){
  if(is.na(base_bis$NumberDosesReceived[i]) == TRUE){
    base_bis$NumberDosesReceived[i] = 0}
  if (is.na(base_bis$NumberDosesExported[i]) == TRUE){
    base_bis$NumberDosesExported[i]= 0
  }
  
}

# Group by sum of multiple columns
dffr <- base_bis %>% group_by(Vaccine) %>% 
  summarise(across(c(NumberDosesReceived, NumberDosesExported),sum),
            .groups = 'drop') %>%
  as.data.frame()


##### Italy
base_bis1 = subset(data, ReportingCountry== "IT")
base_bis1 = subset(base_bis1, TargetGroup== "ALL")
base_bis1$Vaccine = as.factor(base_bis1$Vaccine)

for(i in 1:length(base_bis1$NumberDosesReceived)){
  if(is.na(base_bis1$NumberDosesReceived[i]) == TRUE){
    base_bis1$NumberDosesReceived[i] = 0}
  if (is.na(base_bis1$NumberDosesExported[i]) == TRUE){
    base_bis1$NumberDosesExported[i]= 0
  }
 
}

# Group by sum of multiple columns
dfit <- base_bis1 %>% group_by(Vaccine) %>% 
  summarise(across(c(NumberDosesReceived, NumberDosesExported),sum),
            .groups = 'drop') %>%
  as.data.frame()





# Suède
base_bis2 = subset(data, ReportingCountry== "SE")
base_bis2 = subset(base_bis2, TargetGroup== "ALL")
base_bis2$Vaccine = as.factor(base_bis2$Vaccine)

for(i in 1:length(base_bis2$NumberDosesReceived)){
  if(is.na(base_bis2$NumberDosesReceived[i]) == TRUE){
    base_bis2$NumberDosesReceived[i] = 0}
  if (is.na(base_bis2$NumberDosesExported[i]) == TRUE){
    base_bis2$NumberDosesExported[i]= 0
  }
  
}

# Group by sum of multiple columns
dfse <- base_bis2 %>% group_by(Vaccine) %>% 
  summarise(across(c(NumberDosesReceived, NumberDosesExported),sum),
            .groups = 'drop') %>%
  as.data.frame()




#graphiques des 3 pays pour les doses de vaccins reçues
library(cowplot)

dffr = dffr[-c(3,4,6,8,9),]
FR <- ggplot(data=dffr, aes(x=Vaccine, y=NumberDosesReceived)) +
  geom_bar(stat="identity")+
  coord_cartesian(ylim = c(0, 150000000))+ labs(caption = "France")




dfit = dfit[-c(3,4,5,7,8),]
IT<-ggplot(data=dfit, aes(x=Vaccine, y=NumberDosesReceived)) +
  geom_bar(stat="identity")+
  coord_cartesian(ylim = c(0, 150000000))+ labs(caption = "Italie")



SU<-ggplot(data=dfse, aes(x=Vaccine, y=NumberDosesReceived)) +
  geom_bar(stat="identity")+
  coord_cartesian(ylim = c(0, 150000000))+ labs(caption = "Suède")


#labels=c("France", "Italy","Suède")
plot_grid(FR, IT,SU, ncol = 3, nrow = 1)+ labs(
  title    = "Nombre de dose de vaccin reçu")+ theme_bw()



# graphiques des 3 pays pour les doses de vaccins exportées^
library(cowplot)
dffr = dffr[-c(3,4,6,8,9),]
FR2 <- ggplot(data=dffr, aes(x=Vaccine, y=NumberDosesExported)) +
  geom_bar(stat="identity")+
  coord_cartesian(ylim = c(0, 1000000))+ labs(caption = "France")


IT2<-ggplot(data=dfit, aes(x=Vaccine, y=NumberDosesExported)) +
  geom_bar(stat="identity")+
  coord_cartesian(ylim = c(0, 1000000))+ labs(caption = "Italie")


SU2<-ggplot(data=dfse, aes(x=Vaccine, y=NumberDosesExported)) +
  geom_bar(stat="identity")+
  coord_cartesian(ylim = c(0, 1000000))+ labs(caption = "Suède")


#labels=c("France", "Italy","Suède")
plot_grid(FR2, IT2,SU2, ncol = 3, nrow = 1)+ labs(
  title    = "Nombre de dose de vaccin exporté"
)+ theme_bw()






########## Nombre de 1ère dose de vaccin reçues pour les 3 pays 

# France
base_bis3 = subset(data, ReportingCountry== "FR")

for(i in 1:length(base_bis3$FirstDose)){
  if(is.na(base_bis3$FirstDose[i]) == TRUE){
    base_bis$FirstDose[i] = 0}
  if (is.na(base_bis3$SecondDose[i]) == TRUE){
    base_bis3$SecondDose[i]= 0
  }
  
}

# Group by sum of multiple columns
dffr1 <- base_bis3 %>% group_by(TargetGroup) %>% 
  summarise(across(c(FirstDose, SecondDose),sum),
            .groups = 'drop') %>%
  as.data.frame()


##### Italy
base_bis4 = subset(data, ReportingCountry== "IT")

for(i in 1:length(base_bis4$FirstDose)){
  if(is.na(base_bis4$FirstDose[i]) == TRUE){
    base_bis4$FirstDose[i] = 0}
  if (is.na(base_bis4$SecondDose[i]) == TRUE){
    base_bis4$SecondDose[i]= 0
  }
  
}

# Group by sum of multiple columns
dfit1 <- base_bis4 %>% group_by(TargetGroup) %>% 
  summarise(across(c(FirstDose, SecondDose),sum),
            .groups = 'drop') %>%
  as.data.frame()



# Suède
base_bis5 = subset(data, ReportingCountry== "SE")

for(i in 1:length(base_bis5$FirstDose)){
  if(is.na(base_bis5$FirstDose[i]) == TRUE){
    base_bis5$FirstDose[i] = 0}
  if (is.na(base_bis5$SecondDose[i]) == TRUE){
    base_bis5$SecondDose[i]= 0
  }
  
}

# Group by sum of multiple columns
dfse1 <- base_bis5 %>% group_by(TargetGroup) %>% 
  summarise(across(c(FirstDose, SecondDose),sum),
            .groups = 'drop') %>%
  as.data.frame()





#graphiques des 3 pays pour les 1ère doses 
library(cowplot)

dffr1 = dffr1[-c(8,9),]
FR <- ggplot(data=dffr1, aes(x=TargetGroup, y=FirstDose)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ coord_cartesian(ylim = c(0, 50000000))+ labs(caption = "France")


dfit1 = dfit1[-c(8,9,10),]
IT<-ggplot(data=dfit1, aes(x=TargetGroup, y=FirstDose)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ coord_cartesian(ylim = c(0, 50000000))+ labs(caption = "Italie")


dfse1 = dfse1[-c(8,9,10),]
SU<-ggplot(data=dfse1, aes(x=TargetGroup, y=FirstDose)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ coord_cartesian(ylim = c(0, 50000000))+ labs(caption = "Suède")


#labels=c("France", "Italy","Suède")
plot_grid(FR, IT,SU, ncol = 3, nrow = 1)+ labs(
  title    = "Nombre de première dose de vaccin administrée selon les classes d'age"
) + theme_bw()







# graphiques des 3 pays pour la 2e dose


FR3 <- ggplot(data=dffr1, aes(x=TargetGroup, y=SecondDose)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ coord_cartesian(ylim = c(0, 50000000))+ labs(caption = "France")


IT3<-ggplot(data=dfit1, aes(x=TargetGroup, y=SecondDose)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ coord_cartesian(ylim = c(0, 50000000))+ labs(caption = "Italie")


SU3<-ggplot(data=dfse1, aes(x=TargetGroup, y=SecondDose)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ coord_cartesian(ylim = c(0, 50000000))+ labs(caption = "Suède")


#labels=c("France", "Italy","Suède")
### Graphique final
plot_grid(FR3, IT3,SU3, ncol = 3, nrow = 1)+ labs(
  title    = "Nombre de seconde dose de vaccin administrée selon les classes d'age"
) + theme_bw()




####----------------------------------- Autre étude
library(readxl)
data2 <- read_excel("Desktop/MASTER II MIASHS/PROJET_PRO/BASES_ACT/data2.xlsx")
View(data2)  

data2 =subset(data2, indicator==  "deaths")

data2fr = subset(data2, country_code == "FRA")
data2it = subset(data2, country_code == "ITA")
data2se = subset(data2, country_code == "SWE")

for(i in 1:length(data2fr$weekly_count)){
  if(is.na(data2fr$weekly_count[i]) == TRUE){
    data2fr$weekly_count[i] = 0}
  if(is.na(data2fr$cumulative_count[i]) == TRUE){
    data2fr$cumulative_count[i] = 0}
  }

for(i in 1:length(data2it$weekly_count)){
  if(is.na(data2it$weekly_count[i]) == TRUE){
    data2it$weekly_count[i] = 0}
  if(is.na(data2it$cumulative_count[i]) == TRUE){
    data2it$cumulative_count[i] = 0}
}

for(i in 1:length(data2se$weekly_count)){
  if(is.na(data2se$weekly_count[i]) == TRUE){
    data2se$weekly_count[i] = 0}
  if(is.na(data2se$cumulative_count[i]) == TRUE){
    data2se$cumulative_count[i] = 0}
}


### Graphique 1
str(data2fr)
typeof(data2fr$year_week)
data2fr$year_week = as.factor(data2fr$year_week)

ggplot(data   = data2fr,             
       mapping = aes(x=seq(1:159),y = weekly_count)) +  
  geom_line(color = "blue", linetype = 1)+
  geom_line(mapping = aes(x = seq(1:159), y = weekly_count), data = data2it, col = "red") +
  geom_line(mapping = aes(x = seq(1:159), y = weekly_count), data = data2se, col = "#808000") +
  labs(
    title    = "Evolution du nombre de décès par semaine",
    x        = "Semaines",
    y        = "Decès"
  ) + theme_bw() 


### Graphique 2

ggplot(data   = data2fr,             
       mapping = aes(x=seq(1:159),y = cumulative_count)) +  
  geom_line(color = "blue", linetype = 1)+
  geom_line(mapping = aes(x = seq(1:159), y = cumulative_count), data = data2it, col = "red") +
  geom_line(mapping = aes(x = seq(1:159), y = cumulative_count), data = data2se, col = "#808000") +
  labs(
    title    = "Evolution du nombre total de décès ",
    x        = "Semaines",
    y        = "Decès"
  ) + theme_bw()+ scale_fill_manual(values = c('green','blue', 'red')) 





##### ALPES MARITIMES 


" Here is an example of an R script that uses the forecast package to make predictions using 
time series analysis:"
# First, we'll need to install the forecast package if it isn't already installed
install.packages("forecast")

# Load the package
library(forecast)



# Load your data into a variable, let's call it "data"
library(ggplot2)
library(readr)
pred <- read_delim("dep_covid-hospit-2023-01-20-19h00.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(pred)
names(pred)
pred<-subset(pred,sexe==0 & dep=="06")

pred<-subset(pred, select = -c(dep, sexe))
names(pred)

# Calculons le nombre de décès par jour dans les Alpes maritimes à partir des décès cumulés
# de la variable pred$dc

pred$dc_nc[1] <- 0
for (i in 2:length(pred$dc)){
  pred$dc_nc[i]=pred$dc[i]-pred$dc[i-1]
}
pred$dc_nc[1]<-2

"1. Prediction du taux de personnes hospitalisées (pour 100.000 hbts)"
# Convert the data into a time series object
ts_data <- ts(pred$hosp, start = c(2020,03,18), frequency = 365)

# Fit an ARIMA model to the data
fit <- auto.arima(ts_data)

# Make predictions for the next 1 months
predictions <- forecast(fit, h = 30)

# Plot the predictions along with the actual data
plot(predictions, col = "orange", main = "Prediction du nombre de personnes hospitalisées \ndans les Alpes Maritimes",
     xlab="Temps", ylab="Nombre d'hospitalisations")

# Create a dataframe from the predictions
predictions_df <- data.frame(predictions)

# Create a line plot of the predictions
ggplot(predictions_df, aes(x = 1:nrow(predictions_df), y = Point.Forecast)) +
  geom_line(size = 2, color = 'blue') +
  geom_ribbon(aes(x = 1:nrow(predictions_df), ymin = Lo.80, ymax = Hi.80), alpha = 0.2) +
  xlab('Temps') +
  ylab("Nombre d'hospitalisations") +
  ggtitle('Prediction du nombre de personnes hospitalisées \ndans les Alpes Maritimes') +
  theme_minimal()


"2. Prediction du nombre de personnes décédées "
# Convert the data into a time series object
ts_data2 <- ts(pred$dc_nc, start = c(2020,03,18), frequency = 365)

# Fit an ARIMA model to the data
fit2 <- auto.arima(ts_data2)
# ou
fit2 <- ets(ts_data2)
fit2 <- stlf(ts_data2)



# Make predictions for the next 30 days
predictions2 <- forecast(fit2, h = 30)

# Plot the predictions along with the actual data
plot(predictions2, col = "red", main = "Prediction du nombre de personnes décédées \ndans les Alpes Maritimes",
     xlab="Temps", ylab="Nombre de personnes décédées")

# Create a dataframe from the predictions
predictions2_df <- data.frame(predictions2)

# Create a line plot of the predictions
ggplot(predictions2_df, aes(x = 1:nrow(predictions2_df), y = Point.Forecast)) +
  geom_line(size = 2, color = 'blue') +
  geom_ribbon(aes(x = 1:nrow(predictions2_df), ymin = Lo.80, ymax = Hi.80), alpha = 0.2) +
  xlab('Temps') +
  ylab("Nombre de personnes décédées") +
  ggtitle('Prediction du nombre de personnes décédées \ndans les Alpes Maritimes') +
  theme_minimal()

# Evolution des hospitalisations et décès dans les Alpes Maritimes
ggplot(data   = pred,              
       mapping = aes(x = jour,    
                     y = hosp)) +   
  geom_line(color = "blue", linetype = 1) +
  geom_line(mapping = aes(x = jour, y = dc_nc), data = pred, col = "red") +
  labs(
    title    = "Evolution du nombre de personnes hospitalisées et \ndécédées ",
    subtitle = "Période allant du 18/03/2020 - 20/01/23",
    x        = "Jours",
    y        = "Nombre de personnes"
  ) + theme_bw()



