
#################################################################
######                                                     ######
###### Master 1 : Modélisation Statistique et Informatique ######
######                                                     ######
###### Projet en Analyse des Composantes principales       ######
###### Présenté par : MAYENE Bienvenue Schékina            ######
######                                                     ######
#################################################################


# Chargement des librairies --------------

library(FactoMineR) # Pour l'ACP
library(factoextra) # Pour Visualiser les graphes de l'ACP
library(corrplot) # Pour la Matrice de corrélation
library(prettyR) #Pour la commande describe au niveau de l'analyse
#univariée
library(questionr) #Pour les tableaux de contingence


# 1. Importation de la base de données 'co2_emission' ------------
co2_emission <- read.csv("co2_emission.csv", 
                         header=TRUE, 
                         sep=",",
                         dec=".",
                         row.names = 1)

"
Lexique des arguments de la commande :

sep =',' : Separateur des colonnes
dec = '.' : Séparateur décimale
header = T: Permet de considerer la 1ere ligne comme entete
row.names : Permet de considerer la variable mark.model qui était en première
position comme le nom des voitures

"

#Visualisation de la base
View(co2_emission)



# * 1.1 Inspection de la base ---------------------------------
dim(co2_emission)   # dimension
names(co2_emission) # nom des variables
str(co2_emission)   # structure ou Type de varaible

#Liste des modalités des variables factorielles
levels(co2_emission$Fuel.Type)
levels(co2_emission$Vehicle.Class)
levels(co2_emission$Transmission)

#Voir le nombre de modalités de nos variables factorielles
nlevels(co2_emission$Fuel.Type)
nlevels(co2_emission$Vehicle.Class)
nlevels(co2_emission$Transmission)



#* 1.2 Recodage de la structure des variables ----------------
# Factoriser les variables nominales ou catégorielles
co2_emission$Vehicle.Class <- as.factor(co2_emission$Vehicle.Class)  
co2_emission$Transmission <- as.factor(co2_emission$Transmission)
co2_emission$Fuel.Type <- as.factor(co2_emission$Fuel.Type)


# Pour voir les nouvelles classes de nos variables qualitatives
# On peut proceder par les deux façon suivantes :

# Structure 
str(co2_emission) 

#Class des variables
class(co2_emission$Fuel.Type)
class(co2_emission$Transmission)
class(co2_emission$Vehicle.Class)


# 1.3 Ordonner la position des variables

# Recueil des variables continues 
ordered_vars <- c("Engine.Size", "Cylinders",
                  "Fuel.Cons.Comb.mpg", "CO2.Emissions",
                  "Fuel.Cons.Comb", "Fuel.Cons.City", 
                  "Fuel.Cons.Hwy", "Vehicle.Class",
                  "Transmission", "Fuel.Type"
)
# Base réordonnée 
co2_emission <- co2_emission[ordered_vars]  

# Structure 
str(co2_emission) 




# * 2 Statistique Descriptive -----------------------

"
Mesures (parametres ou indicateur) de position et de dispersion :

*** Indice de position : la moyenne et la mediane
*** Indice de dispersion : L'écart type et les quartiles

Première méthode avec la commande 'summary(nom_base)' 
"

## * 2.1  Analyse Univariée ---------------------




#** Les Statistiques de base avec 'summary' ----------------------
summary(co2_emission) 
# NB: cette commande ne marche que si nos variables sont dans 
# leur bon type

#Noms colonnes
names(co2_emission)

#Deuxieme methode

# statistique de base avec describe------
describe(co2_emission, num.desc = c("mean", "sd", "median",
                                           "min", "max", "valid.n"))
#valid.n pour nous renseigner si on a des valeurs manquantes



#Etude de la répartition des variables qualitatives----------




# Tableau des frequences  

# Tableau des effectifs de la variable type de transmission du moteur
tab1 = table(co2_emission$Transmission, deparse.level = 2, useNA = "always")
tab1

# Tableau des effectifs de la variable type de carburant 'Fuel.Type'
tab2 = table(co2_emission$Fuel.Type, deparse.level = 2, useNA = "always")
tab2

#Tableau des effectifs de la variable 'Vehicule.Class'
tab3 = table(co2_emission$Vehicle.Class, deparse.level = 2, useNA = "always")
tab3

"deparse.level: permet de specifier à R d'afficher les noms des modalités
 uneNA = permet de spécifier si on a des valeurs manquantes
"
#Tableau des porcentages
# Tableau des pourcentage de la variable type de transmission du moteur
prop_tab = round((prop.table(tab1)*100), digits=2)
prop_tab
# Tableau des pourcentages de la variable type de carburant 'Fuel.Type'
prop_tab2 = round((prop.table(tab2)*100), digits=2)
prop_tab2
#Tableau des pourcentage de la variable 'Vehicule.Class'
prop_tab3 = round((prop.table(tab3)*100), digits=2)
prop_tab3


"
round = permet d'arraondir un chiffre
digits = permet de spécifier le nombre de chiffre après la virgule 
on souhaite afficher 

"
# Propre choix de couleur (couleurs par le code hexadécimal)
my_colors = c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")
my_colors2 = c("#21177D", "#26619C", "#1560BD", "#EFEFEF","#BEF574","#9EFD38","#568203", 
              "#CC5500", "#FFE4C4", "#E73E01", "#EED153", "#FCD21C", "#E8D630",
              "#DB0073" , "#D473D4", "#E73E01")

# Pie plot : Diagramme circulaire de nos variables qualitatives
pie(tab1, main = "Répartition du type de transmission des moteurs", col = my_colors)
pie(tab2, main = "Répartition du type de carburant", col = my_colors2[9:11])
pie(tab3, main = "Répartition des classes de véhicules", col = my_colors2)




## * 2.2  Analyse Bivariée ---------------------
##### Première Etude 

#Tableau de contingence avec 
#la variable fuel.type en ligne (variabl dependante) et Transmission en 
#colonne (variable indépendante)

tableau1 =table(co2_emission$Fuel.Type, co2_emission$Transmission)
tableau1


#** tableaux marginal avec % colonne
tab_prop_col3 = cprop(tableau1)
tab_prop_col3


#**** Tableaux marginaux avec % ligne
tab_prop_row3 = rprop(tableau1)
tab_prop_row3


# Diagramme en barre
barplot(tableau1, beside = T , 
        legend.text = T,
        col = my_colors2[3:5], 
        ylim= c(0, 700),
        main = " Répartition de la transmission des moteurs selon le type de carburant",
        xlab = "Type de transmission des motreurs",
        ylab = "Effectifs")

# Test de comparaison d deux pourcentages 'Test de chi-deux

chisq.test(tableau1 , correct = FALSE)
"argument correct= F, sans cet argument, R nous propose un test de chi-deux 
avec correction de continuité qui est un test plus
robuste mais nettement moins puissant " 

#### Deuxième Etude 

#* Matrice de corrélation des variables continues---------

#Selection des variables continues pour la matrice
continuous_variables = c("Engine.Size", "Cylinders", "Fuel.Cons.Comb", 
                         "Fuel.Cons.Comb.mpg", "CO2.Emissions", 
                         "Fuel.Cons.Hwy", "Fuel.Cons.City")
# Matrice de corrélation
correlation_matrix = cor(co2_emission[continuous_variables])
correlation_matrix

# Corrélogramme ou Graphe de corrélation avec la librairie 'corrplot'
corrplot(correlation_matrix, method = "number")



# 3. Choix des variables actives ou supplémentaies ------------------


#* Variables actives ------------------

# Variables quantitatives actives
names(co2_emission)[1:5]


#* Variables supplémentaies ------------

# Variables quantitatives supplémentaies
names(co2_emission)[6:7]

# Variables qualitatives supplémentaies
names(co2_emission)[8:10]



# 5. Réaliser l'ACP (Analyse en Composantes Principales) ----------


# Position des variables
names(co2_emission)


#* L'ACP des variables actives --------------------

# Créer le graphe des Variables et celui des individus
res <- PCA(co2_emission[279:309, 1:5], scale.unit = T)

# Affichage des résultats globaux de l'ACP
summary(res)

summary(res, nbelements=Inf) # Tout afficher



#* L'ACP avec des variables supplémentaires --------
res <- PCA(co2_emission[279:309, ], scale.unit = T,
           quanti.sup = 6:7,
           quali.sup = 8:10)
# summary(res, nbelements=Inf)



# 6. Choisir le nombre d’axes (dimensions) à interpréter -----------


# Afficher les valeurs propres (Inertie Totale)
res$eig


# Pour aller plus loin : Même chose que la précédente commande
# Afficher les valeurs propres avec la librairie 'factoextra'
val_prop <- get_eigenvalue(res)
head(val_prop)



# Tableau des Valeurs propres 
tab_val_prop <- as.data.frame(res$eig)


#* Diagramme en barres des valeurs propres 
titre <- "Diagramme en barres des Valeurs Propres"
fviz_eig(res, addlabels = TRUE, main = titre)


# 7. Analyser les résultats -----------------------


# L'ACP des variables actives ****************
res <- PCA(co2_emission[279:309, ], quanti.sup = 6:7,
           quali.sup = 8:10)
summary(res)
# Afficher les résultats
summary(res, nbelements=Inf) # Tout afficher 










#* Graphe des individus ----------------------------
(pca_ind <- get_pca_ind(res))


# Afficher les coordonnées des individus
head(pca_ind$coord, 10) # 10 premiers individus 


# Afficher le cos2 (cos carré) pour les individus
head(pca_ind$cos2, 10)


# Contribution des individus  
head(pca_ind$contrib, 10)


# >>> Afficher le graphe des Individus selon leur contribution
fviz_pca_ind(res, 
             col.ind = "contrib",
             gradient.cols = c("yellow", "red" ,"blue"),
             repel = TRUE
)
# repel = TRUE : éviter de surcharger les étiquettes de texte



#### Axe 1------------------------------
# >>> Pour visualiser la contribution des individus a l'axe1
fviz_contrib(res, 
             choice = "ind", 
             axes = 1, 
             top = 30
)

# >>> Pour Visualiser la qualité de représentation (cos2) a l'axe1
fviz_cos2(res, 
          choice = "ind", 
          axes = 1, 
          top = 30
)


##### Axe2----------------------------------


# >>> Pour visualiser la contribution des individus a l'axe2
fviz_contrib(res, 
             choice = "ind", 
             axes = 2, 
             top = 30
)

# >>> Pour Visualiser la qualité de représentation (cos2) a l'axe2
fviz_cos2(res, 
          choice = "ind", 
          axes = 2, 
          top = 30
)


# >>> Visualiser la contribution des individus 
###*** aux deux axes principaux 1 et 2 *******
fviz_contrib(res, 
             choice = "ind",
             axes = 1:2
)


# >>> Visualiser la qualité de représentation 
#***** des individus aux deux axes principaux 1 et 2 *******
fviz_cos2(res, 
          choice = "ind", 
          axes = 1:2
)









#* Graphe des Variables --------------------------------------
(pca_var <- get_pca_var(res))


# Afficher les coordonnées des Variables
head(pca_var$coord, 10) # 10 premiers Variables 


# Afficher la corrélation des Variables avec les axes
head(pca_var$cor, 10)


# Afficher le cos2 (cos carré) pour les Variables
head(pca_var$cos2, 10)


# Contribution des Variables  
head(pca_var$contrib, 10)


# Matrice de corrélation des variables actives
correlation_matrix1 = cor(co2_emission[279:309, 1:5])
correlation_matrix1

# >>> Afficher le graphe des Variables selon leur contribution
fviz_pca_var(
  res, 
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red"),
  repel = TRUE, 
  title = "Cercle de corrélation des Variables
           selon leur contribution"
)

# repel = TRUE : Évite le chevauchement de texte


# >>> Afficher le graphe des Variables selon leur cos2
# Qualité de représentation
fviz_pca_var(
  res, 
  col.var = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE, 
  title = "Cercle de corrélation des Variables selon 
           leur Qualité de représentation (cos2)"
)
# Afficher le cos2 ders variables sur les deux axes
library("corrplot")
corrplot(pca_var$cos2, is.corr=FALSE)




# >>> Visualiser la qualité de représentation 
# des variables sur les deux axes principaux 1 et 2 *******
fviz_cos2(res, 
          choice = "var", 
          axes = 1:2
)














##Variables supplémentaire -----------------






#Variables quantitatives supplémentaire

#Résultats prédites (coordonnées, corrélation et cos2) pour 
#les variables quantitatives supplémentaires:

res$quanti.sup
#Visualiser toutes les variables quantitatives supplémentaire
fviz_pca_var(res)

##Variables qualitatives supplémentaire

#Visualisation des individus selon les modalités 
#de la variable
fviz_pca_ind(res, habillage = (8),
             addEllipses =TRUE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE) 

fviz_pca_ind(res, habillage = (9),
             addEllipses =TRUE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE) 

fviz_pca_ind(res, habillage = (10),
             addEllipses =TRUE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE) 







#* Graphe biplot (Variables-Individus) ---------------------
#à commenter dans le projet
# Graphe Individus-Variables
fviz_pca_biplot(res, 
                col.var = "#2E9FDF",   # Couleur des variables
                col.ind = "#696969",   # Couleur des individues
                repel = TRUE, 
                title = "Graphe Individus-Variables"
)


#### FIN
