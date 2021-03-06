library(MASS)
library(klaR)
library(ade4)

#######################################################################################################
#Fichier de base des trois esp�ces, peltatus, trichophyllus et penicillatus, sans NA, 77 individus, 
#Rajouter dans la ligne suivante le chemin vers le fichier C:...
RanunculusBase<-read.table("C:\\Mettre ici le chemin pour arriver au fichier\\RanunculusBase2020.txt", header=T)
#Pour faire en sorte que les nom de variables puisse �tre utilis�es
attach(RanunculusBase)
#Pour lister les variables (pour contr�le) afin de connaitre leurs noms
names(RanunculusBase)

#Assignation de trois couleurs qui seront utilis�es pour chacune des esp�ces
couleurs<-c("darkorange1","royalblue3","green")
couleurs

#Assignation de trois symboles qui peuvent �galement �tre utilis�s pour chacune des esp�ces
#16=rond plein, 15=carr� plein et 17=triangle plein
symbole<-c(pch=21, pch=15, pch=17)
symbole

#Liste des symboles en fonction des esp�ces dans le fichier RanunculusTest
symbole2<-c(pch=1, pch=1, pch=1, pch=1, pch=0, pch=0, pch=0, pch=2, pch=2, pch=2)
symbole2

#Analyse discriminante sans cross validation (CV=F). 
#La parenth�se [,5:14] indique qu'on travaille avec les variable n�5 � 14
#Le facteur discriminant est la variable ESPECE
lc <-lda(RanunculusBase[,5:14], grouping=ESPECE, CV=F)
lc

#Calcul des coordonn�es sur les axes discriminants et des probabilit�s � post�riori
plc <- predict(lc)
#Donne les corrdonn�es des individus selon les deux axes discriminants
plc
#Donne l'attribution de chaque individu � une classe � post�riori
plc$class

#Affichage de la table de confusion: en ligne la classification a priori et en colonne les assignations 
#� posteriori
table(RanunculusBase$ESPECE,plc$class)

#Identification de la ou les variables les plus discriminantes et attribution � l'objet disc
disc<-stepclass(RanunculusBase[,5:14], ESPECE, method = "lda")
disc

#On projette (ouvrir le script plotg et copier-coller le contenu dans la Console)

plot(plc$x, # Plot des deux axes discriminants contenus dans plc$x
     pch=21, # 21 correspond � un rond vide,
     bg = couleurs[RanunculusBase$ESPECE], # Colorie les ronds vides selon ESPECE et les 3 couleurs
     xlab=list("Premier axe discriminant (75.1%)", cex=1), #Titre de l'axe X et taille donn�e par cex (>1 augmente; <1 diminue)
     ylab=list("Deuxi�me axe discriminant (24.9%)", cex=1), #Titre de l'axe Y et taille donn�e par cex (>1 augmente; <1 diminue)
     las=1) #Donne l'orientation des chiffres sur les axes (valeurs 0, 1, 2 ou 3)

#On peut utiliser la commande x11(); si l'on veut que le graphique s'affiche dans une fen�tre � part 
s.class(plc$x, # Plot des deux axes discriminants contenus dans plc$x 
        RanunculusBase$ESPECE, col=couleurs, # Colorie les ronds selon ESPECE et les 3 couleurs
        clabel=1, # Si non NULL, une taille de caract�re pour les labels des groupes
        #label=RanunculusBase$COLLECTEUR,
        axesell=F, # Une valeur logique (T ou F) indicant si l'axe des ellipses doit �tre dessin� 
        cstar=1, # Un chiffre entre 0 and 1 qui d�finit la longueur des traits vers les points
        cellips=1.96, # Donne les ellipse avec 95% des valeurs dans l'ellipse
        xax=1, # Le num�ro de la colonne � utiliser en X
        yax=2, # Le num�ro de la colonne � utiliser en Y
        grid=T, # Quadrillage du graphique
        addaxes = T, # Dessine les axes 
        sub= "          ", # Donne un titre au graphique
        possub= "topright", # Donne la position du titre
        ylim=c(-5,5)) # Etend les limites de Y � -5 et +5 (� ajuster en fonction des r�sultats)
               
s.class(plc$x, 
        RanunculusBase$ESPECE, pch=symbole1, # Donne les points selon ESPECE et les 3 symboles choisis
        clabel=1, 
        axesell=F, 
        cstar=1, 
        cellips=1.96, 
        sub= "Analyse lin�aire discriminante sur 77 individus", # Donne un titre au graphique
        possub= "topright", # Donne la position du titre
        xax=1,         
        yax=2, 
        grid=T, 
        ylim=c(-5,5))

#Si on veut les variances sur les deux axes
lc$svd^2
#Pour les correlations sur les 2 axes discriminants
lc$svd^2/sum(lc$svd^2)
#Pour obtenir le pourcentage de variation expliqu�e par chacun des axes discriminants
100*lc$svd^2/sum(lc$svd^2)

###############################################################################################################
#Fichier des individus douteux sans NA
RanunculusTest<-read.table("C:\\Mettre ici le chemin vers le fichier\\RanunculusTest2020.txt", header=T)
#Pour faire en sorte que les nom de variables puisse �tre utilis�es
attach(RanunculusTest)
#Pour lister les variables (pour contr�le) afin de connaitre leurs noms
names(RanunculusTest)

#Comparaison des individus douteux sur la base de l'analyse LDA pr�c�dente
plctest <- predict(lc, RanunculusTest[,5:14])
#Donne les corrdonn�es des individus selon les deux axes discriminants
plctest
#Donne l'attribution de chaque individu � une classe � post�riori
plctest$class
#Affichage de la table de confusion: en ligne la classification a priori et en colonne les assignations 
#� posteriori
table(RanunculusTest$ESPECE,plctest$class)

#Donne un graphe des individus douteux
s.class(plctest$x, RanunculusTest$ESPECE, col=couleurs, clabel=1, axesell=F, cstar=1, cellips=1.96, xax=1, yax=2, grid=T, ylim=c(-5,5))
s.class(plctest$x, RanunculusTest$ESPECE, pch=symbole2, clabel=1, axesell=F, cstar=1, cellips=1.96, xax=1, yax=2, grid=T, ylim=c(-5,5))


#############################################################################################
#Fichier de base des trois esp�ces, peltatus, trichophyllus et penicillatus, sans NA, 77 individus, 
#Validation croisee type jackknife. Dans ce cas, il n'est pas possible de generer des graphiques car
#la fonction ne donne pas les coordonnees sur les axes.
lc1<-lda(RanunculusBase[,5:14], RanunculusBase$ESPECE, CV=T)
#Donne les probabilit� �a posteriori d'appartenir � chacune des classes
lc1
#Affichage de la table de confusion: en ligne la classification a priori et en colonne les assignations 
#� posteriori
table(RanunculusBase$ESPECE,lc1$class)

