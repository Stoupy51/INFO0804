
rm(list = ls())

#### Classifieurs Régression logistique binaire, ADL, ADQ et k-NN ####

#chargement des données heart.xlsx
library(xlsx)
heart <- read.xlsx(file.choose(), sheetIndex = 1, 
                   header = TRUE, stringsAsFactors = TRUE)

str(heart)

#on transforme les variables explicatives qualitatives en variables numérique
XX <- model.matrix(coeur ~., data = heart)[,-1] #cette fonction construit la matrice de design en remplaçant 
                                                #chacune des variables qualitatives pour les indicatrices 
                                                #de ses modalités (la première modalité est supprimée)
                                                #on supprime la première colonne correspondant à l'intercept

heart.num.data <- cbind(as.data.frame(XX), coeur = as.factor(heart[,"coeur"])) 
#bd constituée que de variables explicatives numériques 
#et une variable réponse qualitative (binaire) 

str(heart.num.data)

#On compare les 4 classifieurs en terme de l'erreur de classification, 
#estimée par validation croisée : train + test
#On découpe l'échantillon en deux parties : train + test
index <- 1:nrow(heart.num.data)
l <- 3 #(l-1)/l d'observations pour l'apprentissage et 1/l observations pour le test
test.set.index <- sample(x = index, size = trunc(length(index)/l), replace = FALSE)
heart.test.set <- heart.num.data[test.set.index,]
heart.train.set <- heart.num.data[- test.set.index,]

#### erreur du classifieur RegLog ####
modele.RegLog <- glm(formula = coeur ~ ., data = heart.train.set, family = binomial)
pred.proba <- predict(object = modele.RegLog, newdata = heart.test.set, type = "response")
pred.moda <- factor(ifelse(pred.proba > 0.5,"presence","absence"))
err.classif.RegLog <- mean(!(pred.moda == heart.test.set$coeur))
err.classif.RegLog

library(MASS)
#erreur du classifieur ADL :
modele.ADL <- lda(formula = coeur ~ ., data = heart.train.set)
ADL.pred <- predict(object = modele.ADL, newdata = heart.test.set)
err.classif.ADL <- mean(!(ADL.pred$class == heart.test.set$coeur))
print(err.classif.ADL)

#### erreur du classifieur ADQ : ####
modele.ADQ <- qda(formula = coeur ~ ., data = heart.train.set)
ADQ.pred <- predict(object = modele.ADQ, newdata = heart.test.set)
err.classif.ADQ <- mean(!(ADQ.pred$class == heart.test.set$coeur))
print(err.classif.ADQ)

#### erreur du classifieur k-NN : ####
library(class)
train.X <- heart.train.set[,!(colnames(heart.train.set) == c("coeur"))]
test.X <- heart.test.set[,!(colnames(heart.test.set) == c("coeur"))]
tab.X <- rbind(train.X,test.X) #tableau des variables explicatives
# on normalise les variables explicatives (pour une contribution équilibrée des variables 
# dans la sélection des voisins)
standardized.X <- scale(tab.X)
ind = 1:nrow(train.X)
train.X <- standardized.X[1:nrow(train.X),]
test.X <- standardized.X[- ind,]
train.coeur <- heart.train.set[,(colnames(heart.train.set) == c("coeur"))]
#
#recherche du nombre optimal k de voisins
err.classif.KNN <- NULL
for (k in seq(from = 1, to = 23, by = 1) )
{  
knn.pred <- knn(train = train.X, test = test.X,  cl = train.coeur, prob = TRUE, k = k)
err.classif.KNN[k] <- mean(!(knn.pred == heart.test.set$coeur))
}

plot(err.classif.KNN, type="b")
k_opt <- which.min(err.classif.KNN)
k_opt

knn.pred.opt <- knn(train = train.X, test = test.X, cl = train.coeur, prob = TRUE, k = k_opt)
err.classif.KNN_opt <- mean(!(knn.pred.opt == heart.test.set$coeur))
err.classif.KNN_opt

##comparaison
err.classif.RegLog
err.classif.ADL
err.classif.ADQ
err.classif.KNN_opt

#######################################################################

#### Classifieurs (multi-class) RegLogMultinom, ADL, ADQ et k-NN : iris data ####
str(iris) 
levels(iris$Species)

#on cherche à prédire les 3 modalités de la variable réponse ``Species'' en fonction
#de quatre variables explicatives numériques

#On compare les 4 classifieurs en terme de l'erreur de classification 
#estimée par validation croisée : train + test
#On découpe l'échantillon en deux parties : train + test

index <- 1:nrow(iris)
l <- 3 #(l-1)/l d'observations pour l'apprentissage et 1/l observations pour le test
test.set.index <- sample(x = index, size = trunc(length(index)/l), replace = FALSE)
iris.test.set <- iris[test.set.index, ]
iris.train.set <- iris[- test.set.index, ]

#### erreur du classifieur RegLogMultinom ####
library(nnet)
modele.RegLogM <- multinom(formula = Species ~ ., data = iris.train.set, maxit = 3000) 
RegLogM.pred <- predict(object = modele.RegLogM, newdata = iris.test.set)
err.classif.RegLogM <- mean(!(RegLogM.pred == iris.test.set$Species))
print(err.classif.RegLogM)

#### erreur du classifieur ADL : ####
modele.ADL <- lda(formula = Species ~ ., data = iris.train.set)
ADL.pred <- predict(object = modele.ADL, newdata = iris.test.set)
err.classif.ADL <- mean(!(ADL.pred$class == iris.test.set$Species))
print(err.classif.ADL)

#### erreur du classifieur ADQ : ####
modele.ADQ <- qda(formula = Species ~ ., data = iris.train.set)
ADQ.pred <- predict(object = modele.ADQ, newdata = iris.test.set)
err.classif.ADQ <- mean(!(ADQ.pred$class == iris.test.set$Species))
print(err.classif.ADQ)

#### erreur du classifieur k-NN : ####
train.X <- iris.train.set[,!(colnames(iris.train.set) == c("Species"))]
test.X <- iris.test.set[,!(colnames(iris.test.set) == c("Species"))]
tab.X <- rbind(train.X,test.X) #tableau des variables explicatives
# on normalise les variables explicatives (pour une contribution uniforme des variables 
# dans la sélection des voisins)
standardized.X <- scale(tab.X)
ind = 1:nrow(train.X)
train.X <- standardized.X[1:nrow(train.X), ]
test.X <- standardized.X[- ind, ]
train.Species <- iris.train.set[, (colnames(iris.train.set) == c("Species"))]

#recherche du nombre k optimal de voisins
err.classif.KNN <- NULL
for (k in seq(from = 1, to = 23, by = 1) )
{  
  knn.pred <- knn(train = train.X, test = test.X,  cl = train.Species, k = k)
  err.classif.KNN[k] <- mean(!(knn.pred == iris.test.set$Species))
}

plot(err.classif.KNN, type="b")
k_opt <- which.min(err.classif.KNN)
k_opt

knn.pred.opt <- knn(train = train.X, test = test.X, cl = train.Species, 
                    prob = TRUE, k = k_opt)
err.classif.KNN_opt <- mean(!(knn.pred.opt == iris.test.set$Species))
err.classif.KNN_opt

##comparaison
err.classif.RegLogM
err.classif.ADL
err.classif.ADQ
err.classif.KNN_opt

