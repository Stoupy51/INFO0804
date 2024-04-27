
#### Modèle de régression logistique multinomial pour la classification multi-groupe ####

#vider la mémoire
rm(list = ls())

library(nnet)

data(iris)
str(iris)
levels(iris$Species)

#variable réponse : Species
#variables explicatives (numériques) : le autres variables de la bd

#la fonction multinom() calcule les estimateurs des paramètres, 
#du model de RegLogMultinomial, par maximum de vraisemblance;
#maxit : est le nombre maximal d'itérations pour le calcul des estimateurs MV
modele.complet <- multinom(formula = Species ~ ., data = iris, model = TRUE, maxit = 3000)    
print(modele.complet)
print(summary(modele.complet))
attributes(modele.complet)
modele.complet$edf #donne le nombre de paramètres du modèle de RegLogMultinomial
formula(modele.complet$model)

modele.trivial <- multinom(formula = Species ~ 1, data = iris, model = TRUE, maxit = 3000) 
print(modele.trivial)
print(summary(modele.trivial))
modele.trivial$edf
attributes(modele.trivial)
formula(modele.trivial$model)

#### Sélection de modèles (de variables) selon le critère AIC par algorithmes de recherche pas-à-pas ####
modele.complet <- multinom(formula = Species ~ . , data = iris, model = TRUE, maxit = 3000) 
modele.trivial <- multinom(formula = Species ~ 1, data = iris, model = TRUE, maxit = 3000) 

#la méthode backward elimination
library(MASS)
modele.back <- step(object = modele.complet, 
                   scope = list(lower = Species ~ 1, upper = formula(modele.complet$model)), 
                   direction = "backward")
modele.back
formula(modele.back$model) #le modèle optimal obtenu

#la méthode forward selection
modele.forward <- step(object = modele.trivial, 
                      scope = list(lower = Species ~ 1, upper = formula(modele.complet$model)), 
                      direction = "forward")
modele.forward
formula(modele.forward$model) #le modèle optimal obtenu

#la méthode bidirectional elimination
modele.bidirect.elim <- step(object = modele.complet, 
                            scope = list(lower = Species ~ 1, upper = formula(modele.complet$model)), 
                            direction = "both")
modele.bidirect.elim
formula(modele.bidirect.elim$model) #le modèle optimal obtenu

#la méthode bidirectional selection
modele.bidirect.select <- step(object = modele.trivial, 
                              scope = list(lower = Species ~ 1, upper = formula(modele.complet$model)), 
                              direction = "both")
modele.bidirect.select
formula(modele.bidirect.select$model) #le modèle optimal obtenu

#les 4 algorithmes précédents donne le même modèle.

#### Sélection de modèles (de variables) selon le critère BIC par algorithmes de recherche pas-à-pas ####
modele.complet <- multinom(formula = Species ~ . , data = iris, model = TRUE, maxit = 3000) 
modele.trivial <- multinom(formula = Species ~ 1, data = iris, model = TRUE, maxit = 3000) 
n <- nrow(iris) #nombre d'observation

#la méthode backward elimination
modele.back <- step(object = modele.complet, 
                   scope = list(lower = Species ~ 1, upper = formula(modele.complet$model)), 
                   direction = "backward", k = log(n))
modele.back
formula(modele.back$model) #le modèle optimal obtenu

#la méthode forward selection
modele.forward <- step(object = modele.trivial, 
                      scope = list(lower = Species ~ 1, upper = formula(modele.complet$model)), 
                      direction = "forward", k = log(n))
modele.forward
formula(modele.forward$model) #le modèle optimal obtenu

#la méthode bidirectional elimination
modele.bidirect.elim <- step(object = modele.complet, 
                            scope = list(lower = Species ~ 1, upper = formula(modele.complet$model)), 
                            direction = "both", k = log(n))
modele.bidirect.elim
formula(modele.bidirect.elim$model) #le modèle optimal obtenu

#la méthode bidirectional selection
modele.bidirect.select <- step(object = modele.trivial, 
                              scope = list(lower = Species ~ 1, upper = formula(modele.complet$model)), 
                              direction = "both", k = log(n))
modele.bidirect.select
formula(modele.bidirect.select$model) #le modèle optimal obtenu

#les 4 algorithmes précédents donne le même modèle. 

#Les deux critères AIC et BIC donne le même modèle.

#################################################################################

#### Tests d'hypothèses par maximum de vraisemblance dans le modèle de RegLogMultinom ####
#Nous allons considérer le modèle optimal sélectionné selon le critère AIC
#i.e., le modèle Species ~ Petal.Width + Petal.Length + Sepal.Width
#on crée d'abord le sous-ensemble de données correspondant

iris.opt.data <- iris[,!(colnames(iris)=="Sepal.Length")]
View(iris.opt.data)
View(iris)

#### Test de validité du modèle global : H_0 : w_2 = w_3 = (0,0,0) ####
modele <-  multinom(formula = Species ~  ., data = iris.opt.data, maxit = 3000) 
modele

modele.reduit <- multinom(formula = Species ~ 1, data = iris.opt.data, maxit = 3000) 
Sn <- modele.reduit$deviance-modele$deviance #la statistique du rapport de vraisemblance
print(Sn)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue <- pchisq(q = Sn, df = d, lower.tail = F)
print(pvalue) #on obtient  2.601647e-65, le modèle optimal est très significatif

#### Tester si la variable Sepal.Width n'est pas significative dans le modèle : H_0 : w_{2,1} = w_{3,1} = 0 ####
modele <-  multinom(formula = Species ~  ., data = iris.opt.data, maxit = 3000) 
modele.reduit <- multinom(formula = Species ~ ., 
                          data = iris.opt.data[, !(colnames(iris.opt.data)=="Sepal.Width")], 
                          maxit = 2000) #le modèle réduit
Sn <- modele.reduit$deviance - modele$deviance
print(Sn)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.Sepal.Width <- pchisq(q = Sn, df = d, lower.tail = F)
print(pvalue.Sepal.Width)

#Tester si la variable Petal.Length n'est pas significative : H_0 = w_{2,2} = w_{3,2} = 0
modele.reduit <- multinom(formula = Species ~ ., 
                          data = iris.opt.data[, !(colnames(iris.opt.data)=="Petal.Length")], 
                          maxit = 2000) #le modèle réduit
Sn <- modele.reduit$deviance - modele$deviance
print(Sn)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.Petal.Length <- pchisq(q = Sn, df = d, lower.tail = F)
print(pvalue.Petal.Length)

#Tester si la variable Petal.Width n'est pas significative : H_0 = w_{2,3}=w_{3,3}=0
modele.reduit <- multinom(formula = Species ~ ., 
                          data = iris.opt.data[, !(colnames(iris.opt.data)=="Petal.Width")], 
                          maxit = 2000) #le modèle réduit
Sn <- modele.reduit$deviance - modele$deviance
print(Sn)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.Petal.Width <- pchisq(q = Sn, df = d, lower.tail = F)
print(pvalue.Petal.Width)

#### Classer les variables ####
pvalues <- c(pvalue.Sepal.Width, pvalue.Petal.Length, pvalue.Petal.Width)
names(pvalues) <- colnames(iris.opt.data[, !(colnames(iris.opt.data) == "Species")])
variables.classees <- sort(pvalues)
print(variables.classees) # on obtient le classement suivant :
#Petal.Width Petal.Length  Sepal.Width 
#0.0001089506 0.0008533257 0.0259165680 

#################################################################################################

#### Erreurs de classification, évaluées par validation croisées, 
#pour le modèle optimal selon AIC (réduit aux trois variables Sepal.Width, Petal.Length et Petal.Width),  
#et pour le modèle complet utilisant les quatre variables #### 
indices <- 1:nrow(iris)
#la fonction évaluant l'erreur de classification des deux modèles, pour une partition donnée
err_classif <- function(l = 3){
  #on partage le tableau en deux parties : par exemple (l-1)/l pour apprentissage et 1/l pour le test
  indices.ensemble.test <- sample(indices, trunc(length(indices)/l), replace = FALSE)
  ensemble.test <- iris[indices.ensemble.test, ]
  ensemble.apprentissage <- iris[-indices.ensemble.test, ]
  modele.AIC <- multinom(formula = Species ~ Sepal.Width+Petal.Length+Petal.Width, 
                            data = ensemble.apprentissage, maxit = 3000) # le modèle optimal
  modele.complet <- multinom(formula = Species ~ ., 
                            data = ensemble.apprentissage, maxit = 3000) # le modèle complet
  pred.moda.modele.AIC <- predict(object = modele.AIC, newdata = ensemble.test)
  pred.moda.modele.complet <- predict(object = modele.complet, newdata = ensemble.test)
  erreur.modele.AIC <- mean(!(pred.moda.modele.AIC == ensemble.test$Species))
  erreur.modele.complet <- mean(!(pred.moda.modele.complet == ensemble.test$Species))   
  return(c(erreur.modele.AIC, erreur.modele.complet))
}  

print(err_classif(3))

#on applique la fonction précédente M = 100 fois, à l'aide de la fonction ``replicate'', 
#et on met les résultats dans le tableau resultats, de dimension 2xM,
#cela évite l'utilisation de boucles for. 
M <- 100 #nombre de réplications
resultats <- replicate(M, err_classif(3))
resutats.moyens <- apply(resultats,1,mean) #on calcule la moyenne par colonne
err.classif.modele.AIC <- resutats.moyens[1]
err.classif.modele.complet <- resutats.moyens[2]
err.classif.modele.AIC
err.classif.modele.complet

#on obtient des résultats comparables, pour cet exemple, l'erreur de classification est de 4% en moyenne pour les deux modèles,
#le modèle optimal est quand même mieux, car utilise peu de variables, par rapport au modèle complet, et prédit avec efficacité comparable ...

