
##### Modèle de régression logistique binomial pour la classification binaire #####

#vider la mémoire
rm(list = ls())

#installer et charger le package xlsx
library(xlsx)

#importation des données heart.xlsx
heart <- read.xlsx(file.choose(), sheetIndex = 1, 
                   header = TRUE, stringsAsFactors = TRUE)

#vérifications
str(heart)
View(heart)

#quelques statistiques
summary(heart)

#faire une régression logistique de la variable binaire coeur en fonction des variables (explicatives) 
#de la bd heart : 
modele.RL <- glm(formula = coeur ~ .,  data = heart, family = binomial)
print(modele.RL)
summary(modele.RL)
attributes(modele.RL)

#### Tester (avec rapport de vraisemblance) la validité du modèle complet ####
# i.e., tester  H0 : ``w1=0, ..., w9=0'' contre H1 : ``le contraire de H_0'' 
Sn <- modele.RL$null.deviance - modele.RL$deviance #la statistique du rapport de vraisemblance
print(Sn)
ddl <- modele.RL$df.null - modele.RL$df.residual #nombre de degrés de liberté de la loi limite de Sn, sous H_0
print(ddl)
pvalue <- pchisq(q = Sn, df = ddl, lower.tail = F) #p_value du test : P(Z>Sn) où Z suit une loi du chi^2(ddl)
print(pvalue) #on obtient 1.253064e-27, on rejette H0, donc le modèle est "très" significatif

###################################################################################################################

#### Sélection de modèles (de variables) selon les critères AIC, AICC et BIC ####
#### Recherche exhaustive ####
library(glmulti)
#AIC
select.modele.aic <- glmulti(coeur ~.,  data = heart, family = binomial, level = 1, 
                             fitfunction = glm, crit = "aic", 
                               plotty = FALSE, method = "h")
#BIC
select.modele.bic <- glmulti(coeur ~.,  data = heart, family = binomial, level = 1, 
                             fitfunction = glm, crit = "bic", 
                              plotty = FALSE, method = "h")
#AICC
select.modele.aicc <- glmulti(coeur ~.,  data = heart, family = binomial, level = 1, 
                              fitfunction = glm, crit = "aicc", 
                               plotty = FALSE, method = "h")

summary(select.modele.aic)$bestmodel
summary(select.modele.aicc)$bestmodel
summary(select.modele.bic)$bestmodel

##Si on veut choisir parmi les variables de la matrice de design, on fait comme suit :
XX <- model.matrix(coeur ~., data = heart)[,-1] #cette fonction construit la matrice de design en remplaçant 
                                                #chacune des variables qualitatives pour les indicatrices 
                                                #de ses modalités (la première modalité est supprimée)
                                                #on supprime la première colonne correspondant à l'intercept
heart.num.data <- cbind(as.data.frame(XX), coeur = as.factor(heart[,"coeur"])) #bd constituée que de variables explicatives numériques 
                                                                               #et une variable réponse qualitative (binaire) 
str(heart.num.data)
View(heart.num.data)

#AIC
select.modele.num.aic <- glmulti(coeur ~.,  data = heart.num.data, family = binomial, level = 1, 
                                 fitfunction = glm, crit = "aic", 
                                 plotty = FALSE, method = "h")
#BIC
select.modele.num.bic <- glmulti(coeur ~.,  data = heart.num.data, family = binomial, level = 1, 
                                 fitfunction = glm, crit = "bic", 
                                 plotty = FALSE, method = "h")
#AICC
select.modele.num.aicc <- glmulti(coeur ~.,  data = heart.num.data, family = binomial, level = 1, 
                                  fitfunction = glm, crit = "aicc", 
                                  plotty = FALSE, method = "h")

summary(select.modele.num.aic)$bestmodel
summary(select.modele.num.aicc)$bestmodel
summary(select.modele.num.bic)$bestmodel

#### Algorithme génétique ####
#AIC
select.modele.aic <- glmulti(coeur ~.,  data = heart, family = binomial, level = 1, 
                             fitfunction = glm, crit = "aic", 
                             plotty = FALSE, method = "g")
#BIC
select.modele.bic <- glmulti(coeur ~.,  data = heart, family = binomial, level = 1, 
                             fitfunction = glm, crit = "bic", 
                             plotty = FALSE, method = "g")
#AICC
select.modele.aicc <- glmulti(coeur ~.,  data = heart, family = binomial, level = 1, 
                              fitfunction = glm, crit = "aicc", 
                              plotty = FALSE, method = "g")

summary(select.modele.aic)$bestmodel
summary(select.modele.aicc)$bestmodel
summary(select.modele.bic)$bestmodel

##Si on veut choisir parmi les variables de la matrice de design, on fait comme suit :
XX <- model.matrix(coeur ~., data = heart)[,-1] #cette fonction construit la matrice de design en remplaçant 
                                                #chacune des variables qualitatives pour les indicatrices 
                                                #de ses modalités (la première modalité est supprimée)
                                                #on supprime la première colonne correspondant à l'intercept
heart.num.data <- cbind(as.data.frame(XX), coeur = as.factor(heart[,"coeur"])) #bd constituée que de variables explicatives numériques 
                                                                               #et une variable réponse qualitative (binaire) 

#AIC
select.modele.num.aic <- glmulti(coeur ~.,  data = heart.num.data, family = binomial, level = 1, 
                                 fitfunction = glm, crit = "aic", 
                                 plotty = FALSE, method = "g")
#BIC
select.modele.num.bic <- glmulti(coeur ~.,  data = heart.num.data, family = binomial, level = 1, 
                                 fitfunction = glm, crit = "bic", 
                                 plotty = FALSE, method = "g")
#AICC
select.modele.num.aicc <- glmulti(coeur ~.,  data = heart.num.data, family = binomial, level = 2, 
                                  fitfunction = glm, crit = "aicc", 
                                  plotty = FALSE, method = "g")

summary(select.modele.num.aic)$bestmodel
summary(select.modele.num.aicc)$bestmodel
summary(select.modele.num.bic)$bestmodel

#### Recherche pas-à-pas ####
#installer et charger le package MASS
library(MASS)

modele.complet <- glm(formula = coeur ~ ., family = binomial, data = heart)
modele.trivial <- glm(formula = coeur ~ 1, family = binomial, data = heart)
##la méthode backward elimination
#AIC
select.modele.aic.back <- step(object = modele.complet, 
                          scope = list(lower = modele.trivial, upper = modele.complet), 
                          direction = "backward", k = 2) #le choix k = 2 est pris par défaut correspondant au critère AIC
#BIC
n <- nrow(heart) #nombre d'observations
select.modele.bic.back <- step(object = modele.complet, 
                          scope = list(lower = modele.trivial, upper = modele.complet), 
                          direction = "backward", k = log(n)) #le choix k = log(n) correspond au critère BIC
#affichage
select.modele.aic.back$formula
select.modele.bic.back$formula

##la méthode forward selection
modele.trivial <- glm(formula = coeur ~ 1, family = binomial, data = heart)
modele.complet <- glm(formula = coeur ~ ., family = binomial, data = heart)
#AIC
select.modele.aic.forward <- step(modele.trivial, 
                             scope = list(lower = modele.trivial, upper = modele.complet),
                             direction = "forward")
#BIC
select.modele.bic.forward <- step(modele.trivial, 
                             scope = list(lower = modele.trivial, upper = modele.complet),
                             direction = "forward", k = log(n))

select.modele.aic.forward$formula
select.modele.bic.forward$formula

##la méthode bidirectional elimination
modele.complet <- glm(formula = coeur ~ ., family = binomial, data = heart)
modele.trivial <- glm(formula = coeur ~ 1, family = binomial, data = heart)
#AIC
select.modele.aic.bidirect.elim <- step(object = modele.complet, 
                                   scope = list(lower = modele.trivial, upper = modele.complet), 
                                   direction = "both")
#BIC
select.modele.bic.bidirect.elim <- step(object = modele.complet, 
                                   scope = list(lower = modele.trivial, upper = modele.complet), 
                                   direction = "both", k = log(n))

select.modele.aic.bidirect.elim$formula
select.modele.bic.bidirect.elim$formula

##la méthode bidirectional selection
modele.trivial <- glm(formula = coeur ~ 1, family = binomial, data = heart)
modele.complet <- glm(formula = coeur ~ ., family = binomial, data = heart)
#AIC
select.modele.aic.bidirect.sel <- step(object = modele.trivial, 
                                  scope = list(lower = modele.trivial, upper = modele.complet), 
                                  direction = "both")
#BIC
select.modele.bic.bidirect.sel <- step(object = modele.trivial, 
                                  scope = list(lower = modele.trivial, upper = modele.complet), 
                                  direction = "both", k = log(n))

select.modele.aic.bidirect.sel$formula
select.modele.bic.bidirect.sel$formula

##Si on veut choisir parmi les variables de la matrice de design, on fait comme suit :
XX <- model.matrix(coeur ~., data = heart)[,-1] #cette fonction construit la matrice de design en remplaçant 
                                                #chacune des variables qualitatives pour les indicatrices 
                                                #de ses modalités (la première modalité est supprimée)
                                                #on supprime la première collone correspondant à l'intercept
heart.num.data <- cbind(as.data.frame(XX), coeur = as.factor(heart[,"coeur"])) #bd constituée que de variables explicatives numériques 
                                                                               #et une variable réponse qualitative (binaire) 

#### Recherche pas-à-pas ####
modele.complet <- glm(formula = coeur ~ ., family = binomial, data = heart.num.data)
modele.trivial <- glm(formula = coeur ~ 1, family = binomial, data = heart.num.data)
##la methode backward elimination
#AIC
select.modele.num.aic.back <- step(object = modele.complet, 
                              scope = list(lower = modele.trivial, upper = modele.complet), 
                              direction = "backward", k = 2) #le choix k = 2 est pris par défaut correspondant au critère AIC
#BIC
n <- nrow(heart) #nombre d'observations
select.modele.num.bic.back <- step(object = modele.complet, 
                              scope = list(lower = modele.trivial, upper = modele.complet), 
                              direction = "backward", k = log(n)) #le choix k = log(n) correspond au critère BIC
#affichage
select.modele.num.aic.back$formula
select.modele.num.bic.back$formula

##la methode forward selection
modele.trivial <- glm(formula = coeur ~ 1, family = binomial, data = heart.num.data)
modele.complet <- glm(formula = coeur ~ ., family = binomial, data = heart.num.data)
#AIC
select.modele.num.aic.forward <- step(modele.trivial, 
                                 scope = list(lower = modele.trivial, upper = modele.complet),
                                 direction = "forward")
#BIC
select.modele.num.bic.forward <- step(modele.trivial, 
                                 scope = list(lower = modele.trivial, upper = modele.complet),
                                 direction = "forward", k = log(n))

select.modele.num.aic.forward$formula
select.modele.num.bic.forward$formula

##la méthode bidirectional elimination
modele.complet <- glm(formula = coeur ~ ., family = binomial, data = heart.num.data)
modele.trivial <- glm(formula = coeur ~ 1, family = binomial, data = heart.num.data)
#AIC
select.modele.num.aic.bidirect.elim <- step(object = modele.complet, 
                                       scope = list(lower = modele.trivial, upper = modele.complet), 
                                       direction = "both")
#BIC
select.modele.num.bic.bidirect.elim <- step(object = modele.complet, 
                                       scope = list(lower = modele.trivial, upper = modele.complet), 
                                       direction = "both", k = log(n))

select.modele.num.aic.bidirect.elim$formula
select.modele.num.bic.bidirect.elim$formula

##la méthode bidirectional selection
modele.trivial <- glm(formula = coeur ~ 1, family = binomial, data = heart.num.data)
modele.complet <- glm(formula = coeur ~ ., family = binomial, data = heart.num.data)
#AIC
select.modele.num.aic.bidirect.sel <- step(object = modele.trivial, 
                                      scope = list(lower = modele.trivial, upper = modele.complet), 
                                      direction = "both")
#BIC
select.modele.num.bic.bidirect.sel <- step(object = modele.trivial, 
                                      scope = list(lower = modele.trivial, upper = modele.complet), 
                                      direction = "both", k = log(n))

select.modele.num.aic.bidirect.sel$formula
select.modele.num.bic.bidirect.sel$formula

#############################################################################################################

#### Estimation de l'erreur de classification par les méthodes de validation croisée ####
#### Méthode 1 : de l'ensemble de validation ####
# On divise l'échantillon en deux sous ensembles, le premier d'apprentissage (training set) 
# de taille souvent supérieur à 60%, et le second de test. On construit le modèle sur le sous échantillon 
# d'apprentissage, et on le teste sur l'échantillon de validation. 
# L'erreur de test du modèle (ou l'erreur de prédiction) est évaluée sur l'échantillon de validation.
# Nous allons évaluer l'erreur de classification issue du modèle optimal selon le critère BIC
str(heart.num.data)

#rappelons le modèle optimal selon BIC
modele.opt.bic.formula <- summary(select.modele.num.bic)$bestmodel
modele.opt.bic.formula

n <- nrow(heart.num.data)

#la fonction évaluant l'erreur de classif pour une partition donnée, en deux sous échantillons :
err_classif <- function(l){
  #on partage le tableau en deux parties : par exemple 1-1/l pour apprentissage et 1/l pour le test
  indices_ensemble_test = sample(x = n, trunc(n/l), replace = FALSE)
  ensemble_test = heart.num.data[indices_ensemble_test, ]
  ensemble_apprentissage = heart.num.data[ - indices_ensemble_test, ]
  modele = glm(formula = modele.opt.bic.formula, data = ensemble_apprentissage, family = binomial)
  pred.proba = predict(object = modele, newdata = ensemble_test, type = "response") #les probabilités prédites des probabilités p_1(x)
  pred.moda <- factor(ifelse(pred.proba > 0.5, "presence", "absence")) #on les transforme en modalités prédites des modalités de la variable réponse `coeur`
  mc = table(ensemble_test$coeur, pred.moda) #matrice de confusion
  return(1- sum(diag(mc))/sum(mc))  
}

# Pour l = 3, on obtient l'erreur
print(err_classif(3))

#### Pour améliorer la méthode précédente ####
# On répète la méthode précédente M fois, et on calcule la moyenne
# Par exemple, pour M = 1000, on obtient
M <- 1500
resultats <- replicate(M, err_classif(3))
err_moy_classif <- mean(resultats) #une estimation de l'erreur de classification
print(err_moy_classif) # on obtient 0.23 en moyenne

#### Méthode 3 : ####
# une autre méthode pour l'estimation de l'erreur de classification, par K-fold cross-validation 
# On peut utiliser le fonction cv.glm du package boot
library(boot)
heart_modele_glm <- glm(formula = modele.opt.bic.formula, data = heart.num.data, family = binomial)
cout <- function(r, pi) mean(abs(r-pi) > 0.5) #la fonction de coût, ce choix est approprié au cas d'une variable réponse binaire
#K = nrow(heart) correspond au leave-one-out
#Par exemple, pour K = 10, on obtient
K <- 10
cv.err <- cv.glm(data = heart.num.data, glmfit = heart_modele_glm, cost = cout, K = K)
cv.err$delta[1] 

#comparaison avec le modèle complet
modele.glm.complet <- glm(formula = coeur ~., data = heart.num.data, family = binomial)
cv.err.modele.complet <- cv.glm(data = heart.num.data, 
                                glmfit = modele.glm.complet, cost = cout, K = K)
cv.err.modele.complet$delta[1] 

#le cas K = n, n étant le nombre d'observations, correspond à la méthode leave-one-out :
n <- nrow(heart.num.data)
K <- n
cv.err <- cv.glm(data = heart.num.data, glmfit = heart_modele_glm, cost = cout, K = K)
cv.err$delta[1] 

#comparaison avec le modèle complet
modele.glm.complet <- glm(formula = coeur ~., data = heart.num.data, family = binomial)
cv.err.modele.complet <- cv.glm(data = heart.num.data, 
                                glmfit = modele.glm.complet, cost = cout, K = K)
cv.err.modele.complet$delta[1] 

######################################################################################################

#### classer les variables d'un modèle selon leur niveau de significativité ####
# on utilise une approche test : (utiliser les p_values des tests correspondants, 
# test de Wald et/ou test du rapport de vraisemblance).

# Considérons par exemple le modèle optimal selon BIC 
# et classons les variables par ordre décroissant des valeurs des p_values de chacun des tests de Wald 

#rappelons le modèle optimal selon BIC
modele.opt.bic.formula <- summary(select.modele.num.bic)$bestmodel
modele.opt.bic.formula #ce modèle utilise les v.a. explicatives :
                       #sexemasculin, typedouleurD, tauxmax, depression
                       #la bd correspondante est la suivante
heart.opt.data <- heart.num.data[,c("sexemasculin", "typedouleurD", "tauxmax", "depression", 
                                    "coeur")]
str(heart.opt.data)
View(heart.opt.data)

modele <- glm(formula = coeur~., data = heart.opt.data, family = binomial)
print(modele)
summary(modele)
tab.modele <- summary(modele)$coefficients
tab.modele <- as.data.frame(tab.modele)
str(tab.modele)
View(tab.modele)
vect.des.pvalues.Wald <- tab.modele[,"Pr(>|z|)"]
names(vect.des.pvalues.Wald) <- row.names(tab.modele)

#on supprime la pvalue de l'intercept
vect.des.pvalues.Wald <- vect.des.pvalues.Wald[!(names(vect.des.pvalues.Wald) == "(Intercept)")]
vect.des.pvalues.Wald

#ranger les variables par ordre croissant des valeurs des p_values 
sort(vect.des.pvalues.Wald) # la variable la plus significative est depression, ensuite sexe, ensuite typedouleurD, ensuite tauxmax, ... 

#On classe maintenant les variables par ordre décroissant des valeurs des pvalues de chacun des tests par maximum de vraisemblance 
#### classement des variables par pvalues du test du rapport de vraisemblance, préférable à celui de Wald ####
modele <- glm(formula = coeur ~ ., data = heart.opt.data, family = binomial)
print(modele)

##on procède maintenant au test d'hypothèses par la statistique du rapport de vraisemblance, et au calcul des p_values correspondantes
##Tester l'hypothèse : la variable sexe n'est pas significative 
#i.e., tester H_0 : w1 = 0 contre H_1 : w1 != 0  
modele.reduit <- glm(coeur ~ ., data = heart.opt.data[,!(colnames(heart.opt.data) 
                                                               == "sexemasculin")], family = binomial)
#Statistique du rapport de vraisemblance
Sn <- modele.reduit$deviance - modele$deviance
print(Sn)
pvalue.sexe <- pchisq(q = Sn, df = 1, lower.tail = F) #donne P(Z>Sn) où Z est une variable suivant une chi2(1).
print(pvalue.sexe)

##Tester H_0 : la variable typedouleurD n'est pas significative
modele.reduit <- glm(coeur ~ ., data = heart.opt.data[,!(colnames(heart.opt.data) 
                                                        == "typedouleurD")], family = binomial)
#Statistique du rapport de vraisemblance
Sn <- modele.reduit$deviance - modele$deviance
print(Sn)
pvalue.typedouleurD = pchisq(q = Sn, df = 1, lower.tail = F)
print(pvalue.typedouleurD)

##Tester H_0 : la variable tauxmax n'est pas significative
modele.reduit <- glm(coeur ~ ., data = heart.opt.data[,!(colnames(heart.opt.data) 
                                                        == "tauxmax")], family = binomial)
#Statistique du rapport de vraisemblance
Sn <- modele.reduit$deviance - modele$deviance
print(Sn)
pvalue.tauxmax = pchisq(q = Sn, df= 1, lower.tail = F)
print(pvalue.tauxmax)

##Tester H_0 : la variable depression n'est pas significative
modele.reduit <- glm(coeur ~ ., data = heart.opt.data[,!(colnames(heart.opt.data) 
                                                         == "depression")], family = binomial)
#Statistique du rapport de vraisemblance
Sn <- modele.reduit$deviance - modele$deviance
print(Sn)
pvalue.dep <- pchisq(q = Sn, df = 1, lower.tail = F)
print(pvalue.dep)

#vecteur des p_values
vect.des.pvalues.MV <- c(pvalue.sexe, pvalue.typedouleurD, pvalue.tauxmax, pvalue.dep)
names(vect.des.pvalues.MV) <- colnames(heart.opt.data[,!(colnames(heart.opt.data) == "coeur")])
vect.des.pvalues.MV

sort(vect.des.pvalues.MV) #on ordonne les variables de la plus significative à la moins significative
sort(vect.des.pvalues.Wald) 
#on obtient le même classement ici! pas toujours le cas ... 
#il est recommandé de retenir le classement selon le test du rapport de vraisemblances.
