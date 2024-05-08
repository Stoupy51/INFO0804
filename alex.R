
## Objectifs : Exercices 2, 3, 4, 5,  11, 12, 13

# Vider la mémoire
rm(list = ls())

# Charger les librairies
library(ISLR)
library(lmtest)
library(leaps)
library(glmulti)
library(lattice)
library(boot)
library(glmnet)
library(gglasso)

# On récupère les données
path = "E:/my_folders/3_professional/1_education/Master/INFO0804_ModelisationLineaire/Projet/Housing.csv"
if (!file.exists(path)) {
	path = file.choose()
}
data = read.csv(path)
str(data)
View(data)


## Exercice 2
## Algorithmes de recherche exhaustive ##
#La fonction regsubsets() : sélection de variables explicatives numériques
XX = model.matrix(price ~., data = data) #Matrice de design
p = ncol(XX)-1 #Nombre de variables numériques explicatives dans le modèle de RLM complet
p
select.modeles = regsubsets(price ~ ., data = data, 
                             nbest = 1, nvmax = p, method = "exhaustive")
summary(select.modeles)
summary(select.modeles)$rsq
summary(select.modeles)$adjr2
summary(select.modeles)$bic
summary(select.modeles)$cp
summary(select.modeles)$which
plot(select.modeles, scale = "r2")
plot(select.modeles, scale = "adjr2")
plot(select.modeles, scale = "bic")
plot(select.modeles, scale = "Cp")

# La fonction glmulti() : sélection de variables explicatives numériques/qualitatives
select.modele.aic = glmulti(price ~., data = data, level = 1, 
                             fitfunction = lm, crit = "aic", plotty = FALSE, method = "h")
modele.opt.aic = summary(select.modele.aic)$bestmodel
modele.opt.aic
anova(lm(modele.opt.aic, data = data))
select.modele.bic = glmulti(price ~., data = data, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic = summary(select.modele.bic)$bestmodel
modele.opt.bic
anova(lm(modele.opt.bic, data = data))

#La fonction glmulti() appliquée aux variables explicatives numériques 
#après transformation des variables explicatives qualitatives en quantitatives
select.modele.aic = glmulti(price ~., data = data.num.data, level = 1, 
                             fitfunction = lm, crit = "aic", plotty = FALSE, method = "h")
modele.opt.aic = summary(select.modele.aic)$bestmodel
modele.opt.aic
anova(lm(modele.opt.aic, data = data.num.data))
select.modele.bic = glmulti(price ~., data = data.num.data, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic = summary(select.modele.bic)$bestmodel
modele.opt.bic
anova(lm(modele.opt.bic, data = data.num.data))

#### Algorithmes de recherche pas-à-pas ####




## Exercice 3

#Forward selection, version 1 : fonction regsubsets()
select.mod.for = regsubsets(price ~., data = data, 
                             nbest = 1, nvmax = p, method = "forward")
#Backward elimination, version 1 : fonction regsubsets()
select.mod.bac = regsubsets(price ~., data = data, 
                             nbest = 1, nvmax = p, method = "backward")
#Résultats pour le critère BIC des deux algorithmes
par(mfrow = c(1,2))
plot(select.mod.for, scale = "bic", main = "Forward selection")
plot(select.mod.bac, scale = "bic", main = "Backward elimination")
#Résultas des deux algorithmes pour le critère Cp de Mallow 
par(mfrow = c(1,2))
plot(select.mod.for, scale = "Cp", main = "Forward selection")
plot(select.mod.bac, scale = "Cp", main = "Backward elimination")
#Résultats des deux algorithmes pour le critère du R2 ajusté 
par(mfrow = c(1,2))
plot(select.mod.for, scale = "adjr2", main = "Forward selection")
plot(select.mod.bac, scale = "adjr2", main = "Backward elimination")





## Exercice 4

##Forward selection, version 2 : fonction step()
# Création du model trivial et complet sur nos données
modele.trivial = lm(price ~ 1, data = data)
modele.complet = lm(price ~ ., data = data)

#selon AIC (k = 2)
res.select.AIC.for = step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = data, direction = "forward", k = 2)
#selon BIC (k = log(n))
n = nrow(data) #le nombre d'observations
res.select.BIC.for = step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = data, direction = "forward", k = log(n))
#selon le critère de la statistique de Fisher
res.select.F.for = step(modele.trivial, 
                         scope = list(lower = modele.trivial, upper = modele.complet),
                         data = data, direction = "forward", test = "F")

##Backward elimination, version 2 : fonction step()
modele.complet = lm(price ~ ., data = data)
#selon AIC (k = 2)
res.select.AIC.bac = step(modele.complet, data = data, direction = "backward", k = 2)
#selon BIC (k = log(n))
n = nrow(data)
res.select.BIC.bac = step(modele.complet, data = data, direction = "backward", k = log(n))
#selon le critère de Fisher
n = nrow(data)
res.select.F.bac = step(modele.complet, data = data, direction = "backward", test = "F")

##Même chose pour la bd data.num.data
##Forward selection, version 2 : fonction step()
modele.trivial = lm(price ~ 1, data = data.num.data)
modele.complet = lm(price ~ ., data = data.num.data)
#selon AIC (k = 2)
res.select.AIC.for2 = step(modele.trivial, 
                            scope = list(lower = modele.trivial, upper = modele.complet),
                            data = data.num.data, direction = "forward", k = 2)
#selon BIC (k = log(n))
n = nrow(data.num.data)
res.select.BIC.for2 = step(modele.trivial, 
                            scope = list(lower = modele.trivial, upper = modele.complet),
                            data = data.num.data, direction = "forward", k = log(n))
#selon le critère de Fisher
n = nrow(data.num.data)
res.select.F.for2 = step(modele.trivial, 
                          scope = list(lower = modele.trivial, upper = modele.complet),
                          data = data.num.data, direction = "forward", test = "F")

##Backward elimination, version 2 : fonction step()
modele.complet = lm(price ~ ., data = data.num.data)
#selon AIC (k = 2)
res.select.AIC.bac2 = step(modele.complet, data = data.num.data, direction = "backward", k = 2)
#selon BIC (k = log(n))
n = nrow(data.num.data)
res.select.BIC.bac2 = step(modele.complet, data = data.num.data, direction = "backward", k = log(n))
#selon le test de Fisher
n = nrow(data.num.data)
res.select.F.bac2 = step(modele.complet, data = data.num.data, direction = "backward", test = "F")




## Exercice 5

##Bidirectional forward selection : fonction step()
modele.trivial = lm(price ~ 1, data = data)
modele.complet = lm(price ~ ., data = data)
#selon AIC (k = 2)
res.AIC.bidirect.select = step(modele.trivial, 
                                scope = list(lower = modele.trivial, upper = modele.complet),
                                data = data, direction = "both", k = 2)
#selon BIC (k = log(n))
n = nrow(data)
res.BIC.bidirect.select = step(modele.trivial, 
                                scope = list(lower = modele.trivial, upper = modele.complet),
                                data = data, direction = "both", k = log(n))
#selon le critère de Fisher
n = nrow(data)
res.F.bidirect.select = step(modele.trivial, 
                              scope = list(lower = modele.trivial, upper = modele.complet),
                              data = data, direction = "both", test = "F")

##Bidirectional backward elimination : fonction step()
modele.complet = lm(price ~ ., data = data)
#selon AIC (k = 2)
res.AIC.bidirect.elim = step(modele.complet, data = data, direction = "both", k = 2)
#selon BIC (k = log(n))
n = nrow(data)
res.BIC.bidirect.elim = step(modele.complet, data = data, direction = "both", k = log(n))
#selon le test de Fisher
n = nrow(data)
res.F.bidirect.elim = step(modele.complet, data = data, direction = "both", test = "F")

#Même chose pour la bd data.num.data 
##Bidirectional forward selection : fonction step()
modele.trivial = lm(price ~ 1, data = data.num.data)
modele.complet = lm(price ~ ., data = data.num.data)
#selon AIC (k = 2)
res.AIC.bidirect.select2 = step(modele.trivial, 
                                 scope = list(lower = modele.trivial, upper = modele.complet),
                                 data = data.num.data, direction = "both", k = 2)
#selon BIC (k = log(n))
n = nrow(data.num.data)
res.BIC.bidirect.select2 = step(modele.trivial, 
                                 scope = list(lower = modele.trivial, upper = modele.complet),
                                 data = data.num.data, direction = "both", k = log(n))
#selon le critère de Fisher
n = nrow(data)
res.F.bidirect.select2 = step(modele.trivial, 
                               scope = list(lower = modele.trivial, upper = modele.complet),
                               data = data.num.data, direction = "both", test = "F")

##Bidirectional backward elimination : fonction step()
modele.complet = lm(price ~ ., data = data.num.data)
#selon AIC (k = 2)
res.AIC.bidirect.elim2 = step(modele.complet, data = data.num.data, direction = "both", k = 2)
#selon BIC (k = log(n))
n = nrow(data)
res.BIC.bidirect.elim2 = step(modele.complet, data = data.num.data, direction = "both", k = log(n))
#selon le critère de Fisher
n = nrow(data.num.data)
res.F.bidirect.elim2 = step(modele.complet, data = data.num.data, direction = "both", test = "F")







## Exercice 11

#### Régression ridge ####
XX = model.matrix(price ~., data = data)[,-1] #matrice de design sans l'intercept
View(XX)
require(glmnet)
reg.ridge = glmnet(x = scale(XX), y = data[,"price"], alpha = 0) #alpha=0 pour construire les modèles ridge
par(mfrow = c(1,1))
plot(reg.ridge, label = TRUE)
plot(reg.ridge, xvar = "lambda", label = TRUE, lwd = 2)
reg.cvridge = cv.glmnet(x = scale(XX), y = data[,"price"], alpha = 0)
bestlam = reg.cvridge$lambda.min
bestlam
plot(reg.cvridge)
min(reg.cvridge$cvm) #erreur de prevision du modele ridge optimal 
coef(reg.cvridge)

#Comparaison avec le modele de RLM complet
reg.cvridge = cv.glmnet(x = scale(XX), y = data[,"price"], alpha = 0)
erreur.modele.ridge.opt = min(reg.cvridge$cvm) #erreur de prevision du modele ridge optimal 
erreur.modele.RLM.complet = cv.glm(data = data, glmfit =  glm(formula = price ~., 
                                                                   data = data), K = 10)$delta[1]

erreur.modele.ridge.opt
erreur.modele.RLM.complet

## Exercice 12

#### le Lasso ####
reg.lasso = glmnet(x = scale(XX), y = data[,"price"], alpha = 1) # alhpa=1 pour construire le modèle lasso
par(mfrow = c(1,2))
plot(reg.lasso, label = TRUE)
plot(reg.lasso, xvar = "lambda", label = TRUE, lwd = 2)

reg.cvlasso = cv.glmnet(x = scale(XX), y = data[,"price"], alpha = 1)
bestlam = reg.cvlasso$lambda.min
bestlam
par(mfrow = c(1,1))
plot(reg.cvlasso)
min(reg.cvlasso$cvm) #erreur de prevision du modele lasso optimal 
coef(reg.cvlasso)

#Comparaison avec le modele de RLM complet
reg.cvlasso = cv.glmnet(x = scale(XX), y = data[,"price"], alpha = 1)
erreur.modele.lasso.opt = min(reg.cvlasso$cvm) #erreur de prevision du modele lasso optimal 
erreur.modele.RLM.complet = cv.glm(data = data, glmfit =  glm(formula = price ~., 
                                                                   data = data), K = 10)$delta[1]

erreur.modele.lasso.opt
erreur.modele.RLM.complet

## Exercice 13

#### Groupe lasso ####

#Les variables explicatives numériques (avec les dummy variables)
XX = model.matrix(price ~ ., data = data)[,-1] #matrice de design sans l'intercept
str(XX)
model = glmnet(x = scale(XX), y = data$price, alpha = 1)
summary(model)
plot(model,label = TRUE, xvar = "lambda")

#On définit les groupes de variables
require(gglasso)
groupe = 1:dim(XX)[2] # Ici 1:13
model1 = gglasso(x = scale(XX), y = data$price, group = groupe)
plot(model1)

reg.cv.grouplasso = cv.gglasso(x = scale(XX), y = data$price, group = groupe)  
reg.cv.grouplasso$lambda.min
erreur.modele.glasso.opt = min(reg.cv.grouplasso$cvm)

model.RLM.complet = glm(formula = price ~ . , data = data)
erreur.modele.RLM.complet = cv.glm(data = data, glmfit = model.RLM.complet, K = 10)$delta[1]

erreur.modele.glasso.opt
erreur.modele.RLM.complet

coef(reg.cv.grouplasso)

















