
# Modèle de régression linéaire multiple
# Tests d'hypothèses
# Algorithmes de sélection de modèles (de variables)
# Estimation de l'erreur théorique (de prévision) d'un modèle par validation croisée
# Régression ridge, lasso et group-lasso

#Vider la mémoire
rm(list = ls())

library(ISLR)
library(lmtest)
library(leaps)
library(glmulti)
library(lattice)
library(boot)
library(glmnet)
library(gglasso)

str(Carseats)
View(Carseats)
?Carseats

## Exercice 1
# 1 - 2 :
modele.RLM <- lm(formula = Sales ~ ., data = Carseats)
attributes(modele.RLM)
summary(modele.RLM)
summary(modele.RLM)$coefficients

#3
# (i) : Vérifier graphiquement la non-corrélation des erreurs (à partir des résidus)
acf(modele.RLM$residuals, main = "Autocorrélations des erreurs")

# (ii) : 
#Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
#graphiquement :
plot(modele.RLM, 1)

# (iii) : 
#Vérifier l'hypothèse d'homoscedasticité des erreurs;
#graphiquement :
plot(modele.RLM, 3)

# 4 :
#Tester la non-corrélation (d'ordre 1) des erreurs : test de Durbin-Watson
dwtest(modele.RLM, alternative = c("two.sided"))

# 5 : 
#Test d'homoscedasticité de Breusch-Pagan
require(lmtest)
bptest(modele.RLM, studentize = FALSE)

#Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
#graphiquement :
plot(modele.RLM, 1)

# 6 :
# vérifier la normalité des erreurs
#Graphiquement : normal Q-Q plot, et histogramme versus densité normale
#normal Q-Q plot
plot(modele.RLM, 2)
#histogramme versus densité normale
residus <- modele.RLM$residuals
hist(residus, freq = FALSE, ylim = c(0,0.48), main = "Histogramme des résidus")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), col = 2, lty = 2, lwd = 2, add = TRUE)

#Test de Shapiro-Wilk pour tester l'hypothèse de normalité du terme d'erreur
shapiro.test(residuals(modele.RLM))

#7 :
summary(modele.RLM)
#Ordonner les variables explicatives numériques selon les valeurs 
#des p_values croissantes (du test de Student)
vect.pvalues.Student <- summary(modele.RLM)$coefficients[,"Pr(>|t|)"]
vect.pvalues.Student
#On supprime la p_value de l'intercept 
vect.pvalues.Student <-vect.pvalues.Student[2:length(vect.pvalues.Student)] 
#Variables explicatives ordonnées, de la plus significative à la moins significative
sort(vect.pvalues.Student) 


# 8 - 9 :
#Ordonner les variables explicatives numériques/qualitatives 
#selon les valeurs des P_values croissantes du test de Fisher
tests.Fisher <- anova(modele.RLM)
tests.Fisher
str(tests.Fisher)
m <- nrow(tests.Fisher)
vect.pvalues.Fisher <- tests.Fisher[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher) <- rownames(tests.Fisher[1:m-1,])
sort(vect.pvalues.Fisher) #Attention pour les variables explicatives qualitatives ayant
                          #plus de deux modalités (on compare à dimension égale)
                          #dans ce cas il faudrait faire comme suit
XX <- model.matrix(Sales ~., data = Carseats)[,-c(1)] #Cette fonction construit la matrice de design en remplaçant 
                                                   #chacune des variables qualitatives par les indicatrices 
                                                   #de ses modalités (la première modalité est supprimée)
                                                   #on supprime la première colonne correspondant à l'intercept

View(model.matrix(Sales ~., data = Carseats))

View(XX)
Carseats.num.data <- cbind(Sales = Carseats[,"Sales"],XX)
Carseats.num.data <- as.data.frame(Carseats.num.data) #Bd constituée que de variables numériques
View(Carseats.num.data)
tests.Fisher2 <- anova(lm(Sales~., data = Carseats.num.data))
tests.Fisher2
m <- nrow(tests.Fisher2)
vect.pvalues.Fisher2 <- tests.Fisher2[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher2) <- rownames(tests.Fisher2[1:m-1,])
sort(vect.pvalues.Fisher2)

#Comparaison avec le classement selon le test de Student
sort(vect.pvalues.Fisher2)
sort(vect.pvalues.Student) #On n'obtient pas le même classement; 
                           #il est recommandé de retenir celui de Fisher

#### Sélection de modèles (variables) ####

## Exercice 2
## Algorithmes de recherche exhaustive ##
#La fonction regsubsets() : sélection de variables explicatives numériques
XX <- model.matrix(Sales ~., data = Carseats) #Matrice de design
p <- ncol(XX)-1 #Nombre de variables numériques explicatives dans le modèle de RLM complet
p
require(leaps)
select.modeles <- regsubsets(Sales ~ ., data = Carseats, 
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

#La fonction glmulti() : sélection de variables explicatives numériques/qualitatives
require(glmulti)
select.modele.aic <- glmulti(Sales ~., data = Carseats, level = 1, 
                           fitfunction = lm, crit = "aic", plotty = FALSE, method = "h")
modele.opt.aic <- summary(select.modele.aic)$bestmodel
modele.opt.aic
anova(lm(modele.opt.aic, data = Carseats))
select.modele.bic <- glmulti(Sales ~., data = Carseats, level = 1,
                         fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic <- summary(select.modele.bic)$bestmodel
modele.opt.bic
anova(lm(modele.opt.bic, data = Carseats))

#La fonction glmulti() appliquée aux variables explicatives numériques 
#après transformation des variables explicatives qualitatives en quantitatives
select.modele.aic <- glmulti(Sales ~., data = Carseats.num.data, level = 1, 
                             fitfunction = lm, crit = "aic", plotty = FALSE, method = "h")
modele.opt.aic <- summary(select.modele.aic)$bestmodel
modele.opt.aic
anova(lm(modele.opt.aic, data = Carseats.num.data))
select.modele.bic <- glmulti(Sales ~., data = Carseats.num.data, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic <- summary(select.modele.bic)$bestmodel
modele.opt.bic
anova(lm(modele.opt.bic, data = Carseats.num.data))

#### Algorithmes de recherche pas-à-pas ####

## Exercice 3

#Forward selection, version 1 : fonction regsubsets()
select.mod.for <- regsubsets(Sales ~., data = Carseats, 
                             nbest = 1, nvmax = p, method = "forward")
#Backward elimination, version 1 : fonction regsubsets()
select.mod.bac <- regsubsets(Sales ~., data = Carseats, 
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
modele.trivial <- lm(Sales ~ 1, data = Carseats)
modele.complet <- lm(Sales ~ ., data = Carseats)
#selon AIC (k = 2)
res.select.AIC.for <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = Carseats, direction = "forward", k = 2)
#selon BIC (k = log(n))
n <- nrow(Carseats) #le nombre d'observations
res.select.BIC.for <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = Carseats, direction = "forward", k = log(n))
#selon le critère de la statistique de Fisher
n <- nrow(Carseats)
res.select.F.for <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = Carseats, direction = "forward", test = "F")

##Backward elimination, version 2 : fonction step()
modele.complet <- lm(Sales ~ ., data = Carseats)
#selon AIC (k = 2)
res.select.AIC.bac <- step(modele.complet, data = Carseats, direction = "backward", k = 2)
#selon BIC (k = log(n))
n <- nrow(Carseats)
res.select.BIC.bac <- step(modele.complet, data = Carseats, direction = "backward", k = log(n))
#selon le critère de Fisher
n <- nrow(Carseats)
res.select.F.bac <- step(modele.complet, data = Carseats, direction = "backward", test = "F")

##Même chose pour la bd Carseats.num.data
##Forward selection, version 2 : fonction step()
modele.trivial <- lm(Sales ~ 1, data = Carseats.num.data)
modele.complet <- lm(Sales ~ ., data = Carseats.num.data)
#selon AIC (k = 2)
res.select.AIC.for2 <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = Carseats.num.data, direction = "forward", k = 2)
#selon BIC (k = log(n))
n <- nrow(Carseats.num.data)
res.select.BIC.for2 <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = Carseats.num.data, direction = "forward", k = log(n))
#selon le critère de Fisher
n <- nrow(Carseats.num.data)
res.select.F.for2 <- step(modele.trivial, 
                         scope = list(lower = modele.trivial, upper = modele.complet),
                         data = Carseats.num.data, direction = "forward", test = "F")

##Backward elimination, version 2 : fonction step()
modele.complet <- lm(Sales ~ ., data = Carseats.num.data)
#selon AIC (k = 2)
res.select.AIC.bac2 <- step(modele.complet, data = Carseats.num.data, direction = "backward", k = 2)
#selon BIC (k = log(n))
n <- nrow(Carseats.num.data)
res.select.BIC.bac2 <- step(modele.complet, data = Carseats.num.data, direction = "backward", k = log(n))
#selon le test de Fisher
n <- nrow(Carseats.num.data)
res.select.F.bac2 <- step(modele.complet, data = Carseats.num.data, direction = "backward", test = "F")

## Exercice 5

##Bidirectional forward selection : fonction step()
modele.trivial <- lm(Sales ~ 1, data = Carseats)
modele.complet <- lm(Sales ~ ., data = Carseats)
#selon AIC (k = 2)
res.AIC.bidirect.select <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = Carseats, direction = "both", k = 2)
#selon BIC (k = log(n))
n <- nrow(Carseats)
res.BIC.bidirect.select <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = Carseats, direction = "both", k = log(n))
#selon le critère de Fisher
n <- nrow(Carseats)
res.F.bidirect.select <- step(modele.trivial, 
                         scope = list(lower = modele.trivial, upper = modele.complet),
                         data = Carseats, direction = "both", test = "F")

##Bidirectional backward elimination : fonction step()
modele.complet <- lm(Sales ~ ., data = Carseats)
#selon AIC (k = 2)
res.AIC.bidirect.elim <- step(modele.complet, data = Carseats, direction = "both", k = 2)
#selon BIC (k = log(n))
n <- nrow(Carseats)
res.BIC.bidirect.elim <- step(modele.complet, data = Carseats, direction = "both", k = log(n))
#selon le test de Fisher
n <- nrow(Carseats)
res.F.bidirect.elim <- step(modele.complet, data = Carseats, direction = "both", test = "F")

#Même chose pour la bd Carseats.num.data 
##Bidirectional forward selection : fonction step()
modele.trivial <- lm(Sales ~ 1, data = Carseats.num.data)
modele.complet <- lm(Sales ~ ., data = Carseats.num.data)
#selon AIC (k = 2)
res.AIC.bidirect.select2 <- step(modele.trivial, 
                                scope = list(lower = modele.trivial, upper = modele.complet),
                                data = Carseats.num.data, direction = "both", k = 2)
#selon BIC (k = log(n))
n <- nrow(Carseats.num.data)
res.BIC.bidirect.select2 <- step(modele.trivial, 
                                scope = list(lower = modele.trivial, upper = modele.complet),
                                data = Carseats.num.data, direction = "both", k = log(n))
#selon le critère de Fisher
n <- nrow(Carseats)
res.F.bidirect.select2 <- step(modele.trivial, 
                              scope = list(lower = modele.trivial, upper = modele.complet),
                              data = Carseats.num.data, direction = "both", test = "F")

##Bidirectional backward elimination : fonction step()
modele.complet <- lm(Sales ~ ., data = Carseats.num.data)
#selon AIC (k = 2)
res.AIC.bidirect.elim2 <- step(modele.complet, data = Carseats.num.data, direction = "both", k = 2)
#selon BIC (k = log(n))
n <- nrow(Carseats)
res.BIC.bidirect.elim2 <- step(modele.complet, data = Carseats.num.data, direction = "both", k = log(n))
#selon le critère de Fisher
n <- nrow(Carseats.num.data)
res.F.bidirect.elim2 <- step(modele.complet, data = Carseats.num.data, direction = "both", test = "F")

#### Selection par algorithme génétique ####

## Exercice 6

#AIC
res.AIC.gen <- glmulti(Sales ~., data = Carseats, level = 2, method = "g",
                       fitfunction = lm, crit = 'aic', plotty = FALSE)
AIC.best.model <- summary(res.AIC.gen)$bestmodel
AIC.best.model
#BIC
res.BIC.gen <- glmulti(Sales ~., data = Carseats, level = 1, method = "g",
                       fitfunction = lm, crit = 'bic', plotty = FALSE)
BIC.best.model <- summary(res.BIC.gen)$bestmodel
BIC.best.model
#AICC : AIC corrigé (utile si le nombre d'observations est petit)
res.AICC.gen <- glmulti(Sales ~., data = Carseats, level = 1, method = "g",
                       fitfunction = lm, crit = 'aicc', plotty = FALSE)
AICC.best.model <- summary(res.AICC.gen)$bestmodel
AICC.best.model

#Même chose pour la bd Carseats.num.data
#AIC
res.AIC.gen2 <- glmulti(Sales ~., data = Carseats.num.data, level = 1, method = "g",
                       fitfunction = lm, crit = 'aic', plotty = FALSE)
AIC.best.model2 <- summary(res.AIC.gen2)$bestmodel
AIC.best.model2
#BIC
res.BIC.gen2 <- glmulti(Sales ~., data = Carseats.num.data, level = 2, method = "g",
                       fitfunction = lm, crit = 'bic', plotty = FALSE)
BIC.best.model2 <- summary(res.BIC.gen2)$bestmodel
BIC.best.model2
#AICC : AIC corrigé (utile si le nombre d'observations est petit)
res.AICC.gen2 <- glmulti(Sales ~., data = Carseats.num.data, level = 1, method = "g",
                        fitfunction = lm, crit = 'aicc', plotty = FALSE)
AICC.best.model2 <- summary(res.AICC.gen2)$bestmodel
AICC.best.model2


## Exercice 7

#### Etude du modèle optimal selectionné selon le critère BIC ####
#vérifier la non-corrélation des erreurs (résidus)
modele.RLM <- lm(BIC.best.model, data = Carseats)
par(mfrow = c(1,1))
acf(modele.RLM$residuals, main = "Autocorrélations des résidus")

#Tester la non-corrélation (d'ordre 1) des erreurs : test de Durbin-Watson
dwtest(modele.RLM, alternative = c("two.sided"))

#vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives
#graphiquement
plot(modele.RLM, 1)

#vérifier l'hypothèse d'homoscedasticité des erreurs
#graphiquement
plot(modele.RLM, 3)
#test d'homoscedasticité de Breusch-Pagan
require(lmtest)
bptest(modele.RLM, studentize = FALSE)

#### vérifier la normalité des erreurs ####
#graphiquement : normal Q-Q plot, et histogramme versus densité normale
plot(modele.RLM, 2)
residus <- residuals(modele.RLM)
hist(residus, freq = FALSE, xlab = "", ylim = c(0,0.45), main = "Histogramme des résidus")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), col = 2, lty = 2, lwd = 2, add = TRUE)

#test de normalité de Shapiro-Wilk 
shapiro.test(residuals(modele.RLM))

#### Evaluation de l'erreur théorique du modele sélectionné par critère BIC ####
# comparaison avec le modèle complet

## Exercice 8

## Méthode1 : L'approche de l'ensemble de validation 

#Vecteur d'indices, de taille `partie entière de (2/3)*n'; 
#les indices sont tirés de manière aléatoire sans remise dans l'ensemble {1,2, ...,n} 
n <- nrow(Carseats)
indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE) 
#On choisit ici de prendre 2/3*n observations pour l'apprentissage, et le reste pour le test (ou la validation)
ensemble.apprentissage <- Carseats[indices, ] #la partie de la bd Carseats qui va servir de bd d'apprentissage
ensemble.validation <- Carseats[-indices, ] #la bd de validation

modele1 <- lm(formula = BIC.best.model, data = ensemble.apprentissage) 

#Estimation de l'erreur du modele1
valeurs.predites <- predict(object = modele1, newdata = ensemble.validation) 
estimation.erreur.modele1 <- mean((ensemble.validation$Sales - valeurs.predites)^2)  
estimation.erreur.modele1

modele2 <- lm(formula = Sales ~., data = ensemble.apprentissage) #le modèle complet
summary(modele2)

#Estimation de l'erreur du modele2 (le modèle complet)
valeurs.predites <- predict(object = modele2, newdata = ensemble.validation) 
estimation.erreur.modele2 <- mean((ensemble.validation$Sales - valeurs.predites)^2)  

print(c("Résultats des estimations par la méthode de l'ensemble de validation : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation.erreur.modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation.erreur.modele2)))) 

##### Comment améliorer la méthode 1  ####

erreur.modele1 = NULL
erreur.modele2 = NULL
Err1 = NULL
Err2 = NULL
M <- 1000 #nombre de réplications de la méthode 1
for (i in 1:M)
{
  indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
  ensemble.apprentissage <- Carseats[indices, ]
  ensemble.validation <- Carseats[-indices, ]
  modele1 <- lm(formula = BIC.best.model, data = ensemble.apprentissage) #modèle optimal selon BIC
  modele2 <- lm(formula = Sales ~ ., data = ensemble.apprentissage) #modèle complet
  valeurs.predites1 <- predict(object = modele1, newdata = ensemble.validation)
  valeurs.predites2 <- predict(object = modele2, newdata = ensemble.validation)
  erreur.modele1[i] <- mean((ensemble.validation$Sales - valeurs.predites1)^2)
  erreur.modele2[i] <- mean((ensemble.validation$Sales - valeurs.predites2)^2)
  Err1[i] <- mean(erreur.modele1[1:i])
  Err2[i] <- mean(erreur.modele2[1:i])
}
a <- min(c(min(Err1), min(Err2)))
b <- max(c(max(Err1), max(Err2)))
par(mfrow = c(1,1))
require(lattice)
xyplot(Err1+Err2 ~ 1:M, type = "l", col = c("blue", "red"), xlab = "m", 
       ylab = "Erreurs du modèle1 (bleu) et du modèle2 (rouge)")
estimation.erreur.modele1 <- Err1[M] #meilleur estimation de l'erreur théorique du modele1
estimation.erreur.modele2 <- Err2[M] #meilleur estimation de l'erreur théorique du modele2

estimation.erreur.relative.modele1 <- Err1[M]/mean((ensemble.validation$Sales)^2)*100 
estimation.erreur.relative.modele2 <- Err2[M]/mean((ensemble.validation$Sales)^2)*100 

estimation.erreur.relative.modele1
estimation.erreur.relative.modele2

print(c("Résultats des estimations par la méthode de l'ensemble de validation : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation.erreur.modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation.erreur.modele2)))) 
####################################################################################################

## Exercice 9

#### Méthode 2 : estimation de l'erreur par "leave-one-out Cross-Validation" (LOOCV), i.e., la K-fold CV avec K = n, le nombre d'observations ####
modele1 <- glm(formula = Sales ~ ShelveLoc + CompPrice + Income + Advertising + Price + Age,  data = Carseats)
#Estimation de l'erreur du modele1 par la méthode LOOCV
require(boot) 
estimation.erreur.modele1 <- cv.glm(data = Carseats, glmfit = modele1, K = n)$delta[1] 

modele2 <- glm(formula = Sales ~ ., data = Carseats)
#Estimation de l'erreur du modele2 par la méthode LOOCV
estimation.erreur.modele2 <- cv.glm(data = Carseats, glmfit = modele2, K = n)$delta[1] 

print(c("Résultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation.erreur.modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation.erreur.modele2)))) 


## Exercice 10

#### Méthode 3 : estimation de l'erreur des deux modèles précédents par K-fold CV, avec K = 10 ####
#Estimation de l'erreur du modele1 par la méthode  K-fold CV
estimation.erreur.modele1 <- cv.glm(data = Carseats, glmfit = modele1, 
                                    K = 10)$delta[1] 
#Estimation de l'erreur du modele2 par la méthode K-fold CV
estimation.erreur.modele2 <- cv.glm(data = Carseats, glmfit =  modele2, 
                                    K = 10)$delta[1] 

print(c("Résultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation.erreur.modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation.erreur.modele2)))) 

#### Bootstrap ################################################################

#Fonction donnant les estimateurs MC du modèle de RLM à partir 
#d'un sous-ensemble des données extrait de la bd `data' suivant le vecteur
#d'incices `index'
f_estimateurs_w <- function(data, index){
     return(coef(lm(formula = Sales ~ ., data = data, subset = index)))
}

#Exemple : Estimation utilisant la moitié de la bd Carseats
n <- nrow(Carseats) #nombre d'observations
f_estimateurs_w(data = Carseats, index = 1:n/2)

#Estimation utilisant la bd Carseats
n <- nrow(Carseats) #nombre d'observations
f_estimateurs_w(data = Carseats, index = 1:n)

#Estimation à partir d'un échantillon bootstrapé de la bd Carseats
f_estimateurs_w(data = Carseats, index = sample(n, n, replace = T))

require(boot)
boot(data = Carseats, statistic = f_estimateurs_w, R = 1000)

#On campare avec les résulats de la fonction lm()
summary(lm(formula = Sales ~ ., data = Carseats))
#####################################################################

## Exercice 11

#### Régression ridge ####
XX <- model.matrix(Sales ~., data = Carseats)[,-1] #matrice de design sans l'intercept
View(XX)
require(glmnet)
reg.ridge <- glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 0) #alpha=0 pour construire les modèles ridge
par(mfrow = c(1,1))
plot(reg.ridge, label = TRUE)
plot(reg.ridge, xvar = "lambda", label = TRUE, lwd = 2)
reg.cvridge <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 0)
bestlam <- reg.cvridge$lambda.min
bestlam
plot(reg.cvridge)
min(reg.cvridge$cvm) #erreur de prevision du modele ridge optimal 
coef(reg.cvridge)

#Comparaison avec le modele de RLM complet
reg.cvridge <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 0)
erreur.modele.ridge.opt <- min(reg.cvridge$cvm) #erreur de prevision du modele ridge optimal 
erreur.modele.RLM.complet <- cv.glm(data = Carseats, glmfit =  glm(formula = Sales ~., 
                                                        data = Carseats), K = 10)$delta[1]

erreur.modele.ridge.opt
erreur.modele.RLM.complet

## Exercice 12

#### le Lasso ####
reg.lasso <- glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 1) # alhpa=1 pour construire le modèle lasso
par(mfrow = c(1,2))
plot(reg.lasso, label = TRUE)
plot(reg.lasso, xvar = "lambda", label = TRUE, lwd = 2)

reg.cvlasso <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 1)
bestlam <- reg.cvlasso$lambda.min
bestlam
par(mfrow = c(1,1))
plot(reg.cvlasso)
min(reg.cvlasso$cvm) #erreur de prevision du modele lasso optimal 
coef(reg.cvlasso)

#Comparaison avec le modele de RLM complet
reg.cvlasso <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 1)
erreur.modele.lasso.opt <- min(reg.cvlasso$cvm) #erreur de prevision du modele lasso optimal 
erreur.modele.RLM.complet <- cv.glm(data = Carseats, glmfit =  glm(formula = Sales ~., 
                                                        data = Carseats), K = 10)$delta[1]

erreur.modele.lasso.opt
erreur.modele.RLM.complet

## Exercice 13

#### Groupe lasso ####

#Les variables explicatives numériques (avec les dummy variables)
D <- model.matrix(Sales ~ ., data = Carseats)[,-1] #matrice de design sans l'intercept
str(D)
model <- glmnet(x = scale(D), y = Carseats$Sales, alpha = 1)
summary(model)
plot(model,label = TRUE, xvar = "lambda")

#On définit les groupes de variables
groupe <- c(1,2,3,4,5,6,6,7,8,9,10)
require(gglasso)
model1 <- gglasso(x = scale(D), y = Carseats$Sales, group = groupe)
plot(model1)

reg.cv.grouplasso <- cv.gglasso(x = scale(D), y = Carseats$Sales, group = groupe)  
reg.cv.grouplasso$lambda.min
erreur.modele.glasso.opt <- min(reg.cv.grouplasso$cvm)

model.RLM.complet <- glm(formula = Sales ~ . , data = Carseats)
erreur.modele.RLM.complet <- cv.glm(data = Carseats, glmfit = model.RLM.complet, K = 10)$delta[1]

erreur.modele.glasso.opt
erreur.modele.RLM.complet

coef(reg.cv.grouplasso)

