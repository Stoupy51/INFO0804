#Projet 804

library(ISLR)
library(lmtest)
library(leaps)
library(glmulti)
library(lattice)
library(boot)
library(glmnet)
library(gglasso)
rm(list = ls())

#Chargement des données
path = "C:/Users/axell/Documents/M1/804-ML/INFO0804/Housing.csv"
if (!file.exists(path)) {
  path = file.choose()
}
data = read.csv(path)
summary(data)

## Partie 1
#Construction du modèle
modele.RLM <- lm(formula = price ~ ., data = data)
summary(modele.RLM)

#Vérification de la non-corrélation des erreurs (à partir des résidus)
#graphiquement:
acf(modele.RLM$residuals, main = "Autocorrélations des erreurs")
#Tester la non-corrélation (d'ordre 1) des erreurs : test de Durbin-Watson
dwtest(modele.RLM, alternative = c("two.sided"))

#Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
#graphiquement :
plot(modele.RLM, 1)

#Vérifier l'hypothèse d'homoscedasticité des erreurs;
#graphiquement :
plot(modele.RLM, 3)
#Test d'homoscedasticité de Breusch-Pagan
require(lmtest)
bptest(modele.RLM, studentize = FALSE)

# vérifier la normalité des erreurs
#Graphiquement : normal Q-Q plot, et histogramme versus densité normale
#normal Q-Q plot
plot(modele.RLM, 2)
#histogramme versus densité normale
residus <- modele.RLM$residuals
hist(residus, freq = FALSE,  main = "Histogramme des résidus")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), col = 2, lty = 2, lwd = 2, add = TRUE)
#Test de Shapiro-Wilk pour tester l'hypothèse de normalité du terme d'erreur
shapiro.test(residuals(modele.RLM))

#Ordonner les variables explicatives selon les valeurs 
#des p_values croissantes (du test de Student)
summary(modele.RLM)
vect.pvalues.Student <- summary(modele.RLM)$coefficients[,"Pr(>|t|)"]
vect.pvalues.Student
#On supprime la p_value de l'intercept 
vect.pvalues.Student <-vect.pvalues.Student[2:length(vect.pvalues.Student)] 
#Variables explicatives ordonnées, de la plus significative à la moins significative
sort(vect.pvalues.Student) 

#Ordonner les variables explicatives numériques/qualitatives 
#selon les valeurs des P_values croissantes du test de Fisher
#on a une va qualitatives a plus de 2 modalites
XX <- model.matrix(price ~., data = data)[,-c(1)]
data.num.data <- cbind(price = data[,"price"],XX)
data.num.data <- as.data.frame(data.num.data) #Bd constituée que de variables numériques
tests.Fisher2 <- anova(lm(price~., data = data.num.data))
tests.Fisher2
m <- nrow(tests.Fisher2)
vect.pvalues.Fisher2 <- tests.Fisher2[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher2) <- rownames(tests.Fisher2[1:m-1,])
sort(vect.pvalues.Fisher2)

#Comparaison avec le classement selon le test de Student
sort(vect.pvalues.Fisher2)
sort(vect.pvalues.Student) 
#On n'obtient pas le même classement; 
#il est recommandé de retenir celui de Fisher



#### Selection par algorithme génétique ####

#AIC
res.AIC.gen <- glmulti(price ~., data = data, level = 1, method = "g",
                       fitfunction = lm, crit = 'aic', plotty = FALSE)
AIC.best.model <- summary(res.AIC.gen)$bestmodel
AIC.best.model

#BIC
res.BIC.gen <- glmulti(price ~., data = data, level = 1, method = "g",
                       fitfunction = lm, crit = 'bic', plotty = FALSE)
BIC.best.model <- summary(res.BIC.gen)$bestmodel
BIC.best.model

#AICC : AIC corrigé (utile si le nombre d'observations est petit)
#notre nb d'observations est plutôt élevé donc pas vraiment besoin
res.AICC.gen <- glmulti(price ~., data = data, level = 1, method = "g",
                        fitfunction = lm, crit = 'aicc', plotty = FALSE)
AICC.best.model <- summary(res.AICC.gen)$bestmodel
AICC.best.model
# retorune le même modèle que le critère AIC == modèle complet

#### Etude du modèle optimal selectionné selon le critère BIC ####
#vérifier la non-corrélation des erreurs (résidus)
modele.RLM <- lm(BIC.best.model, data = data)
par(mfrow = c(1,1))
acf(modele.RLM$residuals, main = "Autocorrélations des résidus")

#Tester la non-corrélation (d'ordre 1) des erreurs : test de Durbin-Watson
dwtest(modele.RLM, alternative = c("two.sided"))

#vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives
#graphiquement
plot(modele.RLM, 1)
#à peu près horizontal donc peut être linéaire

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
hist(residus, freq = FALSE, xlab = "", main = "Histogramme des résidus")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), col = 2, lty = 2, lwd = 2, add = TRUE)

#test de normalité de Shapiro-Wilk 
shapiro.test(residuals(modele.RLM))



#### Evaluation de l'erreur théorique du modele sélectionné par critère BIC ####
# comparaison avec le modèle complet

## Méthode1 : L'approche de l'ensemble de validation 
n <- nrow(data)
erreur.modele1 = NULL
erreur.modele2 = NULL
Err1 = NULL
Err2 = NULL
M <- 1000 #nombre de réplications de la méthode
for (i in 1:M)
{
  indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
  ensemble.apprentissage <- data[indices, ]
  ensemble.validation <- data[-indices, ]
  modele1 <- lm(formula = BIC.best.model, data = ensemble.apprentissage) #modèle optimal selon BIC
  modele2 <- lm(formula = price ~ ., data = ensemble.apprentissage) #modèle complet
  valeurs.predites1 <- predict(object = modele1, newdata = ensemble.validation)
  valeurs.predites2 <- predict(object = modele2, newdata = ensemble.validation)
  erreur.modele1[i] <- mean((ensemble.validation$price - valeurs.predites1)^2)
  erreur.modele2[i] <- mean((ensemble.validation$price - valeurs.predites2)^2)
  Err1[i] <- mean(erreur.modele1[1:i])
  Err2[i] <- mean(erreur.modele2[1:i])
}
par(mfrow = c(1,1))
require(lattice)
xyplot(Err1+Err2 ~ 1:M, type = "l", col = c("blue", "red"), xlab = "m", 
       ylab = "Erreurs du modèle1 (bleu) et du modèle2 (rouge)")
estimation.erreur.modele1 <- Err1[M] #meilleur estimation de l'erreur théorique du modele1
estimation.erreur.modele2 <- Err2[M] #meilleur estimation de l'erreur théorique du modele2
estimation.erreur.modele1
estimation.erreur.modele2

print(c("Résultats des estimations par la méthode de l'ensemble de validation : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation.erreur.modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation.erreur.modele2)))) 

estimation.erreur.relative.modele1 <- Err1[M]/mean((ensemble.validation$price)^2)*100 
estimation.erreur.relative.modele2 <- Err2[M]/mean((ensemble.validation$price)^2)*100 

estimation.erreur.relative.modele1 #c'est en pourcent 
estimation.erreur.relative.modele2

#### Méthode 2 : estimation de l'erreur par "leave-one-out Cross-Validation" (LOOCV), i.e., la K-fold CV avec K = n, le nombre d'observations ####
modele1 <- glm(formula = BIC.best.model,  data = data)
#Estimation de l'erreur du modele1 par la méthode LOOCV
require(boot) 
estimation.erreur.modele1 <- cv.glm(data = data, glmfit = modele1, K = n)$delta[1] 

modele2 <- glm(formula = price ~ ., data = data)
#Estimation de l'erreur du modele2 par la méthode LOOCV
estimation.erreur.modele2 <- cv.glm(data = data, glmfit = modele2, K = n)$delta[1] 

print(c("Résultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation.erreur.modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation.erreur.modele2)))) 

#### Méthode 3 : estimation de l'erreur des deux modèles précédents par K-fold CV, avec K = 10 ####

erreur.modele1 = NULL
erreur.modele2 = NULL
Err1 = NULL
Err2 = NULL
M <- 1000 #nombre de réplications de la méthode
for (i in 1:M)
{
  erreur.modele1[i] <- cv.glm(data = data, glmfit = modele1,K = 10)$delta[1] 
  erreur.modele2[i] <- cv.glm(data = data, glmfit =  modele2, K = 10)$delta[1]
  Err1[i] <- mean(erreur.modele1[1:i])
  Err2[i] <- mean(erreur.modele2[1:i])
}

estimation.erreur.modele1 <- Err1[M] #meilleur estimation de l'erreur théorique du modele1
estimation.erreur.modele2 <- Err2[M] #meilleur estimation de l'erreur théorique du modele2

print(c("Résultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation.erreur.modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation.erreur.modele2)))) 

estimation.erreur.relative.modele1 <- Err1[M]/mean((data$price)^2)*100 
estimation.erreur.relative.modele2 <- Err2[M]/mean((data$price)^2)*100 

estimation.erreur.relative.modele1 #c'est en pourcent 
estimation.erreur.relative.modele2
