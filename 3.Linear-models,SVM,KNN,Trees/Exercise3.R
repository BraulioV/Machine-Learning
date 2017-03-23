## ----setup, include=FALSE------------------------------------------------
library("ISLR")
library("ggplot2")
library("class")
library("e1071")
library("ROCR")
library("gbm")
library("boot")
library("glmnet")
library("MASS")
library("randomForest")
library("tree")

pause <-function(){
    print("Pulsar una tecla para continuar")
    scan(what=character(), n=1)
    graphics.off()
}

## ------------------------------------------------------------------------
attach(Auto)
pairs(~ ., data=Auto)
pause()
## ------------------------------------------------------------------------
boxplot(mpg ~ displacement, data=Auto, xlab="Mpg", ylab = "Displacement")
pause()
boxplot(mpg ~ horsepower, data=Auto, xlab="Mpg", ylab = "Horsepower")
pause()
boxplot(mpg ~ weight, data=Auto, xlab="Mpg", ylab = "Weight")
pause()
boxplot(mpg ~ acceleration, data=Auto, xlab="Mpg", ylab = "Acceleration")
pause()

## ------------------------------------------------------------------------
i = sample(x=nrow(Auto), size=floor(nrow(Auto))*0.8)
data.train = Auto[i,]
data.test = Auto[-i,]
## ------------------------------------------------------------------------
i = sample(x=nrow(Auto), size=floor(nrow(Auto))*0.8)
m01= lm(mpg~weight,data = Auto, subset= i)
print(summary(m01))
print((ggplot() + geom_point(data=as.data.frame(data.test), 
            aes(x=data.test$weight, y=data.test$mpg)) 
          + geom_line(aes(x=data.test$weight,
                          y=predict(m01,data.test), color="red"))))

pause()
## ------------------------------------------------------------------------
m02 = lm(mpg ~ weight+displacement+horsepower, data=Auto, subset = i)
print(summary(m02))
print((ggplot() + geom_point(data=as.data.frame(data.test), 
            aes(x=data.test$horsepower*data.test$displacement*data.test$weight,
                y=data.test$mpg)) 
          + geom_line(aes(x=data.test$horsepower*data.test$displacement*data.test$weight, 
                          y=predict(m02,data.test), color="red"))))
pause()
## ------------------------------------------------------------------------
m03 = lm(mpg~weight*I(horsepower+displacement)^2,data = Auto, subset= i)
print(summary(m03))
print((ggplot() + geom_point(data=as.data.frame(data.test), 
            aes(x=data.test$weight*I((data.test$horsepower+data.test$displacement)^2), 
                y=data.test$mpg)) 
          + geom_line(aes(x=data.test$weight*I((data.test$horsepower+
                data.test$displacement)^2), 
                y=predict(m03,data.test), color="red"))))

pause()
## ------------------------------------------------------------------------
m04 = lm(mpg~weight*I(horsepower*displacement*weight)^2,data = Auto, subset= i)
print(summary(m04))
print((ggplot() + geom_point(data=as.data.frame(data.test), 
            aes(x=data.test$weight*I(data.test$horsepower*
              data.test$displacement*data.test$weight)^2, 
              y=data.test$mpg)) 
          + geom_line(aes(x=data.test$weight*I(data.test$horsepower*
              data.test$displacement*data.test$weight)^2, 
              y=predict(m04,data.test), color="red"))))
pause()
## ------------------------------------------------------------------------
# Calculamos la media de mpg
medianMPG = median(mpg)
# Y etiquetamos los datos con valores 0/1 en función de si son
# mayores o menores que la mediana de mpg
mpg01 = (mpg >= medianMPG)*1
# Y creamos un dataframe con estos datos, eliminando
# los nombres del dataframe. También generamos un
# dataframe con los datos de test
datos = data.frame(Auto,as.factor(mpg01))
colnames(datos)[ncol(datos)]="mpg01"
datos$name = NULL
datos.test = data.frame(datos[-i,])
pause()
## ------------------------------------------------------------------------
# Calculamos la regresión logística
attach(datos)
glmModelErrorTest <- function(glm_model, trainingIndex, data, testData, getPredicts = F) {
  # Hacemos la predicción con los datos de test
  glm.prediction.model.train = predict(glm_model, type="response")
  glm.prediction.model.test = predict(object=glm_model, data.frame(testData), type="response")
  # Y calculamos las etiquetas con predict
  glm.prediction.model.train.y = glm.prediction.model.train
  glm.prediction.model.train.y[glm.prediction.model.train.y <= 0.5] = 0
  glm.prediction.model.train.y[glm.prediction.model.train.y > 0.5] = 1
  glm.prediction.model.test.y = glm.prediction.model.test
  glm.prediction.model.test.y[glm.prediction.model.test.y <= 0.5] = 0
  glm.prediction.model.test.y[glm.prediction.model.test.y > 0.5] = 1
  # Generamos la matriz de confusión con los datos de test
  print(table(glm.prediction.model.train.y, data$mpg01[trainingIndex]))
  print(table(glm.prediction.model.test.y, testData$mpg01))
  if(!getPredicts){
    cat("Ein: ", mean(glm.prediction.model.train.y != mpg01[trainingIndex]) ,"\n")
    cat("Eout: ", mean(glm.prediction.model.test.y != testData$mpg01) ,"\n")
  }
  else
    list(glm.prediction.model.test, glm.prediction.model.train)
}

glm.model1 = glm(formula = mpg01 ~ weight, 
     data = datos, subset = i, family = "binomial")

glmModelErrorTest(glm.model1, i, datos, datos.test)
pause()
## ------------------------------------------------------------------------
glm.model2 = glm(mpg01 ~ horsepower, data = datos,
   subset = i, family = "binomial")

glmModelErrorTest(glm.model2, i, datos, datos.test)
pause()
## ------------------------------------------------------------------------
glm.model3 = glm(mpg01 ~ displacement, data = datos,
  subset = i, family = "binomial")
glmModelErrorTest(glm.model3, i, datos, datos.test)
pause()
## ------------------------------------------------------------------------
glm.model4 = glm(mpg01 ~ weight*horsepower*displacement, 
  data = datos, subset = i, family = "binomial")
glmModelErrorTest(glm.model4, i, datos, datos.test)
pause()
## ------------------------------------------------------------------------
glm.model5 = glm(mpg01 ~ weight*I(horsepower+displacement)^2, 
  data = datos, subset = i, family="binomial")
glmModelErrorTest(glm.model5, i, datos, datos.test)
pause()
## ------------------------------------------------------------------------
glm.model6 = glm(mpg01 ~ weight*I(horsepower*displacement*weight)^2, 
  data = datos, subset = i, family = "binomial")
glmModelErrorTest(glm.model6, i, datos, datos.test)
pause()

## ------------------------------------------------------------------------
set.seed(1)

normalize <- function(data) {
    apply(X=data, MARGIN=2, FUN=function(x) {
        max <- max(x)
        min <- min(x)
        sapply(X=x, FUN=function(xi) (xi-min)/(max-min))
    })
}
# normalizamos los datos

caracteristicas = data.frame(horsepower, weight, displacement)
names(caracteristicas) = c("horsepower", "weight", "displacement")

vs_train = data.frame(data.frame(normalize(caracteristicas[i,])), as.factor(mpg01[i]))
colnames(vs_train) = c("horsepower", "weight", "displacement", "mpg01")
vs_test = data.frame(data.frame(normalize(caracteristicas[-i,])), datos.test$mpg01)
colnames(vs_test) = c("horsepower", "weight", "displacement", "mpg01")
pause()
# esta función clasifica los datos con KNN
learnKNN <- function(train, test, label, testLabel, k, pintarM = T, useProb=F){
   knn.model=knn(train, test, label, k = k, prob = useProb)
   if(pintarM) print(table(knn.model, testLabel))
     knn.model
   }
knn1=learnKNN(train=subset(vs_train, select=-mpg01) , test=subset(vs_test, select=-mpg01),
  vs_train$mpg01, vs_test$mpg01, k = 1)
error1 = mean(knn1 != datos.test$mpg01)
cat("Eout: ", error1, "\n")