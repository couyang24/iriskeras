library(keras)
library(tensorflow)
library(tidyverse)
library(corrplot)
library(caret)


data(iris)

head(iris)
str(iris)
dim(iris)
names(iris)

iris%>%ggplot(aes(Petal.Width,Petal.Length,col=Species))+geom_point(size=3,alpha=.5)+
  labs(x='Petal Length',y='Petal Width')


cor(iris$Petal.Length,iris$Petal.Width)
corrplot.mixed(cor(iris[,-5]))

"""normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}"""

iris[,5]<-iris[,5]%>%as.numeric()-1

iris<-as.matrix(iris)
iris%>%tail()

dimnames(iris)<-NULL
dimnames(iris)

iris2<-normalize(iris[,1:4])

set.seed(0)

inTrain<-createDataPartition(iris[,1],p=.67,list=F)
iris.training <- iris2[inTrain,]
iris.testing  <- iris2[-inTrain,]

iristarget.training <- iris[inTrain,5]
iristarget.testing  <- iris[-inTrain,5]

rm(inTrain,iris,iris2)

iristarget.training<-iristarget.training %>% to_categorical()


#print(iristarget.training)

model<-keras_model_sequential()

model %>% 
  layer_dense(units=128,activation='relu',input_shape=c(4))%>%
  layer_dense(units=64,activation='relu')%>%
  layer_dense(units=32,activation='relu')%>%
  layer_dense(units=3,activation='softmax')

"""summary(model)
get_config(model)
get_layer(model, index = 1)
model$layers
model$inputs
model$outputs"""

model%>%compile(
  loss='categorical_crossentropy',
  optimizer='adam',
  metrics='accuracy'
)

history<-model %>%  fit(
  iris.training,
  iristarget.training,
  epochs=200,
  batch_size=5,
  validation_split=.1
)

plot(history)

pred<-model %>% predict_classes(iris.testing,batch_size=128)

confusionMatrix(iristarget.testing,pred)
