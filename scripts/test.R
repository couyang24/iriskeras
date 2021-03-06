library(tidyverse)
library(keras)
library(tensorflow)
library(tidyverse)

setwd("C:/Users/oouyang/Dropbox/Analysis/Database/Digit Recognizer")

train<-data.matrix(read_csv("train.csv"))
test<-data.matrix(read_csv("test.csv"))

dim(train)
dim(test)

colnames(train)

train[,1] %>% table() %>% barplot(col=1:10)

train[4,-1] %>% matrix(nrow=sqrt(dim(train)[2]-1),byrow=T) %>% apply(2,rev)%>% t() %>% image(col=grey.colors(255))

train.label<-train[,1] %>% to_categorical()

train.feature<-train[,-1] %>% normalize()
test.feature<-test %>% normalize()

dim(train.feature)<-c(nrow(train.feature),28,28,1)
dim(test.feature)<-c(nrow(test.feature),28,28,1)

model<-keras_model_sequential()

model %>% 
  layer_conv_2d(filters = 32, kernel_size = c(5,5),padding = 'Same',
                activation = 'relu', input_shape = c(28,28,1))%>%
  layer_conv_2d(filters = 32, kernel_size = c(5,5),padding = 'Same', 
                activation ='relu')%>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3),padding = 'Same',
                activation = 'relu')%>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),padding = 'Same', 
                activation ='relu')%>%
  layer_max_pooling_2d(pool_size = c(2, 2), strides=c(2,2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units=1024,activation='relu')%>%
  layer_dense(units=512,activation='relu')%>%
  layer_dense(units=256,activation='relu')%>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units=10,activation='softmax')

model%>%compile(
  loss='categorical_crossentropy',
  optimizer='adam',
  metrics='accuracy'
)

history<-model %>%  fit(
  train.feature,
  train.label,
  epochs=1,
  batch_size=500,
  validation_split=.1
)

plot(history)
