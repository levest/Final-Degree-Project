library(h2o)
h2o.init()
source("~/TFG/FunctionsTFG.R")
#Import the csv
holidays2008 <- holidaysFunction(2008)
holidays2007 <-holidaysFunction(2007)
holidays2006 <-holidaysFunction(2006)

#Put it together
dataHolidays<- rbind(holidays2008, holidays2007,holidays2006)

#remove the non necessary data
rm(holidays2008, holidays2007, holidays2006)

#Read airports csv
airports <- read.csv("~/TFG/CSV/airports.csv")

#Create new columns
dataHolidays$OriginState = "a"
dataHolidays$DestinationState = "a"

#Add states to Data Holidays
dataHolidays <- addStatesintoDataHolidays()  #CAN TAKE A VERY LONG TIME

#Factor the new fields
#Factor the data
dataHolidays$Dest<-as.factor(dataHolidays$Dest)
dataHolidays$Origin<-as.factor(dataHolidays$Origin)
dataHolidays$UniqueCarrier<-as.factor(dataHolidays$UniqueCarrier)
dataHolidays$TailNum<-as.factor(dataHolidays$TailNum)
dataHolidays$CancellationCode<-as.factor(dataHolidays$CancellationCode)
dataHolidays$OriginState<-as.factor(dataHolidays$OriginState)
dataHolidays$DestinationState<-as.factor(dataHolidays$DestinationState)
dataHolidays$CancellationCode<-as.factor(dataHolidays$CancellationCode)
dataHolidays$Cancelled<-as.factor(dataHolidays$Cancelled)
dataHolidays$Diverted<-as.factor(dataHolidays$Diverted)

#DATA CLEAN
dataHolidays <- dataHolidays[!(dataHolidays$DepDelay == ""), ]
dataHolidays <- dataHolidays[!is.na(dataHolidays$DepDelay), ]
dataHolidays <- dataHolidays[!(dataHolidays$ArrDelay == ""), ]
dataHolidays <- dataHolidays[!is.na(dataHolidays$ArrDelay), ]

#Now, we save the file after cleaning. More cleaning can be done in the future.
#write_csv(path = "~/TFG/CSV/dataHolidays.csv",x = dataHolidays)
#TO H2O
dataHolidaysH2o <- as.h2o(dataHolidays)

#Deep learning using the holidays previusly selected. We will compute the delay
#Split the data in three. Training, validation and testing.

splits <- h2o.splitFrame(dataHolidaysH2o, c(0.6,0.2), seed=1234)
train  <- h2o.assign(splits[[1]], "train.hex") # 60%
valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%
test   <- h2o.assign(splits[[3]], "test.hex")  # 20%

#We want to learn about the arribal delay
Y = "ArrDelay" 

#The variables that we will take into account are the following ones.

X= c("DepDelay","Origin","Dest","DayOfWeek","OriginState","DestinationState", "Month","DayofMonth")

model1 <- h2o.deeplearning(
  x = X,
  y = Y,
  training_frame = train,#training frame used
  model_id = "model1" ,
  validation_frame = valid #validation frame used
  )
#Save the model if needed for future 
h2o.saveModel(model1, path = "./TFG/models")
summary(model1)
model1
h2o.performance(model1, newdata=test)     ## full test data

#Prediction for comparing the results with the test
predmodel1 <- h2o.predict(model1, test)
predmodel1$predict
test$ArrDelay
str(test)

#transform to absolute all data
resultpredModel1 <- ((abs(predmodel1$predict) + abs(test$ArrDelay))/abs(predmodel1$predict))<=1.70 | ((abs(predmodel1$predict) + abs(test$ArrDelay))/abs(test$ArrDelay))<= 1.70 
resultpredModel1
mean(resultpredModel1)
h2o.hist(x = resultpredModel1)

#Tunned deep learning model
model2 <- h2o.deeplearning(
  model_id="model2_tunedModel", 
  training_frame=train, 
  validation_frame=valid,
  epochs = 15,
  input_dropout_ratio = 0.2,
  x=X, 
  y=Y, 
  train_samples_per_iteration = 100000, 
  overwrite_with_best_model=T,    
  hidden=c(120,120,120,120),          
  score_validation_samples=15000  
)

h2o.saveModel(model2, path = "./TFG/models") #Save the model
predmodel2 <- h2o.predict(model2, test)
predmodel2$predict
test$ArrDelay
str(test)
#transform to absolute all data
resultpredModel2 <- ((abs(predmodel2$predict) + abs(test$ArrDelay))/abs(predmodel2$predict))<=1.70 | ((abs(predmodel2$predict) + abs(test$ArrDelay))/abs(test$ArrDelay))<= 1.70
resultpredModel2
mean(resultpredModel2)
h2o.hist(x = resultpredModel2)
#thrid data modelh2

model3 <- h2o.deeplearning(
  model_id="model3_fastermodel", 
  training_frame=train, 
  validation_frame=valid,
  x=X,
  y=Y,
  hidden=c(32,32),                  ## small network, runs faster
  epochs=10,              
  score_validation_samples=10000      ## sample the validation dataset (faster)
)

h2o.saveModel(model3, path = "./TFG/models") #Save the model
predmodel3 <- h2o.predict(model3, test)
predmodel3$predict
test$ArrDelay
str(test)
#transform to absolute all data
resultpredModel3 <- ((abs(predmodel3$predict) + abs(test$ArrDelay))/abs(predmodel3$predict))<=1.7 | ((abs(predmodel3$predict) + abs(test$ArrDelay))/abs(test$ArrDelay))<= 1.7 
resultpredModel3
mean(resultpredModel3)
h2o.hist(x = resultpredModel3)
#GLM

GLM <- h2o.glm(x=X,y = Y,training_frame = train,model_id = "glm",validation_frame = valid, family="gaussian")
h2o.saveModel(GLM, path = "./models") #Save the model
GLM
summary (GLM)
predglm <- h2o.predict(GLM, test)
predglm$predict
test$ArrDelay
str(test)

#transform to absolute all data
resultpredGLM <- ((abs(predglm$predict) + abs(test$ArrDelay))/predglm$predict)<=1 | ((abs(predglm$predict) + abs(test$ArrDelay))/test$ArrDelay)<= 1
resultpredGLM
mean(resultpredGLM)
h2o.hist(x = resultpredGLM)


#DEEP LEARNING PART 2 WITH LESS PARAMETERS TO SEE THE DIFERENCE
Y = "ArrDelay" 
#The variables that we will take into account are the following ones.
X= c("DepDelay","Origin","Dest")
model1LessParamenters <- h2o.deeplearning(
  x = X,
  y = Y,
  training_frame = train,#training frame used
  model_id = "model1LessParameters" ,
  validation_frame = valid#validation frame used
)

h2o.saveModel(model1LessParamenters, path = "./TFG/models") #Save the model
summary(model1LessParamenters)
head(as.data.frame(h2o.varimp(model1LessParamenters)))
model1LessParamenters
h2o.performance(model1LessParamenters, newdata=test)  
predmodel1LessParamenters <- h2o.predict(model1LessParamenters, test)
predmodel1LessParamenters$predict
test$ArrDelay
str(test)

#transform to absolute all data
resultpredModelLP1 <- ((abs(predmodel1LessParamenters$predict) + abs(test$ArrDelay))/abs(predmodel1LessParamenters$predict))<=1.7 | ((abs(predmodel1LessParamenters$predict) + abs(test$ArrDelay))/abs(test$ArrDelay))<= 1.7 
resultpredModelLP1
mean(resultpredModelLP1)
h2o.hist(x = resultpredModelLP1)

#Tunned deep learning model
model2LessParameters <- h2o.deeplearning(
  model_id="model2_tunedModel_LessParameters", 
  training_frame=train, 
  validation_frame=valid,
  epochs = 15,
  input_dropout_ratio = 0.2,
  x=X, 
  y=Y, 
  train_samples_per_iteration = 100000, 
  overwrite_with_best_model=T,    
  hidden=c(120,120,120,120),          
  score_validation_samples=15000 
) 

h2o.saveModel(model2LessParameters, path = "./TFG/models") #Save the model
predmodel2LessParameters <- h2o.predict(model2LessParameters, test)
predmodel2LessParameters$predict
test$ArrDelay
str(test)

#transform to absolute all data
resultpredModelLP2 <- ((abs(predmodel2LessParameters$predict) + abs(test$ArrDelay))/abs(predmodel2LessParameters$predict))<=1.7 | ((abs(predmodel2LessParameters$predict) + abs(test$ArrDelay))/abs(test$ArrDelay))<= 1.7 
resultpredModelLP2
mean(resultpredModelLP2)
h2o.hist(x = resultpredModelLP2)

#thrid data model
model3LessParameters <- h2o.deeplearning(
  model_id="model3_fastermodel_LessParameters", 
  training_frame=train, 
  validation_frame=valid,
  x=X,
  y=Y,
  hidden=c(32,32),                  ## small network, runs faster
  epochs=10,              
  score_validation_samples=10000     ## sample the validation dataset (faster)
)
h2o.saveModel(model3LessParameters, path = "./TFG/models") #Save the model
predLessParametersLP3 <- h2o.predict(model3LessParameters, test)
predLessParameters$predict
test$ArrDelay
str(test)
#transform to absolute all data
resultpredModelLP3 <- ((abs(predLessParametersLP3$predict) + abs(test$ArrDelay))/abs(predLessParametersLP3$predict))<=1.7 | ((abs(predLessParametersLP3$predict) + abs(test$ArrDelay))/abs(test$ArrDelay))<= 1.7 
resultpredModelLP3
mean(resultpredModelLP3)
h2o.hist(x = resultpredModelLP3)
#GLM

glmLessParameters <- h2o.glm(x=X,y = Y,training_frame = train,model_id = "glmLessParameters",validation_frame = valid, family="gaussian")
h2o.saveModel(glmLessParameters, path = "./TFG/models") #Save the model
glmLessParameters
summary (glmLessParameters)
predglmLessParameters <- h2o.predict(glmLessParameters, test)
predglmLessParameters$predict
test$ArrDelay
str(test)

#transform to absolute all data
resultpredglmLessParameters <- ((abs(predglmLessParameters$predict) + abs(test$ArrDelay))/abs(predglmLessParameters$predict))<=1.7 | ((abs(predglmLessParameters$predict) + abs(test$ArrDelay))/abs(test$ArrDelay))<= 1.7
resultpredglmLessParameters
mean(resultpredglmLessParameters)
h2o.hist(x = resultpredglmLessParameters)

