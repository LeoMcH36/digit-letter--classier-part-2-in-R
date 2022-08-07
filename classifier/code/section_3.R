setwd("C:/assignment3_40225145/code")

#set seed so random values are consistent for each run
set.seed(42)

#https://rdocumentation.org/packages/caret/versions/6.0-86
library(caret)

#import new data features
doodleData <-  read.csv("../40225145_2100items_features.csv",sep = "\t",header = FALSE)

#create column names for data
names(doodleData) <- c("label","index","nr_pix","rows_with_2","cols_with_2" ,"rows_with_3p","cols_with_3p","height","width","left2tile","right2tile","verticalness","top2tile","bottom2tile","horizontalness","curvedness")

#set up cross-validation
train_control <- trainControl(method="cv", number=5)

#https://rpubs.com/phamdinhkhanh/389752
#create grid
tunegrid <- expand.grid(.mtry=c(2,4,6,8))


#create model for each tree number

model_Doodle_cv25 <- train(label~.,
                           data = doodleData,
                           method = 'rf',
                           metric = 'Accuracy',
                           tuneGrid = tunegrid,
                           trControl = train_control,
                           ntree = 25)

model_Doodle_cv50 <- train(label~.,
                           data = doodleData,
                           method = 'rf',
                           metric = 'Accuracy',
                           tuneGrid = tunegrid,
                           trControl = train_control,
                           ntree = 50)
model_Doodle_cv75 <- train(label~.,
                           data = doodleData,
                           method = 'rf',
                           metric = 'Accuracy',
                           tuneGrid = tunegrid,
                           trControl = train_control,
                           ntree = 75)
model_Doodle_cv100 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 100)
model_Doodle_cv125 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 125)
model_Doodle_cv150 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 150)
model_Doodle_cv175 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 175)
model_Doodle_cv200 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 200)
model_Doodle_cv225 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 225)
model_Doodle_cv250 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 250)
model_Doodle_cv275 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 275)
model_Doodle_cv300 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 300)
model_Doodle_cv325 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 325)
model_Doodle_cv350 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 350)
model_Doodle_cv375 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 375)
model_Doodle_cv400 <- train(label~.,
                            data = doodleData,
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid,
                            trControl = train_control,
                            ntree = 400)


#create function to find highest accuracy from model
sorting <- function(mtry2,mtry4,mtry6,mtry8)
{
  most_accurate<- max(c(mtry2,mtry4,mtry6,mtry8))
  
 
  return (most_accurate)
}

#create function to find the number of predictors for the highest accuracy
get_np <- function(modelBest,mtry2,mtry4,mtry6,mtry8)
{
  if(modelBest == mtry2)
  {
    np = 2
  }
  else if(modelBest == mtry4)
  {
    np = 4
  }
  else if(modelBest == mtry6)
  {
    np = 6
  }
  else
  {
    np = 8
  }
  return(np)
}

#apply functions
#number of trees = 25
model_Doodle_cv25
model_Doodle_cv25_best <- sorting(model_Doodle_cv25$results[1,2],model_Doodle_cv25$results[2,2],model_Doodle_cv25$results[3,2],model_Doodle_cv25$results[4,2])
model_Doodle_cv25_best
model_Doodle_cv25_np <- get_np(model_Doodle_cv25_best,model_Doodle_cv25$results[1,2],model_Doodle_cv25$results[2,2],model_Doodle_cv25$results[3,2],model_Doodle_cv25$results[4,2])
model_Doodle_cv25_np

#number of trees = 50
model_Doodle_cv50
model_Doodle_cv50_best <- sorting(model_Doodle_cv50$results[1,2],model_Doodle_cv50$results[2,2],model_Doodle_cv50$results[3,2],model_Doodle_cv50$results[4,2])
model_Doodle_cv50_best
model_Doodle_cv50_np <- get_np(model_Doodle_cv50_best,model_Doodle_cv50$results[1,2],model_Doodle_cv50$results[2,2],model_Doodle_cv50$results[3,2],model_Doodle_cv50$results[4,2])
model_Doodle_cv50_np

#number of trees = 75
model_Doodle_cv75
model_Doodle_cv75_best <- sorting(model_Doodle_cv75$results[1,2],model_Doodle_cv75$results[2,2],model_Doodle_cv75$results[3,2],model_Doodle_cv75$results[4,2])
model_Doodle_cv75_best
model_Doodle_cv75_np <- get_np(model_Doodle_cv75_best,model_Doodle_cv75$results[1,2],model_Doodle_cv75$results[2,2],model_Doodle_cv75$results[3,2],model_Doodle_cv75$results[4,2])
model_Doodle_cv75_np

#number of trees = 100
model_Doodle_cv100
model_Doodle_cv100_best <- sorting(model_Doodle_cv100$results[1,2],model_Doodle_cv100$results[2,2],model_Doodle_cv100$results[3,2],model_Doodle_cv100$results[4,2])
model_Doodle_cv100_best
model_Doodle_cv100_np <- get_np(model_Doodle_cv100_best,model_Doodle_cv100$results[1,2],model_Doodle_cv100$results[2,2],model_Doodle_cv100$results[3,2],model_Doodle_cv100$results[4,2])
model_Doodle_cv100_np

#number of trees = 125
model_Doodle_cv125
model_Doodle_cv125_best <- sorting(model_Doodle_cv125$results[1,2],model_Doodle_cv125$results[2,2],model_Doodle_cv125$results[3,2],model_Doodle_cv125$results[4,2])
model_Doodle_cv125_best
model_Doodle_cv125_np <- get_np(model_Doodle_cv125_best,model_Doodle_cv125$results[1,2],model_Doodle_cv125$results[2,2],model_Doodle_cv125$results[3,2],model_Doodle_cv125$results[4,2])
model_Doodle_cv125_np

#number of trees = 150
model_Doodle_cv150
model_Doodle_cv150_best <- sorting(model_Doodle_cv150$results[1,2],model_Doodle_cv150$results[2,2],model_Doodle_cv150$results[3,2],model_Doodle_cv150$results[4,2])
model_Doodle_cv150_best
model_Doodle_cv150_np <- get_np(model_Doodle_cv150_best,model_Doodle_cv150$results[1,2],model_Doodle_cv150$results[2,2],model_Doodle_cv150$results[3,2],model_Doodle_cv150$results[4,2])
model_Doodle_cv150_np

#number of trees = 175
model_Doodle_cv175
model_Doodle_cv175_best <- sorting(model_Doodle_cv175$results[1,2],model_Doodle_cv175$results[2,2],model_Doodle_cv175$results[3,2],model_Doodle_cv175$results[4,2])
model_Doodle_cv175_best
model_Doodle_cv175_np <- get_np(model_Doodle_cv175_best,model_Doodle_cv175$results[1,2],model_Doodle_cv175$results[2,2],model_Doodle_cv175$results[3,2],model_Doodle_cv175$results[4,2])
model_Doodle_cv175_np

#number of trees = 200
model_Doodle_cv200
model_Doodle_cv200_best <- sorting(model_Doodle_cv200$results[1,2],model_Doodle_cv200$results[2,2],model_Doodle_cv200$results[3,2],model_Doodle_cv200$results[4,2])
model_Doodle_cv200_best
model_Doodle_cv200_np <- get_np(model_Doodle_cv200_best,model_Doodle_cv200$results[1,2],model_Doodle_cv200$results[2,2],model_Doodle_cv200$results[3,2],model_Doodle_cv200$results[4,2])
model_Doodle_cv200_np

#number of trees = 225
model_Doodle_cv225
model_Doodle_cv225_best <- sorting(model_Doodle_cv225$results[1,2],model_Doodle_cv225$results[2,2],model_Doodle_cv225$results[3,2],model_Doodle_cv225$results[4,2])
model_Doodle_cv225_best
model_Doodle_cv225_np <- get_np(model_Doodle_cv225_best,model_Doodle_cv225$results[1,2],model_Doodle_cv225$results[2,2],model_Doodle_cv225$results[3,2],model_Doodle_cv225$results[4,2])
model_Doodle_cv225_np

#number of trees = 250
model_Doodle_cv250
model_Doodle_cv250_best <- sorting(model_Doodle_cv250$results[1,2],model_Doodle_cv250$results[2,2],model_Doodle_cv250$results[3,2],model_Doodle_cv250$results[4,2])
model_Doodle_cv250_best
model_Doodle_cv250_np <- get_np(model_Doodle_cv250_best,model_Doodle_cv250$results[1,2],model_Doodle_cv250$results[2,2],model_Doodle_cv250$results[3,2],model_Doodle_cv250$results[4,2])
model_Doodle_cv250_np

#number of trees = 275
model_Doodle_cv275
model_Doodle_cv275_best <- sorting(model_Doodle_cv275$results[1,2],model_Doodle_cv275$results[2,2],model_Doodle_cv275$results[3,2],model_Doodle_cv275$results[4,2])
model_Doodle_cv275_best
model_Doodle_cv275_np <- get_np(model_Doodle_cv275_best,model_Doodle_cv275$results[1,2],model_Doodle_cv275$results[2,2],model_Doodle_cv275$results[3,2],model_Doodle_cv275$results[4,2])
model_Doodle_cv275_np

#number of trees = 300
model_Doodle_cv300
model_Doodle_cv300_best <- sorting(model_Doodle_cv300$results[1,2],model_Doodle_cv300$results[2,2],model_Doodle_cv300$results[3,2],model_Doodle_cv300$results[4,2])
model_Doodle_cv300_best
model_Doodle_cv300_np <- get_np(model_Doodle_cv300_best,model_Doodle_cv300$results[1,2],model_Doodle_cv300$results[2,2],model_Doodle_cv300$results[3,2],model_Doodle_cv300$results[4,2])
model_Doodle_cv300_np

#number of trees = 325
model_Doodle_cv325
model_Doodle_cv325_best <- sorting(model_Doodle_cv325$results[1,2],model_Doodle_cv325$results[2,2],model_Doodle_cv325$results[3,2],model_Doodle_cv325$results[4,2])
model_Doodle_cv325_best
model_Doodle_cv325_np <- get_np(model_Doodle_cv325_best,model_Doodle_cv325$results[1,2],model_Doodle_cv325$results[2,2],model_Doodle_cv325$results[3,2],model_Doodle_cv325$results[4,2])
model_Doodle_cv325_np

#number of trees = 350
model_Doodle_cv350
model_Doodle_cv350_best <- sorting(model_Doodle_cv350$results[1,2],model_Doodle_cv350$results[2,2],model_Doodle_cv350$results[3,2],model_Doodle_cv350$results[4,2])
model_Doodle_cv350_best
model_Doodle_cv350_np <- get_np(model_Doodle_cv350_best,model_Doodle_cv350$results[1,2],model_Doodle_cv350$results[2,2],model_Doodle_cv350$results[3,2],model_Doodle_cv350$results[4,2])
model_Doodle_cv350_np

#number of trees = 375
model_Doodle_cv375
model_Doodle_cv375_best <- sorting(model_Doodle_cv375$results[1,2],model_Doodle_cv375$results[2,2],model_Doodle_cv375$results[3,2],model_Doodle_cv375$results[4,2])
model_Doodle_cv375_best
model_Doodle_cv375_np <- get_np(model_Doodle_cv375_best,model_Doodle_cv375$results[1,2],model_Doodle_cv375$results[2,2],model_Doodle_cv375$results[3,2],model_Doodle_cv375$results[4,2])
model_Doodle_cv375_np

#number of trees = 400
model_Doodle_cv400
model_Doodle_cv400_best <- sorting(model_Doodle_cv400$results[1,2],model_Doodle_cv400$results[2,2],model_Doodle_cv400$results[3,2],model_Doodle_cv400$results[4,2])
model_Doodle_cv400_best
model_Doodle_cv400_np <- get_np(model_Doodle_cv400_best,model_Doodle_cv400$results[1,2],model_Doodle_cv400$results[2,2],model_Doodle_cv400$results[3,2],model_Doodle_cv400$results[4,2])
model_Doodle_cv400_np


#store highest values from each model for graph
best_values <- c(model_Doodle_cv25_best,model_Doodle_cv50_best,model_Doodle_cv75_best,
                       model_Doodle_cv100_best,model_Doodle_cv125_best,model_Doodle_cv150_best,
                       model_Doodle_cv175_best,model_Doodle_cv200_best,model_Doodle_cv225_best,
                       model_Doodle_cv250_best,model_Doodle_cv275_best,model_Doodle_cv300_best,
                       model_Doodle_cv325_best,model_Doodle_cv350_best,model_Doodle_cv375_best,
                       model_Doodle_cv400_best)

#get the highest model
highestAccuracy <- max(best_values)
highestAccuracy

#get model with highest accuracy, number of trees and number of random predictors

index <- 1

for(i in 1:16)
{
  if(highestAccuracy == best_values[i])
  {
    index = i
  }
 
}
NT <- switch(index,25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400)
NP <- switch(index,model_Doodle_cv25_np,model_Doodle_cv50_np,model_Doodle_cv75_np,
                    model_Doodle_cv100_np,model_Doodle_cv125_np,model_Doodle_cv150_np,
                    model_Doodle_cv175_np,model_Doodle_cv200_np,model_Doodle_cv225_np,
                    model_Doodle_cv250_np,model_Doodle_cv275_np,model_Doodle_cv300_np,
                    model_Doodle_cv325_np,model_Doodle_cv350_np,model_Doodle_cv375_np,
                    model_Doodle_cv400_np)


#Best Combination Of predictors and Trees
#Number of Trees
NT
#Number of Predictors
NP

#Resulting Accuracy
highestAccuracy

#x axis
numtrees <- c(25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400)

#vector for (x,y)
graph_data <- data.frame(best_values,numtrees)
graph_data

#create scatter graph
model_scatterplot <- ggplot(graph_data, aes(x=numtrees, y=best_values ))  + geom_point(size = 6,shape="circle", color = "#379de6") +
  scale_x_continuous(breaks = round(seq(min(numtrees), max(numtrees), by = 25),1)) + geom_smooth(method=NULL, se=FALSE,color="#ffffff00") + labs(title="Best Accuracies Of Random Forest Models",x = "Number of Trees",y = "Highest Accuracy") 

model_scatterplot






#section 3.2

#set up cross validation
train_control <- trainControl(method="cv", number=5)

#https://rpubs.com/phamdinhkhanh/389752
#create grid
tunegrid <- expand.grid(.mtry=c(2,4,6,8))

#vector to store accuracies
Accuracies <- numeric()

#refitting model 15 times using best combination of trees and predictors
for(i in 1:15)
{
  model_Doodle_Refit<- train(label~.,
                              data = doodleData,
                              method = 'rf',
                              metric = 'Accuracy',
                              tuneGrid = tunegrid,
                              trControl = train_control,
                              ntree = NT)
  #add accuracies to vector
  Accuracies <- append(Accuracies,model_Doodle_Refit$results[4,2],after = length(Accuracies)) 
}

Accuracies

#get mean
model_mean <- mean(Accuracies)

#get standard deviation
model_SD <- sd(Accuracies)

model_mean
model_SD




#section 3.3





# train the model
model_knn <- train(label ~ nr_pix + rows_with_2 + cols_with_2 + rows_with_3p + cols_with_3p + height + width + left2tile + right2tile + verticalness + top2tile + bottom2tile + horizontalness, data=doodleData, tuneGrid   = expand.grid(k = c(1,3,5,7,9,11,13,15,17,19,21,23,25)), trControl=train_control, method="knn" )
#  summarize results
model_knn

#remove nr_pix to improve accuracy
model_knn <- train(label ~  rows_with_2 + cols_with_2 + rows_with_3p + cols_with_3p + height + width + left2tile + right2tile + verticalness + top2tile + bottom2tile + horizontalness, data=doodleData, tuneGrid   = expand.grid(k = c(1,3,5,7,9,11,13,15,17,19,21,23,25)), trControl=train_control, method="knn" )
#  summarize results
model_knn

#remove top2tile to improve accuracy
model_knn <- train(label ~  rows_with_2 + cols_with_2 + rows_with_3p + cols_with_3p + height + width + left2tile + right2tile + verticalness + bottom2tile + horizontalness, data=doodleData, tuneGrid   = expand.grid(k = c(1,3,5,7,9,11,13,15,17,19,21,23,25)), trControl=train_control, method="knn" )
#  summarize results
model_knn

#get highest accuracy
highest_accuracy <- max(model_knn$results[1,2],model_knn$results[2,2],model_knn$results[3,2],model_knn$results[4,2],model_knn$results[5,2],model_knn$results[6,2],model_knn$results[7,2],model_knn$results[8,2],model_knn$results[9,2],model_knn$results[10,2],model_knn$results[11,2],model_knn$results[12,2])
highest_accuracy


#naive bayes
#remove un-useful columns and get values
Doodle_values<-doodleData[,c(-1,-2,-16)]
Doodle_values

#get labels
Doodle_labels <- doodleData$label
Doodle_labels

#train model
model_nb = train(Doodle_values,Doodle_labels,'nb',trControl=trainControl(method='cv',number=5))
model_nb

#vector to hold accuracies
Accuracies2 <- numeric()

#grid
tunegrid_test <- expand.grid(.mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13))

#run model using best number of trees for all numbers of predictors
model_Doodle_Refit2<- train(label~.,
                            data = doodleData[,c(-2,-3,-13,-16)],
                            method = 'rf',
                            metric = 'Accuracy',
                            tuneGrid = tunegrid_test,
                            trControl = train_control,
                            ntree = NT)

model_Doodle_Refit2


#get number of predictors which was most accurate
  np <- which.max(c(model_Doodle_Refit2$results[1,2],model_Doodle_Refit2$results[2,2],
                    model_Doodle_Refit2$results[3,2],model_Doodle_Refit2$results[4,2],
                    model_Doodle_Refit2$results[5,2],model_Doodle_Refit2$results[6,2],
                    model_Doodle_Refit2$results[7,2],model_Doodle_Refit2$results[8,2],
                    model_Doodle_Refit2$results[9,2],model_Doodle_Refit2$results[10,2],
                    model_Doodle_Refit2$results[11,2],model_Doodle_Refit2$results[12,2],
                    model_Doodle_Refit2$results[13,2]))
  
np

#grid using best number of predictors
tunegrid_best <- expand.grid(.mtry=4)
#random forests with only best predictors
#refit 15 times
for(i in 1:15)
{


  model_Doodle_Refit2<- train(label~.,
                             data = doodleData[,c(-2,-3,-13,-16)],
                             method = 'rf',
                             metric = 'Accuracy',
                             tuneGrid = tunegrid_best,
                             trControl = train_control,
                             ntree = NT)
  model_Doodle_Refit2
  model_Doodle_Refit2$results[1,2]
  Accuracies2 <- append(Accuracies2,model_Doodle_Refit2$results[1,2],after = length(Accuracies2)) 

}
Accuracies2

#get highest accuracy
highest_acc <- max(Accuracies2)

#get sd
standard_deviation_model <- sd(Accuracies2)

#get mean
mean_model <- mean(Accuracies2)

#highest Accuracy
highest_acc

#mean
mean_model

#SD
standard_deviation_model


