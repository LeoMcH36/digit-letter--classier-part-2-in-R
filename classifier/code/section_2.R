setwd("C:/assignment3_40225145/code")

#set seed so random values are the same each run
set.seed(42)

#section 2.1

#https://rdocumentation.org/packages/class/versions/7.3-17
#for knn
library(class) 

#https://rdocumentation.org/packages/data.table/versions/1.14.0
#for fread 
library(data.table)

#https://rdocumentation.org/packages/caret/versions/6.0-86
#for model training
library(caret)


#import features
features_knn = fread("../40225145_features.csv",nrows= 168)

#create new column for image category and set letter as default
features_knn$Category <-"letter"

#vectors to store math symbol label names to be compared to
symbols <- c("less","greater","equal","lessequal","greaterequal","notequal","approxequal")

#vectors to store digit label names to be compared to
digits <- c("one","two","three","four","five","six","seven")



#make checks and set category value to correct value
features_knn$Category[features_knn$label %in% symbols == TRUE] <- "symbol"
features_knn$Category[features_knn$label %in% digits == TRUE] <- "digit"



#summarise results
summary(features_knn[,3:8]) 

#create predictions for different k values
Knn.pred.1 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 1)
Knn.pred.1
Knn.pred.3 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 3)
Knn.pred.3
Knn.pred.5 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 5)
Knn.pred.5
Knn.pred.7 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 7)
Knn.pred.7
Knn.pred.9 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 9)
Knn.pred.9
Knn.pred.11 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 11)
Knn.pred.11
Knn.pred.13 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 13)
Knn.pred.13
Knn.pred.15 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 15)
Knn.pred.15
Knn.pred.17 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 17)
Knn.pred.17
Knn.pred.19 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 19)
Knn.pred.19
Knn.pred.21 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 21)
Knn.pred.21
Knn.pred.23 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 23)
Knn.pred.23
Knn.pred.25 <- knn(features_knn[, c(3:8)],  features_knn[, c(3:8)],  features_knn$Category, k = 25)
Knn.pred.25


#get accuracy for each model


correct_items_k1 = Knn.pred.1 == features_knn[["Category"]] 
accuracy_k1 <- (nrow(features_knn[correct_items_k1,])/nrow(features_knn)) * 100
accuracy_k1


correct_items_k3 = Knn.pred.3 == features_knn[["Category"]] 
accuracy_k3 <- (nrow(features_knn[correct_items_k3,])/nrow(features_knn)) * 100
accuracy_k3



correct_items_k5 = Knn.pred.5 == features_knn[["Category"]] 
accuracy_k5 <- (nrow(features_knn[correct_items_k5,])/nrow(features_knn)) * 100
accuracy_k5



correct_items_k7 = Knn.pred.7 == features_knn[["Category"]] 
accuracy_k7 <- (nrow(features_knn[correct_items_k7,])/nrow(features_knn)) * 100
accuracy_k7


correct_items_k9 = Knn.pred.9 == features_knn[["Category"]] 
accuracy_k9 <- (nrow(features_knn[correct_items_k9,])/nrow(features_knn)) * 100
accuracy_k9


correct_items_k11 = Knn.pred.11 == features_knn[["Category"]] 
accuracy_k11 <- (nrow(features_knn[correct_items_k11,])/nrow(features_knn)) * 100
accuracy_k11


correct_items_k13 = Knn.pred.13 == features_knn[["Category"]] 
accuracy_k13 <- (nrow(features_knn[correct_items_k13,])/nrow(features_knn)) * 100
accuracy_k13


correct_items_k15 = Knn.pred.15 == features_knn[["Category"]] 
accuracy_k15 <- (nrow(features_knn[correct_items_k15,])/nrow(features_knn)) * 100
accuracy_k15

correct_items_k17 = Knn.pred.17 == features_knn[["Category"]] 
accuracy_k17 <- (nrow(features_knn[correct_items_k17,])/nrow(features_knn)) * 100
accuracy_k17


correct_items_k19 = Knn.pred.19 == features_knn[["Category"]] 
accuracy_k19 <- (nrow(features_knn[correct_items_k19,])/nrow(features_knn)) * 100
accuracy_k19


correct_items_k21 = Knn.pred.21 == features_knn[["Category"]] 
accuracy_k21 <- (nrow(features_knn[correct_items_k21,])/nrow(features_knn)) * 100
accuracy_k21


correct_items_k23 = Knn.pred.23 == features_knn[["Category"]] 
accuracy_k23 <- (nrow(features_knn[correct_items_k23,])/nrow(features_knn)) * 100
accuracy_k23


correct_items_k25 = Knn.pred.25 == features_knn[["Category"]] 
accuracy_k25 <- (nrow(features_knn[correct_items_k25,])/nrow(features_knn)) * 100
accuracy_k25

#https://www.rdocumentation.org/packages/RColorBrewer
#library to get colours for graphs
library(RColorBrewer)

#https://rdocumentation.org/packages/gt/versions/0.2.2
#used to create table
library(gt)

#create colour pallete
diffColor <- brewer.pal(6, "Blues")


#vector to hold k values for column
k_value <- c(1,3,5,7,9,11,13,15,17,19,21,23,25)

#column to hold accuracies
accuracies <- c(accuracy_k1,accuracy_k3,accuracy_k5,accuracy_k7,accuracy_k9,accuracy_k11,accuracy_k13,accuracy_k15,accuracy_k17,accuracy_k19,accuracy_k21,accuracy_k23,accuracy_k25)

#create table
classificationAcc <- data.frame("k-value" = k_value,"Accuracy" = accuracies)

#https://gt.rstudio.com/articles/intro-creating-gt-tables.html
classificationAcc <- classificationAcc %>%
  gt() %>%
  tab_header(
    title = md("Accuracy in % of knn model by k value")
    
  )
#https://gt.rstudio.com/reference/data_color.html
data_color(
  classificationAcc,
  "Accuracy",
  colors = scales::col_numeric(
    palette = diffColor,
    domain = c(0:100) 
  )
  
)

#section 2.2

#set cross validation
train_control <- trainControl(method="cv", number=5)
# train the model
model_cv25 <- train(Category ~ nr_pix + rows_with_2 + cols_with_2 + rows_with_3p + cols_with_3p + height, data=features_knn, tuneGrid   = expand.grid(k = c(1,3,5,7,9,11,13,15,17,19,21,23,25)), trControl=train_control, method="knn" )
#  summarize results
model_cv25

#loop for model accuracies
accuracies_cv <- numeric()
for( i in 1:13)
{
  #add accuradcies to vector
  accuracies_cv <- append(accuracies_cv,model_cv25$results[i,2],after = length(accuracies_cv))
}

accuracies_cv

#create colour pallete
diffColor2 <- brewer.pal(6, "Greens")

#create table
classificationAccCV <- data.frame("k-value" = k_value,"Accuracy" = accuracies_cv)

classificationAccCV <- classificationAccCV %>%
  gt() %>%
  tab_header(
    title = md("Accuracy in % of knn model by k value")
    
  )
#https://gt.rstudio.com/reference/data_color.html
data_color(
  classificationAccCV,
  "Accuracy",
  colors = scales::col_numeric(
    palette = diffColor2,
    domain = c(0:1) 
  )
  
)

#get error rates.
errorRate_class <- (1 - (accuracies/100) )
errorRate_class

errorRate_cv <- (1 - accuracies_cv)
errorRate_cv

#create vectors to store x,y values
tests_class<- data.frame((1/k_value),errorRate_class)
tests_class
tests_cv <- data.frame((1/k_value),errorRate_cv)
tests_cv

#x axis scale
one_over_k <- c(0.01,0.02,0.05,0.10,0.20,0.50,1.00)
one_over_k

# Plot the graph.
plot(log = "x",tests_class,type = "o",col = "#fca121", xlab = "1/k", ylab = "Error Rate", 
     main = "Line Graph For Knn",ylim=c(0.00,0.25))

lines(log = "x",tests_cv, type = "o", col = "#2acdfa")
legend("bottomleft",
       c("Cross Validated Classification Error Rate","Classification Error rate"),
       fill=c("#2acdfa","#fca121"),cex = 0.8
)

