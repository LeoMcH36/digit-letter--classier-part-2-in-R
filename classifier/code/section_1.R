
setwd("C:/assignment3_40225145/code")

#https://rdocumentation.org/packages/data.table/versions/1.14.0
#for fread function
library(data.table)

#set seed so random values are the same each run
set.seed(42)



#section 1.1

#read in features
features_df <- read.csv("../40225145_features.csv")

#create new factor column to predict if math symbol and set to 0 as default
features_df$dummy.math.symbol <-0

#vector to store labels that are math symbols to use for comparison
symbols <- c("less","greater","equal","lessequal","greaterequal","notequal","approxequal")



#if row has label in symbols set factor column to 1
features_df$dummy.math.symbol[features_df$label %in% symbols == TRUE] <- 1
features_df$dummy.math.symbol

#run logistic regression
glmfit<-glm(dummy.math.symbol ~ nr_pix, 
            data = features_df, 
            family = 'binomial') 

#summarise it to get more information
summary(glmfit)

#change from 0 and 1 to no and yes so that the training and evaluating will work properly
#create new factor column to predict if math symbol and set to no as default
features_df$dummy.math.symbol <-"no"



#if row has label in symbols set factor column to yes
features_df$dummy.math.symbol[features_df$label %in% symbols == TRUE] <- "yes"

#using the logistic regression model, make predictions
predicted = predict(glmfit, features_df, type="response")

predicted

#https://rdocumentation.org/packages/ggplot2/versions/3.3.3
#to use for plots and graphs
library(ggplot2)


#for logistic regression graph 
#get range
x.range = range(features_df[["nr_pix"]])


#get x values
x.values = seq(x.range[1],x.range[2],length.out=1000)


#create the curve
fitted.curve <- data.frame(nr_pix = x.values)
fitted.curve[["dummy.math.symbol"]] = predict(glmfit, fitted.curve, type="response")

#plot graph
plt <-ggplot(features_df, aes(x=nr_pix, y=dummy.math.symbol)) + 
  geom_point(aes(colour =as.factor(dummy.math.symbol)), 
             show.legend = T, position="dodge")+
  geom_line(data=fitted.curve, colour="orange", size=1)

plt









#section 1.2


#set column to factor
features_df$dummy.math.symbol <- as.factor(features_df$dummy.math.symbol)

#store predictions
features_df[["predicted_val"]] = predict(glmfit, features_df, type="response")
features_df[["predicted_class"]] = "no"
features_df[["predicted_class"]][features_df[["predicted_val"]] > 0.5] = "yes"

#get number of correct predictions
correct_items = features_df[["predicted_class"]] == features_df[["dummy.math.symbol"]] 

features_df[["predicted_class"]]

# get proportion correct:
nrow(features_df[correct_items,])/nrow(features_df)



#https://rdocumentation.org/packages/caret/versions/6.0-86
#used for model training
library(caret)


#cross validate
train_control <- trainControl(method="cv", number=5, 
                              savePredictions=TRUE, 
                              classProbs = TRUE) 


# train the model
model <- train(dummy.math.symbol ~ nr_pix, data= features_df, trControl=train_control, method="glm",family="binomial" )

#  summarize results
print(model)

#predictions
model$pred

#accuracy
mean(model$pred$pred==model$pred$obs)




#https://rdocumentation.org/packages/gt/versions/0.2.2
#used to make table
library(gt)

#https://rdocumentation.org/packages/MLeval/versions/0.3
#for evalm function
library(MLeval)

#evaluate model
res <- evalm(model)

#ROC curve
res$roc

#results
res

#f1 score
F1 <- res$stdres$`Group 1`$Score[8]
F1
#TP
TP <- res$stdres$`Group 1`$Score[9]
TP
#FP
FP <- res$stdres$`Group 1`$Score[10]
FP
#TN
TN <- res$stdres$`Group 1`$Score[11]
TN 
#FN
FN <- res$stdres$`Group 1`$Score[12]
FN

#accuracy
Accuracy <- (TP + TN)/(TP+TN+FP+FN)

#precision
precision <- (TP/(TP + FP))
precision

#recall
Recall <- (TP/(TP + FN))

#FP Rate
False_Positive_Rate <- (FP/(FP+TN))

#table columns
model_data <- c(Accuracy,False_Positive_Rate,precision,Recall,F1)
names <- c("Accuracy", "False positive rate", "precision", "recall" , "F1-score")

#create table
model_table <- data.frame("Data" = names,"Values" = model_data)

model_table <- model_table %>%
  gt() %>%
  tab_header(
    title = md("Model Data")
    
  )

#table  
model_table




#section 1.3

#roc curve
ROC <- evalm(model,plots='r',rlinethick=0.8,fsize=8,bins=8)



