audi <- filter(df,brand=="audi")
table(audi$model)
audi_f6 <- tail(sort(table(audi$model)))

bmw <- filter(df,brand=="bmw")
table(bmw$model)
bmw_f6 <- tail(sort(table(bmw$model)))

ford <- filter(df,brand=="ford")
ford_f6 <- tail(sort(table(ford$model)))


top_3_cars <- matrix(c(audi_f6,bmw_f6,ford_f6),nrow = 3,byrow = T,dimnames = list(c("audi","bmw","ford"),
                                                                                  c(1,2,3,4,5,6)))

top_3_cars
?matrix

chi_test_result <- chisq.test(top_3_cars)
print(chi_test_result)

bubble_df <- df[,c("price","milage","transmission")]

bubble_df$milage[bubble_df$milage==0] <- 1
min(bubble_df$price)
min(bubble_df$milage)


bubble_df$transmission <- relevel(bubble_df$transmission, ref = "Unknown")
bubble_model <- lm(log(price)~log(milage)+transmission,data=bubble_df)
summary(bubble_model)

ad.test(df$price)


# Shapiro-Wilk test for normality for each transmission type
library(dplyr)
df %>%
  group_by(transmission) %>%
  summarise(p_value = ad.test(price)$p.value)

# Alternatively, visualize using Q-Q plots
par(mfrow = c(2, 2)) # Layout for 4 plots
for (trans in unique(df$transmission)) {
  qqnorm(df$price[df$transmission == trans], main = trans)
  qqline(df$price[df$transmission == trans])
}



library(nortest)
atest <- ad.test(df$price)
atest$p.value

library(car)
leveneTest(price ~ transmission, data = df)


kruskal.test(price ~ transmission, data = df)

install.packages("dunn.test")
library(dunn.test)
dunn.test(df$price, df$transmission, method = "bonferroni")


df_for_auto <- df
df_for_auto$transmission <- as.character(df_for_auto$transmission)
df_for_auto$fuel <- as.character(df_for_auto$fuel)


auto_df <- df_for_auto %>%
  filter(transmission == "Automatic" & fuel %in% c("Diesel", "Petrol", "Hybrid", "Electric", "LPG"))
table(auto_df$transmission)
auto_fuel <- table(auto_df$fuel)

manual_df <- df_for_auto %>% 
  filter(transmission== "Manual"& fuel %in% c("Diesel", "Petrol", "Hybrid", "Electric", "LPG"))
manuel_fuel <- table(manual_df$fuel)


trans_matrix <- matrix(c(auto_fuel,manuel_fuel),byrow = T,nrow=2,dimnames = list(c("Auto","Manuel"),c("Diesel","Electric","Hybrid","LPG","Petrol")))
trans_matrix


chi_test_result <- chisq.test(trans_matrix)
print(chi_test_result)

str(df)


fuel_power_df <- df
fuel_power_df$fuel <- as.character(fuel_power_df$fuel)


fp_df <- fuel_power_df %>%
  filter(fuel %in% c("Diesel", "Petrol", "Hybrid", "Electric", "LPG"))

fp_df$fuel <- as.factor(fp_df$fuel)
table(fp_df$fuel)

library(dplyr)
fp_df %>%
  group_by(fuel) %>%
  summarise(p_value = ad.test(power_kw)$p.value)

# Alternatively, visualize using Q-Q plots
par(mfrow = c(3, 2)) # Layout for 4 plots
for (trans in unique(fp_df$fuel)) {
  qqnorm(fp_df$power_kw[fp_df$fuel == trans], main = trans)
  qqline(fp_df$power_kw[fp_df$fuel == trans])
}

kruskal_result <- kruskal.test(power_kw ~ fuel, data = fp_df)
print(kruskal_result)

library(dunn.test)
dunn.test(fp_df$power_kw, fp_df$fuel, method = "bonferroni")


fuelcons_trans <- df[,c("transmission","fuel_consumption_l_100km")]

fuelcons_trans$transmission <- as.character(fuelcons_trans$transmission)

fct <- fuelcons_trans %>% 
  filter(transmission %in% c("Automatic","Manual"))
fct

fct$transmission <- as.factor(fct$transmission)

str(fct)



# Check normality assumption
# Shapiro-Wilk test for normality
ad.test(fct$fuel_consumption_l_100km[fct$transmission == "Manual"])
ad.test(fct$fuel_consumption_l_100km[fct$transmission == "Automatic"])

# Visual inspection (optional)
# Example: Histograms
hist(fct$fuel_consumption_l_100km[fct$transmission == "Manual"], main = "Manual Transmission", xlab = "Fuel Consumption",breaks = "Scott")
hist(fct$fuel_consumption_l_100km[fct$transmission == "Automatic"], main = "Automatic Transmission", xlab = "Fuel Consumption",breaks = "Scott")

# Check homogeneity of variance assumption
# Levene's test for homogeneity of variance
library(car)
leveneTest(fuel_consumption_l_100km ~ transmission, data = fct)
# Visual inspection (optional)
# Example: Boxplots
boxplot(fuel_consumption_l_100km ~ transmission, data = fct, main = "Fuel Consumption by Transmission Type")


t_test_result <- t.test(fuel_consumption_l_100km ~ transmission, data = fct)

result <- wilcox.test(fct$fuel_consumption_l_100km[fct$transmission == "Manual"],
                      fct$fuel_consumption_l_100km[fct$transmission == "Automatic"])

result
# Step 5: Interpret the Results
# Print the results
print(t_test_result)

# Check the p-value to determine statistical significance
if (t_test_result$p.value < 0.05) {
  print("There is a significant difference in fuel consumption between manual and automatic transmissions.")
} else {
  print("There is no significant difference in fuel consumption between manual and automatic transmissions.")
}



###########################
library(caret)

cor_data <- df[,c("price","milage","age","fuel_consumption_l_100km","fuel_consumption_g_km","power_kw")]

corrs <- cor(cor_data)
library(corrplot)
corrplot(corrs,method = "number")

library(caret)
set.seed(412)

df <- read.csv("df.csv")

str(df)
num_df <- df[,c("price","power_kw","fuel_consumption_l_100km","fuel_consumption_g_km",
                "milage","age")]


scale_variables <- function(dataframe, variable_names) {
  # Iterate over each variable name
  for (variable_name in variable_names) {
    # Extract the variable
    variable <- dataframe[[variable_name]]
    
    # Compute the minimum and maximum of the variable
    min_value <- min(variable)
    max_value <- max(variable)
    
    # Perform min-max scaling
    scaled_variable <- (variable - min_value) / (max_value - min_value)
    
    # Replace the variable in the dataframe with the scaled values
    dataframe[[variable_name]] <- scaled_variable
  }
  
  # Return the dataframe with scaled variables
  return(dataframe)
}


colnames(num_df)

scaled_df <- scale_variables(num_df, c("price","power_kw","fuel_consumption_l_100km",
                                       "fuel_consumption_g_km","milage","age"))
scaled_df


scaled_df$transmission <- df$transmission

scaled_df$transmission <- as.factor(scaled_df$transmission)

str(scaled_df)
library(dplyr)
library(caret)
train_ind = scaled_df$price %>%
  createDataPartition(p = 0.8, list = FALSE) #createDataPartition helps you define train set index #ratio: 80% and 20%
train  = scaled_df[train_ind, ]
test = scaled_df[-train_ind, ]


d_original=dim(scaled_df)
d_train=dim(train)
d_test=dim(test)
dimens=cbind(d_original,d_train,d_test)
rownames(dimens)=c("number of rows","number of columns")
dimens

str(train)

library(bestNormalize)

norm_trans <- orderNorm(train$price)

# View the transformation object
print(norm_trans)

# Transform the datalibrary()
transformed_data <- predict(norm_trans, train$price)
library(nortest)
hist(transformed_data)
ad.test(transformed_data)

train$price <- transformed_data
hist(train$price)
ad.test(train$price)
train$transmission <- relevel(train$transmission, ref = "Unknown")

str(train)



model <- lm(price~., data=train)
summary(model)

library(car)
vif(model)

durbinWatsonTest(model)



ad.test(residuals(model))

library(lmtest)
bptest(model)

str(train)

# Assuming your data is in a dataframe called `data` and the model is `lm_model`
library(car)  # for influencePlot
library(dplyr)

# Fit the model
lm_model <- lm(price ~ ., data = train)
summary(lm_model)
# Calculate leverage, Cook's distance, and studentized residuals
leverage <- hatvalues(lm_model)
cooks_distance <- cooks.distance(lm_model)
studentized_residuals <- rstudent(lm_model)

# Define thresholds for identifying outliers/influential points
leverage_threshold <- 2 * (ncol(train) + 1) / nrow(train)
cooks_threshold <- 4 / (nrow(train) - ncol(train) - 1)
residual_threshold <- 2  # Common threshold for studentized residuals

# Identify outliers/influential points
outliers <- which(leverage > leverage_threshold | cooks_distance > cooks_threshold | abs(studentized_residuals) > residual_threshold)

# Remove outliers/influential points from the data
cleaned_data <- train[-outliers, ]

# Refit the model with cleaned data
lm_model_cleaned <- lm(price ~ ., data = cleaned_data)

# Check the summary of the new model
summary(lm_model_cleaned)
table(cleaned_data$transmission)

car::vif(lm_model_cleaned)

vif(lm_model)

durbinWatsonTest(lm_model_cleaned)
ad.test(residuals(lm_model_cleaned))
par(mfrow=c(2,2))
plot(lm_model_cleaned)

library(lmtest)
bptest(model)


attach(train)
colnames(train)
plot(price,age)

detach(train)

# Apply transformations
# Apply transformations with checks for non-finite values
train$log_power_kw <- ifelse(is.finite(log(train$power_kw)), log(train$power_kw), NA)
train$log_fuel_consumption_g_km <- ifelse(is.finite(log(train$fuel_consumption_g_km)), log(train$fuel_consumption_g_km), NA)
train$log_fuel_consumption_l_100km <- ifelse(is.finite(log(train$fuel_consumption_l_100km)), log(train$fuel_consumption_l_100km), NA)
train$log_mileage <- ifelse(is.finite(log(train$milage + 1)), log(train$milage + 1), NA)
train$age_squared <- ifelse(is.finite(train$age^2), train$age^2, NA)


# Fit the model with transformed variables
model_transformed <- lm(price~log_power_kw + log_fuel_consumption_g_km + 
                          log_fuel_consumption_l_100km + log_mileage + age_squared+transmission, data = train)


summary(model_transformed)

plot(model_transformed)

str(train)
# Check the model diagnostics
par(mfrow = c(2, 2))
plot(model_transformed)

# Summary of the transformed model
summary(model_transformed)
library(dplyr)
table(test$transmission)
# Assuming your dataset is named 'df'
# Filter the dataset based on the "transmission" column
filtered_test <- subset(test, transmission %in% c("Manual", "Automatic"))
filtered_train <- subset(train, transmission %in% c("Manual", "Automatic"))

pred = lm_model_cleaned %>%  predict(filtered_test)
pred  #see predictions

pred2 <- lm_model_cleaned %>%  predict(filtered_train)
pred2

trainPred <- predict(norm_trans,pred2,inverse = TRUE)

testPred <- predict(norm_trans, pred, inverse = TRUE)

# Assuming your actual values are in filtered_train$actual and filtered_test$actual

# Actual values
actual_train <- filtered_train$price
actual_test <- filtered_test$price

# RMSE
rmse_train <- sqrt(mean((actual_train - trainPred)^2))
rmse_test <- sqrt(mean((actual_test - testPred)^2))

# MAE
mae_train <- mean(abs(actual_train - trainPred))
mae_test <- mean(abs(actual_test - testPred))

# MAPE
mape_train <- mean(abs((actual_train - trainPred) / actual_train)) 
mape_test <- mean(abs((actual_test - testPred) / actual_test)) 

# MPE
mpe_train <- mean((actual_train - trainPred) / actual_train) 
mpe_test <- mean((actual_test - testPred) / actual_test) 

# Print results
cat("Train RMSE: ", rmse_train, "\n")
cat("Test RMSE: ", rmse_test, "\n")
cat("Train MAE: ", mae_train, "\n")
cat("Test MAE: ", mae_test, "\n")
cat("Train MAPE: ", mape_train, "%\n")
cat("Test MAPE: ", mape_test, "%\n")
cat("Train MPE: ", mpe_train, "%\n")
cat("Test MPE: ", mpe_test, "%\n")

###### Machine Learning Method

data <- read.csv("df.csv")
data <- data[,c("price","power_kw","power_ps","fuel_consumption_l_100km","fuel_consumption_g_km",
                "milage","age","transmission")]


library(caret)
library(dplyr)


dummies <- dummyVars(~ transmission, data = data)
transmission_dummies <- predict(dummies, newdata = data)




transmission_dummies <- as.data.frame(transmission_dummies)
data <- cbind(data %>% select(-transmission), transmission_dummies)

# Display the first few rows of the modified data
head(data)

# View the current names of the new columns


colnames(data)


colnames(data)[10] <- "tr_semi_auto"

colnames(data)








scale_variables <- function(dataframe, variable_names) {
  # Iterate over each variable name
  for (variable_name in variable_names) {
    # Extract the variable
    variable <- dataframe[[variable_name]]
    
    # Compute the minimum and maximum of the variable
    min_value <- min(variable)
    max_value <- max(variable)
    
    # Perform min-max scaling
    scaled_variable <- (variable - min_value) / (max_value - min_value)
    
    # Replace the variable in the dataframe with the scaled values
    dataframe[[variable_name]] <- scaled_variable
  }
  
  # Return the dataframe with scaled variables
  return(dataframe)
}




scaled_df <- scale_variables(data, c("price","power_kw","power_ps","fuel_consumption_l_100km",
                                     "fuel_consumption_g_km","milage","age","transmissionAutomatic",
                                     "transmissionManual","tr_semi_auto","transmissionUnknown"))
scaled_df


library(caret)
library(nnet)
library(MLmetrics)

# Load your data
# Assuming your data is in a data frame called `data`
# and the target variable is called `price`
# Split the data into train and test sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(scaled_df$price, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- scaled_df[trainIndex, ]
testData <- scaled_df[-trainIndex, ]

# Separate features and target variable
trainX <- trainData[, -which(names(trainData) == "price")]
trainY <- trainData$price
testX <- testData[, -which(names(testData) == "price")]
testY <- testData$price

trainData <- cbind(trainX, price = trainY)
testData <- cbind(testX, price = testY)

install.packages("NeuralNetTools")
library(NeuralNetTools)
set.seed(123)
nnModel <- neuralnet(price ~ ., data = trainData, linear.output = TRUE,stepmax = 1e6)

nnModel$weights


dim(trainData)
plot(nnModel,rep="best",
     show.weights = TRUE, 
     information = FALSE,
     col.hidden = 'black',
     col.intercept = 'black',
     col.hidden.synapse = 'blue',
     col.synapse = 'red',
     col.entry = 'black',
     col.entry.synapse = 'lightblue',
     fill="lightblue")

tuneGrid <- expand.grid(size = c(1, 3, 5),
                        decay = c(0.001, 0.01, 0.1))

# Train the model with hyperparameter tuning
set.seed(123)
tunedNNModel <- train(trainX, trainY,
                      method = "nnet",
                      linout = TRUE,
                      trace = FALSE,
                      tuneGrid = tuneGrid,
                      trControl = trainControl(method = "cv", number = 10))

# Check the best tuned parameters
bestParams <- tunedNNModel$bestTune
print(bestParams)

bestModel <- tunedNNModel$finalModel

extract_weights <- function(model) {
  weights <- model$wts
  n <- model$n
  nlayer <- length(n) - 1
  index <- 0
  for (i in 1:nlayer) {
    cat("Layer", i, "weights:\n")
    for (j in 1:n[i]) {
      for (k in 1:n[i + 1]) {
        index <- index + 1
        cat("From", j, "to", k, ":", weights[index], "\n")
      }
    }
  }
}

# Extract and print the weights
(extract_weights(bestModel))


nnModel_tuned <- neuralnet(price ~ ., data = trainData, linear.output = TRUE,stepmax = 1e6,
                           hidden=3,learningrate = 0.001)
nnModel_tuned$weights
summary(nnModel_tuned)
par(mar = c(5, 8, 4, 2) + 0.1, oma = c(1, 1, 1, 1))
plot(nnModel_tuned, rep = "best",
     show.weights = FALSE,
     information = FALSE,
     fill = 'lightblue',  # Fill color for nodes
     circle_col = 'lightblue', # Color for nodes
     arrow_col = 'blue',  # Color for arrows
     intercept_col = 'darkred', # Color for intercepts
     line_stag = 0.5,     # Stagger lines to reduce overlap
     cex = 0.7,           # Size of node labels
     cex.axis = 0.7,      # Size of axis labels
     cex.lab = 0.7,       # Size of variable labels
     fontsize = 16,       # Font size
     col.edge = "black",  # Edge color for arrows
     pos.col = 'blue',    # Color for positive weights
     neg.col = 'red',     # Color for negative weights
     alpha = 0.7)






# Make predictions on the training set
trainPred <- predict(tunedNNModel, trainX)

# Make predictions on the test set
testPred <- predict(tunedNNModel, testX)

MPE <- function(pred, actual) {
  mean((pred - actual) / actual) 
}


# Calculate RMSE, MAPE, MAE, and MPE for train set
trainRMSE <- RMSE(trainPred, trainY)
trainMAPE <- MAPE(trainPred, trainY)
trainMAE <- MAE(trainPred, trainY)
trainMPE <- MPE(trainPred, trainY)

# Calculate RMSE, MAPE, MAE, and MPE for test set
testRMSE <- RMSE(testPred, testY)
testMAPE <- MAPE(testPred, testY)
testMAE <- MAE(testPred, testY)
testMPE <- MPE(testPred, testY)

# Print performance metrics
cat("Training Set Performance Metrics:\n")
cat("RMSE: ", trainRMSE, "\n")
cat("MAPE: ", trainMAPE, "\n")
cat("MAE: ", trainMAE, "\n")
cat("MPE: ", trainMPE, "\n\n")

cat("Test Set Performance Metrics:\n")
cat("RMSE: ", testRMSE, "\n")
cat("MAPE: ", testMAPE, "\n")
cat("MAE: ", testMAE, "\n")
cat("MPE: ", testMPE, "\n")


####SVM


library(caret)
library(e1071)
library(MLmetrics)

model <- svm(price ~ ., data = trainData)
print(model)



set.seed(123)
svmModel <- train(trainX, trainY,
                  method = "svmRadial",
                  trControl = trainControl(method = "cv", number = 10),
                  tuneLength = 10)
svmModel


# Define the grid of hyperparameters
tuneGrid <- expand.grid(sigma = c(0.01, 0.05, 0.1, 0.5),
                        C = c(0.5, 1, 2, 5, 10))

# Train the SVM model with hyperparameter tuning
set.seed(123)
tunedSVMModel <- train(trainX, trainY,
                       method = "svmRadial",
                       trControl = trainControl(method = "cv", number = 10),
                       tuneGrid = tuneGrid)
# Check the best tuned parameters
bestParams <- tunedSVMModel$bestTune
print(bestParams)

# Make predictions on the training set
trainPred <- predict(tunedSVMModel, trainX)

# Make predictions on the test set
testPred <- predict(tunedSVMModel, testX)


# Define Mean Percentage Error (MPE) function

# Calculate RMSE, MAPE, MAE, and MPE for train set
trainRMSE <- RMSE(trainPred, trainY)
trainMAPE <- MAPE(trainPred, trainY)
trainMAE <- MAE(trainPred, trainY)
trainMPE <- MPE(trainPred, trainY)

# Calculate RMSE, MAPE, MAE, and MPE for test set
testRMSE <- RMSE(testPred, testY)
testMAPE <- MAPE(testPred, testY)
testMAE <- MAE(testPred, testY)
testMPE <- MPE(testPred, testY)

# Print performance metrics
cat("Training Set Performance Metrics:\n")
cat("RMSE: ", trainRMSE, "\n")
cat("MAPE: ", trainMAPE, "\n")
cat("MAE: ", trainMAE, "\n")
cat("MPE: ", trainMPE, "\n\n")

cat("Test Set Performance Metrics:\n")
cat("RMSE: ", testRMSE, "\n")
cat("MAPE: ", testMAPE, "\n")
cat("MAE: ", testMAE, "\n")
cat("MPE: ", testMPE, "\n")

## Random Forest

library(randomForest)

rf1<-randomForest(price~.,data=trainData)
rf1
plot(rf1)

rf2 <- randomForest(price~.,data=trainData,ntree=600)
rf2
options(scipen = 999)
plot(rf2,main = "Number of Trees vs Error")

rf3 <- randomForest(price~.,data=trainData,ntree=450)
rf3
plot(rf3)

which.min(rf1$mse)


oob.err <- double(10)
test.err <- double(10)

#mtry is no of Variables randomly chosen at each split

for(mtry in 1:10) {
  rf=randomForest(price~ . , data = trainData ,mtry=mtry,ntree=450) 
  oob.err[mtry] = rf$mse[450] #Error of all Trees fitted
  
  pred<-predict(rf,testData[-11,]) #Predictions on Test Set for each Tree
  test.err[mtry]= mean((testData[,11] - pred)^2) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

oob.err
test.err

matplot(1:mtry , cbind(oob.err,test.err), pch=20 , col=c("red","orange"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","orange"),cex=0.4)


optimal_mtry <- 5

final_rfModel <- randomForest(price ~ ., data = trainData, mtry = optimal_mtry, ntree = 450)

(final_rfModel)
plot(final_rfModel)


parameters<-tuneRF(trainData[,-4],trainData[,4])

final_rfModel <- randomForest(price ~ ., data = trainData, mtry = 3, ntree = 450)
final_rfModel


trainPred <- predict(final_rfModel, trainX)

# Make predictions on the test set
testPred <- predict(final_rfModel, testX)

# Calculate Performance Metrics
trainRMSE <- RMSE(trainPred, trainY)
trainMAPE <- MAPE(trainPred, trainY)
trainMAE <- MAE(trainPred, trainY)
trainMPE <- MPE(trainPred, trainY)

testRMSE <- RMSE(testPred, testY)
testMAPE <- MAPE(testPred, testY)
testMAE <- MAE(testPred, testY)
testMPE <- MPE(testPred, testY)

# Print performance metrics
cat("Training Set Performance Metrics:\n")
cat("RMSE: ", trainRMSE, "\n")
cat("MAPE: ", trainMAPE, "\n")
cat("MAE: ", trainMAE, "\n")
cat("MPE: ", trainMPE, "\n\n")

cat("Test Set Performance Metrics:\n")
cat("RMSE: ", testRMSE, "\n")
cat("MAPE: ", testMAPE, "\n")
cat("MAE: ", testMAE, "\n")
cat("MPE: ", testMPE, "\n")

varImpPlot(final_rfModel)


#######XGBOOST
# Define the grid of hyperparameters
tuneGrid <- expand.grid(nrounds = c(50,100,150),
                        max_depth = c(1,2,3),
                        eta = c(0.01, 0.1),
                        gamma = c(0, 0.1),
                        colsample_bytree = c(0.6,0.8),
                        min_child_weight = c(1, 5),
                        subsample = c(0.5, 0.75, 1.0))

# Train the XGBoost model with hyperparameter tuning
set.seed(123)
tunedXGBModel <- train(trainX, trainY,
                       method = "xgbTree",
                       trControl = trainControl(method = "cv", number = 5),
                       tuneGrid = tuneGrid)

tunedXGBModel$bestTune


# Check the best tuned parameters
bestParams <- tunedXGBModel$bestTune
print(bestParams)

# Make predictions on the training set
trainPred <- predict(tunedXGBModel, trainX)

# Make predictions on the test set
testPred <- predict(tunedXGBModel, testX)



# Calculate RMSE, MAPE, MAE, and MPE for train set
trainRMSE <- RMSE(trainPred, trainY)
trainMAPE <- MAPE(trainPred, trainY)
trainMAE <- MAE(trainPred, trainY)
trainMPE <- MPE(trainPred, trainY)

# Calculate RMSE, MAPE, MAE, and MPE for test set
testRMSE <- RMSE(testPred, testY)
testMAPE <- MAPE(testPred, testY)
testMAE <- MAE(testPred, testY)
testMPE <- MPE(testPred, testY)

# Print performance metrics
cat("Training Set Performance Metrics:\n")
cat("RMSE: ", trainRMSE, "\n")
cat("MAPE: ", trainMAPE, "\n")
cat("MAE: ", trainMAE, "\n")
cat("MPE: ", trainMPE, "\n\n")

cat("Test Set Performance Metrics:\n")
cat("RMSE: ", testRMSE, "\n")
cat("MAPE: ", testMAPE, "\n")
cat("MAE: ", testMAE, "\n")
cat("MPE: ", testMPE, "\n")



# Load necessary libraries
library(randomForest)
library(xgboost)
library(e1071)
library(NeuralNetTools)
library(caret)

# Assuming models have been trained as final_rfModel, tunedXGBModel, tunedSVMModel, final_nnModel

# Random Forest Variable Importance
varImpPlot(final_rfModel, main = "Variable Importance - Random Forest")

# XGBoost Variable Importance
importance_matrix <- xgb.importance(feature_names = colnames(trainX), model = tunedXGBModel$finalModel)
xgb.plot.importance(importance_matrix, main = "Variable Importance - XGBoost")


# SVM Variable Importance
svmVarImp <- varImp(tunedSVMModel, scale = FALSE)
plot(svmVarImp, main = "Variable Importance - SVM")

# Neural Network Variable Importance
nn_importance <- olden(tunedNNModel$finalModel)
olden(tunedNNModel$finalModel, main = "Variable Importance - Neural Network")
