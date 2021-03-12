#Task 1:
  
setwd(dir = "E:/7089CEM - Introduction to Statistical Methods in Data Science/Coursework/EEG_Dataset/")
inputs = read.csv(file = "Inputs_x.csv", header=FALSE, sep=",",col.names = c("Input_x1","Input_x2","Input_x3","Input_x4"))
inputs
output = read.csv(file="Output_y.csv",header=FALSE,col.names = "Output_y")
output
time_series=read.csv(file="time_series.csv",header=FALSE,col.names = "Time_in_units", fileEncoding = "UTF-8-BOM" )
time_series
#analysis = data.frame("time_series","inputs","output")
#par(mfrow = c(3, 2))
pairs(inputs)
plot(x = time_series$Time_in_units, y = inputs$Input_x1, xlab = "Time(units)", ylab ="X1",type = "l")
plot(x = time_series$Time_in_units, y = inputs$Input_x2, xlab = "Time(units)", ylab ="X2",type = "l")
plot(x = time_series$Time_in_units, y = inputs$Input_x3, xlab = "Time(units)", ylab ="X3",type = "l")
plot(x = time_series$Time_in_units, y = inputs$Input_x4, xlab = "Time(units)", ylab ="X4",type = "l")
plot(x = time_series$Time_in_units, y = output$Output_y, xlab = "Time(units)", ylab ="Y",type = "l")
par(mfrow = c(2,2))
hist(inputs$Input_x1)
hist(inputs$Input_x2)
hist(inputs$Input_x3)
hist(inputs$Input_x4)
hist(output$Output_y)


#Task 2:

#Task 2.1:

#Model 1:

theta_one_m1 = matrix(inputs$Input_x4,length(inputs$Input_x4),1)
theta_two_m1 = matrix((inputs$Input_x1)^2,length(inputs$Input_x1),1)
theta_three_m1 = matrix((inputs$Input_x1)^3,length(inputs$Input_x1),1)
theta_four_m1 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_bias_m1 = matrix(1 ,length(inputs$Input_x1),1)

output_matrix = matrix(output$Output_y ,length(output$Output_y),1)

output_matrix


model_1 = cbind(theta_one_m1,theta_two_m1,theta_three_m1,theta_four_m1,theta_bias_m1)
model_1

thetaHat_model_1 = solve(t(model_1) %*% model_1) %*% t(model_1) %*% output_matrix
thetaHat_model_1

#Model 2:

theta_one_m2 = matrix((inputs$Input_x3)^3,length(inputs$Input_x3),1)
theta_two_m2 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_bias_m2 = matrix(1 ,length(inputs$Input_x1),1)

model_2 = cbind(theta_one_m2,theta_two_m2,theta_bias_m2)
model_2

thetaHat_model_2 = solve(t(model_2) %*% model_2) %*% t(model_2) %*% output_matrix
thetaHat_model_2

#Model 3:

theta_one_m3 = matrix(inputs$Input_x2,length(inputs$Input_x2),1)
theta_two_m3 = matrix((inputs$Input_x1)^3,length(inputs$Input_x1),1)
theta_three_m3 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_bias_m3 = matrix(1 ,length(inputs$Input_x1),1)

model_3 = cbind(theta_one_m3,theta_two_m3,theta_three_m3,theta_bias_m3)
model_3

thetaHat_model_3 = solve(t(model_3) %*% model_3) %*% t(model_3) %*% output_matrix
thetaHat_model_3

#Model 4:

theta_one_m4 = matrix(inputs$Input_x4,length(inputs$Input_x4),1)
theta_two_m4 = matrix((inputs$Input_x1)^3,length(inputs$Input_x1),1)
theta_four_m4 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_bias_m4 = matrix(1 ,length(inputs$Input_x1),1)

model_4 = cbind(theta_one_m4,theta_two_m4,theta_four_m4,theta_bias_m4)
model_4

thetaHat_model_4 = solve(t(model_4) %*% model_4) %*% t(model_4) %*% output_matrix
thetaHat_model_4

#Model 5:

theta_one_m5 = matrix(inputs$Input_x4,length(inputs$Input_x4),1)
theta_two_m5 = matrix((inputs$Input_x1)^2,length(inputs$Input_x1),1)
theta_three_m5 = matrix((inputs$Input_x1)^3,length(inputs$Input_x1),1)
theta_four_m5 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_five_m5 = matrix((inputs$Input_x1)^4,length(inputs$Input_x1),1)
theta_bias_m5 = matrix(1 ,length(inputs$Input_x1),1)

model_5 = cbind(theta_one_m5,theta_two_m5,theta_three_m5,theta_four_m5,theta_five_m5,theta_bias_m5)
model_5

thetaHat_model_5 = solve(t(model_5) %*% model_5) %*% t(model_5) %*% output_matrix
thetaHat_model_5

# Task 2.2:

# Model 1:

y_Hat_m1 = model_1 %*% thetaHat_model_1
y_Hat_m1

error_m1 = output_matrix - y_Hat_m1
error_m1

RSS_m1 = sum((error_m1)^2)  
RSS_m1 

# Model 2:

y_Hat_m2 = model_2 %*% thetaHat_model_2
y_Hat_m2

error_m2 = output_matrix - y_Hat_m2
error_m2

RSS_m2 = sum((error_m2)^2)  
RSS_m2 

# Model 3:

y_Hat_m3 = model_3 %*% thetaHat_model_3
y_Hat_m3

error_m3 = output_matrix - y_Hat_m3
error_m3

RSS_m3 = sum((error_m3)^2)  
RSS_m3 

# Model 4:

y_Hat_m4 = model_4 %*% thetaHat_model_4
y_Hat_m4

error_m4 = output_matrix - y_Hat_m4
error_m4

RSS_m4 = sum((error_m4)^2)  
RSS_m4 

# Model 5:

y_Hat_m5 = model_5 %*% thetaHat_model_5
y_Hat_m5

error_m5 = output_matrix - y_Hat_m5
error_m5

RSS_m5 = sum((error_m5)^2)  
RSS_m5 

RSS_m1
RSS_m2
RSS_m3
RSS_m4
RSS_m5


# Task 2.3:

n= length(inputs$Input_x1)

sigma_square_m1=RSS_m1/(length(inputs$Input_x1) - 1)
sigma_square_m2=RSS_m2/(length(inputs$Input_x1) - 1)
sigma_square_m3=RSS_m3/(length(inputs$Input_x1) - 1)
sigma_square_m4=RSS_m4/(length(inputs$Input_x1) - 1)
sigma_square_m5=RSS_m5/(length(inputs$Input_x1) - 1)

sigma_square_m1
sigma_square_m2
sigma_square_m3
sigma_square_m4
sigma_square_m5

log_like_m1 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m1)) - RSS_m1/(2*sigma_square_m1) 
log_like_m2 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m2)) - RSS_m2/(2*sigma_square_m2) 
log_like_m3 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m3)) - RSS_m3/(2*sigma_square_m3) 
log_like_m4 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m4)) - RSS_m4/(2*sigma_square_m4) 
log_like_m5 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m5)) - RSS_m5/(2*sigma_square_m5) 

log_like_m1
log_like_m2
log_like_m3
log_like_m4
log_like_m5

# Task 2.4 :

k1 = length(thetaHat_model_1)
k2 = length(thetaHat_model_2)
k3 = length(thetaHat_model_3)
k4 = length(thetaHat_model_4)
k5 = length(thetaHat_model_5)

AIC_m1 = 2*k1 - 2*log_like_m1
BIC_m1 = k1*log(n) - 2*log_like_m1
AIC_m1
BIC_m1

AIC_m2 = 2*k2 - 2*log_like_m2
BIC_m2 = k2*log(n) - 2*log_like_m2
AIC_m2
BIC_m2

AIC_m3 = 2*k3 - 2*log_like_m3
BIC_m3 = k3*log(n) - 2*log_like_m3
AIC_m3
BIC_m3

AIC_m4 = 2*k4 - 2*log_like_m4
BIC_m4 = k4*log(n) - 2*log_like_m4
AIC_m4
BIC_m4

AIC_m5 = 2*k5 - 2*log_like_m5
BIC_m5 = k5*log(n) - 2*log_like_m5
AIC_m5
BIC_m5

#Task 2.5:

#par(mfrow = c(3, 2))
hist(error_m1)
qqnorm(error_m1)
qqline(error_m1)

hist(error_m2)
qqnorm(error_m2)
qqline(error_m2)

hist(error_m3)
qqnorm(error_m3)
qqline(error_m3)

hist(error_m4)
qqnorm(error_m4)
qqline(error_m4)

hist(error_m5)
qqnorm(error_m5)
qqline(error_m5)

# Task 2.6:

# Model 3 is the best regression model based on AIC,BIC,Distribution and QQ Plot.


#Task 2.7:

# Retrieve the Input and Output Data

inputs
output
best_model = cbind(inputs,output)
best_model
class(best_model)
length(best_model)
Poly_full_data <- lm(Output_y ~ Input_x2+I(Input_x1^3)+I(Input_x3^4), data=best_model) # multinom Model
summary (Poly_full_data)

# Split the inputs and outputs into 70:30

set.seed(200)
head(best_model)
best_model_time_series = cbind(time_series_matrix,best_model)
best_model_time_series
head(best_model_time_series)
divide = sample(2, nrow(best_model), replace = TRUE , prob = c(0.7,0.3))
training_data = best_model_time_series[divide==1,]
testing_data = best_model_time_series[divide==2,]
training_data
testing_data

training_data_theta_one_m3 = matrix(training_data$Input_x2,length(training_data$Input_x2),1)
training_data_theta_one_m3
training_data_theta_two_m3 = matrix((training_data$Input_x1)^3,length(training_data$Input_x1),1)
training_data_theta_three_m3 = matrix((training_data$Input_x3)^4,length(training_data$Input_x3),1)
training_data_theta_bias_m3 = matrix(1 ,length(training_data$Input_x1),1)

model_3_train = cbind(training_data_theta_one_m3,training_data_theta_two_m3,training_data_theta_three_m3,training_data_theta_bias_m3)
model_3_train

training_data_output = matrix(training_data$Output_y ,length(training_data$Output_y),1)
training_data_output

training_data_thetaHat_model_3 = solve(t(model_3_train) %*% model_3_train) %*% t(model_3_train) %*% training_data_output
training_data_thetaHat_model_3

y_Hat_m3_train = model_3_train %*% training_data_thetaHat_model_3
y_Hat_m3_train

error_m3_train = training_data$Output_y - y_Hat_m3_train
error_m3_train

RSS_m3_train = sum((error_m3_train)^2)
RSS_m3_train

sigma_square_model_3_train=RSS_m3_train/(length(training_data$Output_y) - 1)
sigma_square_model_3_train

cov_thetaHat_3 = sigma_square_model_3_train * (solve(t(model_3_train) %*% model_3_train))

var_y_hat_3 = matrix(0 ,length(training_data$Output_y), 1)
var_y_hat_3

for( i in 1:length(training_data$Output_y)){
  model_3_train_i = matrix( model_3_train[i,] , 1 , 4 ) # X[i,] creates a vector. Convert it to matrix
  var_y_hat_3[i,1] = model_3_train_i %*% cov_thetaHat_3 %*% t(model_3_train_i) # same as sigma_2 * ( X_i %*% ( solve(t(X) %*% X)  ) %*% t(X_i) )
}

cov_thetaHat_3 = sigma_square_model_3_train * (solve(t(model_3_train) %*% model_3_train))
cov_thetaHat_3
cov(model_3_train)

CI_train = 2* sqrt(var_y_hat_3)
CI_train

testing_data_theta_one_m3 = matrix(testing_data$Input_x2,length(testing_data$Input_x2),1)
testing_data_theta_one_m3
testing_data_theta_two_m3 = matrix((testing_data$Input_x1)^3,length(testing_data$Input_x1),1)
testing_data_theta_three_m3 = matrix((testing_data$Input_x3)^4,length(testing_data$Input_x3),1)
testing_data_theta_bias_m3 = matrix(1 ,length(testing_data$Input_x1),1)

model_3_test = cbind(testing_data_theta_one_m3,testing_data_theta_two_m3,testing_data_theta_three_m3,testing_data_theta_bias_m3)
model_3_test

testing_data_output = matrix(testing_data$Output_y ,length(testing_data$Output_y),1)
testing_data_output

testing_data_thetaHat_model_3 = solve(t(model_3_test) %*% model_3_test) %*% t(model_3_test) %*% testing_data_output
testing_data_thetaHat_model_3

y_Hat_m3_test = model_3_test %*% testing_data_thetaHat_model_3
y_Hat_m3_test

error_m3_test = testing_data$Output_y - y_Hat_m3_test
error_m3_test

RSS_m3_test = sum((error_m3_test)^2)
RSS_m3_test

sigma_square_model_3_test=RSS_m3_test/(length(testing_data$Output_y) - 1)
sigma_square_model_3_test

cov_thetaHat_3_test = sigma_square_model_3_test * (solve(t(model_3_test) %*% model_3_test))
cov_thetaHat_3_test

summary(model_3_test)



length(model_3_train[,1])
length(y_Hat_m3_train)
length(time_series)
nrow(time_series_matrix)
length(time_series_matrix)

set.seed(200)
training <- time_series_matrix[c(1:(0.7*nrow(time_series_matrix))),]
test <- time_series_matrix[-c(1:((0.7*nrow(time_series_matrix)))), ]
length(training)
length(y_Hat_m3_train)
length(training_data$time_series_matrix)
length(training_data)
head(training_data)
training_data
y_Hat_m3_train
v=sort(y_Hat_m3_train)
model_3_time=cbind(time_series,model_3)
model_3_time
error_square_y_Hat_m3 = y_Hat_m3^2
error_square_y_Hat_m3


plot(training_data$time_series_matrix,training_data$Output_y)
abline(multinomModel)

plot(model_3_time$Time_in_units, y_Hat_m3_train + CI_train, type = "l")
segments(training_data$time_series_matrix,y_Hat_m3_train-CI_train,y_Hat_m3_train+CI_train) # Adds error bars to the indivigual data points

#Using lm :
multinomModel <- lm(Output_y ~ (Input_x2+I(Input_x1^3)+I(Input_x3^4)), data=training_data) # multinom Model
summary (multinomModel)
test_Pred <- predict(multinomModel, test_data)
test_Prediction = predict(multinomModel, test_data, interval = "confidence")
test_Pred
test_Prediction

plot(x)
test_Pred_df= as.data.frame(test_Pred)
test_Pred_df
test
comp = cbind.data.frame(test_Pred_df,test$Output_y)
comp
correlation_accuracy <- cor(comp)
correlation_accuracy
multinomModel$residuals
RSS_training = sum((multinomModel$residuals)^2) 
RSS_training

#Task_3:

#1:You only need to compute 2 parameter posterior distributions -- the 2 parameters with largest
#absolute values in your least squares estimation (Task 2.1) of the selected model. Fix all the other
#parameters in your model as constant, by using the estimated values from Task 2.1

lambda = .001
thetaHat_ABC = solve(t(model_3) %*% model_3 + lambda*diag(ncol(model_3))) %*% t(model_3) %*% output_matrix

thetaHat_ABC

#2:Use a Uniform distribution as prior, around the estimated parameter values for those 2
#parameters (from the Task 2.1). You will need to determine the range of the prior distribution.

runif(20,0,.5)
best_model
library(BayesFactor)
regressionBF(Output_y ~ (Input_x2+I(Input_x1^3)+I(Input_x3^4)), data=best_model)



