inputs = read.csv(file = "Inputs_x.csv", header=FALSE, sep=",",col.names = c("Input_x1","Input_x2","Input_x3","Input_x4"))
head(inputs)
output = read.csv(file="Output_y.csv",header=FALSE,col.names = "Output_y")
output
time_series=read.csv(file="time_series.csv",header=FALSE,col.names = "Time_in_secs", fileEncoding = "UTF-8-BOM" )
time_series
input_output = cbind(inputs,output)
input_output

#Task 2:

#Task 2.1:

# Creating matrix for each inputs according to the model equation and 
# finding coefficients/parameters for all models using Least Square Equation(theta_HAT)

#Model 1:

theta_one_m1 = matrix(inputs$Input_x4,length(inputs$Input_x4),1)
theta_two_m1 = matrix((inputs$Input_x1)^2,length(inputs$Input_x1),1)
theta_three_m1 = matrix((inputs$Input_x1)^3,length(inputs$Input_x1),1)
theta_four_m1 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_bias_m1 = matrix(1 ,length(inputs$Input_x1),1)
output_matrix = matrix(output$Output_y ,length(output$Output_y),1)
model_1 = cbind(theta_one_m1,theta_two_m1,theta_three_m1,theta_four_m1,theta_bias_m1) # Combining the matrix to form model equation 
thetaHat_model_1 = solve(t(model_1) %*% model_1) %*% t(model_1) %*% output_matrix # theta_Hat Equation of Model 1 
row.names(thetaHat_model_1) = c("theta_1","theta_2","theta_3","theta_4","theta_bias(Intercept)")
colnames(thetaHat_model_1) = c("Model_Parameters")
thetaHat_model_1


#Model 2:

theta_one_m2 = matrix((inputs$Input_x3)^3,length(inputs$Input_x3),1)
theta_two_m2 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_bias_m2 = matrix(1 ,length(inputs$Input_x1),1)
model_2 = cbind(theta_one_m2,theta_two_m2,theta_bias_m2)
thetaHat_model_2 = solve(t(model_2) %*% model_2) %*% t(model_2) %*% output_matrix
row.names(thetaHat_model_2) = c("theta_1","theta_2","theta_bias(Intercept)")
colnames(thetaHat_model_2) = c("Model_Parameters")
thetaHat_model_2

#Model 3:

theta_one_m3 = matrix(inputs$Input_x2,length(inputs$Input_x2),1)
theta_two_m3 = matrix((inputs$Input_x1)^3,length(inputs$Input_x1),1)
theta_three_m3 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_bias_m3 = matrix(1 ,length(inputs$Input_x1),1)
model_3 = cbind(theta_one_m3,theta_two_m3,theta_three_m3,theta_bias_m3)
thetaHat_model_3 = solve(t(model_3) %*% model_3) %*% t(model_3) %*% output_matrix
row.names(thetaHat_model_3) = c("theta_1","theta_2","theta_3","theta_bias(Intercept)")
colnames(thetaHat_model_3) = c("Model_Parameters")
thetaHat_model_3

#Model 4:

theta_one_m4 = matrix(inputs$Input_x4,length(inputs$Input_x4),1)
theta_two_m4 = matrix((inputs$Input_x1)^3,length(inputs$Input_x1),1)
theta_four_m4 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_bias_m4 = matrix(1 ,length(inputs$Input_x1),1)
model_4 = cbind(theta_one_m4,theta_two_m4,theta_four_m4,theta_bias_m4)
thetaHat_model_4 = solve(t(model_4) %*% model_4) %*% t(model_4) %*% output_matrix
row.names(thetaHat_model_4) = c("theta_1","theta_2","theta_4","theta_bias(Intercept)")
colnames(thetaHat_model_4) = c("Model_Parameters")
thetaHat_model_4

#Model 5:

theta_one_m5 = matrix(inputs$Input_x4,length(inputs$Input_x4),1)
theta_two_m5 = matrix((inputs$Input_x1)^2,length(inputs$Input_x1),1)
theta_three_m5 = matrix((inputs$Input_x1)^3,length(inputs$Input_x1),1)
theta_four_m5 = matrix((inputs$Input_x3)^4,length(inputs$Input_x3),1)
theta_five_m5 = matrix((inputs$Input_x1)^4,length(inputs$Input_x1),1)
theta_bias_m5 = matrix(1 ,length(inputs$Input_x1),1)
model_5 = cbind(theta_one_m5,theta_two_m5,theta_three_m5,theta_four_m5,theta_five_m5,theta_bias_m5)
thetaHat_model_5 = solve(t(model_5) %*% model_5) %*% t(model_5) %*% output_matrix
row.names(thetaHat_model_5) = c("theta_1","theta_2","theta_3","theta_4","theta_5","theta_bias(Intercept)")
colnames(thetaHat_model_5) = c("Model_Parameters")
thetaHat_model_5

# Task 2.2:
# Calculation of Residual Sum of Square Error(RSS):

# Model 1:

y_Hat_m1 = model_1 %*% thetaHat_model_1 # Equation for the calculation of y_Hat(Input matrix * thetaHAT)
error_m1 = output_matrix - y_Hat_m1 # Equation for the calculation of Prediction error(Output_y - y_Hat)
RSS_m1 = sum((error_m1)^2)# Square and Summation of Prediction Error(Residual) is RSS
RSS_m1 

# Model 2:

y_Hat_m2 = model_2 %*% thetaHat_model_2
error_m2 = output_matrix - y_Hat_m2
RSS_m2 = sum((error_m2)^2)  
RSS_m2 

# Model 3:

y_Hat_m3 = model_3 %*% thetaHat_model_3
error_m3 = output_matrix - y_Hat_m3
RSS_m3 = sum((error_m3)^2)  
RSS_m3 

# Model 4:

y_Hat_m4 = model_4 %*% thetaHat_model_4
error_m4 = output_matrix - y_Hat_m4
RSS_m4 = sum((error_m4)^2)  
RSS_m4 

# Model 5:

y_Hat_m5 = model_5 %*% thetaHat_model_5
error_m5 = output_matrix - y_Hat_m5
RSS_m5 = sum((error_m5)^2)  
RSS_m5 

# Task 2.3:

# Calculation of log-likelihood function:

n= length(inputs$Input_x1) #  n - Number of data samples

# Calculation of Sigma Square(Variance)

# Model 1:
  
sigma_square_m1=RSS_m1/(length(inputs$Input_x1) - 1)
log_like_m1 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m1)) - RSS_m1/(2*sigma_square_m1)
log_like_m1

# Model 2:

sigma_square_m2=RSS_m2/(length(inputs$Input_x1) - 1)
log_like_m2 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m2)) - RSS_m2/(2*sigma_square_m2)
log_like_m2

# Model 3: 
sigma_square_m3=RSS_m3/(length(inputs$Input_x1) - 1)
log_like_m3 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m3)) - RSS_m3/(2*sigma_square_m3) 
log_like_m3

# Model 4:
sigma_square_m4=RSS_m4/(length(inputs$Input_x1) - 1)
log_like_m4 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m4)) - RSS_m4/(2*sigma_square_m4)
log_like_m4

#Model 5:

sigma_square_m5=RSS_m5/(length(inputs$Input_x1) - 1)
log_like_m5 = - n/2*(log(2*pi)) - n/2*(log(sigma_square_m5)) - RSS_m5/(2*sigma_square_m5)
log_like_m5


# Task 2.4 :

#Calculation of AIC and BIC
#k1,k2,k3,k4,k5 - Number of Parameters of the each model

# Model 1:

k1 = length(thetaHat_model_1) 
AIC_m1 = 2*k1 - 2*log_like_m1
BIC_m1 = k1*log(n) - 2*log_like_m1
AIC_m1
BIC_m1

# Model 2:
k2 = length(thetaHat_model_2)
AIC_m2 = 2*k2 - 2*log_like_m2
BIC_m2 = k2*log(n) - 2*log_like_m2
AIC_m2
BIC_m2

#Model 3:

k3 = length(thetaHat_model_3)
AIC_m3 = 2*k3 - 2*log_like_m3
BIC_m3 = k3*log(n) - 2*log_like_m3
AIC_m3
BIC_m3

#Model 4:

k4 = length(thetaHat_model_4)
AIC_m4 = 2*k4 - 2*log_like_m4
BIC_m4 = k4*log(n) - 2*log_like_m4
AIC_m4
BIC_m4

# Model 5:
  
k5 = length(thetaHat_model_5)
AIC_m5 = 2*k5 - 2*log_like_m5
BIC_m5 = k5*log(n) - 2*log_like_m5
AIC_m5
BIC_m5

#Task 2.5:

#par(mfrow = c(3,3))
hist(error_m1,col='white',border='green',freq = FALSE,xlab = "error_m1")
#lines(density(error_m1),col='red')
curve(dnorm(x,mean=mean(error_m1),sd=sd(error_m1)), add=TRUE,col="black")
abline(v=mean(error_m1),col="blue",lwd = 2)
qqnorm(error_m1, main = "Q-Q Plot of Error_model_1")
qqline(error_m1)
boxplot(error_m1, main="Boxplot_Error_model_1")
summary(error_m1)

hist(error_m2,col='white',border='green',freq = FALSE,xlab = "error_m2")
#lines(density(error_m2),col='red')
curve(dnorm(x,mean=mean(error_m2),sd=sd(error_m2)), add=TRUE,col="black")
abline(v=mean(error_m2),col="blue",lwd = 2)
qqnorm(error_m2,main = "Q-Q Plot of Error_model_2")
qqline(error_m2)
boxplot(error_m2, main="Boxplot_Error_model_2")
summary(error_m2)

hist(error_m3,col='white',border='green',freq = FALSE,xlab = "error_m3")
#lines(density(error_m3),col='red')
curve(dnorm(x,mean=mean(error_m3),sd=sd(error_m3)), add=TRUE,col="black")
abline(v=mean(error_m3),col="blue",lwd = 2)
qqnorm(error_m3,main = "Q-Q Plot of Error_model_3")
qqline(error_m3)
boxplot(error_m3,main="Boxplot_Error_model_3")
summary(error_m3)

hist(error_m4,col='white',border='green',freq = FALSE,xlab = "error_m4")
#lines(density(error_m4),col='red')
curve(dnorm(x,mean=mean(error_m4),sd=sd(error_m4)), add=TRUE,col="black")
abline(v=mean(error_m4),col="blue",lwd = 2)
qqnorm(error_m4,main = "Q-Q Plot of Error_model_4")
qqline(error_m4)
boxplot(error_m4,main="Boxplot_Error_model_4")
summary(error_m4)

hist(error_m5,col='white',border='green',freq = FALSE,xlab = "error_m5")
#lines(density(error_m5))
curve(dnorm(x,mean=mean(error_m5),sd=sd(error_m5)), add=TRUE,col="black")
abline(v=mean(error_m5),col="blue",lwd = 2)
qqnorm(error_m5,main = "Q-Q Plot of Error_model_5")
qqline(error_m5)
boxplot(error_m5,main="Boxplot_Error_model_5")
summary(error_m5)

# Task 2.6:

# Model 3 is the best regression model based on AIC,BIC,Distribution Plot, QQ Plot and the Box Plot. 
# Apart from these various metrics are also used to select the best model (write about these in the inference)

#Task 2.7:

# Retrieve the Input and Output Data:

# Polynomial Regression for model_3: Same calculation as Task 2.1: Model_3 but with lm()function:
Poly_full_data = lm(Output_y ~ Input_x2+I(Input_x1^3)+I(Input_x3^4), data=input_output) 
summary(Poly_full_data) # Cross check the values of Task 2.1:Model_3 parameters
thetaHat_model_3

# Split the inputs and output data into 70:30:

set.seed(20)
divide = sample(2, nrow(input_output), replace = TRUE , prob = c(0.7,0.3))
training_data = input_output[divide==1,]
testing_data = input_output[divide==2,]
training_data
testing_data

# Task 2.7.1: Estimation of Model Parameters using the training data same as Task 2.1 & 2.2:

training_data_theta_one_m3 = matrix(training_data$Input_x2,length(training_data$Input_x2),1)
training_data_theta_two_m3 = matrix((training_data$Input_x1)^3,length(training_data$Input_x1),1)
training_data_theta_three_m3 = matrix((training_data$Input_x3)^4,length(training_data$Input_x3),1)
training_data_theta_bias_m3 = matrix(1 ,length(training_data$Input_x1),1)
training_data_output = matrix(training_data$Output_y ,length(training_data$Output_y),1)
model_3_train = cbind(training_data_theta_one_m3,training_data_theta_two_m3,training_data_theta_three_m3,training_data_theta_bias_m3)
training_data_thetaHat_model_3 = solve(t(model_3_train) %*% model_3_train) %*% t(model_3_train) %*% training_data_output
row.names(training_data_thetaHat_model_3) = c("theta_1","theta_2","theta_3","theta_bias(Intercept)")
colnames(training_data_thetaHat_model_3) = c("Model_Parameters")
training_data_thetaHat_model_3
y_Hat_model_3_train = model_3_train %*% training_data_thetaHat_model_3
colnames(y_Hat_model_3_train) = c("y_Hat_model_3_train_values")
y_Hat_model_3_train

#Task 2.7.2: Calculating the model Output on the Prediction Data:

# Prediction 

testing_data_theta_one_m3 = matrix(testing_data$Input_x2,length(testing_data$Input_x2),1)
testing_data_theta_two_m3 = matrix((testing_data$Input_x1)^3,length(testing_data$Input_x1),1)
testing_data_theta_three_m3 = matrix((testing_data$Input_x3)^4,length(testing_data$Input_x3),1)
testing_data_theta_bias_m3 = matrix(1 ,length(testing_data$Input_x1),1)
testing_data_output = matrix(testing_data$Output_y ,length(testing_data$Output_y),1)
model_3_test = cbind(testing_data_theta_one_m3,testing_data_theta_two_m3,testing_data_theta_three_m3,testing_data_theta_bias_m3)
testing_data_thetaHat_model_3 = solve(t(model_3_test) %*% model_3_test) %*% t(model_3_test) %*% testing_data_output
testing_data_thetaHat_model_3
y_Hat_m3_test = model_3_test %*% training_data_thetaHat_model_3
y_Hat_m3_test
error_m3_test = testing_data$Output_y - y_Hat_m3_test
error_m3_test

qqnorm(error_m3_test)
qqline(error_m3_test)



# Calculating the Confidence Interval Values(Prediction Error(Residual) Bar)

#Using lm() function:

Poly_train_data = lm(Output_y ~ Input_x2+I(Input_x1^3)+I(Input_x3^4), data=training_data) 
summary(Poly_train_data)
predict_best_model = predict(Poly_train_data,testing_data,interval = "confidence")
predict_best_model

# Using the code from the Lab 9(Exercise 2)

RSS_m3_test = sum((error_m3_test)^2)
RSS_m3_test

sigma_square_model_3_test=RSS_m3_test/(length(testing_data$Output_y) - 1)
sigma_square_model_3_test

cov_thetaHat_3_test = sigma_square_model_3_test * (solve(t(model_3_test) %*% model_3_test))
cov_thetaHat_3_test

var_y_hat_3 = matrix(0 ,length(testing_data$Output_y), 1)

for( i in 1:length(testing_data$Output_y)){
  model_3_test_i = matrix( model_3_test[i,] , 1 , k3 ) # X[i,] creates a vector. Convert it to matrix
  var_y_hat_3[i,1] = model_3_test_i %*% cov_thetaHat_3_test %*% t(model_3_test_i) # same as sigma_2 * ( X_i %*% ( solve(t(X) %*% X)  ) %*% t(X_i) )
}

CI_test = 2* sqrt(var_y_hat_3)
CI_test

# Residual plot - Used to find the Goodness of fit

plot(fitted(Poly_train_data),residuals(Poly_train_data))

#Task_3:

# Task 3.1:

# The 2 largest absolute parameters of the best model are selected 
# and remaining two parameters are made constant and here it is assumed as 0.1

row.names(thetaHat_model_3) = c("theta1","theta2","theta3","thetabias(intercepts)")
colnames(thetaHat_model_3) = c("Model_parameters")
thetaHatmodel_3_rejection_ABC = thetaHat_model_3
thetaHatmodel_3_rejection_ABC

thetaHatmodel_3_rejection_ABC[c("theta2","theta3"),"Model_parameters"] = 0
thetaHatmodel_3_rejection_ABC

# Task 3.2:

set.seed(100)
prior_uniform = runif(100,0.01,0.6)
prior_uniform
thetha_bias = sample(prior_uniform,size = 70)
thetha_bias
thetha_one = sample (prior_uniform, size = 70)
thetha_one

#Task 3.3:
#Posterior Distribution:

prior = curve(dunif(x,0.1,1),ylim=c(0,2),xlim=c(0,1.5),col="red",lty=2,ylab="Prior",xlab="Model_parameter")
Numerator_of_Bayes_theorem = log_like_m3*prior_uniform 
Numerator_of_Bayes_theorem
Denominator_of_Bayes_Theorem = sum(Numerator_of_Bayes_theorem)
Denominator_of_Bayes_Theorem
posterior_distribution = Numerator_of_Bayes_theorem/Denominator_of_Bayes_Theorem
posterior_distribution
Distance_between_ABC= prior_uniform - posterior_distribution
Distance_between_ABC
epsilon = 0.1
#Reject - distace > epsilon & Accept- Distance <= epsilon
for (p in Distance_between_ABC) {
  if(p <= epsilon ) {
    print(p)
  }
}
# This step is repeated until the approximated posterior distribution is obtained.


#Task 3.4:

#install.packages("LaplacesDemon")

library(LaplacesDemon)
joint.density.plot(thetha_bias, thetha_one, Title="Joint Density Plot",
                   contour=TRUE, color= TRUE)

