# Task 1:

#Task 1.1:

# Import EEG input,output and time signals:
inputs = read.csv(file = "Inputs_x.csv", header=FALSE, sep=",",col.names = c("Input_x1","Input_x2","Input_x3","Input_x4"))
output = read.csv(file="Output_y.csv",header=FALSE,col.names = "Output_y")
time_series=read.csv(file="time_series.csv",header=FALSE,col.names = "Time_in_secs", fileEncoding = "UTF-8-BOM" )
input_output = cbind(inputs,output)

#Augmented Data:
EEG_Aug_data=cbind(time_series,inputs,output)
head(EEG_Aug_data)

#Task 1.2:

#To find the Central Tendencey Measures:
summary(input_output)

#Task 1.3:

# Outlier Detection in each signals:

par(mfrow=c(2,2)) # Used to plot 4 figures in one plot area. 
                  # Click zoom to observe the plots clearly.

#Calculation of IQR values and Boxplot of 4 input EEG signals:
#Input_X1 data:
IQR_Upper_Bound_Input_X1 = 2.6706 + 1.5 * (2.6706 + 2.2482)
IQR_Lower_Bound_Input_x1 = 2.6706 - 1.5 * (2.6706 + 2.2482)
boxplot(input_output$Input_x1,main = "Input_X1")

#Input_X2 data:
IQR_Upper_Bound_Input_X2 = 1.4723 + 1.5 * (1.4723 + 1.4010)
IQR_Lower_Bound_Input_x2 = 1.4723 - 1.5 * (1.4723 + 1.4010)
boxplot(input_output$Input_x2,main = "Input_X2") 

#Input_X3 data:
IQR_Upper_Bound_Input_X3 = 1.6262 + 1.5 * (1.6262 + 1.5792)
IQR_Lower_Bound_Input_x3 = 1.6262 - 1.5 * (1.6262 + 1.5792)
boxplot(input_output$Input_x3,main = "Input_X3")

#Input_X4 data:
IQR_Upper_Bound_Input_X1 = 2.4441 + 1.5 * (2.4441 + 2.1812)
IQR_Lower_Bound_Input_x1 = 2.4441 - 1.5 * (2.4441 + 2.1812)
boxplot(input_output$Input_x4,main = "Input_X4")

#Task 1.4:

# Time Series Plot of input and output signals to study the stationarity:

par(mfrow = c(3, 2))
plot(x = time_series$Time_in_secs, y = inputs$Input_x1, xlab = "Time(secs)", ylab ="X1",type = "l",main ="Time Vs Input_X1")
abline(h=mean(inputs$Input_x1),col="red")
plot(x = time_series$Time_in_secs, y = inputs$Input_x2, xlab = "Time(secs)", ylab ="X2",type = "l",main = "Time Vs Input_X2")
abline(h=mean(inputs$Input_x2),col="red")
plot(x = time_series$Time_in_secs, y = inputs$Input_x3, xlab = "Time(secs)", ylab ="X3",type = "l",main = "Time Vs Input_X3")
abline(h=mean(inputs$Input_x3),col="red")
plot(x = time_series$Time_in_secs, y = inputs$Input_x4, xlab = "Time(secs)", ylab ="X4",type = "l",main = "Time Vs Input_X4")
abline(h=mean(inputs$Input_x4),col="red")
plot(x = time_series$Time_in_secs, y = output$Output_y, xlab = "Time(secs)", ylab ="Y",type = "l",main = "Time Vs Output_Y")
abline(h=mean(output$Output_y),col="red")

#Task 1.5:

#Histogram plot to observe the distbution of the signals:

par(mfrow = c(3,2))
hist(inputs$Input_x1,freq = FALSE , col = "green",border = "red")
lines(density(inputs$Input_x1,kernel="gaussian"))
abline(v=mean(inputs$Input_x1),col="blue",lwd = 2)
text(5,0.06,"mean=-0.0154",col= "blue", adj = c(0,0))
hist(inputs$Input_x2,freq = FALSE , col = "green",border = "red")
lines(density(inputs$Input_x2,kernel="gaussian"))
abline(v=mean(inputs$Input_x2),col="blue",lwd = 2)
text(2.5,0.1,"mean=-0.0333",col="blue",adj = c(0,0))
hist(inputs$Input_x3,freq = FALSE , col = "green",border = "red")
lines(density(inputs$Input_x3,kernel="gaussian"))
abline(v=mean(inputs$Input_x3),col="blue",lwd = 2)
text(3,0.1,"mean=-0.3360",col="blue",adj = c(0,0))
hist(inputs$Input_x4,freq = FALSE , col = "green",border = "red")
lines(density(inputs$Input_x4,kernel="gaussian"))
abline(v=mean(inputs$Input_x4),col="blue",lwd = 2)
text(3,0.1,"mean=-0.0115",col="blue",adj = c(0,0))
hist(output$Output_y,freq = FALSE , col = "green",border = "red")
lines(density(output$Output_y,kernel="gaussian"))
abline(v=mean(output$Output_y),col="blue",lwd = 2)
text(2,0.1,"mean=-0.1085",col="blue",adj = c(0,0))

#Task 1.6:
# Correlation measures between signals:

correlate_measure = cor(input_output) # Values are printed in Tabular form
correlate_measure

#Task 1.7:

# Scatter Plot between various inputs and output EEG signals:
# Fitting linear model on to the scatter plot to find the dependancy:

par(mfrow = c(3,3))
plot(inputs$Input_x1,inputs$Input_x2)
abline(lm(inputs$Input_x2~inputs$Input_x1),col="red")
plot(inputs$Input_x2,inputs$Input_x3)
abline(lm(inputs$Input_x3~inputs$Input_x2),col="red")
plot(inputs$Input_x3,inputs$Input_x4)
abline(lm(inputs$Input_x4~inputs$Input_x3),col="red")
plot(inputs$Input_x4,inputs$Input_x1)
abline(lm(inputs$Input_x1~inputs$Input_x4),col="red")
plot(inputs$Input_x1,output$Output_y)
abline(lm(output$Output_y~inputs$Input_x1),col="red")
plot(inputs$Input_x2,output$Output_y)
abline(lm(output$Output_y~inputs$Input_x2),col="red")
plot(inputs$Input_x3,output$Output_y)
abline(lm(output$Output_y~inputs$Input_x3),col="red")
plot(inputs$Input_x4,output$Output_y)
abline(lm(output$Output_y~inputs$Input_x4),col="red")

#Task 1.7:

#install.packages("corrplot")
library(corrplot) # using corrplot to add aesthetics
corrplot(correlate_measure,method="number")
