#Define the sequence to visualize the data over.
x = seq(-5,5,length = 100)

#Define the class probabilities
pi_0 = 0.69
pi_1 = 1 - 0.69

#Define the functions
y_1 = pi_0*dnorm(x)
y_2 = pi_1*dnorm(x,1,sqrt(0.5))


#Plot the curve out.
plot(x,y_1,col = "blue", lwd = 4 ,type  ='l', main = "Plot of pi_0f_0 and pi_1f_1",
     xlab = "x", ylab = "pi_if_i(x)")
#Plot the second curve.
points(x, y_2, col="dark red", lwd = 4, type = 'l')

legend("topright",legend = c("Class 0", "Class 1"),
        col = c("blue","dark red"),lwd = 4,
        text.col = "black",
        horiz = FALSE)
###################################################
# Problem 3
# Code given for kNN
## STAT318/462 kNN regression function

kNN <- function(k,x.train,y.train,x.pred) {
  # 
  ## This is kNN regression function for problems with
  ## 1 predictor
  #
  ## INPUTS
  #
  # k       = number of observations in nieghbourhood 
  # x.train = vector of training predictor values
  # y.train = vector of training response values
  # x.pred  = vector of predictor inputs with unknown
  #           response values 
  #
  ## OUTPUT
  #
  # y.pred  = predicted response values for x.pred
  
  ## Initialize:
  n.pred <- length(x.pred);		y.pred <- numeric(n.pred)
  
  ## Main Loop
  for (i in 1:n.pred){
    d <- abs(x.train - x.pred[i])
    dstar = d[order(d)[k]]
    y.pred[i] <- mean(y.train[d <= dstar])		
  }
  ## Return the vector of predictions
  invisible(y.pred)
}

# Load in the data
AutoTrain=read.csv("AutoTrain.csv",header=T,na.strings="?")
AutoTrain = AutoTrain[order(AutoTrain$horsepower),]
AutoTest=read.csv("AutoTest.csv",header=T,na.strings="?")
AutoTest = AutoTest[order(AutoTest$horsepower),]

# Get variables of interest
x.train = AutoTrain$horsepower
y.train = AutoTrain$mpg
x.test = AutoTest$horsepower
y.test = AutoTest$mpg

# Setup the loop variables
k = c(2,5,10,20,30,50,100)
MSE.Train = numeric(length(k))
MSE.Test = numeric(length(k))

#Loop over every value of k, train the data and compute the MSE
# This is run twice to ensure the data has the proper X positions.
for (i in 1:length(k)){
  y.pred_train = kNN(k[i],x.train,y.train,x.train)
  y.pred_test = kNN(k[i],x.train,y.train,x.test)
  MSE.Train[i] = mean((y.train - y.pred_train)^2)
  MSE.Test[i] = mean((y.test - y.pred_test)^2)
}

plot(k,MSE.Train,col = 'red', lwd = 4, type = 'l', main = 'Testing and Training Error vs. 1/k', xlab = '1/k', ylab = 'MSE')
points(k,MSE.Test,col = 'blue', lwd = 4, type = 'l')
legend("topright",legend = c("MSE_Tr", "MSE_Te"),
       col = c("red","blue"),lwd = 4,
       text.col = "black",
       horiz = FALSE)

# Code to test out plotting
y.pred = kNN(20,x.train,y.train,x.test)
plot(x.train,y.train,col = "red" , main = "Training, Testing and Prediction of MPG vs. Horsepower",
     xlab = "Horsepower", ylab = "MPG")
points(x.test,y.test,col = "dark red")
points(x.test,y.pred,pch = 15, col = "blue", cex = 0.6)
legend("topright",legend = c("Training Data", "Testing Data", "Prediction" ),
       col = c("red","dark red",'blue'),lwd = 4,
       text.col = "black",
       horiz = FALSE)
