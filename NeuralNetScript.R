DAY_OUT <- TRUE
WEEK_OUT <- FALSE
MONTH_OUT <- FALSE
ETS <- FALSE
REGULAR <- TRUE
# TUNABLE: lag - this is the correlation number of days to go back
lag <- 50
isbn <- 1
if (ETS) {
  maxDays <- 613
}
if (REGULAR) {
  maxDays <- 731
}
if (REGULAR) {
  inputs <- read.csv("finalData.csv", header=TRUE, sep=",", quote="\"")
}
if(ETS) {
  inputs <- read.csv("exponentialResiduals.csv", header=TRUE, sep=",", quote="\"")
}
library(AMORE)
library(splitstackshape)
if (REGULAR) {
  inputs <- reshape(inputs, idvar="ISBN", timevar="Date", direction="wide")
}
if (ETS) {
  inputs <- getanID(inputs, "ISBN")
  inputs <- reshape(inputs, idvar="ISBN", timevar=".id", direction="wide")
}
inputs <- as.matrix(sapply(data.frame(inputs), as.numeric))
inputs <- matrix(inputs, ncol=ncol(inputs), dimnames=NULL)

if (DAY_OUT) {
  # TUNABLE: c(in, h1, h2, ..., out) - layer setup to be passed in newff
  layers <- c(50, 30, 20, 1)
  #    while(isbn < 79) {
  num <- 2
  testNum <-366
  trainIn <- t(data.frame(inputs[isbn, num:(num+lag)]))
  testIn <- t(data.frame(inputs[isbn, testNum:(testNum+lag)]))
  num <- num+1
  testNum <- testNum+1
  while ((num+lag) < 366) {
    trainIn <- rbind(trainIn, 
                     t(data.frame(inputs[isbn, num:(num+lag)])))
    num <- num+1
    if ((testNum+lag) < maxDays) {
      testIn <- rbind(testIn, 
                      t(data.frame(inputs[isbn, testNum:(testNum+lag)])))
      testNum <- testNum +1
    }
  }
  trainT <- trainIn[,(lag+1):(lag+1)]
  testT <- testIn[,(lag+1):(lag+1)]
  normTrainT <-trainT# (trainT-min(trainT))/(max(trainT)-min(trainT))
  normTrainIn <- t(apply(trainIn[,1:(lag)], 1, 
                         function(x)(x-min(x))/(max(x)-min(x))))
  normTestT <- testT#(testT-min(testT))/(max(testT)-min(testT))
  normTestIn <- t(apply(testIn[,1:(lag)], 1, 
                        function(x)(x-min(x))/(max(x)-min(x))))
  # newff creates the neural net with tunable parameters
  #TUNABLE: learning.rate, momentum, hidden & output.layer are
  #         the activation functions
  net <-  newff(layers, learning.rate.global=0.000001,
                momentum.global=0.1, error.criterium="LMS", Stao=NA,
                hidden.layer="sigmoid", output.layer="purelin",
                method="ADAPTgdwm")
  
  print("PASSED NET CREATION")
  # train() trains the net w/gradient descent using backprop
  # TUNABLE: show.step is the number of iterations, n.shows
  #          tells you how frequently you want it to print errors 
  trainedNet <- train(net, normTrainIn, normTrainT, 
                      error.criterium="LMS", show.step=1000, n.shows=25)
  
  print("PASSED TRAINING")
  # simulate the testing inputs on the trained net
  y <- sim(trainedNet$net, normTestIn)
  # plot the testing target values against the outputs, y
  plot(normTestT, col="red", type="o", pch=22, xlab="Days", 
       ylab="Norm Prices")
  lines(normTestT, col="red")
  points(y, col="blue", type="o", pch=23)
  lines(y, col="blue")
  title(main="Predicted vs Actual Values")
  legend("topleft", c("Actual", "Predicted"),  
         col=c("red", "blue"), pch=22:23)
  #        plot(normTestT, y, col="blue", pch="+", xlab="Actual Values",
  #        ylab="Predicted Values", main="MLP Neural Network Results")
  #        regressionLine <- lm(y~normTestT)
  #        abline(regressionLine)
  isbn <- isbn+1
  #    }
  
}
if (WEEK_OUT) {
  trainIn <- inputs[,2:494]
  testIn <- inputs[,502:724]
  trainT <- inputs[,501:501]
  testT <- inputs[,731:731]
  layers <- c(493, 3, 2, 1)
}
if (MONTH_OUT) {
  trainIn <- inputs[,2:470]
  testIn <- inputs[,502:700]
  trainT <- inputs[,501:501]
  testT <- inputs[,731:731]
  layers <- c(469, 3, 2, 1)
}
