credit.data = read.csv("http://homepages.uc.edu/~maifg/7040/credit0.csv")
subset <- sample(nrow(credit.data), nrow(credit.data) * 0.8)
credit.train = credit.data[subset, ]
credit.test = credit.data[-subset, ]

nrow(credit.data)
nrow(credit.train)
nrow(credit.test)

#(i)
credit1<- glm(Y ~ . - id, family = binomial(link="logit"), credit.train)
summary(credit1)
BIC(credit1)
?glm
#(ii)

credit.glm1<-glm(Y ~ X3 + X8 + X9 + X11_2 + X13_2 + X15_4 + X15_6 + X16_2 + X17_3 + 
                   X17_5 + X17_6 + X18_4 + X18_5 + X18_7 + X19_3 + X19_7 + X19_8 + 
                   X19_9 + X20_3 + X21_3 + X22_2 + X22_9 + X22_10 + X23_3, family=binomial, credit.train)


AIC(credit.glm1)
BIC(credit.glm1)
searchgrid = seq(0.01, 0.99, 0.01)
result = cbind(searchgrid, NA)

cost1 <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  
  c0 = (r == 0) & (pi > pcut)  
  return(mean(weight1 * c1 + weight0 * c0))
}
for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  # assign the cost to the 2nd col
  result[i, 2] <- cost1(credit.train$Y, predict(credit.glm1, type = "response"))
}
plot(result, ylab = "Cost in Training Set",xlab="Search Grid for Cut off", xlim=c(0.01,1), col=ifelse(searchgrid==0.07, "red", "blue"))

index=which(result[,2]==min(result[,2]))
index #location where cost is minimum
searchgrid[index]


prob.glm1.insample <- predict(credit.glm1, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.06
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)
table(credit.train$Y, predicted.glm1.insample, dnn = c("Truth", "Predicted"))
mean(ifelse(credit.train$Y != predicted.glm1.insample, 1, 0))


library("verification")
roc.plot(credit.train$Y == "1", prob.glm1.insample)

roc.plot(credit.train$Y == "1", prob.glm1.insample)$roc.vol
roc.plot(credit.train$Y == "1", prob.glm1.insample)$roc.vol$Area


#(iii)

prob.glm1.outsample <- predict(credit.glm1, credit.test, type = "response")
predicted.glm1.outsample <- prob.glm1.outsample > 0.06
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(credit.test$Y, predicted.glm1.outsample, dnn = c("Truth", "Predicted"))
mean(ifelse(credit.test$Y != predicted.glm1.outsample, 1, 0))

roc.plot(credit.test$Y == "1", prob.glm1.outsample)

roc.plot(credit.test$Y == "1", prob.glm1.outsample)$roc.vol
roc.plot(credit.test$Y == "1", prob.glm1.outsample)$roc.vol$Area

prob.glm0.outsample <- predict(credit.glm0, credit.test, type = "response")
roc.plot(x = credit.test$Y == "1", pred = cbind(prob.glm0.outsample, prob.glm1.outsample), 
         legend = TRUE, leg.text = c("Full Model", "Model from Stepwise AIC"))$roc.vol


#(v)

pcut=0.06
# Asymmetric cost
cost2 <- function(r, pi) {
  weight1 = 5
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}


library(boot)
credit.glm1 <- glm(Y ~ X3 + X8 + X9 + X11_2 + X13_2 + X15_4 + X15_6 + X16_2 + X17_3 + 
                     X17_5 + X17_6 + X18_4 + X18_5 + X18_7 + X19_3 + X19_7 + X19_8 + 
                     X19_9 + X20_3 + X21_3 + X22_2 + X22_9 + X22_10 + X23_3, family = binomial, credit.data)
cv.result = cv.glm(credit.data, credit.glm1, cost2, 5)
cv.result$delta


#(Vi)

credit.rpart<-rpart(formula=Y ~ X3 + X8 + X9 + X11_2 + X13_2 + X15_4 + X15_6 + X16_2 + X17_3 + 
                      X17_5 + X17_6 + X18_4 + X18_5 + X18_7 + X19_3 + X19_7 + X19_8 + 
                      X19_9 + X20_3 + X21_3 + X22_2 + X22_9 + X22_10 + X23_3, data=credit.train, method="class",
                    parms = list(loss=matrix(c(0,5,1,0),nrow=2)))

par(mar=c(2,4,2,4))
plot(credit.rpart, compress=T, uniform=T)
text(credit.rpart)

credit.rpart$variable.importance
credit.rpart$cptable
printcp(credit.rpart)
plotcp(credit.rpart)

credit.train.pred.tree1 = predict(credit.rpart, credit.train, type = "class")
table(credit.train$Y, credit.train.pred.tree1, dnn = c("Truth", "Predicted"))
mean(ifelse(credit.train$Y != credit.train.pred.tree1, 1, 0))

credit.train.pred.tree1 = predict(credit.rpart, credit.test, type = "class")
table(credit.test$Y, credit.train.pred.tree1, dnn = c("Truth", "Predicted"))
mean(ifelse(credit.test$Y != credit.train.pred.tree1, 1, 0))


insample.pred <- predict(credit.rpart, credit.train, type="prob")
pred = prediction(insample.pred[, 2], credit.train$Y)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
slot(performance(pred, "auc"), "y.values")[[1]]

outsample.pred <- predict(credit.rpart, credit.test, type="prob")
pred = prediction(outsample.pred[, 2], credit.test$Y)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
slot(performance(pred, "auc"), "y.values")[[1]]


#cost
cost <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi == 0)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi == 1)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
cost(credit.test$Y, credit.test.pred.tree1)

credit.test.pred.glm = as.numeric(predict(credit.glm1, credit.test, type = "response") > 
                                    0.06)
# Calculate cost using test set
cost(credit.test$Y, credit.test.pred.glm)

?prediction

