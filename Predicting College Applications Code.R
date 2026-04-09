library(ISLR2)
library(PerformanceAnalytics)
library(boot)
library(tree)

data(College)

##EDA
str(College)
hist(College$Apps, main =  "Distribution of Apps",
     xlab = "Number of Applications", col = "lightyellow", border = "black")
#Strong right skew, MSE could be affected by large schools

par(mfrow = c(1,2))

plot(College$Accept, College$Apps,
     main = "Applications vs Acceptances", 
     xlab = "Number Accepted", ylab = "Number of Applications", pch = 19, col = rgb(0,0,0,0.4))
abline(lm(Apps ~ Accept, data = College), lwd = 2, col = "red4")

plot(College$Enroll, College$Apps,
     main = "Applications vs Enrollment",
     xlab = "Number Enrolled", ylab = "Number of Applications", pch = 19, col = rgb(0,0,0,0.4))
abline(lm(Apps ~ Enroll, data = College), lwd = 2, col = "green4")

par(mfrow = c(1,1))

chart.Correlation(College[,2:18], histogram = TRUE, pch = "+")
#Accept, Enroll, F.Undergrad, Terminal
#Strong correlators are if students are accepted and full time, 
#as well as if the staff is qualified

#Other notable variables: Apps/Accept/Enroll and F.Undergrad
#PHD and Terminal


##a
#linear model using least squares
glm.fit <- glm(Apps ~ ., data = College)
summary(glm.fit)
cv.err <- cv.glm(College, glm.fit)
cv.err$delta
#LOOCV estimate of test error = 1276987, 1276825
glm.fit2 <- glm(Apps ~ .-Accept-Enroll-F.Undergrad, data = College)
summary(glm.fit2)
cv.err2 <- cv.glm(College, glm.fit2)
cv.err2$delta
#LOOCV estimate of test error = 7977698 7977427
#Test error higher when removing significant predictors


##b
#fit tree to data
tree.college <- tree(Apps ~ ., College)
summary(tree.college)
plot(tree.college)
text(tree.college, pretty = 0)
tree.college
#Accept/Enroll/F-Undergrad have very linear correlation
tree.college2 <- tree(Apps ~ .-Accept-Enroll-F.Undergrad, College)
summary(tree.college2)
plot(tree.college2)
text(tree.college2, pretty = 0)
tree.college2

##c
#cross validation
cv.college <- cv.tree(tree.college)
cv.college$size
cv.college$dev
best.size <- cv.college$size[which.min(cv.college$dev)]
best.size

#pruned Tree
prune.college <- prune.tree(tree.college, best = best.size)
prune.tree <- prune.tree(tree.college, best = best.size)
plot(prune.college)
text(prune.college, pretty = 0)

#MSE of pruned tree
pred.pruned <- predict(prune.college, College)
mse.pruned <- mean((College$Apps - pred.pruned)^2)
mse.pruned
#MSE pruned: 2010567
pred.unpruned <- predict(tree.college, College)
mse.unpruned <- mean((College$Apps - pred.unpruned)^2)
mse.unpruned
#MSE = 2010567
#Same both pruned and unpruned

#pruning with reduced variables 2nd model
cv.college2 <- cv.tree(tree.college2)
cv.college2$size
cv.college2$dev
best.size2 <- cv.college2$size[which.min(cv.college2$dev)]
best.size2
#best = 14
#pruned Tree
prune.college2 <- prune.tree(tree.college2, best = best.size2)
prune.tree2 <- prune.tree(tree.college2, best = best.size2)
plot(prune.college2)
text(prune.college2, pretty = 0)
#Again, same tree?

#MSE of pruned tree
pred.pruned2 <- predict(prune.college2, College)
mse.pruned2 <- mean((College$Apps - pred.pruned2)^2)
mse.pruned2
#MSE pruned: 5454741
pred.unpruned2 <- predict(tree.college2, College)
mse.unpruned2 <- mean((College$Apps - pred.unpruned2)^2)
mse.unpruned2
#MSE = 5454741
#Same both pruned and unpruned

cv.college$k
cv.college$size
cv.college$dev


##d
#bagging, B=500
library(randomForest)
library(ggplot2)
library(viridis)

set.seed(1)
train <- sample(nrow(College), 0.7 * nrow(College), replace = FALSE)
bag.college <- randomForest(Apps~., data = College,
                            subset = train,
                            mtry = ncol(College)-1,
                            importance = TRUE)
bag.college
yhat.bag <- predict(bag.college, newdata = College[-train,])
test <- College$Apps[-train]
mean((yhat.bag - test)^2)
sqrt(576855)

#Predicted vs. Actual for B=500
yhat.df.500 <- data.frame(Predicted = yhat.bag, Actual = test)

ggplot(yhat.df.500, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red4", linewidth = 0.8) +
  labs(
    title = "Bagging (B=500): Predicted vs. Actual Applications",
    x = "Predicted Applications",
    y = "Actual Applications"
  ) +
  theme_minimal()

#Variable Importance Plot for B = 500
imp.bag500 <- as.data.frame(importance(bag.college))
imp.bag500$Variable <- reorder(rownames(imp.bag500), imp.bag500$`%IncMSE`)

ggplot(imp.bag500, aes(x = `%IncMSE`, y = Variable)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_vline(xintercept = 0, color = "red4", linetype = "dashed") +
  labs(
    title = "Variable Importance: Bagging B=500",
    x = "% Increase in MSE",
    y = NULL
  ) +
  theme_minimal()

####The MSE for this model using bagging and B=500 is 578655,
####which means that the model is off by about 760 applications. 
####Not too bad considering the scale of the data.
####The most important predictors are Accept, Top 10&25 perc, Grad.rate,
####Expend, and Enroll

#bagging, B=1000
set.seed(1)
bag.college1k <- randomForest(Apps~., data = College,
                              subset = train,
                              mtry = ncol(College)-1,
                              importance = TRUE,
                              ntree = 1000)
yhat.bag1k <- predict(bag.college1k, newdata = College[-train,])
test <- College$Apps[-train]
mean((yhat.bag1k - test)^2)
sqrt(558809.5)
importance(bag.college1k)
varImpPlot(bag.college1k)

#Predicted vs. Actual plot for B=1000
yhat.df.1k <- data.frame(Predicted = yhat.bag1k, Actual = test)

ggplot(yhat.df.1k, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.4, color = "mediumpurple") +
  geom_abline(slope = 1, intercept = 0, color = "tomato", linewidth = 0.9) +
  labs(
    title = "Bagging (B=1000): Predicted vs. Actual Applications",
    x  = "Predicted Applications",
    y = "Actual Applications"
  ) +
  theme_minimal()

#Variable Importance for B=1000
imp.bag1k <- as.data.frame(importance(bag.college1k))
imp.bag1k$Variable <- reorder(rownames(imp.bag1k), imp.bag1k$`%IncMSE`)

ggplot(imp.bag1k, aes(x = `%IncMSE`, y = Variable)) +
  geom_bar(stat = "identity", fill = "mediumpurple") +
  geom_vline(xintercept = 0, color = "tomato", linetype = "dashed") +
  labs(
    title = "Variable Importance: Bagging B=1000",
    x = "% Increase in MSE",
    y = NULL
  ) +
  theme_minimal()

###With B=1000, or MSE is slightly lower at 558809.5, or off
###by about 748 applications. The importance of both Accept and Top10perc
###increase quite dramatically with the higher B value, and Top25perc, Grad.rate,
###Expend, perc.alumni, and F.undergrad all see somewhat significant increases
###in importance. All less important predictors are reduced further.


##e
#random forest approach with B=500
set.seed(1)
rf.college <- randomForest(Apps~., data = College,
                           subset = train, mtry = 6,
                           importance = TRUE,
                           ntree = 500)
yhat.rf <- predict(rf.college, newdata = College[-train,])
mean((yhat.rf - test)^2)
summary(rf.college)
par(mar = c(1,1,1,1))

#Predicted vs. Actual for RF ntree=500
yhat.df.rf500 <- data.frame(Predicted = yhat.rf, Actual = test)

ggplot(yhat.df.rf500, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.4, color = "salmon") +
  geom_abline(slope = 1, intercept = 0, color = "midnightblue", linewidth = 0.9) +
  labs(
    title = "Random Forest (B=500): Predicted vs. Actual Applications",
    x = "Predicted Applications",
    y = "Actual Applications"
  ) +
  theme_minimal()

#Residuals for RF ntree=500
resid.df.rf500 <- data.frame(Fitted = yhat.rf, Residuals = test - yhat.rf)

ggplot(resid.df.rf500, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.4, color = "salmon") +
  geom_hline(yintercept = 0, color = "midnightblue", linewidth = 0.8) +
  labs(
    title = "Random Forest (B=500): Residuals vs. Fitted",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

#Variable importance for RF ntree=500
imp.rf500 <- as.data.frame(importance(rf.college))
imp.rf500$Variable <- reorder(rownames(imp.rf500), imp.rf500$`%IncMSE`)

ggplot(imp.rf500, aes(x = `%IncMSE`, y = Variable)) +
  geom_bar(stat = "identity", fill = "salmon") +
  geom_vline(xintercept = 0, color = "midnightblue", linetype = "dashed") +
  labs(
    title = "Variable Importance: Random Forest B=500",
    x = "% Increase in MSE",
    y = NULL
  ) +
  theme_minimal()


###The MSE with the random forest approach is substantially higher than with
###bagging at 1122164, equating to the prediction being off by about 1060
###applications. Looking at the importance of the predictors, the same 
###predictors seem to be important as was the case with bagging, i.e, Accept,
###Enroll, F.undergrad, Expend, and Top10perc.

#random forest with B=1000
set.seed(1)
rf.college1k <- randomForest(Apps~., data = College,
                             subset = train, mtry = 6,
                             importance = TRUE,
                             ntree = 1000)
yhat.rf1k <- predict(rf.college1k, newdata = College[-train,])
mean((yhat.rf1k - test)^2)
summary(rf.college1k)
par(mar = c(1,1,1,1))

#Variable importance for RF ntree=1000
imp.rf1k <- as.data.frame(importance(rf.college1k))
imp.rf1k$Variable <- reorder(rownames(imp.rf1k), imp.rf1k$`%IncMSE`)

ggplot(imp.rf1k, aes(x = `%IncMSE`, y = Variable)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  geom_vline(xintercept = 0, color = "coral", linetype = "dashed") +
  labs(
    title = "Variable Importance: Random Forest B=1000",
    x = "% Increase in MSE",
    y = NULL
  ) +
  theme_minimal()

#Predicted vs. Actual for RF ntree=1000
yhat.df.rf1k <- data.frame(Predicted = yhat.rf1k, Actual = test)

ggplot(yhat.df.rf1k, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.4, color = "forestgreen") +
  geom_abline(slope = 1, intercept = 0, color = "coral", linewidth = 0.9) +
  labs(
    title = "Random Forest (B=1000): Predicted vs. Actual Applications",
    x = "Predicted Applications",
    y = "Actual Applications"
  ) +
  theme_minimal()

#Out of Bag Error for all Bagging and RF models
oob.all <- data.frame(
  trees   = c(1:500, 1:1000, 1:500, 1:1000),
  oob.mse = c(bag.college$mse, bag.college1k$mse, 
              rf.college$mse,  rf.college1k$mse),
  model   = rep(c("Bagging B=500", "Bagging B=1000", 
                  "Random Forest B=500", "Random Forest B=1000"), 
                c(500, 1000, 500, 1000))
)

ggplot(oob.all, aes(x = trees, y = oob.mse, color = model)) +
  geom_line() +
  scale_color_manual(values = c(
    "Bagging B=500"        = "steelblue4",
    "Bagging B=1000"       = "mediumpurple",
    "Random Forest B=500"  = "salmon",
    "Random Forest B=1000" = "forestgreen"
  )) +
  labs(
    title = "OOB MSE vs. Number of Trees: All Models",
    x = "Number of Trees (B)",
    y = "OOB MSE",
    color = NULL
  ) +
  theme_minimal()

ggplot(oob.all, aes(x = trees, y = oob.mse, color = model)) +
  geom_line() +
  scale_color_manual(values = c(
    "Bagging B=500"        = "steelblue4",
    "Bagging B=1000"       = "mediumpurple",
    "Random Forest B=500"  = "salmon",
    "Random Forest B=1000" = "forestgreen"
  )) +
  labs(
    title = "OOB MSE vs. Number of Trees: All Models",
    x = "Number of Trees (B)",
    y = "OOB MSE",
    color = NULL
  ) +
  theme_minimal()

#Comparing MSE of all models

#Comparing MSE of all models
MSE.compare <- data.frame(
  Method = c(
    "OLS (LOOCV)",
    "Unpruned Tree",
    "Pruned Tree",
    "Bagging B=500",
    "Bagging B=1000",
    "Random Forest B=500",
    "Random Forest B = 1000"
  ),
  MSE = c(
    cv.err$delta[1],
    mse.unpruned,
    mse.pruned,
    mean((yhat.bag - test)^2),
    mean((yhat.bag1k - test)^2),
    mean((yhat.rf - test)^2),
    mean((yhat.rf1k - test)^2)
  )
)

MSE.compare$Method <- factor(MSE.compare$Method,
                             levels = MSE.compare$Method[order(MSE.compare$MSE, decreasing = TRUE)])

ggplot(MSE.compare, aes(x = MSE, y = Method, fill = MSE))+
  geom_bar(stat = "identity")+
  scale_fill_gradient(low = "green4", high = "coral3")+
  geom_text(aes(label = format(round(MSE), big.mark = ",")),
            hjust = -0.1, size = 3.5)+
  scale_x_continuous(expand = expansion(mult = c(0,0.15)))+
  labs(
    title = "Test MSE Comparison Across All Models",
    x = "MSE",
    y = NULL,
  )+
  theme_minimal()

###The MSE with B=1000 is lower at 976564.8, albeit not substantially lower.
###This equates to the prediction being about 988 applications off. The importance
###of the Accept variable increases even further with a higher B from about 29%
###up to 42%. Enroll also increases in importance from around 16% to 24%. Other
###predictors remain relatively stable in their importance with the increase in B.


