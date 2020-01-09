require(rpart)
require(rpart.plot)
require(rattle)
require(caret)
require(dplyr)

d <- read.csv('~/Downloads/EmployeeAttrition.csv', header = TRUE)

# Refactoring variable types from int to factors
d$Education <-factor(d$Education)
d$EnvironmentSatisfaction <-factor(d$EnvironmentSatisfaction)
d$JobInvolvement <-factor(d$JobInvolvement)
d$JobLevel <- factor(d$JobLevel)
d$JobSatisfaction <- factor(d$JobSatisfaction)
d$PerformanceRating <- factor(d$PerformanceRating)
d$RelationshipSatisfaction <- factor(d$RelationshipSatisfaction)
d$WorkLifeBalance <- factor(d$WorkLifeBalance)

# Preliminary Logistic Regression Prediction Model to detemine significant predictors
allv <- rpart(Attrition ~., d_train, method = 'class', control = rpart.control(minsplit = 25))
model <- glm(d$Attrition ~ d$Age + d$BusinessTravel + d$DistanceFromHome + d$EnvironmentSatisfaction + d$JobInvolvement + d$JobLevel + d$JobSatisfaction + d$MaritalStatus + d$NumCompaniesWorked + d$OverTime + d$RelationshipSatisfaction + d$WorkLifeBalance + d$YearsInCurrentRole + d$YearsSinceLastPromotion + d$YearsWithCurrManager, family = binomial)
summary(model)

# Classification Tree
summary(d)
rows_test <- sample(1:nrow(d), floor(0.2*nrow(d)))
d_test <- d[rows_test,]
d_train <- d[-rows_test,]

treemod <- rpart(Attrition ~ Age + BusinessTravel + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, d_train, method = 'class', control = rpart.control(minsplit = 25))
fancyRpartPlot(treemod, sub = "")

treemod <- glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, family = binomial, d_train)

pred <- predict(treemod, d_test)
d_test$attrition_score <- pred[,2]
test_pred <- d_test %>% select(Attrition, attrition_score)
test_pred <- test_pred %>% arrange(desc(attrition_score))
test_pred$pred <- "No"
top_scores <- floor(nrow(test_pred)*0.10)
test_pred$pred[1:top_scores] <- "Yes"
pred_tab <- table(test_pred$pred,test_pred$Attrition)
confusionMatrix(pred_tab, positive = "Yes")
precision(pred_tab, relevant = 'Yes')
recall(pred_tab, relevant = 'Yes')

# Multiple Classification Trees with different splits, depths, and percents


splits <- c(5,10,15)
depths <- c(2,3,4,5)
percent <- c(.10, .15, .20)
nmods <- length(splits)*length(depths)*length(percent)

results <- data.frame(splits = rep(NA,nmods), depths = rep(NA, nmods), percent = rep(NA,nmods), precision = rep(NA,nmods), recall = rep(NA,nmods), accuracy = rep(NA,nmods))
mod_num <- 1

for(i in 1:length(splits)){ 
  for(j in 1:length(depths)){
    s <- splits[i]
    de <- depths[j]
    treemod <- rpart(Attrition ~., d_train, method = 'class', control = rpart.control(minsplit = s, maxdepth = de))
    pred <- predict(treemod, d_test)
    d_test$attrition_score <- pred[,2]
    test_pred <- d_test %>% select(Attrition, attrition_score) %>% arrange(desc(attrition_score))
    for(k in 1:length(percent)){
      p <- percent[k]
      test_pred$pred <- "No"
      top_scores <- floor(nrow(test_pred)*p)
      test_pred$pred[1:top_scores] <- "Yes"
      
      pred_tab <- table(test_pred$pred,test_pred$Attrition)
      cm <- confusionMatrix(pred_tab, positive = "Yes")
      overall <- cm$overall
      overall.accuracy <- overall['Accuracy']
      results[mod_num,] <- c(s, de, p, precision(pred_tab, relevant = "Yes"), recall(pred_tab, relevant = "Yes"), overall.accuracy)
      mod_num <- mod_num + 1
    }
  }
}

View(results)

# Plotting Logistic Regression Model
rows_test <- sample(1:nrow(d), floor(0.2*nrow(d)))
d_test <- d[rows_test,]
d_train <- d[-rows_test,]
newmod <- glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, family = binomial, data = d_train)
fancyRpartPlot(newmod, sub = "")
plot(newmod)
