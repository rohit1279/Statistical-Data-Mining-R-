german=read.csv(file.choose())
german$Account.Balance=as.factor(german$Account.Balance)
german$Payment.Status.of.Previous.Credit=as.factor(german$Payment.Status.of.Previous.Credit)
german$Purpose=as.factor(german$Purpose)
german$Sex...Marital.Status=as.factor(german$Sex...Marital.Status)
german$Most.valuable.available.asset=as.factor(german$Most.valuable.available.asset)
german$Type.of.apartment=as.factor(german$Type.of.apartment)

indexes = sample(1:nrow(german), size=0.5*nrow(german)) # Random sample of 50% of row numbers created
Train50 <- german[indexes,] # Training data contains created indices
Test50 <- german[-indexes,] # Test data contains the rest

#Logistic Regression
LogisticModel50 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Value.Savings.Stocks + Length.of.current.employment + Sex...Marital.Status + Most.valuable.available.asset + Type.of.apartment + Concurrent.Credits + Duration.of.Credit..month.+ Credit.Amount + Age..years., family=binomial, data = Train50)
summary(LogisticModel50)
LogisticModel50final <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family=binomial, data = Train50)
summary(LogisticModel50final)

pred=predict(LogisticModel50final,data=Test50,type='response')
summary(pred)
# 50% Threshold Value
pred_50 <- ifelse(pred>0.5,1,0)
table(pred_50, Test50$Creditability)

# 75% Threshold Value
pred_75 <- ifelse(pred>0.75,1,0)
table(pred_75, Test50$Creditability)

# 40% Threshold Value
pred_40 <- ifelse(pred>0.4,1,0)
table(pred_40, Test50$Creditability)

#Discriminant Analysis
#LDA 
library(MASS)
ldafit <- lda(Creditability ~ Value.Savings.Stocks + Length.of.current.employment + Duration.of.Credit..month.+ Credit.Amount + Age..years., data = Train50)
ldafit
plot(ldafit)
lda.pred <- predict(ldafit, data=Test50)
ldaclass <- lda.pred$class
table(ldaclass, Test50$Creditability)

#QDA
qdafit <- qda(Creditability ~ Value.Savings.Stocks + Length.of.current.employment + Duration.of.Credit..month.+ Credit.Amount + Age..years., data = Train50)
qdafit
qda.pred <- predict(qdafit, data=Test50)
qdaclass <- qda.pred$class
table(qdaclass, Test50$Creditability)

###Tree based Methods
install.packages("tree")
library(tree)
library(rpart)
Train50_tree <- tree(Creditability ~ Account.Balance+Duration.of.Credit..month.+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Guarantors+Duration.in.Current.address+Most.valuable.available.asset+Age..years.+Concurrent.Credits+Type.of.apartment+No.of.Credits.at.this.Bank+Occupation+No.of.dependents+Telephone, data=Train50, method="class")
summary(Train50_tree)
plot(Train50_tree)
text(Train50_tree, pretty=0,cex=0.6)
Test50_pred <- predict(Train50_tree, Test50, type="vector") ##getting an error here
Test50_pred <- ifelse(Test50_pred>0.5,1,0)
table(Test50_pred, Test50$Creditability)

#Pruned tree
Train50_prune8 <- prune.tree(Train50_tree, best=8)
plot(Train50_prune8)
text(Train50_prune8, pretty=0,cex=0.6)
Test50_prune8_pred <- predict(Train50_prune8, Test50, type="vector")
Test50_prune8_pred <- ifelse(Test50_prune8_pred>0.5,1,0)
table(Test50_prune8_pred, Test50$Creditability)

###Random Forest
install.packages("randomForest")
library(randomForest)
rf50 <- randomForest(Creditability ~., data = Train50, ntree=200, importance=T, proximity=T)
plot(rf50, main="")
rf50
Test50_rf_pred <- predict(rf50, Test50, type="class")
Test50_rf_pred <- ifelse(Test50_rf_pred>0.5,1,0)
table(Test50_rf_pred, Test50$Creditability)
importance(rf50)
varImpPlot(rf50,  main="", cex=0.8)








