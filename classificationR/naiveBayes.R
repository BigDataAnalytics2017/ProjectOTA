#---------------------------#
# date: 2070607             #
# title: Naive Bayes        #
# data: 10,000 rows         #
#---------------------------#
# 目標：預測點擊率click_bool
# 1. read file
# 2. select columns: 由於沒任何一筆資料完整，所以要先前置處理&挑選變數
# 3. naive bayes
#       發現分類結果皆為Negative --> imbalance data
#               嘗試處理imbalance data問題 --> stratified sampling
#               效果未如理想
# 由於這次是單機運行，擔心電腦負荷問題，所以選用簡單的方法
#---------------------------------------------------------------------

##########################
## 1. read file         ##
## 2. select columns    ##
##########################

rm(list = ls())
source('classificationR/dataPreparation.R')
ls()
library(e1071)
library(caret)

############################
## 3. Naive Bayes         ##
############################
model = naiveBayes(click_bool~., data = dataTrain)
class(model)
summary(model)
print(model)

preds = predict(model, newdata = dataTest[,-16])
confusMatrix = table(predict = factor(preds, levels = c("TRUE","FALSE")), 
                    trueClass = factor(dataTest$click_bool, levels = c("TRUE","FALSE")))
print(confusMatrix)

a2 = data.frame(item = c('precision','recall','accuracy','F-measure'),
                number = c(confusMatrix[1,1] / (confusMatrix[1,1]+confusMatrix[1,2]),
                           confusMatrix[1,1] / (confusMatrix[1,1]+confusMatrix[2,1]),
                           (confusMatrix[1,1]+confusMatrix[2,2]) / (sum(confusMatrix)),
                           (2)/( (1/(confusMatrix[1,1]/(confusMatrix[1,1]+confusMatrix[1,2]))) + (1/(confusMatrix[1,1]/(confusMatrix[1,1]+confusMatrix[2,1]))))
                )
)
print(confusMatrix)
print(a2)

# 又是全部猜不會點擊...
# 看來一定要調整有/無點擊比例






