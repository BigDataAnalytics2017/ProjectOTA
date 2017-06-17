#---------------------------#
# date: 2070325             #
# title: Decision Tree Test #
# data: 100MB               #
#---------------------------#
# 目標：預測點擊率click_bool
# 1. read file
# 2. drop some columns: 由於沒任何一筆資料完整，所以要先挑選變數
# 3. classification tree
#       發現分類結果皆為Negative --> imbalance data
#               嘗試處理imbalance data問題 --> stratified sampling
#               效果未如理想
# 由於這次是單機運行，擔心電腦負荷問題，所以選用簡單的方法
# 至於Spark還在測試，由於不斷出現Warning或Error，未能在此報告使用
#---------------------------------------------------------------------
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(readr)

##################
## 1. read file ##
##################
# we want to predict booking_bool # n_max = 350000 # 
# 不隨機抽樣，是因為考慮到srch_id，我們應否算人數與人次問題，這樣比較容易觀察使用者行為
dat = read_csv('train.csv', n_max = 10000, na = 'NULL',
               col_types = cols(booking_bool = col_logical(), 
                                click_bool = col_logical(), 
                                position = col_number(), 
                                prop_id = col_character(), 
                                random_bool = col_logical(), 
                                site_id = col_character(), 
                                srch_id = col_character(), 
                                visitor_location_country_id = col_character(),
                                visitor_hist_adr_usd = col_number(),
                                srch_destination_id = col_character(),
                                prop_country_id = col_character(),
                                srch_saturday_night_bool = col_integer(),
                                prop_brand_bool = col_logical()
               )
)

format(object.size(dat), units = "Mb")


# 曾被點擊的飯店，評價如何?
a1 = dat %>% 
        filter(click_bool==1) %>% 
        group_by(prop_id) %>% 
        summarize(click = n(), 
                  book = sum(booking_bool==1),
                  prop_starrating = mean(prop_starrating),
                  prop_review_score = mean(prop_review_score)) %>% 
        arrange(-click,-book)
print(a1)


# 每位使用者點擊次數
a1 = dat %>% group_by(srch_id) %>%
        summarize(count = sum(click_bool)) %>%
        arrange(-count)
table(a1$count)

# click rate per srch_id
a1 = dat %>% select(srch_id, click_bool) %>% group_by(srch_id) %>% summarize(mean = mean(click_bool)) 
sum(a1$mean>0) / (length(unique(a1$srch_id)))

# booking rate per srch_id
a1 = dat %>% select(srch_id, booking_bool) %>% group_by(srch_id) %>% summarize(mean = mean(booking_bool)) 
sum(a1$mean>0) / (length(unique(a1$srch_id)))

# 點擊次數÷ 曝光次數= 點閱率
sum(dat$click_bool>0) / dim(dat)[1]             # 0.0457
sum(dat$booking_bool>0) / dim(dat)[1]           # 0.0281
sum(dat$booking_bool>0) / sum(dat$click_bool>0) #0.61488

##########################
## 2. drop some columns ##
##########################
# # 判斷捨棄哪些欄位，若為類別型且太多種即棄, NA太多即棄
# # id即棄，因會與testing data衝突，若testing出現新的id，會無法判斷，只能以人次計算
# for (i in 1:dim(dat)[2]){
#         print(table(dat[,i]));
#         print(paste("NA:",sum(is.na(dat[,i]))))
#         message(colnames(dat)[i]);
#         Sys.sleep(4);
#         cat(i)
# }

t1 = vector()
for (i in 1:dim(dat)[2]){
        t1[i] = sum(is.na(dat[,i]))
}

naPercent = data.frame(colname = colnames(dat), 
                na = t1, percent = round(t1/dim(dat)[1]*100,2), 
                class = as.vector(unlist(sapply(dat,class)))[-3]
           ) %>%
        arrange(-percent, class)
naPercent$keep = c(rep('X',30),'O','X','O',rep('X',4),rep('O',11), 'X', rep('O',4), 'X')

naPercent = naPercent %>%
        arrange(keep, -percent, class)

# dat %>% ggplot(aes(x = click_bool, y = orig_destination_distance, group = click_bool)) + geom_boxplot()
# dat %>% ggplot(aes(x = prop_starrating, group = click_bool, fill = click_bool)) + geom_density(alpha=0.5)

# select some columns
dataSelect = dat %>% select(
        # srch_id,
        # date_time,
        site_id,                                  # 29可以等樣本數大d先放
        # visitor_location_country_id,            # 56
        # visitor_hist_starrating,
        # visitor_hist_adr_usd,
        # prop_country_id,                        # 62
        # prop_id,                                # 14568
        prop_starrating,
        prop_review_score,
        prop_brand_bool,
        prop_location_score1,
        # prop_location_score2,                     # 2187 NAs
        prop_log_historical_price,
        position,                                # 很重要，但真testing data沒有
        price_usd,
        promotion_flag,
        # srch_destination_id,
        srch_length_of_stay,
        srch_booking_window,                       #(可考慮整合0-5,6-10)
        srch_adults_count,
        srch_children_count,
        srch_room_count,
        srch_saturday_night_bool,
        # srch_query_affinity_score,               # 3137 NAs
        # orig_destination_distance,               # NAs
        random_bool,
        # comp1_rate,                              # 可相加或取平均處理, 可能大量雜訊，例如忽視自己分數較低填NA. bool相加，diff取平均, NA<-0
        # comp1_inv,
        # comp1_rate_percent_diff,
        # comp2_rate,
        # comp2_inv,
        # comp2_rate_percent_diff,
        # comp3_rate,
        # comp3_inv,
        # comp3_rate_percent_diff,
        # comp4_rate,
        # comp4_inv,
        # comp4_rate_percent_diff,
        # comp5_rate,
        # comp5_inv,
        # comp5_rate_percent_diff,
        # comp6_rate,
        # comp6_inv,
        # comp6_rate_percent_diff,
        # comp7_rate,
        # comp7_inv,
        # comp7_rate_percent_diff,
        # comp8_rate,
        # comp8_inv,
        # comp8_rate_percent_diff,
        click_bool                                      # 很重要，本次預測目標
        # gross_bookings_usd,                           # 很重要，但真testing data沒有
        # booking_bool                                  # 很重要，但真testing data沒有
        ) %>%
        na.omit()

dim(dataSelect)
a1 = cor(dataSelect[, which(sapply(dataSelect,class) %in% c("numeric","integer","logical"))], use = "complete.obs")
corrplot(a1, method="circle", order = "hclust", tl.col = "black", tl.srt = 90)
# 變數之間的相關性較低,無共線性問題

##############
## 資料切割 ##
##############

# 以人次計算，按比例抽樣 八二分
dataClickTrue = dataSelect %>% filter(click_bool==TRUE)
dataClickFalse = dataSelect %>% filter(click_bool==FALSE)

dataClickTrueTrain = dataClickTrue[sample(dim(dataClickTrue)[1],dim(dataClickTrue)[1]*0.8),]
dataClickFalseTrain = dataClickFalse[sample(dim(dataClickFalse)[1],dim(dataClickFalse)[1]*0.8),]

dataClickTrueTest = setdiff(dataClickTrue, dataClickTrueTrain)
dataClickFalseTest = setdiff(dataClickFalse, dataClickFalseTrain)

dataTrain = rbind(dataClickTrueTrain, dataClickFalseTrain)
dataTest = rbind(dataClickTrueTest, dataClickFalseTest)


############################
## 3. Classification Tree ##
############################
tree_control = rpart.control(maxdepth = 10, , xval = 10)#,
                             # minisplit=2, minbucket = 1)

tree_origin = rpart(formula = click_bool ~ ., 
                      data = dataTrain,
                      method = "class",
                      control=tree_control)
rpart.plot(tree_origin, extra = 100)

# printcp(tree_origin)
# training
tree_pred = predict(tree_origin, dataSelect, type = "class")
table(tree_pred, dataSelect$click_bool)

# testing
# tree_pred = predict(tree_origin, testData, type = "class")
# table(testData$click_bool , tree_pred)

#--------------------------------------------------------------------------------
# deal with problem of imbalance data --> stratified sampling (under-sampling)
set.seed(1234)
dataClickFalseTrainUnder = 
        dataClickFalseTrain[
        sample(dim(dataClickFalse)[1], 
               dim(dataClickTrueTrain)
               ),
        ]

dataTrainUnderSample = rbind(dataClickFalseTrainUnder, dataClickTrueTrain)

tree_origin = rpart(formula = click_bool ~ ., 
                      data = dataTrainUnderSample,
                      method = "class",
                      control=tree_control)
rpart.plot(tree_origin, extra = 100)

printcp(tree_origin)
# resubstitution error rate          : rel error*Root node error = 0.52459*0.5 = 0.262295
# 10-fold cross-validated error rate : xerror*Root node error    = 0.80656*0.5 = 0.40328
summary(tree_origin)

# # prune the tree
tree_prune = prune.rpart(tree_origin,cp=0.001)
rpart.plot(tree_prune, extra = 100, varlen = -5, faclen = -500)

# training
tree_pred = predict(tree_prune, dataTrainUnderSample, type = "class")
table(tree_pred, dataTrainUnderSample$click_bool)

# testing
tree_pred = predict(tree_prune, dataTest, type = "class")
table(tree_pred, dataTest$click_bool)

#-------------------------------------------------------
# deal with problem of imbalance data --> over-sampling
f1 = floor(dim(dataClickFalseTrain)[1]/dim(dataClickTrueTrain)[1])
t1 = data.frame(matrix(NA, nrow = dim(dataClickTrueTrain)[1]*f1, ncol = dim(dataClickTrueTrain)[2]))
endInd = 0
for (i in 1:f1){
        startInd = endInd + 1
        endInd = startInd + dim(dataClickTrueTrain)[1] - 1
        t1[startInd:endInd, ] = dataClickTrueTrain
}
colnames(t1) = colnames(dataClickTrueTrain)

dataTrainOverSample = rbind(t1, as.data.frame(dataClickFalseTrain))
dim(dataTrainOverSample)

tree_origin = rpart(formula = click_bool ~ ., 
                      data = dataTrainOverSample,
                      method = "class",
                      control=tree_control)
rpart.plot(tree_origin)

printcp(tree_origin)
# resubstitution error rate          : rel error*Root node error = 0.52459*0.5 = 0.262295
# 10-fold cross-validated error rate : xerror*Root node error    = 0.80656*0.5 = 0.40328
summary(tree_origin)

# # prune the tree
tree_prune = prune.rpart(tree_origin,cp=0.001)
rpart.plot(tree_prune)

# training 
tree_pred = predict(tree_prune, dataTrainOverSample, type = "class")
table(tree_pred, dataTrainOverSample$click_bool)

# testing
tree_pred = predict(tree_prune, dataTest, type = "class")
table(tree_pred, dataTest$click_bool)




##############
## Appendix ##
##############

# 有人真會的住一整年....
dat %>% filter(srch_booking_window>0) %>%
        group_by(srch_booking_window) %>% 
        summarize(count = n(), click = sum(click_bool), book = sum(booking_bool)) %>%
        as.data.frame()

mean(dat$prop_review_score, na.rm = TRUE)
table(dat$prop_starrating)


tree_origin = rpart(formula = Species ~ Sepal.Length, 
                      data = iris[1:120,],
                      method = "class")
tree_origin = rpart(formula = Species ~ Sepal.Length, 
                      data = iris,
                      method = "class")

rpart.plot(tree_origin)

predict(tree_origin, iris[1:120,], type = "class")
table(iris[1:120, "Species"],predict(tree_origin, iris[1:120,], type = "class"))
predict(tree_origin, iris[121:150,], type = "class")
table(iris[121:150, "Species"],predict(tree_origin, iris[121:150,], type = "class"))
























