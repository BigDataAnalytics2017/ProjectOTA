#---------------------------#
# date: 2070325             #
# title: Decision Tree Test #
# data: sample 10000        #
#---------------------------#
library(rpart)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(pROC)
#----------------------------
# 1. read file
# 2. drop some columns: 由於沒任何一筆資料完整，所以要先挑選變數
# 3. classification tree
#       發現分類結果皆為Negative --> imbalance data
#               嘗試處理imbalance data問題 --> stratified sampling
#               效果未如理想
#----------------------------

##################
## 1. read file ##
##################
# we want to predict booking_bool
library(readr)
dat = read_csv("~/Documents/big_data_course/ProjectOTA/sample10000.csv", 
                col_types = cols(booking_bool = col_logical(), 
                                 click_bool = col_logical(), 
                                 position = col_character(), 
                                 prop_id = col_character(), 
                                 random_bool = col_logical(), 
                                 site_id = col_character(), 
                                 srch_id = col_character(), 
                                 visitor_location_country_id = col_character(),
                                 visitor_hist_adr_usd = col_number(),
                                 srch_destination_id = col_character(),
                                 prop_country_id = col_character(),
                                 srch_saturday_night_bool = col_logical(),
                                 prop_brand_bool = col_logical()
                                 )
               )

##########################
## 2. drop some columns ##
##########################
# # 判斷捨棄哪些欄位，若為類別型且太多種即棄, NA太多即棄
# for (i in 1:dim(dat)[2]){
#         print(table(dat[,i]));
#         print(paste("NA:",sum(is.na(dat[,i]))))
#         message(colnames(dat)[i]);
#         Sys.sleep(4);
#         cat(i)
# }


# select some columns
dataSelect = dat %>% select(
        # srch_id,
        # date_time,
        # site_id,                                  # 可以等樣本數大d先放
        # visitor_location_country_id,
        # visitor_hist_starrating,
        # visitor_hist_adr_usd,
        prop_country_id,
        # prop_id,
        prop_starrating,
        prop_review_score,
        prop_brand_bool,
        prop_location_score1,
        # prop_location_score2,                     # 2187 NAs
        prop_log_historical_price,
        # position,
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
        orig_destination_distance,
        random_bool,
        # comp1_rate,
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
        # click_bool,
        # gross_bookings_usd,
        booking_bool
        ) #%>%
        #na.omit()

dim(dataSelect)
a1 = cor(dataSelect[, which(sapply(dataSelect,class) %in% c("numeric","integer","logical"))], use = "complete.obs")
corrplot(a1, method="circle", order = "hclust", tl.col = "black", tl.srt = 90)
# 變數之間的相關性較低


############################
## 3. Classification Tree ##
############################

tree_control = rpart.control(minisplit=2, minbucket = 1, xval = 10)

tree_treeorig = rpart(formula = booking_bool ~ ., 
                      data = dataSelect,
                      method = "class",
                      control=tree_control)

rpart.plot(tree_treeorig)
# Fxxk! 直接全部分類至FALSE,連分支也沒有！

# deal with problem of imbalance data --> stratified sampling
a1 = dataSelect %>% filter(booking_bool==TRUE)
a2 = dataSelect %>% filter(booking_bool==FALSE)
a2 = a2[sample(dim(a2)[1],dim(a1)[1]),]
dataBalance = rbind(a1,a2)
dim(dataBalance)

tree_control = rpart.control(minisplit=2, minbucket = 1, xval = 10)

tree_treeorig = rpart(formula = booking_bool ~ ., 
                       data = dataBalance,
                       method = "class",
                       control=tree_control)

rpart.plot(tree_treeorig)

printcp(tree_treeorig)
# resubstitution error rate          : rel error*Root node error = 0.52459*0.5 = 0.262295
# 10-fold cross-validated error rate : xerror*Root node error    = 0.80656*0.5 = 0.40328
summary(tree_treeorig)

# # prune the tree
# tree_prunetree <- prune.rpart(tree_treeorig,cp=0.02)
# rpart.plot(tree_prunetree)

