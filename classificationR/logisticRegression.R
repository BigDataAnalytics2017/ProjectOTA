#---------------------------
# date: 2070403             
# title: logistic regression 
# data: sample 10000        
#---------------------------
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(pROC)
#----------------------------
# 1. read file
# 2. drop some columns: 由於沒任何一筆資料完整，所以要先挑選變數
# 3. forward selection
# 4. backward selection
# 5. logistic regression
#       選擇backward selection的欄位
#       發現分類結果皆為Negative --> imbalance data
#               嘗試處理imbalance data問題 --> stratified sampling
#               效果未如理想
#----------------------------

##################
## 1. read file ##
##################
# we want to predict booking_bool
library(readr)
dat = read_csv('train.csv', n_max = 20000, na = 'NULL',
               col_types = cols(booking_bool = col_integer(), 
                                click_bool = col_integer(), 
                                position = col_number(), 
                                prop_id = col_character(), 
                                random_bool = col_integer(), 
                                site_id = col_character(), 
                                srch_id = col_character(), 
                                visitor_location_country_id = col_character(),
                                visitor_hist_adr_usd = col_number(),
                                srch_destination_id = col_character(),
                                prop_country_id = col_character(),
                                srch_saturday_night_bool = col_integer(),
                                prop_brand_bool = col_integer()
               )
)

test_data = dat[10001:20000,]

dat = dat[1:10000,]

length(unique(dat$srch_id))
length(unique(dat$site_id))
length(unique(dat$visitor_location_country_id))

a1 = dat %>% group_by(srch_id, click_bool, booking_bool) %>% summarize(count = n())
print(a1)

# click rate per srch_id
a1 = dat %>% select(srch_id, click_bool) %>% group_by(srch_id) %>% summarize(mean = mean(click_bool)) 
sum(a1$mean>0) / (length(unique(a1$srch_id)))

# booking rate per srch_id
a1 = dat %>% select(srch_id, booking_bool) %>% group_by(srch_id) %>% summarize(mean = mean(booking_bool)) 
sum(a1$mean>0) / (length(unique(a1$srch_id)))

# 點擊次數÷ 曝光次數= 點閱率
sum(dat$click_bool>0) / 10000                   # 0.0457
sum(dat$booking_bool>0) / 10000                 # 0.0281
sum(dat$booking_bool>0) / sum(dat$click_bool>0) #0.61488

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

t1 = vector()
for (i in 1:dim(dat)[2]){
        t1[i] = sum(is.na(dat[,i]))
}
data.frame(colname = colnames(dat), na = t1)


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
        position,
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
        click_bool
        # gross_bookings_usd,
        # booking_bool
) %>%
        na.omit()

dim(dataSelect)
a1 = cor(dataSelect[, which(sapply(dataSelect,class) %in% c("numeric","integer","logical"))], use = "complete.obs")
corrplot(a1, method="circle", order = "hclust", tl.col = "black", tl.srt = 90)
# 變數之間的相關性較低

##########################
## 3. Forward Selection ##
##########################
# formu = summary(step(glm(click_bool~., data = dataSelect, family = 'binomial'), direction = "forward"))$call

###########################
## 4. Backward Selection ##
###########################
# 搵AIC最低
formu = summary(step(glm(click_bool~., data = dataSelect, family = 'binomial'), direction = "backward"))$call

# glm(formula = click_bool ~ prop_starrating + prop_review_score + position + price_usd + promotion_flag, family = "binomial", data = dataSelect)

############################
## 5. Logistic Regression ##
############################

a1 = as.character(formu)[[2]]
a1 = unlist(strsplit(a1 , split = '[~+]'))
a1 = gsub(" ","",a1)
cat(a1, sep = ', ')

dataSelect = dat %>% 
        select(click_bool, prop_starrating, prop_review_score, position, price_usd, promotion_flag) %>% 
        na.omit()
logistic = glm(formula = click_bool ~ ., 
               data = dataSelect, family = "binomial")
print(summary(logistic))

hist(predict.glm(logistic, type = "response"), col = "grey90")
plot(roc(dataSelect$click_bool ~ predict.glm(logistic, type = "response")),
     main = "ROC curve", print.auc = TRUE)

PosOrNeg = ifelse(predict.glm(logistic, type = "response") >=0.2, 'Positive', 'Negative')
table(dataSelect$click_bool, PosOrNeg)

PosOrNeg = ifelse(predict.glm(logistic, type = "response") >=0.15, 'Positive', 'Negative')
table(dataSelect$click_bool, PosOrNeg)

PosOrNeg = ifelse(predict.glm(logistic, type = "response") >=0.1, 'Positive', 'Negative')
table(dataSelect$click_bool, PosOrNeg)


####################

#############
## testing ##
#############

test_data_select = test_data %>% 
        select(click_bool, prop_starrating, prop_review_score, position, price_usd, promotion_flag) %>% 
        na.omit()
plot(roc(dataSelect$click_bool ~ ),
     main = "ROC curve", print.auc = TRUE)

PosOrNeg = ifelse(predict.glm(logistic, 
                              newdata = test_data_select,
                              type = "response") >=0.1, 'Positive', 'Negative')
table(test_data_select$click_bool, PosOrNeg)

















