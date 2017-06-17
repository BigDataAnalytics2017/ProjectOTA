######################
## Data preparation ##
######################
suppressMessages(library(dplyr))
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

cat('read csv ok!\n')

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

# select some columns
dataSelect = dat %>% select(
        # srch_id,
        # date_time,
        # site_id,                                  # 29可以等樣本數大d先放
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

# dim(dataSelect)
# a1 = cor(dataSelect[, which(sapply(dataSelect,class) %in% c("numeric","integer","logical"))], use = "complete.obs")
# corrplot(a1, method="circle", order = "hclust", tl.col = "black", tl.srt = 90)
# 變數之間的相關性較低,無共線性問題

cat('select columns ok!\n')

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

rm(dat) ; rm(dataClickFalse) ; rm(dataClickTrue) ; 
rm(dataClickFalseTrain) ; rm(dataClickFalseTest) ;
rm(dataClickTrueTrain) ; rm(dataClickTrueTest)

cat('training and testing data are ready!\n')

