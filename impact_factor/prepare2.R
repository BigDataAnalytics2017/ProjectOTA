# 請先執行
# sh prepare.sh
# 將train.csv切割為二十份

# 這部份是將二十個分割檔分別處理，
# 需要data_time(YYYY-MM-DD), click_bool, booking_bool, sum(gross_bookings_usd, obs(觀察值數),以便之後計算干擾因子。
# 分別columns index為2,52,54,53
# 計算每日總資料筆數，每日點擊筆數，每日訂房筆數，每日營收
# 將二十個檔案各自算好後，再根據日期計算後合併

library(dplyr)

tmp = readr::read_csv("~/Documents/big_data_course/ProjectOTA/impact_factor/train.csv", n_max = 3, col_names = TRUE)
colnames(tmp)


lf = sprintf("/home/leongkaon/Documents/big_data_course/ProjectOTA/impact_factor/part%s.csv", 1:20)

t1 = vector("list", length = length(lf))
for (i in 1:length(lf)){
        tmp = suppressMessages(
                readr::read_csv(lf[i], col_names = FALSE, na = 'NULL')[,c(2,52,54,53)]
                ) %>% 
                data.frame()
        
        colnames(tmp) = c('date_time','click_bool','booking_bool','gross_bookings_usd')        
        
        tmp$date = substr(tmp[,1], 1, 10)
        
        t1[[i]] = tmp %>% 
                group_by(date) %>%
                mutate() %>%
                summarize(
                        obs = n(),
                        click_bool = sum(click_bool),
                        booking_bool = sum(booking_bool), 
                        gross_bookings_usd = sum(gross_bookings_usd, na.rm = TRUE)
                ) %>%
                data.frame()
        cat(i,'\n')
        
}
t1 = do.call('rbind', t1)

click_per_day = t1 %>% 
        group_by(date) %>%
        summarize(
                obs = sum(obs),
                click_bool = sum(click_bool),
                booking_bool = sum(booking_bool), 
                gross_bookings_usd = sum(gross_bookings_usd, na.rm = TRUE)
        ) %>%
        data.frame
        
write.csv(click_per_day, '/home/leongkaon/Documents/big_data_course/ProjectOTA/impact_factor/click_per_day.csv', row.names = FALSE)

####################################################################

# 20170612 加埋時間，幾點鐘

rm(list=ls())
library(dplyr)

tmp = readr::read_csv("~/Documents/big_data_course/ProjectOTA/impact_factor/train.csv", n_max = 3, col_names = TRUE)
colnames(tmp)


lf = sprintf("/home/leongkaon/Documents/big_data_course/ProjectOTA/impact_factor/part%s.csv", 1:20)

t1 = vector("list", length = length(lf))
for (i in 1:length(lf)){
        tmp = suppressMessages(
                readr::read_csv(lf[i], col_names = FALSE, na = 'NULL')[,c(2,52,54,53)]) %>% 
                data.frame()
        
        colnames(tmp) = c('date_time','click_bool','booking_bool','gross_bookings_usd')        
        
        tmp$date = substr(tmp[,1], 1, 10)
        tmp$hour = substr(tmp[,1], 12, 13)
        
        t1[[i]] = tmp %>% 
                group_by(date,hour) %>%
                mutate() %>%
                summarize(
                        obs = n(),
                        click_bool = sum(click_bool),
                        booking_bool = sum(booking_bool), 
                        gross_bookings_usd = sum(gross_bookings_usd, na.rm = TRUE)
                ) %>%
                data.frame()
        cat(i,'\n')
        
}
t1 = do.call('rbind', t1)

click_per_day = t1 %>% 
        group_by(date,hour) %>%
        summarize(
                obs = sum(obs),
                click_bool = sum(click_bool),
                booking_bool = sum(booking_bool), 
                gross_bookings_usd = sum(gross_bookings_usd, na.rm = TRUE)
        ) %>%
        data.frame

write.csv(click_per_day, '/home/leongkaon/Documents/big_data_course/ProjectOTA/impact_factor/click_per_day&hour.csv', row.names = FALSE)






























