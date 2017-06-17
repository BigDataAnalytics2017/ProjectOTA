rm(list=ls())
x = readr::read_csv('impact_factor/click_per_day.csv')
summary(x)

# date 2012-11-01 ~ 2013-06-30
head(x)
tail(x)

acf(x$click_bool)
pacf(x$click_bool)

a1 = as.POSIXlt(x$date, format = "%Y-%m-%d")
x$week = a1$wday

# 扔進google correlate用

## 點擊數
# write.table(x[,c(1,3)], 'impact_factor/tmp.csv', sep = "," , row.names = FALSE, col.names = FALSE)

## 點擊率
x$click_percent = x$click_bool / x$obs
# write.table(x[,c(1,7)], 'impact_factor/tmp2.csv', sep = "," , row.names = FALSE, col.names = FALSE)

## 訂房數
# write.table(x[,c(1,4)], 'impact_factor/tmp3.csv', sep = "," , row.names = FALSE, col.names = FALSE)

## 訂房率 = 訂房數除以資料筆數
x$book_percent = x$booking_bool / x$obs
# write.table(x[, colnames(x) %in% c('date','book_percent')], 'impact_factor/tmp4.csv', sep = "," , row.names = FALSE, col.names = FALSE)

## 有點擊下訂房率 = 訂房數除以點擊數
x$book_whithin_click = x$booking_bool / x$click_bool
# write.table(x[, colnames(x) %in% c('date','book_whithin_click')], 'impact_factor/tmp5.csv', sep = "," , row.names = FALSE, col.names = FALSE)


plot(x = x$date, y = x$obs, type = 'l', xlab = "", ylab = "", main = "number of obs")
plot(x = x$date, y = x$click_bool, type = 'l', xlab = "", ylab = "", main = "點擊數")
plot(x = x$date, y = x$click_percent, type = 'l', xlab = "", ylab = "", main = "點擊率")
plot(x = x$date, y = x$booking_bool, type = 'l', xlab = "", ylab = "", main = "訂房數")
plot(x = x$date, y = x$book_percent, type = 'l', xlab = "", ylab = "", main = "訂房率(訂房數除以觀測值)")
plot(x = x$date, y = x$book_whithin_click, type = 'l', xlab = "", ylab = "", main = "有點擊下訂房率(訂房數除以點擊數)")

# 2013-01-31 ~ 2013-02-06 期間，應該有d唔妥，平時訂房率咁穩，點彈突然彈左一下?
# 可能一、
# 我目前搵到最可能既係"House of Card"第一季開播,似乎netflix係一次放一季上去
# http://bgr.com/2016/02/05/netflix-shows-release-schedule-exec/
# 原來佢仲係首個netflix播放的自家製節目
# 咪住先，咁點解係訂房率彈上去?
# http://www.onthisday.com/date/2013/february
# https://en.wikipedia.org/wiki/House_of_Cards_(season_1)#Episodes
x[, colnames(x) %in% c('date','book_percent','obs')] %>% data.frame()

# 如果係有班人慣左平時無無聊聊都會check下房價FF自己去旅行，
# 然後戈幾日播house of card個班無聊人喪煲劇無搵hotel,剩返一班真係plan去旅行既人睇hotel，咁book房比率高左，係咪好合理?
# 我要check埋佢地查詢時間做對比，以每個鐘頭同星期幾為單位，睇下佢地平時習慣係點，戈幾日有無咩唔同?
# 例如係平時返工book房，戈日無咁做；或者夜晚book房，但戈晚走左去煲劇無乜人夜晚book房。


# 可能二、
# https://www.slideshare.net/ExpediaCoupons/expedia-coupons-code-february-2013-march-2013-april-2013
# 有條友係2013-02-04將expedia coupon既資料擺上左linkin，所以多左人book房?（咁點解係1月31開始高?）


# 多謝曾彥華提醒，可能三：情人節開房..。居然咁簡單我都諗唔到,Orz，........


# 觀察數最少戈兩日，係平安夜同聖誕節, 但訂房率都好穩，0.027左右。
x[x$obs %in% sort(x$obs)[1:50],] %>% 
        arrange(obs) %>% 
        select(date,obs,week,book_percent) %>%
        data.frame() %>%
        arrange(date)

# 1月31到2月6好特別，連住幾日都係高訂房率。
x %>% select(date,obs,week,book_percent) %>% 
        filter(book_percent > quantile(x$book_percent,0.90)) %>%
        data.frame()

# book房前10%高既日期，好多都係星期五或六
tmp = x %>% select(date,obs,week,book_percent) %>% 
        filter(book_percent > quantile(x$book_percent,0.90)) %>%
        data.frame()
table(tmp$week)


#
rm(list = ls())
######################

x = readr::read_csv('/home/leongkaon/Documents/big_data_course/ProjectOTA/impact_factor/EXPE.csv')

plot(x$Close, type = 'l')

#######################

# 搭股市資料
# 2012-11-01 ~ 2013-06-30
rm(list=ls())
suppressMessages(library(CausalImpact))
suppressMessages(library(dplyr))

x = data.frame(readr::read_csv('impact_factor/click_per_day.csv')) ; x$book_percent = x$booking_bool / x$obs
y = data.frame(readr::read_csv('/home/leongkaon/Documents/big_data_course/ProjectOTA/impact_factor/EXPE.csv'))
x$click_percent = x$click_bool / x$obs

colnames(y)[1] = 'date'
z = left_join(x, y) %>%
        select(date, book_percent, Close)

matplot(z[,c(2:3)], type = "l")

plot(z$book_percent, type = 'l')
plot(z$Close, type = 'l')

pre.period = c(1,212)
post.period = c(213,242)

# 補NA, 由於係股市資料無六日，我想用一五平均數補
s1 = seq(2,242,by=7)
s2 = seq(5,242,by=7)
loop = min(length(s1),length(s2))

for (i in s1[1:loop]){
        complement = c(i+1,i+2)
        z[complement,3] = mean(c(z[i,3],z[i+3,3]))
}

# 中間仲有d窿，下策，比個mean()佢先
z[,3][is.na(z[,3])] = mean(z[,3], na.rm = TRUE)

impact = CausalImpact(z[,c(2:3)], pre.period, post.period)
plot(impact)

summary(impact)
summary(impact, "report")
impact$summary

#####################################################################################

# 搭google-correlate
# 2012-11-01 ~ 2013-06-30
rm(list=ls())
suppressMessages(library(CausalImpact))
suppressMessages(library(dplyr))

x = data.frame(readr::read_csv('impact_factor/click_per_day.csv'))
y = data.frame(readr::read_csv('/home/leongkaon/Documents/big_data_course/ProjectOTA/impact_factor/google-correlate-click.csv',
                               skip = 11))

x$click_percent = x$click_bool / x$obs

colnames(y)[1] = 'date'

z = left_join(x, y) %>%
       select(date, book_percent, what.is.business.casual.for.men)

# 補數字，因為google-correlate係一個禮拜得一個數字，記係星期日，我想將星期日當做佢成個禮拜都係呢個數字。
# 逐個掃落去，搵到數字，就將呢個位以及上面數字後既位置補上
number = 1
for (i in 1:dim(z)[1]){
        if (is.na(z[i,3])){
                next
        } else {
                z[(number:i),3] = z[i,3]
                number = i + 1
        }
}


matplot(z[,c(2:3)], type = "l")

plot(z[,2], type = 'l')
plot(z[,3], type = 'l')

pre.period = c(1,212)
post.period = c(213,242)

impact = CausalImpact(z[,c(2:3)], pre.period, post.period)
plot(impact)

summary(impact)
summary(impact, "report")
impact$summary


################################

# 搭隊友比既資料
# 2012-11-01 ~ 2013-06-30
rm(list=ls())
suppressMessages(library(CausalImpact))
suppressMessages(library(dplyr))

x = data.frame(readr::read_csv('impact_factor/click_per_day.csv')) ; x$book_percent = x$booking_bool / x$obs
y = data.frame(readr::read_csv('/home/leongkaon/Documents/big_data_course/ProjectOTA/impact_factor/FRED-Data/DCOILWTICO.csv'))

x$click_percent = x$click_bool / x$obs

colnames(y)[1] = 'date'

z = left_join(x, y)
z = z[,c(1,6,8)]

# 補數字，因為經濟數字無星期六日，我用星期五同星期一取平均(未做，填平均睇下點先)
z$DCOILWTICO = as.numeric(z$DCOILWTICO)
s1 = seq(2,242,by=7)
s2 = seq(5,242,by=7)
loop = min(length(s1),length(s2))

for (i in s1[1:loop]){
        complement = c(i+1,i+2)
        z[complement,3] = mean(c(z[i,3],z[i+3,3]))
}

plot(z[,2], type = 'l')
plot(z[,3], type = 'l')

pre.period = c(1,212)
post.period = c(213,242)

impact = CausalImpact(z[,c(2:3)], pre.period, post.period)
plot(impact)

summary(impact)
summary(impact, "report")
impact$summary


### 全面改用book_percent計
################################################################################
# 諗諗下，causalimpact個X，唔一定要上網搵，我用ARIMA fit咪得囉
# date 2012-11-01 ~ 2013-06-30

92  2013-01-31
93  2013-02-01
94  2013-02-02
95  2013-02-03
96  2013-02-04
97  2013-02-05
98  2013-02-06

library(forecast)
library(dplyr)
library(ggplot2)
library(scales)

# Setting
train = 1:91
test = 92:120 # 120係二月尾

# Start
x = readr::read_csv('impact_factor/click_per_day.csv')
a1 = as.POSIXlt(x$date, format = "%Y-%m-%d")
x$week = a1$wday
x$book_percent = x$booking_bool / x$obs

plot(x$date, x$book_percent, type = 'l')
acf(x$book_percent)
pacf(x$book_percent)

PP.test(x$book_percent)
auto.arima(x$book_percent, seasonal = TRUE) # auto.arima係咪都差分，佢係咪玩野?

model = Arima(x$book_percent[train], order = c(1,1,1))
model = Arima(x$book_percent[train], order = c(7,0,10))
model = Arima(x$book_percent[train], order = c(0,0,10))
model = Arima(x$book_percent[train], order = c(10,0,0))

plot(forecast(model, level=95, h=length(test)),main = "Forecasts from ARIMA(p,i,q)")

explain = c(model$fitted,forecast(model,h=length(test),level = 95)$mean)

pre.period = c(1,max(train))
post.period = c(min(test),max(test))

impact = CausalImpact(cbind(book_percent=x$book_percent[1:max(test)], explain), pre.period, post.period)
plot(impact)

summary(impact)
summary(impact, "report")
impact$summary

###################
tmp = data.frame(date = x$date[test], 
                 realValue = x$gross_bookings_usd[test], 
                 fittedValue = forecast(model,h=length(test),level = 95)$mean,
                 fittedLower = as.numeric(forecast(model,h=length(test),level = 95)$lower),
                 fittedUpper = as.numeric(forecast(model,h=length(test),level = 95)$upper))

tmp %>% ggplot() +
        # ribbon要放上層，因為圖型疊加, 除非set alpha
        geom_ribbon(aes(x = date, ymin = fittedLower, ymax = fittedUpper), fill = "deepskyblue3", alpha = 0.3) +
        geom_line(aes(x = date, y = realValue, group = 1, colour = "實際值"), size = 1) + 
        geom_line(aes(x = date, y = fittedValue, group = 1, colour = "預測值"), size = 1) +
        scale_color_manual(name="",values=c(實際值="black",預測值="dodgerblue3")) +
        theme(legend.position="bottom") +
        ggtitle('營收預測值與實際值比較(95%C.I.)')+
        xlab("")+ylab("")+
        scale_y_continuous(labels = comma)


# Mean Absolute Percentage Error
# https://en.wikipedia.org/wiki/Mean_absolute_percentage_error
(100/length(tmp$realValue))*sum(abs((tmp$realValue - tmp$fittedValue) / tmp$realValue))

# Root Mean Square Error
sqrt(sum((tmp$fittedValue - tmp$realValue)^2) / length(tmp$realValue))






