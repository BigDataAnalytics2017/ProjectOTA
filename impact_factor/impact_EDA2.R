rm(list=ls())
x = readr::read_csv('impact_factor/click_per_day.csv')
summary(x)

# date 2012-11-01 ~ 2013-06-30

# find weekday
a1 = as.POSIXlt(x$date, format = "%Y-%m-%d")
x$week = a1$wday

## 點擊率
x$click_percent = x$click_bool / x$obs

## 訂房率 = 訂房數除以資料筆數
x$book_percent = x$booking_bool / x$obs

## 有點擊下訂房率 = 訂房數除以點擊數
x$book_whithin_click = x$booking_bool / x$click_bool

plot(x = x$date, y = x$obs, type = 'l', xlab = "", ylab = "", main = "number of obs")
plot(x = x$date, y = x$click_bool, type = 'l', xlab = "", ylab = "", main = "點擊數")
plot(x = x$date, y = x$click_percent, type = 'l', xlab = "", ylab = "", main = "點擊率(點擊數除以資料筆數)")
plot(x = x$date, y = x$booking_bool, type = 'l', xlab = "", ylab = "", main = "訂房數")
plot(x = x$date, y = x$book_percent, type = 'l', xlab = "", ylab = "", main = "訂房率(訂房數除以資料筆數)")
plot(x = x$date, y = x$book_whithin_click, type = 'l', xlab = "", ylab = "%", main = "有點擊下訂房率(訂房數除以點擊數)")

x %>% ggplot()+
        geom_line(aes(x=date,y=book_whithin_click), colour="#0165c1", size=0.8) + 
        xlab('') + ylab('') +
        ggtitle("有點擊下訂房率(訂房數除以點擊數)") +
        theme(panel.background = element_rect(fill = 'grey95')) +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_rect(aes(xmin = as.Date("2013-01-31"), xmax = as.Date("2013-02-06"), ymin = 0, ymax = 1),
                  alpha=0.01, fill="#b2daff") + 
        geom_label(aes(x=as.Date("2013-01-06"), y=0.73, label='31 Jan ~ 6 Feb'), size = 4) +
        coord_cartesian(ylim=c(0.55,0.75))

# 2013-01-31 ~ 2013-02-06 期間，應該有d唔妥，平時訂房率咁穩，點彈突然彈左一下?
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

################################################################################
# 諗諗下，causalimpact個X，唔一定要上網搵，我用ARIMA fit咪得囉
# date 2012-11-01 ~ 2013-06-30
rm(list=ls())

# 92  2013-01-31
# 93  2013-02-01
# 94  2013-02-02
# 95  2013-02-03
# 96  2013-02-04
# 97  2013-02-05
# 98  2013-02-06

# 用有點擊下的訂房率，可能會好d
library(forecast)
library(dplyr)
library(ggplot2)
library(scales)
library(CausalImpact)

# Setting
train = 1:91
test = 92:120 # 120係二月尾

# Start
x = readr::read_csv('impact_factor/click_per_day.csv')
a1 = as.POSIXlt(x$date, format = "%Y-%m-%d")
x$week = a1$wday
x$book_whithin_click = x$booking_bool / x$click_bool

plot(x$date, x$book_whithin_click, type = 'l')
par(mfrow=c(1,2))
acf(x$book_whithin_click[train], main = "ACF")
pacf(x$book_whithin_click[train], main = "PACF")
par(mfrow=c(1,1))

PP.test(x$book_whithin_click[train])
auto.arima(x$book_whithin_click[train], seasonal = TRUE) # auto.arima係咪都差分，佢係咪玩野?

loop = 0
t1 = matrix(NA, ncol=5, nrow=121)
for (i in 0:10){
        for (j in 0:10){
                loop = loop + 1
                model = tryCatch(Arima(x$book_whithin_click[train], order = c(i,0,j)), error = function(e){NULL})
                if (is.null(model)) {next}
                t1[loop,1:5] = c(i,j,AIC = model$aic, AICc = model$aicc, BIC = model$bic)
                cat(i,j,'\n',sep = " ")
        }
}
t2 = data.frame(na.exclude(t1))


t2[which.min(t2[,3]),]
t2[which.min(t2[,4]),]
t2[which.min(t2[,5]),]
colnames(t2) = c('p','q','AIC','AICc','BIC')
t2[,3:5] = round(t2[,3:5],5)
# write.csv(t2, '/home/leongkaon/Documents/big_data_course/ProjectOTA/impact_factor/violence_method.csv', row.names = FALSE)

model = Arima(x$book_whithin_click[train], order = c(1,0,1))
model = Arima(x$book_whithin_click[train], order = c(4,0,2))
model = Arima(x$book_whithin_click[train], order = c(5,0,5))

# fitted value
tmp = data.frame(date = x$date[train],
                 realValue = x$book_whithin_click[train],
                 fittedValue = model$fitted)
tmp %>% ggplot() +
        # ribbon要放上層，因為圖型疊加, 除非set alpha
        geom_line(aes(x = date, y = realValue, group = 1, colour = "實際值"), size = 1) + 
        geom_line(aes(x = date, y = fittedValue, group = 1, colour = "配適值"), size = 1, linetype=1) +
        scale_color_manual(name="",values=c(實際值="black",配適值="dodgerblue3")) +
        theme(legend.position="bottom") +
        ggtitle('訓練資料:訂房率配適值與實際值') +
        xlab("") + ylab("") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(hjust = 0.5))
        


# predict value
plot(forecast(model, level=95, h=length(test)),main = "Forecasts from ARIMA(p,d,q)")

explain = c(model$fitted,forecast(model,h=length(test),level = 95)$mean)

pre.period = c(x$date[1],x$date[max(train)])
post.period = c(x$date[min(test)],x$date[max(test)])

# 加時間做row.names()
z1 = zoo(cbind(book_whithin_click=x$book_whithin_click[1:max(test)], explain), x$date[1:max(test)])
impact = CausalImpact(z1, pre.period, post.period)
plot(impact)

summary(impact)
summary(impact, "report")
impact$summary

###################
tmp = data.frame(date = x$date[test], 
                 realValue = x$book_whithin_click[test], 
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























