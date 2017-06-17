rm(list=ls())
# 營收預測
# 抄 '/home/leongkaon/Documents/course/time_series_course/project_20161229.rmd'
library(forecast)
library(dplyr)
library(ggplot2)
library(scales)

train = 1:228
test = 229:242
x = data.frame(readr::read_csv('impact_factor/click_per_day.csv')) %>%
        select(date, gross_bookings_usd)

PP.test(x$gross_bookings_usd) #p-value<0.05 -->拒絕單根假設

acf(x$gross_bookings_usd[train], main = "ACF")
pacf(x$gross_bookings_usd[train], main = "PACF")

par(mfrow=c(1,2))
acf(x$gross_bookings_usd[train], main = "ACF")
pacf(x$gross_bookings_usd[train], main = "PACF")
par(mfrow=c(1,1))

model = Arima(x$gross_bookings_usd[train], order = c(7,0,7)) # 看圖斷估準過佢個auto.Arima(),睇RMSE就知

plot(forecast(model, level=95, h=length(test)),main = "Forecasts from ARIMA(7,0,7)")
lines(x=228:242,y=x$gross_bookings_usd[228:242])
lines(model$fitted, col="blue", lty=2)


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
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(hjust = 0.5))


# Mean Absolute Percentage Error
# https://en.wikipedia.org/wiki/Mean_absolute_percentage_error
(100/length(tmp$realValue))*sum(abs((tmp$realValue - tmp$fittedValue) / tmp$realValue))

# Root Mean Square Error
sqrt(sum((tmp$fittedValue - tmp$realValue)^2) / length(tmp$realValue))

# 可以考慮實際減或除以預測值，做加權調整，雖然唔知有無理論根據，但應該可以做。
# 我見d乜乜預測都有保守預測同樂觀預測（呢個名真係改得好）
# 咁如果加權調整，CI應該要跟住調整
########################################################


tmp3 = tmp[,c(1,2,3)]
tmp3[,2:3] = round(tmp3[,2:3],4)
# write.csv(tmp3, '/home/leongkaon/Documents/big_data_course/ProjectOTA/predict_gross_profit/predict_table.csv', row.names = FALSE)
















