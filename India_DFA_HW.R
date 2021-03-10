# Libraries

library(corrplot)
library(readr)
library(skimr)
library(lubridate)

library(tseries)
library(forecast)
library(dplyr)
library(magrittr)
library(openxlsx)
library(janitor)
library(smooth)

library(pastecs)
library(caret)
library(randomForest)
library(doParallel)

##Getting full siv data for long window
siv_data_lw = read.xlsx(xlsxFile= "C:\Users\ragraw3\Downloads\India Demand Lubes\Long term SIV\demand_trend_all_years_long_format.xlsx",sheet = "Sheet1",detectDates = TRUE)


siv_data_lw2 = siv_data_lw[c(4,6,2,7)]
names(siv_data_lw2)<-c("Distributor","Material","Month","SIV")

str(siv_data_lw2)

siv_data_lw2 = siv_data_lw2 %>% mutate(Material = as.character(Material),
                                       Month = parse_date_time2(x = Month,orders = "b Y"))

str(siv_data_lw2)
siv_data_lw2 = siv_data_lw2 %>% mutate(Month = as.Date(x = Month,format = "%Y %m %d"))
str(siv_data_lw2)

siv_data_lw3 = siv_data_lw2 %>% group_by(Distributor,Material,Month) %>% summarise(total_SIV = sum(SIV))

siv_data_lw4 = siv_data_lw3 %>% group_by(Distributor,Material) %>% summarise(count = n())

lag_data2 = lag_data[c(2,4,5,13)]
names(lag_data2)[3]<-"Month"



##Demand data aggregated

full_siv_data = read.xlsx(xlsxFile= "C:/Users/ragraw3/Downloads/India Demand Lubes/Long term SIV/demand_trend_all_years_long_format.xlsx",sheet = "Sheet1",detectDates = TRUE)

names(full_siv_data)[3]<-"Material"


full_siv_data = full_siv_data %>% mutate(Month = excel_numeric_to_date(timestamp,date_system = "modern"),
                                         key = factor(key))

str(full_siv_data)
full_siv_data2 = full_siv_data[c(2,3,6,4)]

full_siv_data2 = full_siv_data2 %>% group_by(Distributor,Material) %>% arrange(Month,.by_group=TRUE) %>% ungroup()

write.csv(full_siv_data2,file = "C:/Users/ragraw3/Downloads/India Demand Lubes/Long term SIV/full_siv_data.csv",row.names=FALSE)

full_siv_data2 = full_siv_data2 %>% mutate(Distributor = factor(Distributor),
                                           Material = factor(Material)) %>% na.omit()

str(full_siv_data2)
summary(full_siv_data2)

siv_sum2 = full_siv_data2 %>% group_by(Distributor,Material) %>% summarise(count=n()) %>% ungroup()

full_siv_data3 = full_siv_data2 %>% filter(Month < "2019-07-01")

full_siv_data4 = full_siv_data2 %>% filter(Month > "2019-06-01" & Month < "2020-01-01")

ses_pred_data = data.frame(fix.empty.names = TRUE)
ses_pred_data2 = data.frame(fix.empty.names = TRUE)


for(row in 1:nrow(siv_sum2)){
  dist = siv_sum2[row,"Distributor"]
  prod = siv_sum2[row,"Material"]
  print(paste("row",row))
  data2 =  full_siv_data3 %>%filter(Distributor == dist$Distributor & Material==prod$Material)
  data3 =  full_siv_data4 %>%filter(Distributor == dist$Distributor & Material==prod$Material)
  
  ts_data = ts(data = data2[c(4)],start = c(2016,1),end = c(2019,6),frequency = 12)
  ses_mod = ses(ts_data,initial = "simple",h=6,damped=TRUE)
  
  data2 = cbind(data2,as.numeric(ses_mod$fitted))
  data3 = cbind(data3,as.numeric(ses_mod$mean))
  
  ses_pred_data = rbind(ses_pred_data,data2)
  ses_pred_data2 = rbind(ses_pred_data2,data3)
  
}


names(ses_pred_data)[5]<-"ses_pred2"

names(ses_pred_data2)[5]<-"ses_pred2"

write.csv(ses_pred_data2,file = "ses_pred_lw.csv",row.names = FALSE)

lw_ses_data_X = data_X2 %>% inner_join(ses_pred_data2,by=c("Distributor","Material","Month"))


 ##  Fitting Holt winters model on the data

ses_pred_data3 = data.frame(fix.empty.names = TRUE)
ses_pred_data4 = data.frame(fix.empty.names = TRUE)


for(row in 1:nrow(siv_sum2)){
  dist = siv_sum2[row,"Distributor"]
  prod = siv_sum2[row,"Material"]
  print(paste("row",row))
  data2 =  full_siv_data3 %>%filter(Distributor == dist$Distributor & Material==prod$Material)
  data3 =  full_siv_data4 %>%filter(Distributor == dist$Distributor & Material==prod$Material)
  
  ts_data = ts(data = data2[c(4)],start = c(2016,1),end = c(2019,6),frequency = 12)
  holt_mod = ets(y=ts_data,use.initial.values = TRUE,damped = TRUE)
  print(holt_mod$method)
  
  nstepahead <- forecast(object = holt_mod,h = 6)
  
  data2 = cbind(data2,as.numeric(holt_mod$fitted))
  data3 = cbind(data3,as.numeric(nstepahead$mean))
  
  ses_pred_data3 = rbind(ses_pred_data3,data2)
  ses_pred_data4 = rbind(ses_pred_data4,data3)
  
}


names(ses_pred_data3)[5]<-"holt_pred2"

names(ses_pred_data4)[5]<-"holt_pred2"

write.csv(ses_pred_data4,file = "holt_pred_lw2.csv",row.names = FALSE)


lw_holt_data_X = lw_ses_data_X %>% inner_join(ses_pred_data4,by=c("Distributor","Material","Month"))

write.csv(lw_holt_data_X,file = "lw_holt_data_X2.csv",row.names = FALSE)



##  Fitting Arima model on the data

ses_pred_data5 = data.frame(fix.empty.names = TRUE)
ses_pred_data6 = data.frame(fix.empty.names = TRUE)


for(row in 1:nrow(siv_sum2)){
  dist = siv_sum2[row,"Distributor"]
  prod = siv_sum2[row,"Material"]
  print(paste("row",row))
  data2 =  full_siv_data3 %>%filter(Distributor == dist$Distributor & Material==prod$Material)
  data3 =  full_siv_data4 %>%filter(Distributor == dist$Distributor & Material==prod$Material)
  
  ts_data = ts(data = data2[c(4)],start = c(2016,1),end = c(2019,6),frequency = 12)
  arima_mod = auto.arima(y = ts_data)
  
  nstepahead <- forecast(object = arima_mod,h = 6)
  
  data2 = cbind(data2,as.numeric(arima_mod$fitted))
  data3 = cbind(data3,as.numeric(nstepahead$mean))
  
  ses_pred_data5 = rbind(ses_pred_data5,data2)
  ses_pred_data6 = rbind(ses_pred_data6,data3)
  
}


names(ses_pred_data5)[5]<-"arima_pred"

names(ses_pred_data6)[5]<-"arima_pred"

write.csv(ses_pred_data6,file = "arima_pred_lw.csv",row.names = FALSE)


lw_arima_data_X = lw_holt_data_X %>% inner_join(ses_pred_data6,by=c("Distributor","Material","Month"))


lw_arima_dfa <- lw_arima_data_X %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         ses_ABS_ERROR = abs(ses_pred - SIV),
         ses_ABS_ERROR2 = abs(ses_pred2 - SIV),
         holt_ABS_ERROR2 = abs(holt_pred2 - SIV),
         arima_ABS_ERROR = abs(arima_pred - SIV),
         lm_ABS_ERROR = abs(pred_lm - SIV),
         rf_ABS_ERROR = abs(pred_rf - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses = (1-(sum(ses_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses2 = (1-(sum(ses_ABS_ERROR2, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_holt2 = (1-(sum(holt_ABS_ERROR2, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_arima = (1-(sum(arima_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_lm = (1-(sum(lm_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_rf = (1-(sum(rf_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)


write.csv(lw_arima_dfa,file = "lw_arima_dfa.csv",row.names = FALSE)
write.csv(lw_arima_data_X,file = "lw_arima_data_X.csv",row.names = FALSE)


lw_arima_rel_data = lw_arima_data_X %>% inner_join(relevant_summary2[c(1,2)],by = c("Distributor","Material"))

monthly_dfa_lag6_rel <- lw_arima_rel_data %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         ses_ABS_ERROR = abs(ses_pred - SIV),
         ses_ABS_ERROR2 = abs(ses_pred2 - SIV),
         holt_ABS_ERROR = abs(holt_pred - SIV),
         arima_ABS_ERROR = abs(arima_pred - SIV),
         lm_ABS_ERROR = abs(pred_lm - SIV),
         rf_ABS_ERROR = abs(pred_rf - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses = (1-(sum(ses_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses2 = (1-(sum(ses_ABS_ERROR2, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_holt = (1-(sum(holt_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_arima = (1-(sum(arima_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_lm = (1-(sum(lm_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_rf = (1-(sum(rf_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)


lw_arima_irrel_data = lw_arima_data_X %>% inner_join(irrelevant_summary[c(1,2)],by = c("Distributor","Material"))


monthly_dfa_lag6_irrel <- lw_arima_irrel_data %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         ses_ABS_ERROR = abs(ses_pred - SIV),
         ses_ABS_ERROR2 = abs(ses_pred2 - SIV),
         holt_ABS_ERROR = abs(holt_pred - SIV),
         arima_ABS_ERROR = abs(arima_pred - SIV),
         lm_ABS_ERROR = abs(pred_lm - SIV),
         rf_ABS_ERROR = abs(pred_rf - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses = (1-(sum(ses_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses2 = (1-(sum(ses_ABS_ERROR2, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_holt = (1-(sum(holt_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_arima = (1-(sum(arima_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_lm = (1-(sum(lm_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_rf = (1-(sum(rf_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)




dfa_list_lag6 = list("lw_arima_overall" =lw_arima_dfa,"lw_arima_relevant" = monthly_dfa_lag6_rel,"lw_arima_irrelevant" = monthly_dfa_lag6_irrel)
write.xlsx(dfa_list_lag6,file = "lag6_dfa_values_lw_arima_11062020.xlsx")