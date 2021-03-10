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


### Getting the final prediction

data_X2$ses_pred_original = apply(X = data_X2[c(11:16)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5)
  return(ses_mod$mean[2])
})


data_X2$ses_pred_new2 = apply(X = data_X2[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.2)
  return(ses_mod$mean[2])
})


data_X2$ses_pred_new3 = apply(X = data_X2[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.15)
  return(ses_mod$mean[2])
})


data_X2$ses_pred_new4 = apply(X = data_X2[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.12)
  return(ses_mod$mean[2])
})


data_X2$ses_pred_new5 = apply(X = data_X2[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.1)
  return(ses_mod$mean[2])
})

data_X2$ses_pred_new6 = apply(X = data_X2[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.05)
  return(ses_mod$mean[2])
})


# 
# data_X2$holt_pred_new = apply(X = data_X2[c(16:11)],1,FUN = function(y){
#   holt_mod = ets(y,use.initial.values = TRUE,damped=TRUE)
# 
#   onestep <- forecast(object = holt_mod,h = 5)
#   return(onestep$mean[2])
# })

data_X2$ensemble_pred = ifelse((data_X2$Status=="To deplete and delete" | data_X2$Status=="Marked for Elimination 2" |
                                  data_X2$Status=="New Product (10)"),data_X2$sma2_pred,data_X2$ses_pred)



data_X2$ensemble_pred2 = ifelse((data_X2$Status=="To deplete and delete" | data_X2$Status=="Marked for Elimination 2"),
                                 data_X2$sma2_pred,data_X2$ses_pred)


data_X2$ensemble_pred3 = ifelse((data_X2$Status=="To deplete and delete" | data_X2$Status=="Marked for Elimination 2" |
                                   data_X2$Status=="New Product (10)"),data_X2$sma2_pred,data_X2$ses_pred_new2)


###Doing DFA on full data_X2

ensemble_dfa_data_X2 <- data_X2 %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         ses_ABS_ERROR = abs(ses_pred - SIV),
         ses_new_ABS_ERROR = abs(ses_pred_new - SIV),
         ses_new_ABS_ERROR2 = abs(ses_pred_new2 - SIV),
         ses_new_ABS_ERROR3 = abs(ses_pred_new3 - SIV),
         ses_new_ABS_ERROR4 = abs(ses_pred_new4 - SIV),
         ses_new_ABS_ERROR5 = abs(ses_pred_new5 - SIV),
         ses_new_ABS_ERROR6 = abs(ses_pred_new6 - SIV),
         holt_new_ABS_ERROR = abs(holt_pred_new - SIV),
         lm_ABS_ERROR = abs(pred_lm - SIV),
         rf_ABS_ERROR = abs(pred_rf - SIV),
         ens_ABS_ERROR = abs(ensemble_pred - SIV),
         ens_ABS_ERROR2 = abs(ensemble_pred2 - SIV),
         ens_ABS_ERROR3 = abs(ensemble_pred3 - SIV),
         ses_ABS_ERROR_orig = abs(ses_pred_original - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses = (1-(sum(ses_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new = (1-(sum(ses_new_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new2 = (1-(sum(ses_new_ABS_ERROR2, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new3 = (1-(sum(ses_new_ABS_ERROR3, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new4 = (1-(sum(ses_new_ABS_ERROR4, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new5 = (1-(sum(ses_new_ABS_ERROR5, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new6 = (1-(sum(ses_new_ABS_ERROR6, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_holt_new = (1-(sum(holt_new_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_lm = (1-(sum(lm_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_rf = (1-(sum(rf_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ens = (1-(sum(ens_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ens2 = (1-(sum(ens_ABS_ERROR2, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ens3 = (1-(sum(ens_ABS_ERROR3, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_original = (1-(sum(ses_ABS_ERROR_orig, na.rm = T)/sum(SIV, na.rm = T))) * 100)


write.csv(ensemble_dfa_data_X2,file = "ensemble_dfa_multiple_alp4.csv",row.names = FALSE)
write.csv(data_X2,file = "ensemble_lag6_rel_data_multiple_alp4.csv",row.names = FALSE)


## Doing it only for some relevant data

lag6_rel_data = data_X2 %>% inner_join(relevant_summary2[c(1,2)],by = c("Distributor","Material"))

lag6_rel_data$ses_pred_new = apply(X = lag6_rel_data[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE)
  return(ses_mod$mean[2])
})

lag6_rel_data$ses_pred_new2 = apply(X = lag6_rel_data[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.3)
  return(ses_mod$mean[2])
})


lag6_rel_data$ses_pred_new3 = apply(X = lag6_rel_data[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.25)
  return(ses_mod$mean[2])
})


lag6_rel_data$ses_pred_new4 = apply(X = lag6_rel_data[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.2)
  return(ses_mod$mean[2])
})


lag6_rel_data$ses_pred_new5 = apply(X = lag6_rel_data[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.15)
  return(ses_mod$mean[2])
})


lag6_rel_data$ses_pred_new6 = apply(X = lag6_rel_data[c(16:11)],1,FUN = function(y){
  ses_mod = ses(y,initial = "simple",h=5,damped=TRUE,alpha = 0.1)
  return(ses_mod$mean[2])
})



lag6_rel_data$ensemble_pred = ifelse((lag6_rel_data$Status=="To deplete and delete" | lag6_rel_data$Status=="Marked for Elimination 2" |
                                  lag6_rel_data$Status=="New Product (10)"),lag6_rel_data$sma2_pred,lag6_rel_data$ses_pred)



lag6_rel_data$ensemble_pred2 = ifelse((lag6_rel_data$Status=="To deplete and delete" | lag6_rel_data$Status=="Marked for Elimination 2" |
                                  lag6_rel_data$Status=="New Product (10)"),lag6_rel_data$sma2_pred,lag6_rel_data$ses_pred_new)



ensemble_dfa <- lag6_rel_data %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         ses_ABS_ERROR = abs(ses_pred - SIV),
         ses_new_ABS_ERROR = abs(ses_pred_new - SIV),
         ses_new_ABS_ERROR2 = abs(ses_pred_new2 - SIV),
         ses_new_ABS_ERROR3 = abs(ses_pred_new3 - SIV),
         ses_new_ABS_ERROR4 = abs(ses_pred_new4 - SIV),
         ses_new_ABS_ERROR5 = abs(ses_pred_new5 - SIV),
         ses_new_ABS_ERROR6 = abs(ses_pred_new6 - SIV),
         holt_new_ABS_ERROR = abs(holt_pred_new - SIV),
         lm_ABS_ERROR = abs(pred_lm - SIV),
         rf_ABS_ERROR = abs(pred_rf - SIV),
         ens_ABS_ERROR = abs(ensemble_pred - SIV),
         ens_ABS_ERROR2 = abs(ensemble_pred2 - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses = (1-(sum(ses_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new = (1-(sum(ses_new_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new2 = (1-(sum(ses_new_ABS_ERROR2, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new3 = (1-(sum(ses_new_ABS_ERROR3, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new4 = (1-(sum(ses_new_ABS_ERROR4, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new5 = (1-(sum(ses_new_ABS_ERROR5, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses_new6 = (1-(sum(ses_new_ABS_ERROR6, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_holt_new = (1-(sum(holt_new_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_lm = (1-(sum(lm_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_rf = (1-(sum(rf_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ens = (1-(sum(ens_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ens2 = (1-(sum(ens_ABS_ERROR2, na.rm = T)/sum(SIV, na.rm = T))) * 100)


write.csv(ensemble_dfa,file = "ensemble_dfa_multiple_alp2.csv",row.names = FALSE)
write.csv(lag6_rel_data,file = "ensemble_lag6_rel_data_multiple_alp2.csv",row.names = FALSE)

