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

options(scipen = 999)
options(digits = 2)
setwd("C:/Users/ragraw3/Downloads/India Demand Lubes")

lag_data =read.xlsx(xlsxFile = "FCST_lags.xlsx",sheet = "lags",detectDates = TRUE)


lag_data = lag_data  %>% mutate(Brand.Architecture = factor(Brand.Architecture),
                                       Distributor = factor(Distributor),
                                       Distributor_AR = factor(Distributor_AR),
                                       Material = factor(Material),
                                       Package.Group =factor(Package.Group),
                                       Product.Family = factor(Product.Family),
                                       Product.Line = factor(Product.Line),
                                       Synthetic.Indicator = factor(Synthetic.Indicator))
str(lag_data)

### Getting Dist-Material codes with zero sales throughout 2019
siv_sum = lag_data %>% group_by(Distributor,Material) %>% summarise(sum_siv = sum(SIV))
zero_siv = siv_sum %>% filter(sum_siv==0)
nonzero_siv = siv_sum %>% filter(sum_siv!=0)

nonzero_siv = nonzero_siv[c(-3)]


zero_data = lag_data %>% inner_join(zero_siv[-c(3)],by = c("Distributor","Material"))



## Getting Data with relevant non_zero values
relevant_summary = lag_data %>% group_by(Distributor,Material) %>% summarise(count =n(),
                                                                                  nonzero_count = length(which(SIV!=0)),
                                                                                  siv_total = sum(SIV),
                                                                                  non_zero_percent = round((nonzero_count/count),digits = 3)*100)


relevant_summary2 = relevant_summary %>% filter(non_zero_percent>25)

irrelevant_summary = relevant_summary %>% filter(non_zero_percent<=25)

relevant_data = lag_data %>% inner_join(relevant_summary2[c(1,2)],by = c("Distributor","Material"))



lag3_data_with_macro = read.xlsx("lag_data_with_macro_info.xlsx",sheet = "lag3_data")

lag3_data_with_macro = lag3_data_with_macro %>% select(-"Month.of.Effective.Date") %>% mutate(Month = excel_numeric_to_date(Month,date_system = "modern"))


lag6_data_with_macro = read.xlsx("lag_data_with_macro_info.xlsx",sheet = "lag6_data")

lag6_data_with_macro = lag6_data_with_macro %>% select(-"Month.of.Effective.Date") %>% mutate(Month = excel_numeric_to_date(Month,date_system = "modern"))



##Modelling 

## Naive model

data_X = as.data.frame(lag3_data_with_macro[c(1:10,14:22,28:42,12)])
data_X2 = as.data.frame(lag6_data_with_macro[c(1:10,14:31,37:51,12)])

pred_naive = data_X[c(2,4,9:10,35,11)]

names(pred_naive)[6] <- 'naive_pred'


## Moving Average(MA) Model Predictions for lag3 & lag 6 data

str(data_X)


data_X$sma2_pred = apply(X = data_X[c(11:12)],1,FUN = function(x){
  return(mean(x))
})

data_X$sma3_pred = apply(X = data_X[c(11:13)],1,FUN = function(x){
  return(mean(x))
})

data_X2$sma3_pred = apply(X = data_X2[c(11:13)],1,FUN = function(x){
  return(mean(x))
})

data_X2$sma4_pred = apply(X = data_X2[c(11:14)],1,FUN = function(x){
  return(mean(x))
})

data_X2$sma5_pred = apply(X = data_X2[c(11:15)],1,FUN = function(x){
  return(mean(x))
})

data_X2$sma6_pred = apply(X = data_X2[c(11:16)],1,FUN = function(x){
  return(mean(x))
})


##Exponential Smoothing Predictions

exp_smo <- function(x,alp,order=0){
  if(order==2){
    pred = x[1]*alp +(1-alp)*x[2]
  } else if(order==3){
    pred = x[1]*alp +alp*(1-alp)*x[2] + (1-alp)^2*x[3]
  } else if(order==4){
    pred = x[1]*alp +alp*(1-alp)*x[2] + alp*(1-alp)^2*x[3] + (1-alp)^3*x[4]
  } else if(order==5){
    pred = x[1]*alp +alp*(1-alp)*x[2] + alp*(1-alp)^2*x[3] + alp*(1-alp)^3*x[4] +(1-alp)^4*x[5]
  } else {
    pred = x[1]*alp +alp*(1-alp)*x[2] + alp*(1-alp)^2*x[3] + alp*(1-alp)^3*x[4] +alp*(1-alp)^4*x[5]+(1-alp)^5*x[6]
  }
  
  return(pred)
}

### Getting ES prediction for lag 3 data
data_X$es2_pred = apply(X = data_X[c(11:12)],1,FUN = function(x,alp,order){
  exp_smo(x,alp = alp,order = order)
},alp=0.4,order=2)


data_X$es3_pred = apply(X = data_X[c(11:13)],1,FUN = function(x,alp,order){
  exp_smo(x,alp = alp,order = order)
},alp=0.4,order=3)

### Getting DFA Accuracy

monthly_dfa_lag3 <- data_X %>% 
  mutate(EMAPS_ABS_ERROR = abs(EMAPS.Forecast..Ltrs. - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         es2_ABS_ERROR = abs(es2_pred - SIV),
         es3_ABS_ERROR = abs(es3_pred - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es2 = (1-(sum(es2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es3 = (1-(sum(es3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)


lag3_rel_data = data_X %>% inner_join(relevant_summary2[c(1,2)],by = c("Distributor","Material"))

monthly_dfa_lag3_rel <- lag3_rel_data %>% 
  mutate(EMAPS_ABS_ERROR = abs(EMAPS.Forecast..Ltrs. - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         es2_ABS_ERROR = abs(es2_pred - SIV),
         es3_ABS_ERROR = abs(es3_pred - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es2 = (1-(sum(es2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es3 = (1-(sum(es3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)


lag3_irrel_data = data_X %>% inner_join(irrelevant_summary[c(1,2)],by = c("Distributor","Material"))

monthly_dfa_lag3_irrel <- lag3_irrel_data %>% 
  mutate(EMAPS_ABS_ERROR = abs(EMAPS.Forecast..Ltrs. - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         es2_ABS_ERROR = abs(es2_pred - SIV),
         es3_ABS_ERROR = abs(es3_pred - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es2 = (1-(sum(es2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es3 = (1-(sum(es3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)

setwd("C:/Users/ragraw3/Downloads/India Demand Lubes/Model Results")
dfa_list_lag3 = list("lag3_overall" =monthly_dfa_lag3,"lag3_relevant" = monthly_dfa_lag3_rel,"lag3_irrelevant" = monthly_dfa_lag3_irrel)
format(dfa_list_lag3,digits = 2)
write.xlsx(dfa_list_lag3,file = "lag3_dfa_values4.xlsx")


alpha_val = 0.3
## Getting Exp. Smoothing predictions for lag6 data
data_X2$es2_pred = apply(X = data_X2[c(11:12)],1,FUN = function(x,alp,order){
  exp_smo(x,alp = alp,order = order)
},alp=alpha_val,order=2)

data_X2$es3_pred = apply(X = data_X2[c(11:13)],1,FUN = function(x,alp,order){
  exp_smo(x,alp = alp,order = order)
},alp=alpha_val,order=3)


data_X2$es4_pred = apply(X = data_X2[c(11:14)],1,FUN = function(x,alp,order){
  exp_smo(x,alp = alp,order = order)
},alp=alpha_val,order=4)


data_X2$es5_pred = apply(X = data_X2[c(11:15)],1,FUN = function(x,alp,order){
  exp_smo(x,alp = alp,order = order)
},alp=alpha_val,order=5)

data_X2$es6_pred = apply(X = data_X2[c(11:16)],1,FUN = function(x,alp,order){
  exp_smo(x,alp = alp,order = order)
},alp=alpha_val,order=6)


write.csv(data_X2,file = "data_X2.csv",row.names = FALSE)

monthly_dfa_lag6 <- data_X2 %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         sma4_ABS_ERROR = abs(sma4_pred - SIV),
         sma5_ABS_ERROR = abs(sma5_pred - SIV),
         sma6_ABS_ERROR = abs(sma6_pred - SIV),
         es2_ABS_ERROR = abs(es2_pred - SIV),
         es3_ABS_ERROR = abs(es3_pred - SIV),
         es4_ABS_ERROR = abs(es4_pred - SIV),
         es5_ABS_ERROR = abs(es5_pred - SIV),
         es6_ABS_ERROR = abs(es6_pred - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma4 = (1-(sum(sma4_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma5 = (1-(sum(sma5_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma6 = (1-(sum(sma6_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es2 = (1-(sum(es2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es3 = (1-(sum(es3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es4 = (1-(sum(es4_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es5 = (1-(sum(es5_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es6 = (1-(sum(es6_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)



lag6_rel_data = data_X2 %>% inner_join(relevant_summary2[c(1,2)],by = c("Distributor","Material"))

monthly_dfa_lag6_rel <- lag6_rel_data %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         sma4_ABS_ERROR = abs(sma4_pred - SIV),
         sma5_ABS_ERROR = abs(sma5_pred - SIV),
         sma6_ABS_ERROR = abs(sma6_pred - SIV),
         es2_ABS_ERROR = abs(es2_pred - SIV),
         es3_ABS_ERROR = abs(es3_pred - SIV),
         es4_ABS_ERROR = abs(es4_pred - SIV),
         es5_ABS_ERROR = abs(es5_pred - SIV),
         es6_ABS_ERROR = abs(es6_pred - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma4 = (1-(sum(sma4_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma5 = (1-(sum(sma5_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma6 = (1-(sum(sma6_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es2 = (1-(sum(es2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es3 = (1-(sum(es3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es4 = (1-(sum(es4_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es5 = (1-(sum(es5_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es6 = (1-(sum(es6_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)


lag6_irrel_data = data_X2 %>% inner_join(irrelevant_summary[c(1,2)],by = c("Distributor","Material"))


monthly_dfa_lag6_irrel <- lag6_irrel_data %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         sma4_ABS_ERROR = abs(sma4_pred - SIV),
         sma5_ABS_ERROR = abs(sma5_pred - SIV),
         sma6_ABS_ERROR = abs(sma6_pred - SIV),
         es2_ABS_ERROR = abs(es2_pred - SIV),
         es3_ABS_ERROR = abs(es3_pred - SIV),
         es4_ABS_ERROR = abs(es4_pred - SIV),
         es5_ABS_ERROR = abs(es5_pred - SIV),
         es6_ABS_ERROR = abs(es6_pred - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma4 = (1-(sum(sma4_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma5 = (1-(sum(sma5_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma6 = (1-(sum(sma6_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es2 = (1-(sum(es2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es3 = (1-(sum(es3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es4 = (1-(sum(es4_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es5 = (1-(sum(es5_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_es6 = (1-(sum(es6_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)


dfa_list_lag6 = list("lag6_overall" =monthly_dfa_lag6,"lag6_relevant" = monthly_dfa_lag6_rel,"lag6_irrelevant" = monthly_dfa_lag6_irrel)
write.xlsx(dfa_list_lag6,file = "lag6_dfa_values3.xlsx")


## Random forest model

rf_data = relevant_summary2[c(1,2)]  %>% inner_join(data_X,by = c("Distributor","Material"))

write.csv(rf_data,file = "rf_data.csv",row.names = FALSE)

