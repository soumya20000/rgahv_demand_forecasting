library(corrplot)
library(skimr)
library(lubridate)

library(tseries)
library(forecast)
library(dplyr)
library(magrittr)
library(openxlsx)
library(janitor)
library(smooth)
library(caret)
library(randomForest)
library(doParallel)

options(scipen = 999)
options(digits = 2)
setwd("C:/Users/ragraw3/Downloads/India Demand Lubes/Model Results/")

#new_data_X = read.csv(file = "new_data_X.csv",header = TRUE)
 
str(new_data_X)

lag3_rel_data = new_data_X %>% inner_join(relevant_summary2[c(1,2)],by = c("Distributor","Material"))

lag3_irrel_data = new_data_X %>% inner_join(irrelevant_summary[c(1,2)],by = c("Distributor","Material"))


rf_data = new_data_X[c(1,3,5,7,8,37:43,11:34,35)]

# Partitions

str(rf_data)


rf_data = rf_data %>% mutate(Brand.Architecture = factor(Brand.Architecture),
                             Distributor2 = factor(Distributor2),
                             Distributor_AR = factor(Distributor_AR),
                             Material2 = factor(Material2),
                             Package.Group =factor(Package.Group),
                             Product.Family2 = factor(Product.Family2),
                             Product.Line = factor(Product.Line),
                             Synthetic.Indicator = factor(Synthetic.Indicator),
                             Status = as.factor(Status))

str(rf_data)

write.csv(rf_data,file = "rf_data_latest2.csv",row.names = FALSE)


# index <- createDataPartition(rf_data$SIV, p = 0.8, list = FALSE)
# train <- rf_data[index, ]
# test <- rf_data[-index, ]


## Fitting linear regression model

model_lag.lm = lm(formula = rf_data$SIV~.,data = rf_data)

summary(model_lag.lm)

save.image(file = "Linear_model.RData")


cl <- makeCluster(detectCores()-4) # paralellization
registerDoParallel(cl) # paralellization



# str(train_rf)
# 
# time_start <- Sys.time()
# set.seed(505)
# 
# model_lag.rf <- randomForest(x = train_rf[-c(33)],y=train_rf$SIV,ntree = 100,nodesize = 850,mtry = 8,replace = TRUE,importance = TRUE)
# save.image("India_FCST_RF.RData")
# 
# print(model_lag.rf)
# model_lag.rf$rsq


# ## List the importance of the variables.
# impVar <- round(randomForest::importance(model_lag.rf), 2)
# var_imp =impVar[order(impVar[,1], decreasing=TRUE),]


## Tuning Random Forest
tRF <- tuneRF(x = rf_data[-c(37)], 
              y=rf_data$SIV,
              mtryStart = 7, 
              ntreeTry=101, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 1200, 
              importance=TRUE
)

var_imp = tRF$importance
imp_df=var_imp[order(var_imp[,1], decreasing=TRUE),]

write.csv(imp_df,file = "Var_importance_latest.csv")

tRF$ntree


###Preparing data for prediction on data from August 2019

rf_data2 = data_X2[c(1,3,5,7,8,46:51,55,11:13,17:19,23:25,29:43,44)]

rf_data2 = rf_data2 %>% mutate(Brand.Architecture = factor(Brand.Architecture),
                             Distributor2 = factor(Distributor2),
                             Distributor_AR = factor(Distributor_AR),
                             Material2 = factor(Material2),
                             Package.Group =factor(Package.Group),
                             Product.Family2 = factor(Product.Family2),
                             Product.Line = factor(Product.Line),
                             Synthetic.Indicator = factor(Synthetic.Indicator),
                             Status = as.factor(Status))

## Scoring syntax
rf_data2$pred_rf <- predict(tRF,newdata = rf_data2[-c(37)],type = "response")


rf_data2$pred_lm <- predict.lm(model_lag.lm,rf_data2[-c(37)])


data_X2$pred_rf = rf_data2$pred_rf
data_X2$pred_lm = rf_data2$pred_lm

### Getting DFA Accuracy for lag6 for all the methods

monthly_dfa_lag6 <- data_X2 %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         ses_ABS_ERROR = abs(ses_pred - SIV),
         lm_ABS_ERROR = abs(pred_lm - SIV),
         rf_ABS_ERROR = abs(pred_rf - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses = (1-(sum(ses_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_lm = (1-(sum(lm_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_rf = (1-(sum(rf_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)



lag6_rel_data = data_X2 %>% inner_join(relevant_summary2[c(1,2)],by = c("Distributor","Material"))

monthly_dfa_lag6_rel <- lag6_rel_data %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         ses_ABS_ERROR = abs(ses_pred - SIV),
         lm_ABS_ERROR = abs(pred_lm - SIV),
         rf_ABS_ERROR = abs(pred_rf - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses = (1-(sum(ses_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_lm = (1-(sum(lm_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_rf = (1-(sum(rf_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)


lag6_irrel_data = data_X2 %>% inner_join(irrelevant_summary[c(1,2)],by = c("Distributor","Material"))


monthly_dfa_lag6_irrel <- lag6_irrel_data %>% 
  mutate(EMAPS_ABS_ERROR = abs(`EMAPS.Forecast.(Ltrs)` - SIV),
         Naive_ABS_ERROR = abs(lag_2_SIV - SIV),
         sma2_ABS_ERROR = abs(sma2_pred - SIV),
         sma3_ABS_ERROR = abs(sma3_pred - SIV),
         ses_ABS_ERROR = abs(ses_pred - SIV),
         lm_ABS_ERROR = abs(pred_lm - SIV),
         rf_ABS_ERROR = abs(pred_rf - SIV)) %>% 
  group_by(Month) %>% 
  summarise(DFA = (1-(sum(EMAPS_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_naive = (1-(sum(Naive_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma2 = (1-(sum(sma2_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_sma3 = (1-(sum(sma3_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_ses = (1-(sum(ses_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_lm = (1-(sum(lm_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100,
            DFA_rf = (1-(sum(rf_ABS_ERROR, na.rm = T)/sum(SIV, na.rm = T))) * 100)




dfa_list_lag6 = list("lag6_overall" =monthly_dfa_lag6,"lag6_relevant" = monthly_dfa_lag6_rel,"lag6_irrelevant" = monthly_dfa_lag6_irrel)
write.xlsx(dfa_list_lag6,file = "lag6_dfa_values_latest3.xlsx")


write.xlsx(data_X2,file = "India_DFA_rawdata_with_predictions.xlsx")
save.image(file = "India_DFA_latest_model_09062020.RData")

