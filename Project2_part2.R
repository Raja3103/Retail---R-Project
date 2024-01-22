library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(pROC)
library(ggplot2)
library(ROCit)
library(caTools)

Store_Train= read.csv(r'(D:\Raja\R\Project 2 Retail\store_train.csv)', stringsAsFactors = F)
Store_Test = read.csv(r'(D:\Raja\R\Project 2 Retail\store_test.csv)',stringsAsFactors = F)


head(Store_Train)
dim(Store_Train)
summary(Store_Train)

view(Store_Test)
Store_Train$store_Type


## changing columns as required  
Store_Train$storecode = as.factor(Store_Train$storecode)
Store_Train$CouSub = as.character(Store_Train$CouSub)
Store_Train$country = as.character(Store_Train$country)
Store_Train$State = as.character(Store_Train$State)



## Function for store code determination 
ext_func= function(x){
  stringr::str_extract(x,"^\\w{5}")
}

table(ext_func(Store_Train$storecode))

## creating pipe for data preparation 
#
#
dx_pipe = recipe(store ~ . , data = Store_Train) %>% 
  step_mutate_at(storecode, fn= ext_func) %>% 
  update_role(Id,countytownname, country, new_role = 'drop_vars') %>%
  update_role(sales0,sales1,sales2,sales3,sales4,population,new_role = 'to_numeric') %>%
  update_role(store_Type,storecode,CouSub,countyname,state_alpha,Areaname,State,new_role = 'to_dummy') %>%
  step_unknown(has_role('to_dummy'), new_level = '__missing__') %>%
  step_other(store_Type,has_role('to_dummy'), threshold = 0.01,other = '__other__') %>% 
  step_rm(has_role('drop_vars')) %>% 
  step_dummy(has_role('to_dummy')) %>% 
  step_mutate_at(has_role('to_numeric'),fn=as.numeric) %>%
  step_impute_median(all_numeric(),-all_outcomes())


dx_pipe = prep(dx_pipe)

R_train = bake(dx_pipe, new_data = NULL)
R_test = bake(dx_pipe, new_data = Store_Test)

view(R_train)


vis_dat (R_train)


## creating train and test data set from oroginal train data 

set.seed(2)

R= sample(1:nrow(R_train),0.8*nrow(R_train))

t1= R_train[R,]       ##80% data
t2= R_train[-R,]      ##20% data



for_vif = lm(store ~ . -storecode_X__other__ -Areaname_X__other__ 
             -state_alpha_AR -state_alpha_CA -state_alpha_CO -state_alpha_CT 
             -state_alpha_FL -state_alpha_GA - state_alpha_IA -state_alpha_IA                                    
             -state_alpha_ID -state_alpha_IL -state_alpha_IN  -state_alpha_KS       
             -state_alpha_KY -state_alpha_LA -state_alpha_MA -state_alpha_ME 
             - state_alpha_MI - state_alpha_MN - state_alpha_MO - state_alpha_MS                                    
             - state_alpha_MT - state_alpha_NC - state_alpha_NE - state_alpha_NH -state_alpha_NY                           
             -state_alpha_OH -state_alpha_OK -state_alpha_PA -state_alpha_PR
             -state_alpha_SD -state_alpha_TN -state_alpha_TX -state_alpha_VA-state_alpha_VT -state_alpha_WI -state_alpha_WV -state_alpha_X__other__ -store_Type_X__other__
             -sales0 -CouSub_X__other__ -sales2 -sales3 -countyname_X__other__
             -sales1  -State_X23 , data = t1)
  
## checking for duplicate value through vif

sort(vif(for_vif), decreasing = TRUE)
  
## checking for column whos determinant value is 0
alias(for_vif)
## best fit model 
log_fit=stats::step(log_fit)

## fit model 
 
log_fit = glm (store ~ sales4 + State_X18 + State_X20 + State_X22 + State_X25 + 
                 State_X47 + State_X50 + State_X54 + State_X__other__ + countyname_Essex.County + 
                 countyname_Penobscot.County + countyname_Worcester.County + 
                 storecode_METRO , data = t1,
               family = "binomial")

summary(log_fit)



## performance in t2 with auc score 

val.score= predict(log_fit, newdata = t2, type = 'response')

caTools::colAUC(val.score, t2$store, plotROC = TRUE)

## checking same in train data 

train.score= predict(log_fit, newdata = t1, type = 'response')

caTools::colAUC(train.score, t1$store, plotROC = TRUE)  
  


## now fitting the model on the entire data 

for_vif = lm(store ~ sales4 + State_X18 + State_X20 + State_X22 + State_X25 + 
               State_X47 + State_X50 + State_X54 + State_X__other__ + countyname_Essex.County + 
               countyname_Penobscot.County + countyname_Worcester.County + 
               storecode_METRO , data = R_train)

summary(for_vif)


log_fit.final = glm(store ~ sales4 + State_X18 + State_X20 + State_X22 + State_X25 + 
                      State_X47 + State_X50 + State_X54 + State_X__other__ + countyname_Essex.County + 
                      countyname_Penobscot.County + countyname_Worcester.County + 
                      storecode_METRO , data = R_train, 
                      family= 'binomial')

summary(log_fit.final)



## final submit as probability 

test.prob.score = predict(log_fit.final, newdata = R_test, type = 'response')

write.csv(test.prob.score, 'Raja_Barman_P2_part2.csv', row.names = F)
 

