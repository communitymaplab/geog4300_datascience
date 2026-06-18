library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(pdp)

census_data<-readr::read_csv("https://github.com/jshannon75/geog4300/raw/refs/heads/master/data/ACSCtyData_2022ACS.csv") %>%
  select(cty_fips,BADeg_pct,intnet_all_pct,fb_pct,nat_ins_pct,wht_pop_pct,blk_pop_pct,asn_pop_pct,medinc) %>%
  na.omit(.)

train_index <- createDataPartition(census_data$BADeg_pct, p = 0.8, list = FALSE) 
train_data <- census_data[train_index, ]
test_data <- census_data[-train_index, ]

tree_model<-rpart(BADeg_pct~intnet_all_pct+fb_pct+nat_ins_pct+wht_pop_pct+blk_pop_pct+asn_pop_pct+medinc,
                  data=train_data,
                  method="anova")

rpart.plot(tree_model,box.palette="Blues",nn=TRUE)

lm_model<-(lm(BADeg_pct~intnet_all_pct+fb_pct+nat_ins_pct+wht_pop_pct+blk_pop_pct+asn_pop_pct+medinc,
           data=train_data))

lm_preds <- predict(lm_model, newdata = test_data)
R2(lm_preds, test_data$BADeg_pct)

RMSE(lm_preds, test_data$BADeg_pct)

printcp(tree_model)
plotcp(tree_model)

tree_preds <- predict(
  tree_model,
  newdata = test_data
)

RMSE(tree_preds, test_data$BADeg_pct)
R2(tree_preds, test_data$BADeg_pct)

library(randomForest)

rf <- randomForest(
  BADeg_pct~intnet_all_pct+fb_pct+nat_ins_pct+wht_pop_pct+blk_pop_pct+asn_pop_pct+medinc,
  data = train_data,
  importance = TRUE
)

importance(rf)
varImpPlot(rf)

rf_preds <- predict(rf, newdata = test_data)

RMSE(rf_preds, test_data$BADeg_pct)
R2(rf_preds, test_data$BADeg_pct)

partial(rf, pred.var = "medinc",plot=T)
partial(rf, pred.var = "intnet_all_pct",plot=T)
partial(rf, pred.var = "asn_pop_pct",plot=T)
partial(rf, pred.var = "blk_pop_pct")


plot(test_data$BADeg_pct, rf_preds)
abline(0,1,col="red")
