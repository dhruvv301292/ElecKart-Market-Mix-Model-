gaming_ecom<- ecom_data_cleaned[ecom_data_cleaned$product_analytic_sub_category == "GamingAccessory", ]

#creating dummy variables
game_data_cat = gaming_ecom[, c(9,12,32)]
game_data_cat = data.frame(lapply(game_data_cat, function(v) {
  if (is.character(v)) return(factor(v))
  else return(v)
}))

game_dummies = data.frame(sapply(game_data_cat, function(x)
  data.frame(model.matrix(~x-1, data = game_data_cat))[,-1]))

game_dummies = cbind(gaming_ecom$week, game_dummies)
colnames(game_dummies)[colnames(game_dummies)=="gaming_ecom$week"] <- "week"

#aggregating dummy variables
game_dummies_aggregate <- aggregate(.~week, game_dummies, sum, na.rm = TRUE)

gaming_ecom = gaming_ecom[ , -c(9,12,32)]

#aggregating main variables
gaming_ecom = gaming_ecom %>% group_by(week) %>% summarise(gmv = sum(gmv), units = sum(units), sla = mean(sla), product_mrp = sum(product_mrp), product_procurement_sla = mean(product_procurement_sla),
                                                           NPS=mean(NPS),Total_Investment = mean(Total_Investment), TV = mean(TV), Digital = mean(Digital), Sponsorship = mean(Sponsorship),
                                                           Content_Marketing = mean(Content_Marketing), Online_Marketing = mean(Online_Marketing), SEM = mean(SEM), Radio = mean(Radio), Other = mean(Other), TV_adstock= mean(TV_adstock),
                                                           Digital_adstock = mean(Digital_adstock), Content_Marketing_adstock = mean(Content_Marketing_adstock),
                                                           Online_Marketing_adstock = mean(Online_Marketing_adstock), SEM_adstock = mean(SEM_adstock), Radio_adstock = mean(Radio_adstock), Other_adstock = mean(Other_adstock), spec = sum(spec), prepaid = sum(prepaid), sellPrice = sum(sellPrice), PromoOffer = mean(PromoOffer), prepaidPerOrder = mean(prepaidPerOrder))



#merging aggregated main and dummy to produce final data set for modelling
gaming_ecom_final = merge(gaming_ecom, game_dummies_aggregate, by = c("week"), all.x = TRUE)
gaming_ecom_final = gaming_ecom_final[,-(which(colSums(gaming_ecom_final) == 0))]

################################################# For gaming - modeling ########################################################
game_cor = cor(gaming_ecom_final)
View(game_cor)

#removing highly correlated columns
gaming_ecom_final$product_mrp = NULL
gaming_ecom_final$sellPrice = NULL
gaming_ecom_final$units = NULL
gaming_ecom_final = gaming_ecom_final[, -c(15:22)]
gaming_ecom_final = gaming_ecom_final[, -c(19:33)]
gaming_ecom_final = gaming_ecom_final[, -c(19:24)]

#####linear model#####

#splitting into test and train
set.seed(100)
gaming_ecom_scale_final = gaming_ecom_final
gaming_ecom_scale_final[,2:ncol(gaming_ecom_scale_final)] = scale(gaming_ecom_scale_final[,2:ncol(gaming_ecom_scale_final)])
split<-createDataPartition(y = gaming_ecom_scale_final$gmv, p = 0.7, list = FALSE)
traindata<-gaming_ecom_scale_final[split,]
testdata<-gaming_ecom_scale_final[-split,]

#building models
model_1 = lm(gmv~., data=traindata)
summary(model_1)
vif(model_1)
step = stepAIC(model_1, direction = "both")

#removing specDay.xValentinesDaySale, specDay.xDausseraSale, specDay.xPacmanDaySale, PromoOffer and specDay.xRepublicDaySale on the basis of stepAIC
model_2 = lm(gmv ~ week + sla + specDay.xRegularSale + Radio + prepaidPerOrder + Digital + TV + Online_Marketing + Sponsorship + Total_Investment + Other + Content_Marketing + SEM + NPS + product_procurement_sla + specDay.xChristmasNewYearSale + specDay.xBigDiwaliSale + prepaid + spec, data = traindata)
summary(model_2)
vif(model_2)
#removing week due to high p-value

model_3 = lm(gmv ~ sla + specDay.xRegularSale + Radio + prepaidPerOrder + Digital + TV + Online_Marketing + Sponsorship + Total_Investment + Other + Content_Marketing + SEM + NPS + product_procurement_sla + specDay.xChristmasNewYearSale + specDay.xBigDiwaliSale + prepaid + spec, data = traindata)
summary(model_3)
vif(model_3)

#removing sla due to high p-value

model_4 = lm(gmv ~ specDay.xRegularSale + Radio + prepaidPerOrder + Digital + TV + Online_Marketing + Sponsorship + Total_Investment + Other + Content_Marketing + SEM + NPS + product_procurement_sla + specDay.xChristmasNewYearSale + specDay.xBigDiwaliSale + prepaid + spec, data = traindata)
summary(model_4)
vif(model_4)
#removing Total_Investment due to high vif

model_5 = lm(gmv ~ specDay.xRegularSale + Radio + prepaidPerOrder + Digital + TV + Online_Marketing + Sponsorship + Other + Content_Marketing + SEM + NPS + product_procurement_sla + specDay.xChristmasNewYearSale + specDay.xBigDiwaliSale + prepaid + spec, data = traindata)
summary(model_5)
vif(model_5)
#removing content_marketing due to high p-value and high vif

model_6 = lm(gmv ~ specDay.xRegularSale + Radio + prepaidPerOrder + Digital + TV + Online_Marketing + Sponsorship + Other + SEM + NPS + product_procurement_sla + specDay.xChristmasNewYearSale + specDay.xBigDiwaliSale + prepaid + spec, data = traindata)
summary(model_6)
vif(model_6)
#removing TV due to high p-value

model_7 = lm(gmv ~ specDay.xRegularSale + Radio + prepaidPerOrder + Digital + Online_Marketing + Sponsorship + Other + SEM + NPS + product_procurement_sla + specDay.xChristmasNewYearSale + specDay.xBigDiwaliSale + prepaid + spec, data = traindata)
summary(model_7)
vif(model_7)
#removing nps due to high p-value

model_8 = lm(gmv ~ specDay.xRegularSale + Radio + prepaidPerOrder + Digital + Online_Marketing + Sponsorship + Other + SEM + product_procurement_sla + specDay.xChristmasNewYearSale + specDay.xBigDiwaliSale + prepaid + spec, data = traindata)
summary(model_8)
vif(model_8)
#removing specDay.xChristmasNewYearSale due to high p-value

model_9 = lm(gmv ~ specDay.xRegularSale + Radio + prepaidPerOrder + Digital + Online_Marketing + Sponsorship + Other + SEM + product_procurement_sla + specDay.xBigDiwaliSale + prepaid + spec, data = traindata)
summary(model_9)
vif(model_9)
##removing prepaidPerOrder due to high p-value

model_10 = lm(gmv ~ specDay.xRegularSale + Radio + Digital + Online_Marketing + Sponsorship + Other + SEM + product_procurement_sla + specDay.xBigDiwaliSale + prepaid + spec, data = traindata)
summary(model_10)
vif(model_10)
##removing specDay.xBigDiwaliSale due to high p-value

model_11 = lm(gmv ~ specDay.xRegularSale + Radio + Digital + Online_Marketing + Sponsorship + Other + SEM + product_procurement_sla + prepaid + spec, data = traindata)
summary(model_11)
vif(model_11)
#removing Digital due to high p-value

model_12 = lm(gmv ~ specDay.xRegularSale + Radio + Online_Marketing + Sponsorship + Other + SEM + product_procurement_sla + prepaid + spec, data = traindata)
summary(model_12)
vif(model_12)
#removing Other due to high p-value and vif

model_13 = lm(gmv ~ specDay.xRegularSale + Radio + Online_Marketing + Sponsorship + SEM + product_procurement_sla + prepaid + spec, data = traindata)
summary(model_13)
vif(model_13)

#testing on test data
testdata$testgmv = predict(model_13,testdata)
Rsq_gaming = cor(testdata$gmv,testdata$testgmv)^2
Rsq_gaming #0.93 R-squared on test data

#Cross validation on train data with final model:
cv_gaming = train(gmv ~ specDay.xRegularSale + Radio + Online_Marketing + Sponsorship + SEM + product_procurement_sla + prepaid + spec, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_gaming,testdata)
cv_gaming$results   #Rsq = 0.972
Rsq_cv_gaming = cor(testdata$gmv,testdata$testgmv_cv)^2  #
Rsq_cv_gaming #Rsq on CV = 0.9315


#####multiplicative model######

gaming_ecom_mult_final = gaming_ecom_final
gaming_ecom_mult_final[gaming_ecom_mult_final == 0] = 0.00001
gaming_ecom_mult_final=log(gaming_ecom_mult_final)

split<-createDataPartition(y = gaming_ecom_mult_final$gmv, p = 0.7, list = FALSE)
traindata<-gaming_ecom_mult_final[split,]
testdata<-gaming_ecom_mult_final[-split,]

#building models

model_1 = lm(gmv~., data=traindata)
summary(model_1)
vif(model_1)
step = stepAIC(model_1, direction = "both")
#removing sla, Radio, Other and specDay.xBSDSale on the basis of AIC

model_2 = lm(gmv ~ NPS + specDay.xBigDiwaliSale + specDay.xFHSDSale + Sponsorship + product_procurement_sla + week + PromoOffer + specDay.xRepublicDaySale + spec + specDay.xPacmanDaySale + Digital + SEM + specDay.xRegularSale + TV + Content_Marketing + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_2)
vif(model_2)
#removing NPS due to high p-value and high vif

model_3 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xFHSDSale + Sponsorship + product_procurement_sla + week + PromoOffer + specDay.xRepublicDaySale + spec + specDay.xPacmanDaySale + Digital + SEM + specDay.xRegularSale + TV + Content_Marketing + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_3)
vif(model_3)
#removing TV due to high p-value and vif

model_4 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xFHSDSale + Sponsorship + product_procurement_sla + week + PromoOffer + specDay.xRepublicDaySale + spec + specDay.xPacmanDaySale + Digital + SEM + specDay.xRegularSale + Content_Marketing + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_4)
vif(model_4)
#removing specDay.xPacmanDaySale due to high p-value

model_5 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xFHSDSale + Sponsorship + product_procurement_sla + week + PromoOffer + specDay.xRepublicDaySale + spec + Digital + SEM + specDay.xRegularSale + Content_Marketing + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_5)
vif(model_5)
# removing Content_Marketing due to high p-value and vif

model_6 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xFHSDSale + Sponsorship + product_procurement_sla + week + PromoOffer + specDay.xRepublicDaySale + spec + Digital + SEM + specDay.xRegularSale + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_6)
vif(model_6)
#removing spec due to high p-value

model_7 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xFHSDSale + Sponsorship + product_procurement_sla + week + PromoOffer + specDay.xRepublicDaySale + Digital + SEM + specDay.xRegularSale + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_7)
vif(model_7)
#removing specDay.xRegularSale due to high p-value

model_8 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xFHSDSale + Sponsorship + product_procurement_sla + week + PromoOffer + specDay.xRepublicDaySale + Digital + SEM + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_8)
vif(model_8)
#removing product_procurement_sla due to high p-value

model_9 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xFHSDSale + Sponsorship + week + PromoOffer + specDay.xRepublicDaySale + Digital + SEM + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_9)
vif(model_9)
#removing specDay.xRepublicDaySale due to high p-value

model_10 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xFHSDSale + Sponsorship + week + PromoOffer + Digital + SEM + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_10)
vif(model_10)
#removing Sponsorship due to high p-value

model_11 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xFHSDSale + week + PromoOffer + Digital + SEM + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_11)
vif(model_11)
#removing specDay.xFHSDSale due to high p-value

model_12 = lm(gmv ~ specDay.xBigDiwaliSale + week + PromoOffer + Digital + SEM + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, data=traindata)
summary(model_12)
vif(model_12)

#testing on test data
testdata$testgmv = predict(model_12,testdata)
Rsq_gaming = cor(testdata$gmv,testdata$testgmv)^2
Rsq_gaming #0.99 R-squared on test data

#Cross validation on train data with final model:
cv_gaming = train(gmv ~ specDay.xBigDiwaliSale + week + PromoOffer + Digital + SEM + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_gaming,testdata)
cv_gaming$results   #Rsq = 0.91
Rsq_cv_gaming = cor(testdata$gmv,testdata$testgmv_cv)^2  #
Rsq_cv_gaming #Rsq on CV = 0.99

#####distributive lag model######

gaming_lag_final = advanced_kpi(gaming_ecom_final) 
gaming_lag_scaled_final = gaming_lag_final
gaming_lag_scaled_final[,2:ncol(gaming_lag_scaled_final)] = scale(gaming_lag_scaled_final[,2:ncol(gaming_lag_scaled_final)])
gaming_lag_scaled_final$week = NULL

split<-createDataPartition(y = gaming_lag_scaled_final$gmv, p = 0.8, list = FALSE)
traindata<-gaming_lag_scaled_final[split,]
testdata<-gaming_lag_scaled_final[-split,]

#building models
model_1 = lm(gmv~., data=traindata)
summary(model_1)
vif(model_1)
step = stepAIC(model_1, direction = "both")
#removing specDay.xRepublicDaySale, specDay.xRegularSale and GMV_lag_1_per due to stepAIC

model_2 = lm(gmv ~ product_procurement_sla + specDay.xBigDiwaliSale + specDay.xDausseraSale + specDay.xBSDSale + GMV_lag_3_per + spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xValentineDaySale + specDay.xChristmasNewYearSale + sla + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + Total_Investment + TV + Digital + Content_Marketing + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_2)
vif(model_2)
#removing product_procurement_sla due to high p-value

model_3 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xDausseraSale + specDay.xBSDSale + GMV_lag_3_per + spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xValentineDaySale + specDay.xChristmasNewYearSale + sla + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + Total_Investment + TV + Digital + Content_Marketing + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_3)
vif(model_3)
#removing specDay.xDausseraSale due to high p-value

model_4 = lm(gmv ~ specDay.xBigDiwaliSale + specDay.xBSDSale + GMV_lag_3_per + spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xValentineDaySale + specDay.xChristmasNewYearSale + sla + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + Total_Investment + TV + Digital + Content_Marketing + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_4)
vif(model_4)
#removing specDay.xBigDiwaliSale due to high p-value

model_5 = lm(gmv ~ specDay.xBSDSale + GMV_lag_3_per + spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xValentineDaySale + specDay.xChristmasNewYearSale + sla + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + Total_Investment + TV + Digital + Content_Marketing + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_5)
vif(model_5)
#removing specDay.xBSDSale due to high p-value

model_6 = lm(gmv ~ GMV_lag_3_per + spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xValentineDaySale + specDay.xChristmasNewYearSale + sla + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + Total_Investment + TV + Digital + Content_Marketing + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_6)
vif(model_6)
#removing Total_Investment due to very high vif

model_7 = lm(gmv ~ GMV_lag_3_per + spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xValentineDaySale + specDay.xChristmasNewYearSale + sla + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + TV + Digital + Content_Marketing + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_7)
vif(model_7)
#removing GMV_lag_3_per due to very high p-value

model_8 = lm(gmv ~ spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xValentineDaySale + specDay.xChristmasNewYearSale + sla + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + TV + Digital + Content_Marketing + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_8)
vif(model_8)
#removing content_marketing due to high p-value

model_9 = lm(gmv ~ spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xValentineDaySale + specDay.xChristmasNewYearSale + sla + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + TV + Digital + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_9)
vif(model_9)
#removing sla due to high p-value

model_10 = lm(gmv ~ spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xValentineDaySale + specDay.xChristmasNewYearSale + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + TV + Digital + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_10)
vif(model_10)
#removing sla due to high p-value

model_11 = lm(gmv ~ spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xChristmasNewYearSale + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + TV + Digital + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_11)
vif(model_11)
#removing TV due to high p-value

model_12 = lm(gmv ~ spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xChristmasNewYearSale + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + Digital + Radio + prepaidPerOrder + prepaid, data = traindata)
summary(model_12)
vif(model_12)
#removing Radio due to high p-value

model_13 = lm(gmv ~ spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xChristmasNewYearSale + PromoOffer + NPS + SEM + Other + Online_Marketing + Sponsorship + Digital + prepaidPerOrder + prepaid, data = traindata)
summary(model_13)
vif(model_13)
#removing SEM due to high p-value

model_14 = lm(gmv ~ spec + specDay.xFHSDSale + GMV_lag_2_per + specDay.xChristmasNewYearSale + PromoOffer + NPS + Other + Online_Marketing + Sponsorship + Digital + prepaidPerOrder + prepaid, data = traindata)
summary(model_14)
vif(model_14)
#removing GMV_lag_2_per due to high p-value

model_15 = lm(gmv ~ spec + specDay.xFHSDSale + specDay.xChristmasNewYearSale + PromoOffer + NPS + Other + Online_Marketing + Sponsorship + Digital + prepaidPerOrder + prepaid, data = traindata)
summary(model_15)
vif(model_15)
#removing specDay.xFHSDSale due to high p-value

model_16 = lm(gmv ~ spec + specDay.xFHSDSale + specDay.xChristmasNewYearSale + PromoOffer + NPS + Other + Online_Marketing + Sponsorship + Digital + prepaidPerOrder + prepaid, data = traindata)
summary(model_16)
vif(model_16)
#removing specDay.xFHSDSale due to high p-value

model_17 = lm(gmv ~ spec + specDay.xChristmasNewYearSale + PromoOffer + NPS + Other + Online_Marketing + Sponsorship + Digital + prepaidPerOrder + prepaid, data = traindata)
summary(model_17)
vif(model_17)

#testing on test data
testdata$testgmv = predict(model_17,testdata)
Rsq_gaming = cor(testdata$gmv,testdata$testgmv)^2
Rsq_gaming #0.99 R-squared on test data

#Cross validation on train data with final model:
cv_gaming = train(gmv ~ specDay.xBigDiwaliSale + week + PromoOffer + Digital + SEM + Online_Marketing + Total_Investment + prepaidPerOrder + prepaid, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_gaming,testdata)
cv_gaming$results   #Rsq = 0.91
Rsq_cv_gaming = cor(testdata$gmv,testdata$testgmv_cv)^2
Rsq_cv_gaming #Rsq on CV = 0.99
