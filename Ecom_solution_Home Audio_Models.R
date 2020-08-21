
########################For home audio##########################
homeaudio_ecom <- ecom_data_cleaned[ecom_data_cleaned$product_analytic_sub_category == "HomeAudio", ]
#homeaudio_ecom$product_analytic_sub_category = NULL
#homeaudio_ecom$product_analytic_vertical = NULL
#creating dummy variables
ha_data_cat = homeaudio_ecom[, c(9,12,32)]
ha_data_cat = data.frame(lapply(ha_data_cat, function(v) {
  if (is.character(v)) return(factor(v))
  else return(v)
}))

ha_dummies = data.frame(sapply(ha_data_cat, function(x)
  data.frame(model.matrix(~x-1, data = ha_data_cat))[,-1]))

ha_dummies = cbind(homeaudio_ecom$week, ha_dummies)
colnames(ha_dummies)[colnames(ha_dummies)=="homeaudio_ecom$week"] <- "week"

#aggregating dummy variables
ha_dummies_aggregate <- aggregate(.~week, ha_dummies, sum, na.rm = TRUE)

homeaudio_ecom = homeaudio_ecom[ , -c(9,12,32)]

#aggregating main variables
homeaudio_ecom = homeaudio_ecom %>% group_by(week) %>% summarise(gmv = sum(gmv), units = sum(units), sla = mean(sla), product_mrp = sum(product_mrp), product_procurement_sla = mean(product_procurement_sla),
                                                                 NPS=mean(NPS),Total_Investment = mean(Total_Investment), TV = mean(TV), Digital = mean(Digital), Sponsorship = mean(Sponsorship),
                                                                 Content_Marketing = mean(Content_Marketing), Online_Marketing = mean(Online_Marketing), SEM = mean(SEM), Radio = mean(Radio), Other = mean(Other), TV_adstock= mean(TV_adstock),
                                                                 Digital_adstock = mean(Digital_adstock), Sponsorship_adstock = mean(Sponsorship_adstock), Content_Marketing_adstock = mean(Content_Marketing_adstock),
                                                                 Online_Marketing_adstock = mean(Online_Marketing_adstock), SEM_adstock = mean(SEM_adstock), Radio_adstock = mean(Radio_adstock), Other_adstock = mean(Other_adstock), spec = sum(spec), prepaid = sum(prepaid), sellPrice = sum(sellPrice), PromoOffer = mean(PromoOffer), prepaidPerOrder = mean(prepaidPerOrder))

#merging aggregated main and dummy to produce final data set for modelling
homeaudio_ecom_final = merge(homeaudio_ecom, ha_dummies_aggregate, by = c("week"), all.x = TRUE)
homeaudio_ecom_final = homeaudio_ecom_final[,-(which(colSums(homeaudio_ecom_final) == 0))]

############################ MODELING Home Audio #############################################
#####################################Linear Model###################################
#checking for high linear relation
homeaudio_temp <- homeaudio_ecom_final
homeaudio_ecom_final<-homeaudio_temp
homeaudio_cor = cor(homeaudio_ecom_final)
summary(homeaudio_ecom_final)
View(homeaudio_cor)
#deleting variables with high correlation to gmv
homeaudio_ecom_final$product_mrp = NULL
homeaudio_ecom_final$sellPrice = NULL
homeaudio_ecom_final$units = NULL
homeaudio_ecom_final$specDay.xDausseraSale = NULL
homeaudio_ecom_final = homeaudio_ecom_final[, -c(15:22)]
homeaudio_ecom_final = homeaudio_ecom_final[, -c(19:34)]
homeaudio_ecom_final[,2:ncol(homeaudio_ecom_final)] = scale(homeaudio_ecom_final[,2:ncol(homeaudio_ecom_final)])


set.seed(100)
split<-createDataPartition(y = homeaudio_ecom_final$gmv, p = 0.7, list = FALSE)
traindata<-homeaudio_ecom_final[split,]
testdata<-homeaudio_ecom_final[-split,]
model_1 = lm(gmv~., data= traindata)
summary(model_1)
vif(model_1)
#-BSDSale, Christmasnewyearsale, Republicdaysale for high p value

model2 <- lm(formula =  gmv ~  sla + product_procurement_sla + week + Sponsorship + 
               NPS + Total_Investment + TV + Digital + Content_Marketing + 
               Online_Marketing + SEM + Radio + Other+ spec  + 
               prepaid + PromoOffer+ prepaidPerOrder+weekday.xWednesday+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+specDay.xFHSDSale+specDay.xPacmanDaySale+specDay.xRegularSale+specDay.xValentineDaySale, data = traindata)


summary(model2)
vif(model2)

#-Digital

model3 <- lm(formula =  gmv ~  sla + product_procurement_sla + week + Sponsorship + 
               NPS + Total_Investment + TV+ Content_Marketing + 
               Online_Marketing + SEM + Radio + Other+ spec  + 
               prepaid + PromoOffer+ prepaidPerOrder+weekday.xWednesday+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+specDay.xFHSDSale+specDay.xPacmanDaySale+specDay.xRegularSale+specDay.xValentineDaySale, data = traindata)



summary(model3)
vif(model3)

# - NPS and Product Procurement sla         
model4 <- lm(formula =  gmv ~  sla + week + Sponsorship  + Total_Investment + TV+ Content_Marketing + 
               Online_Marketing + SEM + Radio + Other+ spec  + 
               prepaid + PromoOffer+ prepaidPerOrder+weekday.xWednesday+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+specDay.xFHSDSale+specDay.xPacmanDaySale+specDay.xRegularSale+specDay.xValentineDaySale, data = traindata)


summary(model4)
vif(model4)

# - Weekday.wednesday , FHSDSale and PacmanDaySale
model5 <- lm(formula =  gmv ~  sla + week + Sponsorship  + Total_Investment + TV+ Content_Marketing + 
               Online_Marketing + SEM + Radio + Other+ spec  + 
               prepaid + PromoOffer+ prepaidPerOrder+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+specDay.xRegularSale+specDay.xValentineDaySale, data = traindata)


summary(model5)
vif(model5)

#removing prepaid
model6 <- lm(formula =  gmv ~  sla + week + Sponsorship  + Total_Investment + TV+ Content_Marketing + 
               Online_Marketing + SEM + Radio + Other+ spec
               + PromoOffer+ prepaidPerOrder+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+specDay.xRegularSale+specDay.xValentineDaySale, data = traindata)



summary(model6)
vif(model6)

#removing ValentinedaySale
model7 <- lm(formula =  gmv ~  sla + week + Sponsorship  + Total_Investment + TV+ Content_Marketing + 
               Online_Marketing + SEM + Radio + Other+ spec
             + PromoOffer+ prepaidPerOrder+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+specDay.xRegularSale, data = traindata)



summary(model7)
vif(model7)

#removing Total Investment due to high VIF
model8 <- lm(formula =  gmv ~  sla + week + Sponsorship + TV+ Content_Marketing + 
               Online_Marketing + SEM + Radio + Other+ spec
             + PromoOffer+ prepaidPerOrder+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+specDay.xRegularSale, data = traindata)


summary(model8)
vif(model8)

#removing Other  and Prepaidperorder
model9 <- lm(formula =  gmv ~  sla + week + Sponsorship + TV+ Content_Marketing + 
               Online_Marketing + SEM + Radio +spec
             + PromoOffer+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+specDay.xRegularSale, data = traindata)



summary(model9)
vif(model9)

#removing week and TV
model10 <- lm(formula =  gmv ~  sla  + Sponsorship + Content_Marketing + 
               Online_Marketing + SEM + Radio +spec
             + PromoOffer+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+specDay.xRegularSale, data = traindata)

summary(model10)
vif(model10)

#removing EIDRathyatra Sale
model11 <- lm(formula =  gmv ~  sla  + Sponsorship + Content_Marketing + 
                Online_Marketing + SEM + Radio +spec
              + PromoOffer+specDay.xBigDiwaliSale+specDay.xRegularSale, data = traindata)

summary(model11)
vif(model11)


#removing radio
model12 <- lm(formula =  gmv ~  sla  +Sponsorship + Content_Marketing + 
                Online_Marketing + SEM +spec
              + PromoOffer+specDay.xBigDiwaliSale+specDay.xRegularSale, data = traindata)


summary(model12)
vif(model12)


#removing Online Marketing
model13 <- lm(formula =  gmv ~  sla  +Sponsorship + Content_Marketing 
               + SEM +spec
              + PromoOffer+specDay.xBigDiwaliSale+specDay.xRegularSale, data = traindata)



summary(model13)
vif(model13)

#testing on test data
testdata$testgmv = predict(model13,testdata)
Rsq_homeaudio = cor(testdata$gmv,testdata$testgmv)^2
Rsq_homeaudio #0.989 R-squared on test data


crossVal <- cv.lm(data=traindata, form.lm =formula(gmv ~  sla  +Sponsorship + Content_Marketing 
                                                   + SEM +spec
                                                   + PromoOffer+specDay.xBigDiwaliSale+specDay.xRegularSale), m=10, dots=FALSE, seed=29, plotit = TRUE,printit = TRUE )


######For multiplicative model######
####we have to remove the scale method while computing homeaudio_final. For linear modelling compute the scaling right before doing the modelling. I have done the same above


homeaudio_ecom_final<-homeaudio_temp
homeaudio_cor = cor(homeaudio_ecom_final)
summary(homeaudio_ecom_final)
View(homeaudio_cor)
#deleting variables with high correlation to gmv
homeaudio_ecom_final$product_mrp = NULL
homeaudio_ecom_final$sellPrice = NULL
homeaudio_ecom_final$units = NULL
homeaudio_ecom_final$specDay.xDausseraSale = NULL
homeaudio_ecom_final = homeaudio_ecom_final[, -c(15:22)]
homeaudio_ecom_final = homeaudio_ecom_final[, -c(19:34)]

homeaudio_ecom_mult_final = homeaudio_ecom_final
homeaudio_ecom_mult_final[homeaudio_ecom_mult_final == 0] = 0.00001
homeaudio_ecom_mult_final<-log(homeaudio_ecom_mult_final)
sum(is.na(homeaudio_ecom_mult_final))

set.seed(100)
split<-createDataPartition(y = homeaudio_ecom_mult_final$gmv, p = 0.7, list = FALSE)
traindata<-homeaudio_ecom_mult_final[split,]
testdata<-homeaudio_ecom_mult_final[-split,]
sum(is.na(testdata))
model_1 = lm(gmv~., data=traindata)

summary(model_1)
vif(model_1)

step = stepAIC(model_1, direction = "both")
# Removing Valentinesday sale,SEM, NPS, Product procurement sla, FHSD sale, week, Eidrathyatra sale, Digital,Online marketing, BSD sale, Diwalisale, Pacmanday sale on basis of AIC

model_2 = lm(gmv~ specDay.xRepublicDaySale+weekday.xWednesday+ specDay.xChristmasNewYearSale+specDay.xRegularSale+spec+Content_Marketing+Other+Radio+Sponsorship+sla+PromoOffer+TV+Total_Investment+prepaid+prepaidPerOrder, data=traindata )
summary(model_2)
vif(model_2)

#Removing Radio and Other for high VIF value

model_3 = lm(gmv~ specDay.xRepublicDaySale+weekday.xWednesday+ specDay.xChristmasNewYearSale+specDay.xRegularSale+spec+Content_Marketing+Sponsorship+sla+PromoOffer+TV+Total_Investment+prepaid+prepaidPerOrder, data=traindata )
summary(model_3)
vif(model_3)

#Removing Sponsorship and Regular Sale for high VIF and p value

model_4 = lm(gmv~ specDay.xRepublicDaySale+weekday.xWednesday+ specDay.xChristmasNewYearSale+spec+Content_Marketing+sla+PromoOffer+TV+Total_Investment+prepaid+prepaidPerOrder, data=traindata )
summary(model_4)
vif(model_4)

#Removing Weekday Wednesday and Republic day Sale for high p value

model_5 = lm(gmv~ specDay.xChristmasNewYearSale+spec+Content_Marketing+sla+PromoOffer+TV+Total_Investment+prepaid+prepaidPerOrder, data=traindata )
summary(model_5)
vif(model_5)

#Removing TV for high p value

model_6 = lm(gmv~ specDay.xChristmasNewYearSale+spec+Content_Marketing+sla+PromoOffer+Total_Investment+prepaid+prepaidPerOrder, data=traindata )
summary(model_6)
vif(model_6)

#Removing Christmas sale for high p value

model_7 = lm(gmv~ spec+Content_Marketing+sla+PromoOffer+Total_Investment+prepaid+prepaidPerOrder, data=traindata )
summary(model_7)
vif(model_7)

#testing on test data
testdata$testgmv = predict(model_7,testdata)
Rsq_homeaudio = cor(testdata$gmv,testdata$testgmv)^2
Rsq_homeaudio #0.958 R-squared on test data

#Cross validation on train data with final model:
cv_homeaudio= train(gmv ~ spec+Content_Marketing+sla+PromoOffer+Total_Investment+prepaid+prepaidPerOrder, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_homeaudio,testdata)
cv_homeaudio$results   #Rsq = 0.935
Rsq_cv_homeaudio = cor(testdata$gmv,testdata$testgmv_cv)^2  #
Rsq_cv_homeaudio #Rsq on CV = 0.959



##########for distributive lag models###############
advanced_kpi<-function(data){
  
  # Lag Variables for gmv
  data <- data[with(data, order(week)),] 
  
  #Lag gmv (different period lags) [Lag of gmv by 1st week, 2nd week, 3rd week]
  data_dum <- slide(data, Var = "gmv", slideBy = -1)
  data_dum <- slide(data_dum, Var = "gmv", slideBy = -2)
  data_dum <- slide(data_dum, Var = "gmv", slideBy = -3)
  
  data <- data_dum
  
  
  col1<-c("gmv-1")
  col2<-c("gmv-2")
  col3<-c("gmv-3")
  data[, col1] <- imputeTS::na.ma(data[, col1], k=1, weighting = "simple")
  data[, col2] <- imputeTS::na.ma(data[, col2], k=2, weighting = "simple")
  data[, col3] <- imputeTS::na.ma(data[, col3], k=3, weighting = "simple")
  ifelse(is.na(data$PO_lag_3_per),0,data$PO_lag_3_per)
  
  #Incremental Lags of gmv by 1 week, 2 week, 3 week
  data$GMV_lag_1_per <- (data$gmv - data$`gmv-1`)/data$`gmv-1`
  data$GMV_lag_2_per <- (data$gmv - data$`gmv-2`)/data$`gmv-2`
  data$GMV_lag_3_per <- (data$gmv - data$`gmv-3`)/data$`gmv-3`
  
  data$GMV_lag_1_per <- ifelse(is.na(data$GMV_lag_1_per),0,data$GMV_lag_1_per)
  data$GMV_lag_2_per <- ifelse(is.na(data$GMV_lag_2_per),0,data$GMV_lag_2_per)
  data$GMV_lag_3_per <- ifelse(is.na(data$GMV_lag_3_per),0,data$GMV_lag_3_per)
  
  #Removing the columns
  
  data$`gmv-1` <- NULL
  data$`gmv-2` <- NULL
  data$`gmv-3` <- NULL
  
  return(data)
  
}

######for distributed lag model######
homeaudio_ecom_final= homeaudio_temp
homeaudio_ecom_lag_final= homeaudio_ecom_final
homeaudio_ecom_lag_final = advanced_kpi(homeaudio_ecom_lag_final)
homeaudio_ecom_lag_final[,2:ncol(homeaudio_ecom_lag_final)] = scale(homeaudio_ecom_lag_final[,2:ncol(homeaudio_ecom_lag_final)])
homeaudio_ecom_lag_final$week = NULL

split<-createDataPartition(y = homeaudio_ecom_lag_final$gmv, p = 0.8, list = FALSE)
traindata<-homeaudio_ecom_lag_final[split,]
testdata<-homeaudio_ecom_lag_final[-split,]

model_1 = lm(gmv~., data=traindata)
summary(model_1)
vif(model_1)
step = stepAIC(model_1, direction = "both")

#removing specDay.xBSDSale, specDay.FHSDSAle, GMV lag_2_per, product_procurement_sla, SEM, secdayRepublic Day, wekday.Wednesday, prepaidPerorder, specday.Valentinesday, GMV_lag_1, specday.PacmanDay, TV due to stepAIC

model_2 = lm(gmv ~ Content_Marketing+specDay.xEidRathayatraSale+GMV_lag_3_per+specDay.xChristmasNewYearSale +NPS+Digital+prepaid+PromoOffer+sla+Total_Investment+Online_Marketing+Sponsorship+specDay.xBigDiwaliSale+Other+Radio+specDay.xRegularSale+spec , data = traindata)
summary(model_2)
vif(model_2)

#Removing EidRathyatra Sale and GMV_lag_3_per due to high VIF and p value

model_3 = lm(gmv ~ Content_Marketing+specDay.xChristmasNewYearSale +NPS+Digital+prepaid+PromoOffer+sla+Total_Investment+Online_Marketing+Sponsorship+specDay.xBigDiwaliSale+Other+Radio+specDay.xRegularSale+spec , data = traindata)
summary(model_3)
vif(model_3)

#Removing Content marketing for high vif and p value

model_4 = lm(gmv ~ specDay.xChristmasNewYearSale +NPS+Digital+prepaid+PromoOffer+sla+Total_Investment+Online_Marketing+Sponsorship+specDay.xBigDiwaliSale+Other+Radio+specDay.xRegularSale+spec , data = traindata)
summary(model_4)
vif(model_4)

#Removing Christmasdaynewyear sale for high vif and p value

model_5 = lm(gmv ~ NPS+Digital+prepaid+PromoOffer+sla+Total_Investment+Online_Marketing+Sponsorship+specDay.xBigDiwaliSale+Other+Radio+specDay.xRegularSale+spec , data = traindata)
summary(model_5)
vif(model_5)

#Removing BigDiwalisale sale for high p value

model_6 = lm(gmv ~ NPS+Digital+prepaid+PromoOffer+sla+Total_Investment+Online_Marketing+Sponsorship+Other+Radio+specDay.xRegularSale+spec , data = traindata)
summary(model_6)
vif(model_6)

#Removing Digital sale for high p value and vif

model_7 = lm(gmv ~ NPS+prepaid+PromoOffer+sla+Total_Investment+Online_Marketing+Sponsorship+Other+Radio+specDay.xRegularSale+spec , data = traindata)
summary(model_7)
vif(model_7)

#Removing prepaid for high p value
model_8 = lm(gmv ~ NPS+PromoOffer+sla+Total_Investment+Online_Marketing+Sponsorship+Other+Radio+specDay.xRegularSale+spec , data = traindata)
summary(model_8)
vif(model_8)

#Removing Radio for high p value and vif
model_9 = lm(gmv ~ NPS+PromoOffer+sla+Total_Investment+Online_Marketing+Sponsorship+Other+specDay.xRegularSale+spec , data = traindata)
summary(model_9)
vif(model_9)

#Removing NPS for high p value

model_10 = lm(gmv ~ PromoOffer+sla+Total_Investment+Online_Marketing+Sponsorship+Other+specDay.xRegularSale+spec , data = traindata)
summary(model_10)
vif(model_10)

#Removing Promooffer due high p value
model_11 = lm(gmv ~ sla+Total_Investment+Online_Marketing+Sponsorship+Other+specDay.xRegularSale+spec , data = traindata)
summary(model_11)
vif(model_11)

#testing on test data
testdata$testgmv = predict(model_11,testdata)
Rsq_homeaudio = cor(testdata$gmv,testdata$testgmv)^2
Rsq_homeaudio #0.988 R-squared on test data

#Cross validation on train data with final model:
cv_homeaudio = train(gmv ~ sla+Total_Investment+Online_Marketing+Sponsorship+Other+specDay.xRegularSale+spec, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_homeaudio,testdata)
cv_homeaudio$results   #Rsq = 0.981
Rsq_cv_homeaudio = cor(testdata$gmv,testdata$testgmv_cv)^2
Rsq_cv_homeaudio #Rsq on CV = 0.988

################################################   KOYK MODEL   ###############################################################

homeaudio_koyk_final = advanced_kpi(homeaudio_ecom_final) 
homeaudio_koyk_scaled_final = homeaudio_koyk_final
homeaudio_koyk_scaled_final[,2:ncol(homeaudio_koyk_scaled_final)] = scale(homeaudio_koyk_scaled_final[,2:ncol(homeaudio_koyk_scaled_final)])
homeaudio_koyk_scaled_final$GMV_lag_2_per = NULL
homeaudio_koyk_scaled_final$GMV_lag_3_per = NULL

split<-createDataPartition(y = homeaudio_koyk_scaled_final$gmv, p = 0.7, list = FALSE)
traindata<-homeaudio_koyk_scaled_final[split,]
testdata<-homeaudio_koyk_scaled_final[-split,]

model_1 = lm(gmv~., data=traindata)
summary(model_1)
vif(model_1)
step = stepAIC(model_1, direction = "both")

#Removing EIDRathyatra sale, Valentinesday sale, christmasnewyear sale,pacmanday sale, BSDsale on step AIC

model_2 = lm(gmv~ specDay.xRegularSale+spec+SEM+Other+sla+Total_Investment+Online_Marketing+Sponsorship+Radio+prepaid+prepaidPerOrder+NPS+weekday.xWednesday+Digital+week+TV+PromoOffer+specDay.xRepublicDaySale+product_procurement_sla+specDay.xBigDiwaliSale+specDay.xFHSDSale+Content_Marketing+GMV_lag_1_per, data= traindata)
summary(model_2)
vif(model_2)

#Removing Big DiwaliSale and FHSD Sale on high p value
model_2 = lm(gmv~ specDay.xRegularSale+spec+SEM+Other+sla+Total_Investment+Online_Marketing+Sponsorship+Radio+prepaid+prepaidPerOrder+NPS+weekday.xWednesday+Digital+week+TV+PromoOffer+specDay.xRepublicDaySale+product_procurement_sla+Content_Marketing+GMV_lag_1_per, data= traindata)
summary(model_2)
vif(model_2)

#Removing GMV_lag_1, content marketing, Promooffer for high p value
model_3 = lm(gmv~ specDay.xRegularSale+spec+SEM+Other+sla+Total_Investment+Online_Marketing+Sponsorship+Radio+prepaid+prepaidPerOrder+NPS+weekday.xWednesday+Digital+week+TV+specDay.xRepublicDaySale+product_procurement_sla, data= traindata)
summary(model_3)
vif(model_3)

#Removing Product Procurement sla for high p value
model_4 = lm(gmv~ specDay.xRegularSale+spec+SEM+Other+sla+Total_Investment+Online_Marketing+Sponsorship+Radio+prepaid+prepaidPerOrder+NPS+weekday.xWednesday+Digital+week+TV+specDay.xRepublicDaySale, data= traindata)
summary(model_4)
vif(model_4)

#Removing TV and week for high p value
model_5 = lm(gmv~ specDay.xRegularSale+spec+SEM+Other+sla+Total_Investment+Online_Marketing+Sponsorship+Radio+prepaid+prepaidPerOrder+NPS+weekday.xWednesday+Digital+specDay.xRepublicDaySale, data= traindata)
summary(model_5)
vif(model_5)

#Removing Weekday Wednesday for high p value
model_6 = lm(gmv~ specDay.xRegularSale+spec+SEM+Other+sla+Total_Investment+Online_Marketing+Sponsorship+Radio+prepaid+prepaidPerOrder+NPS+Digital+specDay.xRepublicDaySale, data= traindata)
summary(model_6)
vif(model_6)

#Removing Republicday Sale for high p value
model_7 = lm(gmv~ specDay.xRegularSale+spec+SEM+Other+sla+Total_Investment+Online_Marketing+Sponsorship+Radio+prepaid+prepaidPerOrder+NPS+Digital, data= traindata)
summary(model_7)
vif(model_7)

#Removing TotalInvestment Sale for high vif value
model_8 = lm(gmv~ specDay.xRegularSale+spec+SEM+Other+sla+Online_Marketing+Sponsorship+Radio+prepaid+prepaidPerOrder+NPS+Digital, data= traindata)
summary(model_8)
vif(model_8)

#Removing prepaid for high p value
model_9 = lm(gmv~ specDay.xRegularSale+spec+SEM+Other+sla+Online_Marketing+Sponsorship+Radio+prepaidPerOrder+NPS+Digital, data= traindata)
summary(model_9)
vif(model_9)

#Removing SEM for high vif value
model_10 = lm(gmv~ specDay.xRegularSale+spec+Other+sla+Online_Marketing+Sponsorship+Radio+prepaidPerOrder+NPS+Digital, data= traindata)
summary(model_10)
vif(model_10)

#Removing NPS, Radio and other for high p value
model_11 = lm(gmv~ specDay.xRegularSale+spec+sla+Online_Marketing+Sponsorship+prepaidPerOrder+Digital, data= traindata)
summary(model_11)
vif(model_11)

#Removing prepaidperorder for high p value
model_12 = lm(gmv~ specDay.xRegularSale+spec+sla+Online_Marketing+Sponsorship+Digital, data= traindata)
summary(model_12)
vif(model_12)

#Removing online marketing for high p value
model_13 = lm(gmv~ specDay.xRegularSale+spec+sla+Sponsorship+Digital, data= traindata)
summary(model_13)
vif(model_13)

testdata$testgmv = predict(model_13,testdata)
Rsq_homeaudio = cor(testdata$gmv,testdata$testgmv)^2
Rsq_homeaudio #0.996 R-squared on test data

cv_homeaudio = train(gmv ~ specDay.xRegularSale+spec+sla+Sponsorship+Digital, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_homeaudio,testdata)
cv_homeaudio$results   #Rsq = 0.985
Rsq_cv_homeaudio = cor(testdata$gmv,testdata$testgmv_cv)^2
Rsq_cv_homeaudio #Rsq on CV = 0.996



##################################################### multiplicative - lag model #######################################################

homeaudio_lag_final = advanced_kpi(homeaudio_ecom_final)
homeaudio_mult_lag_final = homeaudio_lag_final
homeaudio_mult_lag_final[homeaudio_mult_lag_final == 0] = 0.00001
homeaudio_mult_lag_final$GMV_lag_1_per <- 1 + homeaudio_mult_lag_final$GMV_lag_1_per - min(homeaudio_mult_lag_final$GMV_lag_1_per)
homeaudio_mult_lag_final$GMV_lag_2_per <- 1 + homeaudio_mult_lag_final$GMV_lag_2_per - min(homeaudio_mult_lag_final$GMV_lag_2_per)
homeaudio_mult_lag_final$GMV_lag_3_per <- 1 + homeaudio_mult_lag_final$GMV_lag_3_per - min(homeaudio_mult_lag_final$GMV_lag_3_per)
homeaudio_mult_lag_final=log(homeaudio_mult_lag_final)

split<-createDataPartition(y = homeaudio_mult_lag_final$gmv, p = 0.7, list = FALSE)
traindata<-homeaudio_mult_lag_final[split,]
testdata<-homeaudio_mult_lag_final[-split,]

model_1 = lm(gmv~., data=traindata)
summary(model_1)
vif(model_1)
step = stepAIC(model_1, direction = "both")
#removing Digital,specDay.xBSDSale, specDay.xChristmasNewyearsale, specDay.xRepublicDaySale,specday.xPacmandaysale, GMV_lag_3_per, Total_Investment, Online Marketing, NPS, week, TV, productprocurement sla, Sponsorship, GMV_lag_1

model_2 = lm(gmv ~ Other + Radio + sla  + GMV_lag_2_per + PromoOffer + specDay.xRegularSale + specDay.xFHSDSale +weekday.xWednesday+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+ specDay.xValentineDaySale + spec + prepaid +  SEM + prepaidPerOrder+Content_Marketing, data = traindata)
summary(model_2)
vif(model_2)
#removing weekday wednesday and FHSD Sale due to high p-value

model_3 = lm(gmv ~ Other + Radio + sla  + GMV_lag_2_per + PromoOffer + specDay.xRegularSale+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale+ specDay.xValentineDaySale + spec + prepaid +  SEM + prepaidPerOrder+Content_Marketing, data = traindata)
summary(model_3)
vif(model_3)
#removing valentinesday sale due to high p-value

model_4 = lm(gmv ~ Other + Radio + sla  + GMV_lag_2_per + PromoOffer + specDay.xRegularSale+specDay.xBigDiwaliSale+specDay.xEidRathayatraSale + spec + prepaid +  SEM + prepaidPerOrder+Content_Marketing, data = traindata)
summary(model_4)
vif(model_4)
#removing specDay.xRegularSale and Diwali sale due to high p-value

model_5 = lm(gmv ~ Other + Radio + sla  + GMV_lag_2_per + PromoOffer + specDay.xEidRathayatraSale + spec + prepaid +  SEM + prepaidPerOrder+Content_Marketing, data = traindata)
summary(model_5)
vif(model_5)
#removing specDay.xEidRathayatrasale due to high p-value

model_6 = lm(gmv ~ Other + Radio + sla  + GMV_lag_2_per + PromoOffer + spec + prepaid +  SEM + prepaidPerOrder+Content_Marketing, data = traindata)
summary(model_6)
vif(model_6)
#removing Other due to high vif

model_7 = lm(gmv ~  + Radio + sla  + GMV_lag_2_per + PromoOffer + spec + prepaid +  SEM + prepaidPerOrder+Content_Marketing, data = traindata)
summary(model_7)
vif(model_7)
#removing spec due to high p-value

model_8 = lm(gmv ~  + Radio + sla  + GMV_lag_2_per + PromoOffer  + prepaid +  SEM + prepaidPerOrder+Content_Marketing, data = traindata)
summary(model_8)
vif(model_8)
#removing GMV_lag_2_per due to high p-value

model_9= lm(gmv ~  + Radio + sla+ PromoOffer  + prepaid +  SEM + prepaidPerOrder+Content_Marketing, data = traindata)
summary(model_9)
vif(model_9)
#removing radio due to high p-value

model_10= lm(gmv ~  sla+ PromoOffer  + prepaid +  SEM + prepaidPerOrder+Content_Marketing, data = traindata)
summary(model_10)
vif(model_10)

testdata$testgmv = predict(model_10,testdata)
Rsq_homeaudio = cor(testdata$gmv,testdata$testgmv)^2
Rsq_homeaudio #0.987 R-squared on test data

cv_homeaudio = train(gmv ~  sla+ PromoOffer  + prepaid +  SEM + prepaidPerOrder+Content_Marketing, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_homeaudio,testdata)
cv_homeaudio$results   #Rsq = 0.925
Rsq_cv_homeaudio = cor(testdata$gmv,testdata$testgmv_cv)^2
Rsq_cv_homeaudio #Rsq on CV = 0.987








