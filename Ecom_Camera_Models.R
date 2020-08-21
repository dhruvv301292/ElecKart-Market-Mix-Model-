
#For camera accessory

camera_ecom <- ecom_data_cleaned[ecom_data_cleaned$product_analytic_sub_category == "CameraAccessory", ]
camera_ecom_backup <- camera_ecom # storing data for backup
camera_ecom <- camera_ecom_backup

cam_data_cat = camera_ecom[, c(10,30)]
cam_data_cat = data.frame(lapply(cam_data_cat, function(v) {
  if (is.character(v)) return(factor(v))
  else return(v)
}))
sum(is.na(cam_data_cat))

cam_dummies = data.frame(sapply(cam_data_cat, function(x) 
  data.frame(model.matrix(~x-1, data = cam_data_cat))[,-1]))

cam_dummies = cbind(camera_ecom$week, cam_dummies)
colnames(cam_dummies)[colnames(cam_dummies)=="camera_ecom$week"] <- "week"

#aggregating dummy variables
cam_dummies_aggregate <- aggregate(.~week, cam_dummies, sum, na.rm = TRUE)

camera_ecom = camera_ecom[ , -c(10,30)]

#aggregating main variables
camera_ecom = camera_ecom %>% group_by(week) %>% summarise(gmv = sum(gmv), units = sum(units), sla = mean(sla), product_mrp = sum(product_mrp), product_procurement_sla = mean(product_procurement_sla),
                                                           NPS=mean(NPS),Total_Investment = mean(Total_Investment), TV = mean(TV), Digital = mean(Digital), Sponsorship = mean(Sponsorship),
                                                           Content_Marketing = mean(Content_Marketing), Online_Marketing = mean(Online_Marketing), SEM = mean(SEM), Radio = mean(Radio), Other = mean(Other), TV_adstock= mean(TV_adstock),
                                                           Digital_adstock = mean(Digital_adstock), Sponsorship_adstock = mean(Sponsorship_adstock), Content_Marketing_adstock = mean(Content_Marketing_adstock),
                                                           Online_Marketing_adstock = mean(Online_Marketing_adstock), SEM_adstock = mean(SEM_adstock), Radio_adstock = mean(Radio_adstock), Other_adstock = mean(Other_adstock), spec = sum(spec), prepaid = sum(prepaid), sellPrice = sum(sellPrice), PromoOffer = mean(PromoOffer), prepaidPerOrder = mean(prepaidPerOrder))



#merging aggregated main and dummy to produce final data set for modelling
camera_ecom_final = merge(camera_ecom, cam_dummies_aggregate, by = c("week"), all.x = TRUE)

camera_ecom_final = camera_ecom_final[,-(which(colSums(camera_ecom_final) == 0))]

############################ MODELING Camera Accessories #############################################
########################################################################################################################################################
# Linear
#checking for high linear relation
camera_ecom_final_backup <- camera_ecom_final
cam_cor = cor(camera_ecom_final)
View(cam_cor)
summary(camera_ecom_final)

#deleting variables with linear relationship to gmv
camera_ecom_final = camera_ecom_final[, -c(30:35)]
camera_ecom_final = camera_ecom_final[, -c(17:24)]
camera_ecom_final$product_mrp = NULL
camera_ecom_final$sellPrice = NULL
camera_ecom_final$units = NULL
camera_ecom_final[,2:ncol(camera_ecom_final)] = scale(camera_ecom_final[,2:ncol(camera_ecom_final)])


set.seed(100)
split<-createDataPartition(y = camera_ecom_final12$gmv, p = 0.7, list = FALSE)
traindata<-camera_ecom_final12[split,]
testdata<-camera_ecom_final12[-split,]
model_1 = lm(gmv~., data= traindata)
summary(model_1)
vif(model_1)


model2 <- lm(formula =  gmv ~ units + sla +  product_procurement_sla + week + Sponsorship + 
               NPS + Total_Investment + TV + Digital + Sponsorship + Content_Marketing + 
               Online_Marketing + Affiliates + SEM + Radio + Other + TV_adstock + 
               Digital_adstock + Sponsorship_adstock + Content_Marketing_adstock + 
               Online_Marketing_adstock + Affiliates_adstock + SEM_adstock + 
               Radio_adstock + Other_adstock + spec + count + Week_date + 
               prepaid + sellPrice + PromoOffer, data = traindata)


summary(model2)
vif(model2)

# - TotalInvestment, Sponsorship
model3 <- lm(formula =  gmv ~ units + sla +  product_procurement_sla + week +  
               NPS +  TV + Digital + Sponsorship + Content_Marketing + 
               Online_Marketing + Affiliates + SEM + Radio + Other + TV_adstock + 
               Digital_adstock + Sponsorship_adstock + Content_Marketing_adstock + 
               Online_Marketing_adstock + Affiliates_adstock + SEM_adstock + 
               Radio_adstock + Other_adstock + spec + count + Week_date + 
               prepaid + sellPrice + PromoOffer, data = traindata)

summary(model3)
vif(model3)

# - week, Digital_adstock           
model4 <- lm(formula =  gmv ~ units + sla +  product_procurement_sla +   
               NPS +  TV + Digital + Sponsorship + Content_Marketing + 
               Online_Marketing + Affiliates + SEM + Radio + Other + TV_adstock + 
                Sponsorship_adstock + Content_Marketing_adstock + 
               Online_Marketing_adstock + Affiliates_adstock + SEM_adstock + 
               Radio_adstock + Other_adstock + spec + count + Week_date + 
               prepaid + sellPrice + PromoOffer, data = traindata)

summary(model4)
vif(model4)

#removing Content_Marketing,Other, SEM, Radio 
model5 <- lm(formula =  gmv ~ units + sla +  product_procurement_sla +   
               NPS +  TV + Digital + Sponsorship + 
               Online_Marketing + Affiliates +  TV_adstock + 
               Sponsorship_adstock + Content_Marketing_adstock + 
               Online_Marketing_adstock + Affiliates_adstock + SEM_adstock + 
               Radio_adstock + Other_adstock + spec + count + Week_date + 
               prepaid + sellPrice + PromoOffer, data = traindata)

summary(model5)
vif(model5)

#removing Online_Marketing_adstock
model6 <- lm(formula =  gmv ~ units + sla +  product_procurement_sla +   
               NPS +  TV + Digital + Sponsorship + 
               Online_Marketing + Affiliates +  TV_adstock + 
               Sponsorship_adstock + Content_Marketing_adstock + 
                Affiliates_adstock + SEM_adstock + 
               Radio_adstock + Other_adstock + spec + count + Week_date + 
               prepaid + sellPrice + PromoOffer, data = traindata)

summary(model6)
vif(model6)

#removing SEM_adstock,Sponsorship_adstock
model7 <- lm(formula =  gmv ~ units + sla + product_mrp + product_procurement_sla +   
               NPS +  TV + Digital + Sponsorship + Online_Marketing + Affiliates +  TV_adstock + 
                Content_Marketing_adstock + Affiliates_adstock + 
               Radio_adstock + Other_adstock + spec + count + Week_date + 
               prepaid + sellPrice + PromoOffer, data = traindata)

summary(model7)
vif(model7)

#removing Radio_adstock,TV_adstock
model8 <- lm(formula =  gmv ~ units + sla + product_mrp + product_procurement_sla +   
               NPS + TV + Digital + Sponsorship + Online_Marketing + Affiliates + 
               Content_Marketing_adstock + Affiliates_adstock + 
                Other_adstock + spec + count + Week_date + 
               prepaid + sellPrice + PromoOffer, data = traindata)

summary(model8)
vif(model8)

#removing Sponsorship and Digital
model9 <- lm(formula =  gmv ~ units + sla + product_mrp + product_procurement_sla +   
               NPS + TV + Online_Marketing + Affiliates + 
               Content_Marketing_adstock + Affiliates_adstock + 
               Other_adstock + spec + count + Week_date + 
               prepaid + sellPrice + PromoOffer, data = traindata)

summary(model9)
vif(model9)

#removing Affiliates 
model10 <- lm(formula =  gmv ~ units + sla + product_mrp + product_procurement_sla +   
               NPS + TV + Online_Marketing +  
               Content_Marketing_adstock + Affiliates_adstock + 
               Other_adstock + spec + count + Week_date + 
               prepaid + sellPrice + PromoOffer, data = traindata)

summary(model10)
vif(model10)

#removing OtherAdstock 
model11 <- lm(formula =  gmv ~ units + sla + product_mrp + product_procurement_sla +   
                NPS + TV + Online_Marketing +  
                Content_Marketing_adstock +  spec + count + Week_date + 
                prepaid + sellPrice + PromoOffer, data = traindata)

summary(model11)
vif(model11)


#removing product_procurement_sla  ,units, sellPrice , PromoOffe
model12 <- lm(formula =  gmv ~  sla + product_mrp + 
                NPS + TV + Online_Marketing +  
                Content_Marketing_adstock +  spec + count + Week_date + 
                prepaid , data = traindata)

summary(model12)
vif(model12)


#removing Content_Marketing_adstock
model13 <- lm(formula =  gmv ~  sla + product_mrp + 
                NPS + TV + Online_Marketing +  
                spec + count + Week_date + 
                prepaid , data = traindata)

summary(model13)
vif(model13)

#removing TV
model14 <- lm(formula =  gmv ~  sla + product_mrp + 
                NPS + Online_Marketing +  
                spec + count + Week_date + 
                prepaid , data = traindata)

summary(model14)
vif(model14)

#removing count
model15 <- lm(formula =  gmv ~  sla +  product_mrp + 
                NPS + Online_Marketing +  
                spec + count + Week_date + 
                prepaid  , data = traindata)

summary(model15)
vif(model15)

#testing on test data
testdata$testgmv = predict(model15,testdata)
Rsq_cam = cor(testdata$gmv,testdata$testgmv)^2
Rsq_cam #0.92 R-squared on test data

cv_cam = train(mv ~  sla +  product_mrp + NPS + Online_Marketing + spec + count + Week_date + prepaid, traindata, method = "lm", 
                  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_cam,testdata)
cv_cam$results   #Rsq = 0.98
Rsq_cv_cam = cor(testdata$gmv,testdata$testgmv_cv)^2  #
Rsq_cv_cam #Rsq on CV = 0.9315

########################################################################################################################################################
######For multiplicative model######
####we have to remove the scale method while computing camera_ecom_final. For linear modelling compute the scaling right before doing the modelling. I have done the same above
camera_ecom_mult_final <- camera_ecom_backup
camera_ecom_mult_final <- camera_ecom_mult_final[,-c(4,5,8:10,12:15,18,40:42)]
camera_ecom_mult_final[camera_ecom_mult_final == 0] = 0.00001
camera_ecom_mult_final =log(camera_ecom_mult_final)
sum(is.na(camera_ecom_mult_final))



split<-createDataPartition(y = na.omit(camera_ecom_mult_final$gmv), p = 0.7, list = FALSE)
traindata<-camera_ecom_mult_final[split,]
testdata<-camera_ecom_mult_final[-split,]
sum(is.na(traindata))

#Model Building

model_1 = lm(gmv~., data=traindata)

summary(model_1)
vif(model_1)
step = stepAIC(model_1, direction ="both")

#Removing Radio, other and Online_Marketing_adstock
model2 =lm(gmv ~ Month + week + units + sla + product_mrp + product_procurement_sla + 
             NPS + Total_Investment + TV + Digital + Sponsorship + Content_Marketing + 
             Affiliates + SEM + TV_adstock + Digital_adstock + 
             Sponsorship_adstock + Content_Marketing_adstock +  
             Affiliates_adstock + SEM_adstock + Radio_adstock + Other_adstock + 
             spec + prepaid + sellPrice, data = traindata)

summary(model2)
vif(model2)

#Removing other_adstock
model3 =lm(gmv ~ Month + week + units + sla + product_mrp + product_procurement_sla + 
             NPS + Total_Investment + TV + Digital + Sponsorship + Content_Marketing + 
             Affiliates + SEM + TV_adstock + Digital_adstock + 
             Sponsorship_adstock + Content_Marketing_adstock +  
             Affiliates_adstock + SEM_adstock + Radio_adstock +
             spec + prepaid + sellPrice, data = traindata)

summary(model3)
vif(model3)


#RemovingAffiliates_adstock
model4 =lm(gmv ~ Month + week + units + sla + product_mrp + product_procurement_sla + 
             NPS + Total_Investment + TV + Digital + Sponsorship + Content_Marketing + 
             Affiliates + SEM + TV_adstock + Digital_adstock + 
             Sponsorship_adstock + Content_Marketing_adstock +  
            SEM_adstock + Radio_adstock +
             spec + prepaid + sellPrice, data = traindata)

summary(model4)
vif(model4)

#Removing Total_Investment          
model5 =lm(gmv ~ Month + week + units + sla + product_mrp + product_procurement_sla + 
             NPS +  TV + Digital + Sponsorship + Content_Marketing + 
             Affiliates + SEM + TV_adstock + Digital_adstock + 
             Sponsorship_adstock + Content_Marketing_adstock +  
             SEM_adstock + Radio_adstock +
             spec + prepaid + sellPrice, data = traindata)

summary(model5)
vif(model5)

# Removing Content_Marketing
model6 =lm(gmv ~ Month + week + units + sla + product_mrp + product_procurement_sla + 
             NPS +  TV + Digital + Sponsorship +  
             Affiliates + SEM + TV_adstock + Digital_adstock + 
             Sponsorship_adstock + Content_Marketing_adstock +  
             SEM_adstock + Radio_adstock +
             spec + prepaid + sellPrice, data = traindata)

summary(model6)
vif(model6)

# Removing SEM
model7 =lm(gmv ~ Month + week + units + sla + product_mrp + product_procurement_sla + 
             NPS +  TV + Digital + Sponsorship +  
             Affiliates +  TV_adstock + Digital_adstock + 
             Sponsorship_adstock + Content_Marketing_adstock +  
             SEM_adstock + Radio_adstock +
             spec + prepaid + sellPrice, data = traindata)

summary(model7)
vif(model7)

# Removing Month
model8 =lm(gmv ~  week + units + sla + product_mrp + product_procurement_sla + 
             NPS +  TV + Digital + Sponsorship +  
             Affiliates +  TV_adstock + Digital_adstock + 
             Sponsorship_adstock + Content_Marketing_adstock +  
             SEM_adstock + Radio_adstock +
             spec + prepaid + sellPrice, data = traindata)

summary(model8)
vif(model8)

# Removing Digital and Digital_adStock
model9 =lm(gmv ~  week + units + sla + product_mrp + product_procurement_sla + 
             NPS +  TV +  Sponsorship + Affiliates +  TV_adstock +  
             Sponsorship_adstock + Content_Marketing_adstock +  
             SEM_adstock + Radio_adstock + spec + prepaid + sellPrice, data = traindata)

summary(model9)
vif(model9)

# Removing TV
model10 =lm(gmv ~  week + units + sla + product_mrp + product_procurement_sla + 
             NPS +  Sponsorship + Affiliates +  TV_adstock +  
             Sponsorship_adstock + Content_Marketing_adstock +  
             SEM_adstock + Radio_adstock + spec + prepaid + sellPrice, data = traindata)

summary(model10)
vif(model10)

#Removing  Sponsorship_adstoc
model11 =lm(gmv ~  week + units + sla + product_mrp + product_procurement_sla + 
              NPS +  Sponsorship + Affiliates +  TV_adstock + Content_Marketing_adstock +  
              SEM_adstock + Radio_adstock + spec + prepaid + sellPrice, data = traindata)

summary(model11)
vif(model11)


#Removing  Week
model12 =lm(gmv ~   units + sla + product_mrp + product_procurement_sla + 
              NPS +  Sponsorship + Affiliates +  TV_adstock + Content_Marketing_adstock +  
              SEM_adstock + Radio_adstock + spec + prepaid + sellPrice, data = traindata)

summary(model12)
vif(model12)

#Removing  Radio_adstock, Content_Marketing_adstock, SEM_adStock
model13 =lm(gmv ~   units + sla + product_mrp + product_procurement_sla + 
              NPS +  Sponsorship + Affiliates +  TV_adstock + spec + prepaid + sellPrice, data = traindata)

summary(model13)
vif(model13)

#Removing  Sponsership and Affiliates
model14 =lm(gmv ~   units + sla + product_mrp + product_procurement_sla + 
              NPS + spec + prepaid , data = traindata)

summary(model14)
vif(model14)

#Removing product_procurement_sla
model15 =lm(gmv ~    sla + product_mrp +  
              NPS +  spec + prepaid , data = traindata)

summary(model15)
vif(model15)

model16 =lm(gmv ~  sla +  NPS + spec + prepaid , data = traindata)

summary(model16)
vif(model16)

#testing on test data
testdata$testgmv = predict(model16,testdata)
Rsq_Camera = cor(testdata$gmv,testdata$testgmv)^2
Rsq_Camera 

#Cross validation on train data with final model:
cv_Camera  = train(gmv ~  sla +  NPS + spec + prepaid, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_Camera,testdata)
cv_Camera$results  
Rsq_cv_camera = cor(testdata$gmv,testdata$testgmv_cv)^2  #
Rsq_cv_camera 

########################################################################################################################################################
#####distributive lag model######

camera_lag_final = advanced_kpi(camera_ecom_final) 
camera_lag_scaled_final = camera_lag_final
camera_lag_scaled_final <- camera_lag_scaled_final[,-c(4,5,8:10,12:15,18,40:42)]
camera_lag_scaled_final[,2:ncol(camera_lag_scaled_final)] = scale(camera_lag_scaled_final[,2:ncol(camera_lag_scaled_final)])
camera_lag_scaled_final$week = NULL

split<-createDataPartition(y = camera_lag_scaled_final$gmv, p = 0.8, list = FALSE)
traindata<-camera_lag_scaled_final[split,]
testdata<-camera_lag_scaled_final[-split,]

#building models
model_1 = lm(gmv~., data=traindata)
summary(model_1) #0.997 rSquared
vif(model_1)
step = stepAIC(model_1, direction = "both")

model2_d =lm(gmv ~ units + sla + product_mrp + Total_Investment + Sponsorship + 
  Radio + Sponsorship_adstock + Online_Marketing_adstock + 
  SEM_adstock + Radio_adstock + Other_adstock + spec + prepaid + 
  sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per + 
  GMV_lag_3_per, data= traindata)

summary(model2_d)
vif(model2_d)

#TotalInvestment
model3_d =lm(gmv ~ units + sla + product_mrp + Sponsorship + 
               Radio + Sponsorship_adstock + Online_Marketing_adstock + 
               SEM_adstock + Radio_adstock + Other_adstock + spec + prepaid + 
               sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per + 
               GMV_lag_3_per, data= traindata)

summary(model3_d)
vif(model3_d)

#other_adstock
model4_d =lm(gmv ~ units + sla + product_mrp + Sponsorship + 
               Radio + Sponsorship_adstock + Online_Marketing_adstock + 
               SEM_adstock + Radio_adstock +spec + prepaid + 
               sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per + 
               GMV_lag_3_per, data= traindata)

summary(model4_d)
vif(model4_d)

#Radio_adstock
model5_d =lm(gmv ~ units + sla + product_mrp + Sponsorship + 
               Radio + Sponsorship_adstock + Online_Marketing_adstock + 
               SEM_adstock + spec + prepaid + 
               sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per + 
               GMV_lag_3_per, data= traindata)

summary(model5_d)
vif(model5_d)


model6_d =lm(gmv ~ units + sla + product_mrp + Sponsorship + 
               Radio + Sponsorship_adstock +  
               SEM_adstock + spec + prepaid + 
               sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per + 
               GMV_lag_3_per, data= traindata)

summary(model6_d)
vif(model6_d)

model7_d =lm(gmv ~ units + sla +  Sponsorship + 
               Radio + Sponsorship_adstock +  
               SEM_adstock + spec + prepaid + 
               sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per + 
               GMV_lag_3_per, data= traindata)

summary(model7_d)
vif(model7_d)

model8_d =lm(gmv ~ units + sla +  Sponsorship + 
               Radio + SEM_adstock + spec + prepaid + 
               sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per + 
               GMV_lag_3_per, data= traindata)

summary(model8_d)
vif(model8_d)


model9_d =lm(gmv ~ units + sla +  Sponsorship + 
               Radio + SEM_adstock + spec + prepaid + 
               sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per , data= traindata)

summary(model9_d)
vif(model9_d)


model10_d =lm(gmv ~ units + sla +  Sponsorship + 
               Radio + spec + prepaid +    sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per , data= traindata)

summary(model10_d)
vif(model10_d)


model11_d =lm(gmv ~ units + Sponsorship + 
                Radio + spec + prepaid +    sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per , data= traindata)

summary(model11_d)
vif(model11_d)

model12_d =lm(gmv ~  units +   Radio + spec + prepaid +  sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per , data= traindata)

summary(model12_d)
vif(model12_d)

model13_d =lm(gmv ~  units +    spec + prepaid +  sellPrice + PromoOffer + GMV_lag_1_per + GMV_lag_2_per , data= traindata)

summary(model13_d)
vif(model13_d)


model14_d =lm(gmv ~  units +    spec + prepaid +  sellPrice + PromoOffer +  GMV_lag_2_per , data= traindata)

summary(model14_d)
vif(model14_d)

#testing on test data
testdata$testgmv = predict(model14_d,testdata)
Rsq_cam = cor(testdata$gmv,testdata$testgmv)^2
Rsq_cam #0.99 R-squared on test data

#Cross validation on train data with final model:
cv_cam = train(gmv ~ units +    spec + prepaid +  sellPrice + PromoOffer +  GMV_lag_2_per, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_cam,testdata)
cv_cam$results   #Rsq = 0.99
Rsq_cv_cam = cor(testdata$gmv,testdata$testgmv_cv)^2
Rsq_cv_cam #Rsq on CV = 0.99

########################################################################################################################################################
##########for koyk model ###############
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


########################################################################################################################################################
#####for koyk model######
camera_koyk_scaled_final <- advanced_kpi(camera_ecom_final)
camera_koyk_scaled_final <- camera_koyk_scaled_final[,-c(4,5,8:10,12:15,18,40:42)]

camera_koyk_scaled_final[,2:ncol(camera_koyk_scaled_final)] = scale(camera_koyk_scaled_final[,2:ncol(camera_koyk_scaled_final)])
camera_koyk_scaled_final$GMV_lag_2_per = NULL
camera_koyk_scaled_final$GMV_lag_3_per = NULL

split<-createDataPartition(y = camera_koyk_scaled_final$gmv, p = 0.7, list = FALSE)
traindata<-camera_koyk_scaled_final[split,]
testdata<-camera_koyk_scaled_final[-split,]

model_1 = lm(gmv~., data=traindata)
summary(model_1)
vif(model_1)
step = stepAIC(model_1, direction = "both")

model_2 = lm(gmv ~  week + specDay.xValentineDaySale + PromoOffer + Content_Marketing + product_procurement_sla + 
               specDay.xBSDSale + NPS + specDay.xEidRathayatraSale + TV + specDay.xRakshabandanSale + GMV_lag_1_per + Sponsorship + 
               Total_Investment + Online_Marketing + SEM + specDay.xDausseraSale + Other + Radio + specDay.xRegularSale + spec, data=traindata)
summary(model_2)
vif(model_2)

model_3 = lm(gmv ~  week + specDay.xValentineDaySale + PromoOffer + Content_Marketing + product_procurement_sla +  
               specDay.xBSDSale + NPS + TV + specDay.xRakshabandanSale + GMV_lag_1_per + Sponsorship + 
               Total_Investment + Online_Marketing + SEM + specDay.xDausseraSale + Other + Radio + specDay.xRegularSale + spec, data=traindata)
summary(model_3)
vif(model_3)


model_4 = lm(gmv ~  PromoOffer + Content_Marketing + product_procurement_sla + 
               specDay.xFHSDSale + specDay.xBSDSale + NPS + specDay.xEidRathayatraSale + TV + specDay.xRakshabandanSale + 
               GMV_lag_1_per + Sponsorship + Total_Investment + Online_Marketing + SEM + specDay.xDausseraSale + Other + Radio + 
               specDay.xRegularSale + spec, data=traindata)
summary(model_4)
vif(model_4)


model_5 = lm(gmv ~  PromoOffer + product_procurement_sla + specDay.xFHSDSale + specDay.xBSDSale + NPS + specDay.xEidRathayatraSale + 
               TV + specDay.xRakshabandanSale + GMV_lag_1_per + Sponsorship + Total_Investment + Online_Marketing + SEM + 
               specDay.xDausseraSale + specDay.xRegularSale + spec, data=traindata)
summary(model_5)
vif(model_5)


model_6 = lm(gmv ~  PromoOffer + product_procurement_sla + specDay.xFHSDSale + specDay.xBSDSale + NPS + specDay.xEidRathayatraSale + 
               TV + Sponsorship + Total_Investment + Online_Marketing + SEM + 
               specDay.xDausseraSale + specDay.xRegularSale + spec, data=traindata)
summary(model_6)
vif(model_6)


model_7 = lm(gmv ~ PromoOffer + product_procurement_sla + specDay.xFHSDSale + specDay.xBSDSale + NPS +  
               TV + Sponsorship + Total_Investment + Online_Marketing + SEM + 
               specDay.xDausseraSale + specDay.xRegularSale + spec, data=traindata)
summary(model_7)
vif(model_7)


model_8 = lm(gmv ~ PromoOffer + product_procurement_sla + specDay.xFHSDSale + specDay.xBSDSale + NPS +  
               TV + Sponsorship + Total_Investment + Online_Marketing + SEM + specDay.xRegularSale + spec, data=traindata)
summary(model_8)
vif(model_8)


model_9 = lm(gmv ~ PromoOffer + product_procurement_sla +  NPS +  
               TV + Sponsorship + Total_Investment + Online_Marketing + SEM + specDay.xRegularSale + spec, data=traindata)
summary(model_9)
vif(model_9)


model_10 = lm(gmv ~ PromoOffer + product_procurement_sla +  NPS +  
                TV + Sponsorship + Total_Investment + Online_Marketing + SEM +  spec, data=traindata)
summary(model_10)
vif(model_10)


model_11 = lm(gmv ~ PromoOffer + product_procurement_sla +  NPS +  
                TV + Sponsorship + Total_Investment + Online_Marketing , data=traindata)
summary(model_11)
vif(model_11)


model_12 = lm(gmv ~ PromoOffer +   NPS +  TV + Sponsorship + Total_Investment + Online_Marketing , data=traindata)
summary(model_12)
vif(model_12)


testdata$testgmv = predict(model_12,testdata)
Rsq_cam = cor(testdata$gmv,testdata$testgmv)^2
Rsq_cam #0.9483 R-squared on test data

cv_cam = train(gmv ~ PromoOffer +   NPS +  TV + Sponsorship + Total_Investment + Online_Marketing , traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_cam,testdata)
cv_cam$results   #Rsq = 0.88
Rsq_cv_cam = cor(testdata$gmv,testdata$testgmv_cv)^2
Rsq_cv_cam #Rsq on CV = 0.89

########################################################################################################################################################
######for Multiplicative distributed lag model######
camera_lag_final = advanced_kpi(camera_ecom_final)
camera_lag_final <- camera_lag_final[,-c(4,5,8:10,12:15,18,40:42)]
camera_lag_final[,2:ncol(camera_lag_final)] = scale(camera_lag_final[,2:ncol(camera_lag_final)])
model<-lm(gmv~.,data=camera_lag_final)


#Now applying step method to elminate most of the dependent and insignificant variables:
cam_lag_intercept <- lm(gmv ~ 1 , data= camera_lag_final)  # base intercept only model
cam_lag_all <- lm(gmv ~ . , data= camera_lag_final) # full model with all predictors

summary(cam_lag_all)
vif(cam_lag_all)

cam_lag_stepMod <- step(cam_lag_intercept, scope = list(lower = cam_lag_intercept, upper = cam_lag_all), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
cam_lag_shortlistedVars <- names(unlist(cam_lag_stepMod[[1]])) # get the shortlisted variable.
cam_lag_shortlistedVars <- cam_lag_shortlistedVars[!cam_lag_shortlistedVars %in% "(Intercept)"]  

#Keeping variables from the last call of step method: 
cols<-subset(camera_lag_final,select=c(cam_lag_shortlistedVars))
cols_cam_lag<-cbind(camera_lag_final$gmv,cols)
colnames(cols_cam_lag)[colnames(cols_cam_lag) == 'camera_lag_final$gmv'] <- 'gmv'

#Sampling:
split<-createDataPartition(y = cols_cam_lag$gmv, p = 0.7, list = FALSE)
traindata<-cols_cam_lag[split,]
testdata<-cols_cam_lag[-split,]

#building models
model_1 = lm(gmv~., data=traindata)
summary(model_1)
vif(model_1)
step = stepAIC(model_1, direction = "both")
#removing specDay.xRepublicDaySale, specDay.xRegularSale and GMV_lag_1_per due to stepAIC

model_2 = lm(gmv ~ sellPrice + units + PromoOffer+product_mrp+ GMV_lag_3_per+ sla+GMV_lag_1_per+ GMV_lag_2_per+ prepaid + spec + 
                  Affiliates_adstock+ prepaid.x + Content_Marketing, data = traindata)
summary(model_2)
vif(model_2)

#Removing GMV_lag_3_per
model_3 = lm(gmv ~ sellPrice + units + PromoOffer+product_mrp+ sla+GMV_lag_1_per+ GMV_lag_2_per+ prepaid + spec + 
               Affiliates_adstock+ prepaid.x + Content_Marketing, data = traindata)
summary(model_3)
vif(model_3)

#Removing product mrp
model_4 = lm(gmv ~ sellPrice + units + PromoOffer+ sla+GMV_lag_1_per+ GMV_lag_2_per+ prepaid + spec + 
               Affiliates_adstock+ prepaid.x + Content_Marketing, data = traindata)
summary(model_4)
vif(model_4)

#Removing prepaid
model_5 = lm(gmv ~ sellPrice + units + PromoOffer+ sla+GMV_lag_1_per+ GMV_lag_2_per+ prepaid + spec + 
               Affiliates_adstock+  Content_Marketing, data = traindata)
summary(model_5)
vif(model_5)


model_6 = lm(gmv ~  units + PromoOffer+ sla+GMV_lag_1_per+ GMV_lag_2_per+ prepaid + spec + 
               Affiliates_adstock+  Content_Marketing, data = traindata)
summary(model_6)
vif(model_6)


model_7 = lm(gmv ~  units + PromoOffer+ GMV_lag_1_per+ GMV_lag_2_per+ prepaid +  
               Affiliates_adstock+  Content_Marketing, data = traindata)
summary(model_7)
vif(model_7)


model_8 = lm(gmv ~  units + PromoOffer+ GMV_lag_1_per+ GMV_lag_2_per+ prepaid +  
                 Content_Marketing, data = traindata)
summary(model_8)
vif(model_8)


model_9 = lm(gmv ~  units + PromoOffer+ GMV_lag_1_per+ GMV_lag_2_per+ prepaid , data = traindata)
summary(model_9)
vif(model_9)


model_10 = lm(gmv ~   PromoOffer+ GMV_lag_1_per+ GMV_lag_2_per+ prepaid , data = traindata)
summary(model_10)
vif(model_10)


#Cross validation on train data with final model:
cv_cam = train(gmv ~  PromoOffer+ GMV_lag_1_per+ GMV_lag_2_per+ prepaid, traindata, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
testdata$testgmv_cv = predict(cv_cam,testdata)
cv_cam$results   #Rsq = 0.39
Rsq_cv_cam = cor(testdata$gmv,testdata$testgmv_cv)^2
Rsq_cv_cam #Rsq on CV = 0.67



