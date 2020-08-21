library(readxl)
library(lubridate)
library(ggplot2)
library(scales)
library(cowplot)
library(dplyr)
library(zoo)
library(DataCombine)
library(imputeTS)
library(MASS)
library(car)
library(caret)
library(DAAG)

#Load the consumer data
options(scipen = 999)
ecom_data <- read.csv("ConsumerElectronics.csv")

str(ecom_data)
summary(ecom_data)

#Keep the raw data for reuse
ecom_org <- ecom_data
ecom_data <- ecom_org

###########################
## Data Preparation

#ECOM
ecom_data$order_date <- date(ecom_data$order_date)

# Filter for data from July 2015 to June 2016
ecom_data <- subset(ecom_data, !(order_date < "2015-07-01" | order_date > "2016-06-30"))

#Extract week number from date
ecom_data$week <- week(ecom_data$order_date)

#year has 52 weeks and 2016 week should start from 53. Add 53 to week of 2016
ecom_data$week <- ifelse(ecom_data$Year == 2016, ecom_data$week +53, ecom_data$week)


#make sure the date data is in the required range
table(ecom_data$Year, ecom_data$Month)
#          1      2      3      4      5      6      7      8      9     10     11     12
#2015      0      0      0      0      0      0  88683    271 209202 207301 130203 158151
#2016 143768 144084 153299 134390 157071 121792      0      0      0      0      0      0

ecom_data$weekday <- weekdays(ecom_data$order_date)

#Check for rows with NA values
sum(is.na(ecom_data))
#14712 NA values 

ecom_data <- na.omit(ecom_data)

#Remove the rows who has product mrp as zero
ecom_data <- subset(ecom_data, ecom_data$product_mrp != 0)

#gmv should be less than or equal to MRP*units
#Clean mrv to eliminate the gmv's which are greater than MRP*Units

ecom_data$gmv <- ifelse(ecom_data$gmv == 0, 1, ecom_data$gmv)
ecom_data_updated <- subset(ecom_data, gmv <= (ecom_data$product_mrp * ecom_data$units))

#remove columns which is not required further for anlysis 
ecom_data_updated <- ecom_data_updated[, -c(5,6,13,14)]

##########################
#NPS Score
# updated the excel to add a header row and updated month abbreviations to 3 letters to be able to match with month.abb syntax
# Since the data is very small the above changes are easier 
nps <- read_excel("Media data and other information.xlsx", sheet = "Monthly NPS Score", col_names = TRUE)
View(nps)

#Transpose to read the year and the month data
nps <- data.frame(t(nps))
nps <- nps[-1,]
names(nps) <- c("date", "NPS")

nps_dates <- setNames(data.frame(nps$date,
                    do.call(rbind, strsplit(as.character(nps$date),split="'"))),
         c("attr", paste0("type_", 1:2)))
names(nps_dates) <- c("org", "Month","Year")
nps_dates <-nps_dates[,-1]
nps <- cbind(nps, nps_dates)
nps <- nps[,-1]
nps$Year <- ifelse(nps$Year == 15, 2015, 2016)
nps = nps[-c(14,15,16,17),]
nps$Month <- match(nps$Month, month.abb)


#Merge NPS with main data
ecom_data_updated <- merge(ecom_data_updated, nps, by = c("Year", "Month"), all.x = TRUE)

###########################################################################################

market_investment <- read_excel("Media data and other information.xlsx", sheet = "Media Investment", skip = 2, col_names = TRUE)
View(market_investment)
sum(is.na(market_investment)) #18 NA values only in Radio and others category

market_investment$Radio <- ifelse(is.na(market_investment$Radio), 0, market_investment$Radio)
market_investment$Other <- ifelse(is.na(market_investment$Other), 0, market_investment$Other)
market_investment = market_investment[-13,]
days <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
weekdays <- data.frame('days'=days, Month = month(days), week = week(days),nweek = rep(1,length(days)))
weekdays$week <- ifelse(year(weekdays$days) == 2016, weekdays$week +53, weekdays$week)
weekdays_span <- data.frame(weekdays %>% group_by(Month,week) %>% summarise(countdays = sum(nweek)))
weekdays_span$fracDays <- weekdays_span$countdays/7


market_investment <- cbind(Month=market_investment[,c(2)], market_investment[,-c(1,2)]/4.30)

market_investment_weekly <- merge(weekdays_span,market_investment, by='Month', all.x = TRUE)


## Converting media Investment at weekly granularity
# pro-rate weekly investment as per the ratio of its days span over adjacent months
market_investment_weekly<- data.frame(market_investment_weekly %>% group_by(week) %>% 
                                        summarise(Total_Investment = sum(`Total Investment`*fracDays),
                                                  TV = sum(TV*fracDays), Digital=sum(Digital*fracDays),Sponsorship = sum(Sponsorship*fracDays), 
                                                  Content_Marketing = sum(`Content Marketing`*fracDays),Online_Marketing = sum(`Online marketing`*fracDays), 
                                                  Affiliates = sum(market_investment_weekly$Affiliates*fracDays), SEM = sum(SEM*fracDays),Radio=sum(Radio*fracDays),Other=sum(Other*fracDays)))

nrow(market_investment_weekly)

market_investment_weekly$Content_Marketing<-format(as.numeric(market_investment_weekly$Content_Marketing),scientific = FALSE)

str(market_investment_weekly)
#Converting Content Marketing to numeric:

market_investment_weekly$Content_Marketing<-as.numeric(market_investment_weekly$Content_Marketing)

#Converting to crores:
market_investment_weekly[,2:11] <- market_investment_weekly[,2:11]*10000000

#Merging market_investment data with ecom_data_updated by week:
ecom_data_updated<-merge(ecom_data_updated,market_investment_weekly,by='week',all.x=TRUE)

#Adstock:
# Considering the adstock rate as 50%
adstock_rate = 0.50

# Creating the adstock for each media investment
df <- data.frame(week=1:54)

for(i in 3:ncol(market_investment_weekly)){
  
  df[[paste0(colnames(market_investment_weekly)[i],"_adstock")]] <- stats::filter(x=market_investment_weekly[i], 
                                                                                  filter=adstock_rate, method="recursive")
  
}
df$week = df$week+25
# Merging the adstock with the actual dataset
temp<-ecom_data_updated
ecom_data_updated <- merge(temp, df, by = c("week"), all.x = TRUE)

########################################################################################
# Special day sale data 
#Get all the special day sale dates  into the main data

special_days <- as.Date(c("2015-07-18","2015-07-19", 
                          "2015-08-15","2015-08-16", "2015-08-17", 
                          "2015-08-28", "2015-08-29", "2015-08-30", 
                          "2015-10-15", "2015-10-16", "2015-10-17", 
                          "2015-11-07",  "2015-11-08",  "2015-11-09", "2015-11-10", "2015-11-11", "2015-11-12", "2015-11-13", "2015-11-14",
                          "2015-12-25", "2015-12-26", "2015-12-27","2015-12-28","2015-12-29", "2015-12-30","2015-12-31", "2016-01-01","2016-01-02","2016-01-03",
                          "2016-01-20","2016-01-21","2016-01-22",
                          "2016-02-01","2016-02-02",
                          "2016-02-20","2016-02-21",
                          "2016-02-14","2016-02-15",
                          "2016-03-07","2016-03-08","2016-03-09",
                          "2016-05-25","2016-05-26","2016-05-27"))

#Mark all the special day sales
ecom_data_updated$spec <- ifelse(ecom_data_updated$order_date %in% special_days, "Y", "N")

#Create column to mark which special day it was
ecom_data_updated$specDay <- 'RegularSale'


ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[1:2]), "EidRathayatraSale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[3:5]), "IndependenceDaySale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[6:8]), "RakshabandanSale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[9:11]), "DausseraSale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[12:19]), "BigDiwaliSale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[20:29]), "ChristmasNewYearSale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[30:32]), "RepublicDaySale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[33:34]), "BEDSale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[35:36]), "FHSDSale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[37:38]), "ValentineDaySale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[39:41]), "BSDSale",ecom_data_updated$specDay)
ecom_data_updated$specDay <- ifelse(ecom_data_updated$order_date %in% (special_days[42:44]), "PacmanDaySale",ecom_data_updated$specDay)

#Holiday details
holidays <- special_days
week <- lubridate::isoweek(ymd(holidays))

table(week)
#3  5  6  7 10 21 29 33 34 35 42 45 46 52 53 
#3  2  1  3  3  3  2  2  1  3  3  2  6  3  7 

Year <- lubridate::isoyear(ymd(holidays))
table(Year)
#2015 2016 
#29   15 

details_hols <- data.frame(cbind(Year, week))
details_hols$holidays <- holidays
details_hols$count <- 1
details_hols$week <- ifelse(details_hols$Year == 2016, details_hols$week +53, details_hols$week)
details_hols <- aggregate(count ~ Year+week, details_hols, sum)

View(details_hols)
ecom_data_updated <- merge(ecom_data_updated, details_hols, by = c("Year","week"), all.x = TRUE)
ecom_data_updated$Week_date <- as.Date(cut(ecom_data_updated$order_date,breaks = "week", start.on.monday = FALSE))


ecom_final <- ecom_data_updated #storing the data to retain the state

####################################################################################
table(ecom_data_updated$product_procurement_sla) # many negative values
ecom_data_updated$product_procurement_sla <- ifelse(as.numeric(ecom_data_updated$product_procurement_sla < 0), 0, ecom_data_updated$product_procurement_sla)


###################################################################################################
# KPI's
###################################################################################################

#1. KPI - Payment mode
table (ecom_data_updated$s1_fact.order_payment_type) #COD is more
ecom_data_updated$prepaid <- ifelse(ecom_data_updated$s1_fact.order_payment_type == "Prepaid", 1,0)

#2. KPI - List Price
#Get the list price for all the products
ecom_data_updated$sellPrice <- ecom_data_updated$gmv/ecom_data_updated$units

#3 KPI - Offer Price
#Offer price
ecom_data_updated$PromoOffer <- (ecom_data_updated$product_mrp - ecom_data_updated$sellPrice)/ecom_data_updated$product_mrp

#4. KPI  - Prepaid order percentage
# Total order 
Order_total <-  aggregate(prepaid ~ Year+Month+week, data = ecom_data_updated, FUN=NROW )
#Prepaid order
prepaid_total <- aggregate(prepaid ~ Year+Month+week, data=ecom_data_updated, FUN=sum)
#Percentage of prepaid order 
merged_total <- merge(Order_total, prepaid_total, by=c("Year", "Month", "week"), all.x=TRUE)
merged_total$prepaidPerOrder <- merged_total$prepaid.y/merged_total$prepaid.x

#Adding the above data to ecom data
ecom_data_updated <- merge(ecom_data_updated, merged_total, by = c("Year", "Month", "week"), all.x = TRUE)


#############EDA###############################

#weekly units sold for each of the 3 sub categories
weekly_product_units_sold <- aggregate(units~Week_date + product_analytic_sub_category, ecom_data_updated, sum, na.rm=TRUE)

weekly_product_units_sold <- subset(weekly_product_units_sold, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" | 
                                      product_analytic_sub_category == "GamingAccessory")

ggplot(weekly_product_units_sold, aes(x=Week_date,y=units, fill=as.factor(product_analytic_sub_category))) + geom_bar(stat="identity",position = "dodge", width = 4) + 
  labs(x="Weeks",y="Number of Units Sold") + 
  ggtitle("Weekly sales for each product sub category") + scale_fill_manual("Product Sub-Category: ", values = c("CameraAccessory" = "blue4", "GamingAccessory" = "orange", "HomeAudio" = "red3")) +
  theme(legend.justification="center",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))

#weekly product units sold by payment type

weekly_product_units_sold_by_payment_type <- aggregate(units~Week_date + + s1_fact.order_payment_type, ecom_data_updated, sum, na.rm=TRUE)

ggplot(weekly_product_units_sold_by_payment_type, aes(x=Week_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity") + 
  labs(x="Weeks",y="Number of Units Sold") + 
  ggtitle("Weekly Product Units Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
  theme(legend.justification="center", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))

#weekly gmv for each of the 3 sub categories

camgmv <- ecom_data_updated[ecom_data_updated$product_analytic_sub_category == "CameraAccessory", ]
gamegmv <- ecom_data_updated[ecom_data_updated$product_analytic_sub_category == "GamingAccessory", ]
hagmv <- ecom_data_updated[ecom_data_updated$product_analytic_sub_category == "HomeAudio", ]

weekly_gmv <- aggregate(gmv~Week_date + product_analytic_sub_category, ecom_data_updated, sum, na.rm=TRUE)

weekly_gmv <- subset(weekly_gmv, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" | 
                                      product_analytic_sub_category == "GamingAccessory")

weekly_gmv_sale <- aggregate(gmv~specDay, ecom_data_updated, sum, na.rm=TRUE)

ggplot(weekly_gmv_sale, aes(x=as.factor(specDay), y=gmv)) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=gmv), vjust=-0.3, size=3.5) +  labs(x="specDay",y="GMV") + 
  ggtitle("GMV for different sale days") + theme(legend.position="none",axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 

# weekly gmv for sale vs non sale day
weekly_gmv_spec <- aggregate(gmv~spec, ecom_data_updated, sum, na.rm=TRUE)

ggplot(weekly_gmv_spec, aes(x=as.factor(spec), y=gmv)) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=gmv), vjust=-0.3, size=3.5) +  labs(x="Sale Day?",y="GMV") + 
  ggtitle("GMV for sale day vs normal day") + theme(legend.position="none",axis.text.x = element_text(colour = "black", vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 

# Weekly GMV for each product sub category
ggplot(weekly_gmv, aes(x=Week_date,y=gmv, color=as.factor(product_analytic_sub_category))) + geom_line(stat="identity", size=1) + 
  labs(x="Weeks",y="GMV") + 
  ggtitle("Weekly GMV for each product sub category") + scale_colour_brewer(type = "div") +
  theme(legend.justification="center",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))

##Box Plots to find outliers in Numeric variables
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(ecom_data_updated, aes(x=gmv, y=units))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(ecom_data_updated, aes(x=gmv,y=product_mrp))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(ecom_data_updated, aes(x=gmv,y=ecom_data_updated$`Total Investment`))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(ecom_data_updated, aes(x=gmv, y=ecom_data_updated$sellPrice))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(ecom_data_updated, aes(x=gmv,y=ecom_data_updated$PromoOffer))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(ecom_data_updated, aes(x=gmv,y=ecom_data_updated$sla))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(ecom_data_updated, aes(x=gmv, y=ecom_data_updated$product_procurement_sla))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(ecom_data_updated, aes(x=gmv,y=ecom_data_updated$NPS))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(ecom_data_updated, aes(x=gmv,y=ecom_data_updated$sla))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#EDA for the various KPIs


ggplot(ecom_data_updated, aes(x= ecom_data_updated$week, y= ecom_data_updated$gmv))+geom_line()
#Revenue shoots up during certain weeks. These weeks have promotional offers


ggplot(ecom_data_updated, aes(x= ecom_data_updated$weekday, y= ecom_data_updated$gmv))+geom_line()
#Revenue is same for various days in the week


ggplot(ecom_data_updated, aes(x= ecom_data_updated$specDay, y= ecom_data_updated$gmv))+geom_line()
#SAle on New year and Pacman Day earn highest revenue


ggplot(ecom_data_updated, aes(x= Total_Investment, y= gmv))+geom_line()
#Revenue shoots up when the investment is increased at certain points then again drops down


ggplot(ecom_data_updated, aes(x= ecom_data_updated$s1_fact.order_payment_type, y= ecom_data_updated$gmv))+geom_line()
#Prepaid earns more revenue


ggplot(ecom_data_updated, aes(x= ecom_data_updated$sellPrice, y= ecom_data_updated$gmv))+geom_line()
#SP and Revenue are directly proportional, as SP increases Revenue increases


ggplot(ecom_data_updated, aes(x= ecom_data_updated$PromoOffer, y= ecom_data_updated$units))+geom_line()
#Higher offer gives higher revenue as unit sale increases

#removing unnecessary columns
#ecom_data_cleaned = ecom_data_updated[, -c(4,8,10,11,24,33)]
#ecom_data_cleaned$count = NULL
#ecom_data_cleaned$prepaid.x = NULL
#ecom_data_cleaned$prepaid.y = NULL
ecom_final1 <- ecom_data_updated
ecom_data_cleaned <- ecom_data_updated
ecom_data_cleaned$NPS = as.numeric(ecom_data_cleaned$NPS)
ecom_data_cleaned$spec = ifelse(ecom_data_cleaned$spec == "Y", 1,0)

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



#For home audio
homeaudio_ecom <- ecom_data_cleaned[ecom_data_cleaned$product_analytic_sub_category == "HomeAudio", ]

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







