library(dplyr)
nyc_data<-read.csv("nyc-rolling-sales.csv",stringsAsFactors = FALSE,na.strings = c("NA"," -  ","","Unknown"))
length(nyc_data$SALE.PRICE)
#cleaning data:
#remove the useless index column
nyc_data<-nyc_data[2:ncol(nyc_data)]

#change the column names. the original ones are all in upper case, difficult to read and doesn't comply with the naming convention
colnames(nyc_data)<-c("borough","neighborhood","building_class_category",
                      "tax_class_at_present","block","lot","ease_ment",
                      "building_class_at_present","address","apartment_number",
                      "zipcode","residential_units","commercial_units","total_units",
                      "land_square_feet","gross_square_feet","year_built","tax_class_at_time_of_sale",
                      "building_class_at_time_of_sale","sale_price","sale_date")

##all values in ease_ment is NA, so no need to keep the column
nyc_data$ease_ment<-NULL

##change values in borough into characters
nyc_data$borough<-as.factor(nyc_data$borough)
levels(nyc_data$borough)<-c("Manhattan","Bronx","Brooklyn","Queens","Staten Island")

#remove rows without a listed sales price or those with a price that doesn't make any sense (e.g.,1,10).
data_to_predict=nyc_data[which(complete.cases(nyc_data[,"sale_price"])==FALSE),]
length(data_to_predict[,"sale_price"])
nyc_data<-nyc_data[complete.cases(nyc_data[,"sale_price"]),]


##ok now we have the data set that there is no price information
#and we can actually use data to do some prediction to fill the empty prices
#ok Im pretty  sure that Im right


toolittleorlarge=nyc_data[which(nyc_data$sale_price<10000 | nyc_data$sale_price>=5000000),]

length(toolittleorlarge[,"sale_price"])
data_to_predict<-rbind(data_to_predict, toolittleorlarge)
length(data_to_predict[,"sale_price"])
data_to_predict$sale_price=NA
#OK now we have the data_to_predict as the prediction dataset

#accoding to the data source: a $0 sale indicates that there was a transfer of ownership
#without cash consideration. There can be a number of reasons for a $0 sale including
#transfers of ownership from parents to children. we will exclude those.
nyc_data<-nyc_data[which(nyc_data$sale_price!=0),]
#there are still rows with sale price $1, $10, etc, not sure what number can be the threshold

#and only focus on rsidential properties by filtering out by building code.
#building codes start with "A,B,C" represent residence
nyc_data<-nyc_data[which(substr(nyc_data$building_class_at_time_of_sale,1,1) %in% c("A","B","C")),]
data_to_predict<-data_to_predict[which(substr(data_to_predict$building_class_at_time_of_sale,1,1) %in% c("A","B","C")),]
#convert sale_date into week number,month,and quarter
nyc_data$sale_week_num<-strftime(nyc_data$sale_date,format="%V")
data_to_predict$sale_week_num<-strftime(data_to_predict$sale_date,format="%V")

nyc_data$sale_month<-as.integer(substr(nyc_data$sale_date,6,7))
data_to_predict$sale_month<-as.integer(substr(data_to_predict$sale_date,6,7))


nyc_data$sale_quarter<-nyc_data$sale_month
data_to_predict$sale_quarter<-data_to_predict$sale_month

nyc_data$sale_quarter[which(nyc_data$sale_quarter %in% c(1,2,3))]=1
data_to_predict$sale_quarter[which(data_to_predict$sale_quarter %in% c(1,2,3))]=1


nyc_data$sale_quarter[which(nyc_data$sale_quarter %in% c(4,5,6))]=2
data_to_predict$sale_quarter[which(data_to_predict$sale_quarter %in% c(4,5,6))]=2


nyc_data$sale_quarter[which(nyc_data$sale_quarter %in% c(7,8,9))]=3
data_to_predict$sale_quarter[which(data_to_predict$sale_quarter %in% c(7,8,9))]=3


nyc_data$sale_quarter[which(nyc_data$sale_quarter %in% c(10,11,12))]=4
data_to_predict$sale_quarter[which(data_to_predict$sale_quarter %in% c(10,11,12))]=4


#years so far
nyc_data$years=as.integer(substring(nyc_data$sale_date, 1, 4))-nyc_data$year_built
data_to_predict$years=as.integer(substring(data_to_predict$sale_date, 1, 4))-data_to_predict$year_built


#remove columns
nyc_data$address<-NULL
data_to_predict$address<-NULL


nyc_data$neighborhood<-NULL
data_to_predict$neighborhood<-NULL



nyc_data$block<-NULL
data_to_predict$block<-NULL


nyc_data$lot<-NULL
data_to_predict$lot<-NULL


nyc_data$building_class_at_present<-NULL
data_to_predict$building_class_at_present<-NULL


nyc_data$tax_class_at_present<-NULL
data_to_predict$tax_class_at_present<-NULL


nyc_data<-nyc_data[which(nyc_data$sale_price>=10000 & nyc_data$sale_price<5000000),]


nyc_data<-nyc_data[which(nyc_data$year_built>1800),]
data_to_predict<-data_to_predict[which(data_to_predict$year_built>1800),]


nyc_data$building_class_at_time_of_sale<-substr(nyc_data$building_class_at_time_of_sale,1,1)
data_to_predict$building_class_at_time_of_sale<-substr(data_to_predict$building_class_at_time_of_sale,1,1)

######################################################################################################
nyc_data<-group_by(nyc_data,borough)
data_to_predict<-group_by(data_to_predict,borough)

summ<-summarise(nyc_data,gross_area_mean=mean(gross_square_feet,na.rm=T))
summp<-summarise(data_to_predict,gross_area_mean=mean(gross_square_feet,na.rm=T))


summ1<-summarize(nyc_data,land_area_mean=mean(land_square_feet,na.rm=T))
summ1p<-summarize(data_to_predict,land_area_mean=mean(land_square_feet,na.rm=T))


nyc_data<-merge(nyc_data,summ,by="borough")
data_to_predict<-merge(data_to_predict,summp,by="borough")



nyc_data<-merge(nyc_data,summ1,by="borough")
data_to_predict<-merge(data_to_predict,summ1p,by="borough")

#fill the na value using the average of the group
for (i in 1:nrow(nyc_data)){
  if (is.na(nyc_data$land_square_feet[i])){
    nyc_data$land_square_feet[i]=nyc_data$land_area_mean[i]
  }
  else {}
}

#fill the na value using the average of the group
for (i in 1:nrow(data_to_predict)){
  if (is.na(data_to_predict$land_square_feet[i])){
    data_to_predict$land_square_feet[i]=data_to_predict$land_area_mean[i]
  }
  else {}
}




for (i in 1:nrow(nyc_data)){
  if (is.na(nyc_data$gross_square_feet[i])){
    nyc_data$gross_square_feet[i]=nyc_data$gross_area_mean[i]
  }
  else {}
}

for (i in 1:nrow(data_to_predict)){
  if (is.na(data_to_predict$gross_square_feet[i])){
    data_to_predict$gross_square_feet[i]=data_to_predict$gross_area_mean[i]
  }
  else {}
}
##############
data_to_predict
nyc_data
######################
#data cleaning complete























