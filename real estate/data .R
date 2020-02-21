library(dplyr)
source("DataAnalyticsFunctions.R")
nyc_data <- read.csv("nyc-rolling-sales.csv",stringsAsFactors = FALSE, na.strings = c("NA"," -  ","","Unknown"))
length(nyc_data$SALE.PRICE)

### data cleaning

# remove incorrect index column
nyc_data<-nyc_data[2:ncol(nyc_data)]

# rename columns into standard naming conventions
colnames(nyc_data)<-c("borough","neighborhood","building_class_category",
                      "tax_class_at_present","block","lot","ease_ment",
                      "building_class_at_present","address","apartment_number",
                      "zipcode","residential_units","commercial_units","total_units",
                      "land_square_feet","gross_square_feet","year_built","tax_class_at_time_of_sale",
                      "building_class_at_time_of_sale","sale_price","sale_date")

# all values in ease_ment is NA, so no need to keep the column
nyc_data$ease_ment<-NULL

# change values in borough into characters
nyc_data$borough<-as.factor(nyc_data$borough)
levels(nyc_data$borough)<-c("Manhattan","Bronx","Brooklyn","Queens","Staten Island")

# remove rows without a listed sales price or those with a price that doesn't make any sense (e.g.,1,10).
data_to_predict=nyc_data[which(complete.cases(nyc_data[,"sale_price"])==FALSE),]
length(data_to_predict[,"sale_price"])
nyc_data<-nyc_data[complete.cases(nyc_data[,"sale_price"]),]


### now we have the data set that there is no price information
## and we can actually use data to do some prediction to fill the empty prices

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

#and only focus on residential properties by filtering out by building code.
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

#drop some columns
Dropcolumn<-c('zipcode', 'apartment_number','sale_date','residential_units','commercial_units','gross_square_feet','tax_class_at_time_of_sale','year_built','gross_area_mean','land_area_mean')
nyc_data<-nyc_data[,!(names(nyc_data)%in%Dropcolumn)]
data_to_predict<-data_to_predict[,!(names(data_to_predict)%in%Dropcolumn)]
nyc_data$sale_week_num = as.numeric(nyc_data$sale_week_num)
###################################################
###################################################
###################################################
library(ggplot2)
ggplot(nyc_data[which(nyc_data$total_units < 30),], aes(x = log(land_square_feet), y = sale_price, col = total_units)) + geom_jitter(alpha = 0.5) + facet_grid (building_class_category~borough)
#data cleaning complete
library(foreach)
library(Matrix)
library(glmnet)
Mx<- model.matrix(sale_price ~ ., data=nyc_data)[,-1]
head(Mx)
My<- nyc_data$sale_price
lasso <- glmnet(Mx,My)
lassoCV <- cv.glmnet(Mx,My)
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients", xlab=expression(paste("log(",lambda,")")))
plot(log(lasso$lambda), lasso$beta['total_units',],ylab="Coefficient value", main=paste("Coefficient for total units"),xlab = expression(paste("log(",lambda,")")),type="l")
plot(log(lasso$lambda), lasso$beta['land_square_feet',],ylab="Coefficient value", main=paste("Coefficient for land_square_feet"),xlab = expression(paste("log(",lambda,")")),type="l")
plot(log(lasso$lambda), lasso$beta['boroughQueens',],ylab="Coefficient value", main=paste("Coefficient for Queens"),xlab = expression(paste("log(",lambda,")")),type="l")
plot(log(lasso$lambda), lasso$beta['sale_month',],ylab="Coefficient value", main=paste("Coefficient for sale month"),xlab = expression(paste("log(",lambda,")")),type="l")
plot(log(lasso$lambda), lasso$beta['sale_week_num',],ylab="Coefficient value", main=paste("Coefficient for sale week"),xlab = expression(paste("log(",lambda,")")),type="l")


num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))

features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])

data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)

n <- nrow(nyc_data)
nfold <- 10
OOS <- data.frame(m.null=rep(NA,nfold), m.lr=rep(NA,nfold), L.min=rep(NA,nfold), L.1se=rep(NA,nfold), 
                  PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold),m.tree=rep(NA,nfold))
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
foldid

### k-fold cross validation
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  # null model
  mean(nyc_data$sale_price)
  m.null <-lm( mean(nyc_data$sale_price)~ 0, data=nyc_data, subset=train)
  pred.null <-  mean(nyc_data$sale_price)
  pred.null
  OOS$m.null[k] <- R2(y=nyc_data$sale_price[-train], pred=pred.null)
  
  # linear regression
  m.lr <-lm(sale_price ~ ., data=nyc_data, subset=train)
  pred.lr <- predict(m.lr, newdata=nyc_data[-train,], type="response")
  OOS$m.lr[k] <- R2(y=nyc_data$sale_price[-train], pred=pred.lr)
  
  #regression tree
  library(MASS)
  library(rpart)
  m.tree <- rpart(sale_price ~ borough+building_class_category+total_units+land_square_feet+
                    building_class_at_time_of_sale+sale_week_num+sale_month+sale_quarter+years, data=nyc_data,subset=train)
  
  
  m.tree <- rpart(sale_price ~ borough+building_class_category+total_units+land_square_feet+
                    building_class_at_time_of_sale+sale_week_num+sale_month+sale_quarter+years, data=nyc_data,subset=train)
  pred.tree <- predict(m.tree, newdata=nyc_data[-train,])
  OOS$m.tree[k] <- R2(y=nyc_data$sale_price[-train], pred=pred.tree)
  summary(m.tree)

  
  ### lasso and post-lasso prediction
  rmin <- glm(My~., data=data.min, subset=train)
  if ( length(features.1se) == 0){  r1se <- glm(sale_price~1, data=nyc_data, subset=train) 
  } else {r1se <- glm(My~., data=data.1se, subset=train)
  }
  
  # lasso estimate
  lassomin  <- glmnet(Mx[train,],My[train], lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], lambda = lassoCV$lambda.1se)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  
  OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin)
  OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se)
  
  # post-lasso estimate
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  
  OOS$PL.min[k] <- R2(y=My[-train], pred=predmin)
  OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se)
  
  print(paste("Iteration",k,"of",nfold,"completed"))
  
}
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
# plot OOS R^2 of each model for each fold
m.OOS <- as.matrix(OOS)
m.OOS
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=0.01, yjust=1.0),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))

# average OOS R^2 across all folds
R2performance <- colMeans(OOS)
barplot(t(R2performance), las=2,xpd=FALSE, ylim=c(0,.5) , xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))

### predict obama margin percentage on test data
Dropcolumn2<-c('sale_price','zipcode', 'apartment_number','sale_date','residential_units','commercial_units','gross_square_feet','tax_class_at_time_of_sale','year_built','gross_area_mean','land_area_mean')
data_to_predict<-data_to_predict[,!(names(data_to_predict)%in%Dropcolumn2)]
newMx<-model.matrix( ~. , data=data_to_predict)[,-1]
colnames(newMx)
predtree <-predict(m.tree,newdata=data_to_predict)
data_to_predict$sale_price <- predtree
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
rpart.plot(m.tree,type=4)
install.packages("rpart.plot")
library(rpart.plot)

















