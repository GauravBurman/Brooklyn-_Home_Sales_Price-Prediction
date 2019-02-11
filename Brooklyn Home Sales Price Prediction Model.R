---
  title: "Brooklyn Home Sales Price Predictive Model"
author: "Gaurav Burman"
date: "December 4, 2018"
output: html_document
---
  #Change the path before running the code chunk
  path = "C:/Users/gaura/Downloads/TAMU/613/project"
setwd(path)
install.packages("data.table")
install.packages("bit64")
library(data.table)
library(bit64)
data = fread("brooklyn_sales_map.csv")
dim(data)

#Writing the data in dataframe format and finding the number and percentage of missing values for each attribute. 
data = as.data.frame(data)
NAs = numeric()
percent_NAs = numeric()
for ( i in 1:ncol(data)){
  NAs[i] =sum(is.na(data[,i]))
  percent_NAs[i] = (NAs[i]/390883)*100
}
Missing_values= data.frame(Variable_Name = names(data), Total_Missing_Values = NAs, Percent_Values_missing =percent_NAs)
Missing_values

data_stage2 = subset(data, select = -c(borough, Borough, UnitsRes, UnitsTotal, LotArea, BldgArea, BldgClass, Easements, easement, OwnerType, building_class_category, ZipCode, YearBuilt, MAPPLUTO_F, PLUTOMapID, SHAPE_Leng, SHAPE_Area, Address, EDesigNum,Version, Sanborn, ZoneMap, ZMCode, HistDist,Landmark, APPDate, FIRM07_FLA, PFIRM15_FL, Ext, AreaSource, sale_date,ZoneDist2, ZoneDist3, ZoneDist4, Overlay1, Overlay2, SPDist1, SPDist2, SPDist3, LtdHeight ))      #removing duplicate, irrelevant attributes and ones above 75% NAs
dim(data_stage2)   #dimensions of data_stage2

transferred_properties = which(data_stage2$sale_price == 0)        #observations with sale price = 0 removed
data_stage2 = data_stage2[-(transferred_properties),]
dim(data_stage2)
data_stage2 = subset(data_stage2, select = -c(OwnerName))       #OwnerName variable removed
str(data_stage2)			#structure of object data_stage2

# converting necessary attributes to factors
data_stage2$neighborhood = as.factor(data_stage2$neighborhood)  
data_stage2$tax_class = as.factor(data_stage2$tax_class)
data_stage2$building_class = as.factor(data_stage2$building_class)
data_stage2$address = as.factor(data_stage2$apartment_number)
data_stage2$building_class_at_sale =  as.factor(data_stage2$building_class_at_sale)
data_stage2$FireComp = as.factor(data_stage2$FireComp)
data_stage2$SanitSub = as.factor(data_stage2$SanitSub)
data_stage2$ZoneDist1 = as.factor(data_stage2$ZoneDist1)
data_stage2$SplitZone = as.factor(data_stage2$SplitZone)
data_stage2$IrrLotCode = as.factor(data_stage2$IrrLotCode)
data_stage2$apartment_number = as.factor(data_stage2$apartment_number)
str(data_stage2)

data_stage2 = subset(data_stage2, select = -c(BBL, APPBBL))          #removing BBL and APPBBL
summary(data_stage2$address)       
summary(data_stage2$apartment_number)
data_stage2 = subset(data_stage2, select = -c(address, apartment_number))      #removing address & apartment number

no_NA_data = na.omit(data_stage2)           #removed observations with missing values
dim(no_NA_data)			
hist(no_NA_data$sale_price)
quantile(no_NA_data$sale_price,c(0.1,0.9))    #to check top 10 and 90 percentiles
no_NA_data_trimmed = subset(no_NA_data, sale_price>142524 & sale_price<1250500)     #observations above 90 & below 10 percentiles remove
dim(no_NA_data_trimmed)
hist(no_NA_data_trimmed$sale_price) # plotting histogram for trimmed data
str(no_NA_data_trimmed)

boxplot(sale_price~ neighborhood, no_NA_data_trimmed, col = "blue")
dt = data.table(no_NA_data_trimmed)
nb_class =as.data.frame( dt[,list(mean= mean(sale_price)), by = neighborhood])
nb_class
#creating new attribute as category of neighborhood
nb_class$category = ifelse(nb_class$mean>300000 & nb_class$mean<452407.5, "low price neighborhood",ifelse  
                           (nb_class$mean>452407.5 & nb_class$mean<554618.1, "medium price neighborhood",ifelse
                             (nb_class$mean>554618.1 & nb_class$mean<631312.3, "High price neighborhood", "Expensive Neighborhood")))

#Univariate analysis of different attributes
no_NA_data_trimmed = merge(no_NA_data_trimmed, nb_class[,c("neighborhood","category")])
boxplot(sale_price~category , no_NA_data_trimmed, col= "red")
boxplot(sale_price~tax_class, no_NA_data_trimmed)
no_NA_data_trimmed$tax_class = ifelse ( no_NA_data_trimmed$tax_class %in% c("1","1A", "1B","1C"),1, ifelse(no_NA_data_trimmed$tax_class %in% c("2","2A", "2B","2C"),2,
                                                                                                           ifelse(no_NA_data_trimmed$tax_class ==3,3, ifelse(no_NA_data_trimmed$tax_class == 4,4,0)))) 
boxplot(sale_price~tax_class, no_NA_data_trimmed)
plot(no_NA_data_trimmed$block, no_NA_data_trimmed $sale_price)
plot(no_NA_data_trimmed$lot, no_NA_data_trimmed $sale_price)
no_NA_data_trimmed$type_of_lot = ifelse( no_NA_data_trimmed$lot %in% 1:999, "Traditional Tax Lots", "Condominium Unit Lots")
boxplot(sale_price~ type_of_lot, no_NA_data_trimmed)
boxplot(sale_price~building_class, no_NA_data_trimmed, col = "blue")

#checking attribute building_class
bc = data.frame(Building_Class = levels(no_NA_data_trimmed$building_class), count = tabulate(no_NA_data_trimmed$building_class)) 
bc[order(bc$count),]
no_NA_data_trimmed$building_class = factor(no_NA_data_trimmed$building_class)
bc = data.frame(building_class = levels(no_NA_data_trimmed$building_class), count = tabulate(no_NA_data_trimmed$building_class)) 
bc[order(bc$count),]

no_NA_data_trimmed =  merge (no_NA_data_trimmed, bc , by = "building_class")
no_NA_data_trimmed = subset(no_NA_data_trimmed, no_NA_data_trimmed$count>100)
no_NA_data_trimmed$building_class = factor (no_NA_data_trimmed$building_class)
boxplot(sale_price~building_class,no_NA_data_trimmed, col= "gold")
plot(no_NA_data_trimmed$zip_code, no_NA_data_trimmed$sale_price )

no_NA_data_trimmed =  subset(no_NA_data_trimmed , zip_code != 0)
no_NA_data_trimmed$building_class = factor(no_NA_data_trimmed$building_class)
plot(no_NA_data_trimmed$zip_code, no_NA_data_trimmed$sale_price )
plot( no_NA_data_trimmed$residential_units, no_NA_data_trimmed$sale_price)
plot( no_NA_data_trimmed$commercial_units, no_NA_data_trimmed$sale_price)
plot(no_NA_data_trimmed$total_units, no_NA_data_trimmed$sale_price)
plot(no_NA_data_trimmed$land_sqft, no_NA_data_trimmed$sale_price)

no_NA_data_trimmed =  subset(no_NA_data_trimmed, year_built > 1800 )          #removing observations before 1800s
no_NA_data_trimmed$building_class = factor(no_NA_data_trimmed$building_class)      #changing variables to factors
plot(no_NA_data_trimmed$year_built, no_NA_data_trimmed$sale_price)
boxplot(sale_price~ tax_class_at_sale, no_NA_data_trimmed)
no_NA_data_trimmed$building_class_at_sale = factor(no_NA_data_trimmed$building_class_at_sale)
boxplot(sale_price~ building_class_at_sale, no_NA_data_trimmed, col = "green")
pairs(sale_price ~ year_of_sale+CT2010+ CB2010+CD, no_NA_data_trimmed)
names(no_NA_data_trimmed)

final_data = subset ( no_NA_data_trimmed, select = c(sale_price, building_class, category, tax_class, type_of_lot,zip_code, residential_units, gross_sqft,year_built, year_of_sale, CD, Council, LandUse,ResArea, GarageArea, StrgeArea, NumBldgs,NumFloors, BldgFront, BldgDepth, ProxCode,IrrLotCode, LotType, AssessTot, ExemptTot, YearAlter1, YearAlter2, BuiltFAR, XCoord, YCoord) )  #final data with 17 variables
attach(no_NA_data_trimmed)

#Decison tree
library(tree)
set.seed(1)
tree.brooklyn=tree(sale_price~. -building_class, data=final_data)
summary(tree.brooklyn)
plot(tree.brooklyn)
text(tree.brooklyn, pretty=0)

#Boosting to check variable importance
install.packages("gbm")
final_data$category=as.factor(final_data$category)
final_data$type_of_lot=as.factor(final_data$type_of_lot)
library (gbm)
set.seed (1)
boost.final_data =gbm(sale_price???. -tax_class,data=final_data, distribution="gaussian",n.trees =5000 ,interaction.depth =4)
summary(boost.final_data)
#Removed the variable which are less important and obtained final data  with 17 attributes 
final_data2=subset ( final_data, select = c(sale_price, building_class, category,zip_code, gross_sqft,year_built, year_of_sale, CD, Council,ResArea, BldgFront, BldgDepth, AssessTot, ExemptTot, BuiltFAR, XCoord, YCoord) )

#Using 70% of data as training set to fit the model and test the model on the remaining 30% data 
set.seed(100)
train=sample(1:nrow(final_data2), 0.7*(nrow(final_data2)))
test=final_data2[-train,"sale_price"]
dim(final_data2[train,])
dim(final_data2[-train,])
n=45163

#Fitting the boosting model
set.seed (1)
boost.final_data2 =gbm(sale_price???.,data=final_data2[train,], distribution="gaussian",n.trees =5000 ,interaction.depth =4)
summary(boost.final_data2)
p=length(boost.final_data2$var.names)
#Predicting sales price from he model
predict.boost=predict(boost.final_data2,newdata = final_data2[-train ,],
                      n.trees =5000)
mean(( predict.boost -test)^2)
#Calculating R-Square Adjusted 
actual=final_data2$sale_price
r2.gradient_boosting=1-((sum((actual[-train] - predict.boost)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
r2.gradient_boosting

#Fit the random forest model
final_data2$category=as.factor(final_data2$category)
library(randomForest)
set.seed(1)
bag.brooklyn=randomForest(sale_price~.,data=final_data2, subset=train, mtry=4, ntree=10, importance=TRUE)
bag.brooklyn
p=16
#Predict sales price from the model
predict.bag = predict (bag.brooklyn,newdata =final_data2[-train ,])
plot(predict.bag ,final_data2$sale_price[-train])
abline (0,1)
mean((predict.bag -final_data2$sale_price[-train])^2)
#Calulating R-Square ajusted 
r2.random_forest =1-((sum((actual[-train] - predict.bag)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
r2.random_forest

#Fitting KNN regression
install.packages("FNN")
R2.knn = numeric()
x = model.matrix(sale_price~., final_data2)[,-1]
y= final_data2$sale_price
knn001= FNN::knn.reg(train = x[train,], test = x[-train,], y = y[train], k = 1)
knn005= FNN::knn.reg(train = x[train,], test = x[-train,], y = y[train], k = 5)
knn010= FNN::knn.reg(train = x[train,], test = x[-train,], y = y[train], k = 10)
knn050= FNN::knn.reg(train = x[train,], test = x[-train,], y = y[train], k = 50)
knn060= FNN::knn.reg(train = x[train,], test = x[-train,], y = y[train], k = 60)
knn100= FNN::knn.reg(train = x[train,], test = x[-train,], y = y[train], k = 100)
knn1000= FNN::knn.reg(train = x[train,], test = x[-train,], y = y[train], k = 1000)
#Calculating R-square adjusted
R2.knn[1] =1 - ((sum((actual[-train] - knn001$pred)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
R2.knn[2] =1 - ((sum((actual[-train] - knn005$pred)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
R2.knn[3] =1 - ((sum((actual[-train] - knn010$pred)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
R2.knn[4] =1 - ((sum((actual[-train] - knn050$pred)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
R2.knn[5] =1 - ((sum((actual[-train] - knn060$pred)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
R2.knn[6] =1 - ((sum((actual[-train] - knn100$pred)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
R2.knn[7] =1 - ((sum((actual[-train] - knn1000$pred)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
R2.knn

#Fit the  multiple linear regression model
install.packages("boot")
library(boot)
set.seed(10)
lm.fit2=lm(sale_price~.,data=final_data2[train,])
summary(lm.fit2)
#Predict the model
p=16
predict.lm2=predict(lm.fit2 , final_data2[-train,])
r2.lm=1-((sum((actual[-train] - predict.lm2)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1)))
r2.lm

library(glmnet)
grid = 10^seq(10,-2, length = 100)
#fitting ridge  regression model
ridge.sale_price = glmnet (x[train,],y[train],alpha= 0 , lambda = grid)
coef(ridge.sale_price)
p=12
#selecting best value of lambda for ridge
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train], alpha = 0)
plot(cv.out)
bestlam.ridge = cv.out$lambda.min
bestlam.ridge
#Predict sales price using ridge regression
ridge.pred = predict (ridge.sale_price,s = bestlam.ridge, newx = x[-train,])
mean((ridge.pred - y[-train])^2)
#Calulating R-Square adjusted
R2.ridge =1-((sum((actual[-train] - ridge.pred)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1))) 
R2.ridge

#fitting the lasso regression
lasso.sale_price = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
names(lasso.sale_price)
#selecting best value of lambda for lasso
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train], alpha = 1)
plot(cv.out)
bestlam.lasso = cv.out$lambda.min
bestlam.lasso
#Predict sales price using lasso regression
lasso.pred = predict (lasso.sale_price, s = bestlam.lasso, newx = x[-train,])
mean((lasso.pred - y[-train])^2)
#Calculating R-Square adjusted
R2.lasso = 1-((sum((actual[-train] - lasso.pred)^2)/(n-p-1))/(sum((actual[-train] - mean(actual[-train]))^2)/(n-1))) 
R2.lasso

Result_Comparison = data.frame( "Method" = c("Multiple Linear", "Ridge ", "Lasso", "KNN", "Boosting", "RandomForest"), "Adjusted_R-squared_Values"= c(r2.lm,R2.ridge,R2.lasso,R2.knn[4],r2.gradient_boosting,r2.random_forest))  #Comparing results

barplot(Result_Comparison$Adjusted_R.squared_Values, col= "dodgerblue", names.arg = Result_Comparison$Method, main = "Result comparison", cex.names = 0.8, ylab = "Adjusted R-squared Values",xlab="Method applied for prediction") #Barplot
