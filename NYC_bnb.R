##############################
## New York City bnb Analysis
## Jokobus
##############################

rm(list=ls())
Sys.setenv(LANG="en")

bnb = read.csv("bnb_data.csv")
library(dplyr)
library(ggplot2)
library(leaflet)
library(randomForest)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(doParallel)

#########################
# Part 1 Data Analysis and Visualization

head(bnb)


## 2.1
bnb %>% group_by(neighbourhood) %>% count() %>% arrange(desc(n)) %>% head()
bnb %>% group_by(neighbourhood_group) %>% count() %>% arrange(desc(n))

#bnb_rooms$most_common_room_type = 
bnb %>% group_by(neighbourhood) %>% group_by(room_type) %>% count %>% arrange(desc(n))

## 2.2
bnb %>% group_by(neighbourhood) %>% summarize(mean_price=mean(price, na.rm=TRUE))

# Preprocess the price column -> became obsolete
bnb_preproc = bnb
# bnb_preproc$price <- gsub('\\$', '', bnb_preproc$price)
# bnb_preproc$price <- gsub('', '', bnb_preproc$price)
# bnb_preproc$price <- gsub(',', '', bnb_preproc$price)
# bnb_preproc$price = as.numeric(bnb_preproc$price)
bnb_0price_removed = subset(bnb_preproc, price != 0.00)

# 2.2.1
ggplot (bnb_0price_removed, aes(x=longitude, y=latitude)) + geom_point(aes(color=price)) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + scale_color_gradient(low="gray", high="red")
mean(bnb_0price_removed$price)
max(bnb_0price_removed$price)

ggplot (subset(bnb_0price_removed, price >= 500), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.3) + geom_point(aes(color=price)) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + scale_color_gradient(low="gray", high="red")
ggplot (subset(bnb_0price_removed, price >= 900), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.3) + geom_point(aes(color=price)) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + scale_color_gradient(low="gray", high="red")
ggplot (subset(bnb_0price_removed, price == 999), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.3) + geom_point(color="red") + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + scale_color_gradient(low="gray", high="red")
nrow(subset(bnb_0price_removed, price>=500))
nrow(subset(bnb_0price_removed, price>=900))
table(tapply(subset(bnb_0price_removed, price>=900), subset(bnb_0price_removed, price>=900)$neighbourhood_group))
184/226
nrow(subset(bnb_0price_removed, price>=999))
table(tapply(subset(bnb_0price_removed, price>=999), subset(bnb_0price_removed, price>=999)$neighbourhood_group))
nrow(subset(bnb_0price_removed, price>=1000))

# 2.2.2
budget = 50
ggplot (subset(bnb_0price_removed, price <= budget), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.3) + geom_point(aes(size=budget-price, color=price)) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + scale_color_gradient(low="blue", high="gray")
budget = 10
ggplot (bnb_0price_removed, aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.3) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + geom_point(data=subset(bnb_0price_removed, price <= budget), size=5, color="green")


ggplot (bnb_preproc, aes(x=longitude, y=latitude)) + geom_point(data=bnb_preproc, color="white", alpha=0.3) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + geom_point(data=subset(bnb_preproc, price == 0), size=5, color="green")

# 2.2.3
sort(tapply(bnb_0price_removed$price, bnb_0price_removed$neighbourhood, mean), decreasing=TRUE)
nrow(subset(bnb_0price_removed, neighbourhood=="Grant City")) # to check for: how many listings in neighbourhood

# 2.2.4
# for this task I used leaflet to add those fancy marker signs into the map and see all POIs in the map
bnb_0price_removed$LogPrice = log(bnb_0price_removed$price)
bnb_0price_removed$Lat = round(bnb_0price_removed$latitude, 3)
bnb_0price_removed$Lon = round(bnb_0price_removed$longitude, 3)
pricemax = max(bnb_0price_removed$price)
pal = colorNumeric("YlOrRd", domain = bnb_0price_removed$price)

leaflet(bnb_0price_removed) %>% addTiles() %>% addCircles(lng=~Lon, lat=~Lat,radius = 50, stroke=FALSE, fillColor=~pal(price), fillOpacity = 0.8, label=~paste0(neighbourhood,": ",format(round(price),big.mark=","), " (Lat,Lon): ",Lat,", ",Lon)) %>% addLegend(pal=pal, values=~price, opacity = 0.8) %>% 
addMarkers(lng = ~-73.9860, lat = ~40.7558, icon = list(iconUrl = 'https://icons.iconarchive.com/icons/icons-land/vista-map-markers/256/Map-Marker-Marker-Outside-Azure-icon.png', iconSize = c(30, 30))) %>% 
addMarkers(lng = ~-73.992, lat = ~40.75, icon = list(iconUrl = 'https://icons.iconarchive.com/icons/icons-land/vista-map-markers/256/Map-Marker-Marker-Outside-Chartreuse-icon.png', iconSize = c(30, 30))) %>% 
addMarkers(lng = ~-73.988, lat = ~40.742, icon = list(iconUrl = 'https://icons.iconarchive.com/icons/icons-land/vista-map-markers/256/Map-Marker-Marker-Outside-Pink-icon.png', iconSize = c(30, 30)))


# 2.2.5
ggplot(bnb_0price_removed, aes(x=longitude, y=latitude)) + geom_point(aes(color=room_type)) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92))

mean(subset(bnb_0price_removed, room_type=="Entire home/apt")$price)
mean(subset(bnb_0price_removed, room_type=="Hotel room")$price)
mean(subset(bnb_0price_removed, room_type=="Private room")$price)
mean(subset(bnb_0price_removed, room_type=="Shared room")$price)

nrow(subset(bnb_0price_removed, room_type=="Entire home/apt"))
nrow(subset(bnb_0price_removed, room_type=="Hotel room"))
nrow(subset(bnb_0price_removed, room_type=="Private room"))
nrow(subset(bnb_0price_removed, room_type=="Shared room"))

ggplot(subset(bnb_0price_removed, price >= 900), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.2) + geom_point(aes(color=room_type)) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92))

ggplot(subset(bnb_0price_removed, price <= 50), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.2) + geom_point(aes(color=room_type)) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92))
table(tapply(subset(bnb_0price_removed, price<=50), subset(bnb_0price_removed, price<=50)$room_type))

ggplot(subset(bnb_0price_removed, price <= 10), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.2) + geom_point(aes(color=room_type)) + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92))

# 2.3
mean(subset(bnb_0price_removed, bnb_0price_removed$neighbourhood =="Harlem")$price)
sd(subset(bnb_0price_removed, bnb_0price_removed$neighbourhood =="Harlem")$price)
nrow(subset(bnb_0price_removed, bnb_0price_removed$neighbourhood =="Harlem"))
ggplot (subset(bnb_0price_removed, neighbourhood=="Harlem"), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.3) + geom_point(color="red") + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + scale_color_gradient(low="gray", high="red")
ggplot (subset(bnb_0price_removed, neighbourhood=="Harlem" & price<=50), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.3) + geom_point(color="red") + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + scale_color_gradient(low="gray", high="red")
ggplot (subset(bnb_0price_removed, neighbourhood=="Harlem" & price>=200), aes(x=longitude, y=latitude)) + geom_point(data=bnb_0price_removed, color="white", alpha=0.3) + geom_point(color="red") + scale_x_continuous(limits=c(-74.25, max_long=-73.7)) + scale_y_continuous(limits=c(40.5, 40.92)) + scale_color_gradient(low="gray", high="red")

# 2.4
table(is.na(bnb_0price_removed))
sum(is.na(bnb_0price_removed$host_since)) #0
sum(is.na(bnb_0price_removed$room_type)) #0
sum(is.na(bnb_0price_removed$accommodates)) #0
sum(is.na(bnb_0price_removed$bathrooms_text)) #0
sum(is.na(bnb_0price_removed$bedrooms)) #3751
sum(is.na(bnb_0price_removed$beds)) #890
sum(is.na(bnb_0price_removed$price)) #0
sum(is.na(bnb_0price_removed$number_of_reviews)) #0
sum(is.na(bnb_0price_removed$last_review)) #0
nrow(subset(bnb_0price_removed, is.na(bnb_0price_removed$beds) & is.na(bnb_0price_removed$bedrooms)))

### kNN processing
library(doParallel)
detectCores()
cl<-makePSOCKcluster(8) 

registerDoParallel(cl)
preProc = preProcess(bnb_0price_removed %>% select(host_since, neighbourhood, neighbourhood_group, latitude, longitude, room_type, accommodates, bathrooms_text, bedrooms, beds, number_of_reviews, last_review), method="knnImpute")
bnb_imputed = predict(preProc, bnb_0price_removed)
stopCluster(cl)
summary(bnb_imputed)
head(bnb_imputed)
preProc2 = preProcess(bnb_0price_removed %>% select(host_since, room_type, accommodates, bathrooms_text, bedrooms, beds, number_of_reviews, last_review), method="knnImpute")
bnb_imputed2 = predict(preProc2, bnb_0price_removed)
SSE_beds=sum((bnb_imputed$beds - bnb_imputed2$beds)^2)
RMSE_beds=sqrt(SSE_beds/nrow(bnb_imputed))
RMSE_beds
SSE_bedrooms=sum((bnb_imputed$bedrooms - bnb_imputed2$bedrooms)^2)
RMSE_bedrooms=sqrt(SSE_bedrooms/nrow(bnb_imputed))
RMSE_bedrooms


head(bnb_imputed)
#par(mfrow=c(2,2))
bnb_imputed %>% ggplot() + geom_point(aes(x=host_since, y=price))
bnb_imputed %>% ggplot() + geom_point(aes(x=room_type, y=price))
bnb_imputed %>% ggplot() + geom_point(aes(x=accommodates, y=price))
bnb_0price_removed %>% ggplot() + geom_point(aes(x=bathrooms_text, y=price))
subset(bnb_0price_removed, !is.na(bnb_0price_removed$bedrooms)) %>% ggplot() + geom_point(aes(x=bedrooms, y=price))
subset(bnb_0price_removed, !is.na(bnb_0price_removed$bedrooms)) %>% ggplot() + geom_point(aes(x=bedrooms, y=LogPrice))
bnb_imputed %>% ggplot() + geom_point(aes(x=beds, y=price))
subset(bnb_0price_removed, !is.na(bnb_0price_removed$beds)) %>% ggplot() + geom_point(aes(x=beds, y=price))
subset(bnb_0price_removed, !is.na(bnb_0price_removed$beds)) %>% ggplot() + geom_point(aes(x=beds, y=LogPrice))
bnb_imputed %>% ggplot() + geom_point(aes(x=number_of_reviews, y=price))
bnb_imputed %>%
  mutate(bin_number_of_reviews = cut(number_of_reviews, breaks = seq(0, max(number_of_reviews) + 5, by = 1), include.lowest = TRUE)) %>%
  group_by(bin_number_of_reviews) %>%
  summarise(mean_price = mean(price, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = bin_number_of_reviews, y = mean_price), stat = "identity", fill = "blue", color = "black") +
  labs(x = "Number of Reviews", y = "Mean Price per bin")
bnb_imputed$last_review %>% as.Date(bnb_imputed$last_review, "%m/%d/%Y")
bnb_imputed %>% ggplot() + geom_point(aes(x=last_review, y=price))
bnb_imputed$host_since %>% as.Date(bnb_imputed$host_since, "%m/%d/%Y")


# 2.5
?regex
bnb_0price_removed$host_since_year = gsub("^.*?/","",bnb_0price_removed$host_since)
bnb_0price_removed$host_since_year = gsub("^.*?/","",bnb_0price_removed$host_since_year)
bnb_0price_removed$host_since_year = as.numeric(bnb_0price_removed$host_since_year)

tapply(bnb_0price_removed$host_since_year, bnb_0price_removed$host_since_year, length)
hist(bnb_0price_removed$host_since_year, main="Histogram of new hosts", xlab="Year", ylab="Amount of new hosts")

# 2.6
bnb_0price_removed$revenue = bnb_0price_removed$price * bnb_0price_removed$number_of_reviews
max(bnb_0price_removed$revenue)
bnb_0price_removed[which.max(bnb_0price_removed$revenue),]

max(bnb_0price_removed$number_of_reviews)
bnb_0price_removed[which.max(bnb_0price_removed$number_of_reviews),]


  # no real benefit, but not asked in the task
  boxplot(subset(bnb_0price_removed$revenue, bnb_0price_removed$revenue!=0))
  nrow(subset(bnb_0price_removed, bnb_0price_removed$revenue!=0)) # no NAs
  nrow(subset(bnb_0price_removed, bnb_0price_removed$revenue>=100000))
  80/31764
  boxplot(subset(bnb_0price_removed$revenue, bnb_0price_removed$revenue!=0), ylim=c(0,100000))
  
  hist(bnb_0price_removed$revenue, main="Histogram of revenue" )#, xlab="# of hosts", ylab="revenue")
  hist(subset(bnb_0price_removed$revenue, bnb_0price_removed$revenue!=0), main="Histogram of revenue", xlab="# of hosts", ylab="revenue")
  
# 2.7
0.03*sum(bnb_0price_removed$revenue)



############################################f
# Task 2
bnb_0price_removed = subset(bnb, bnb$price!=0)
bnb_0price_removed$host_since = as.numeric(as.Date(bnb_0price_removed$host_since, "%m/%d/%Y"))
bnb_0price_removed$last_review = as.numeric(as.Date(bnb_0price_removed$last_review, "%m/%d/%Y"))
bnb_0price_removed$id=NULL
bnb_0price_removed$name=NULL
bnb_0price_removed$host_id=NULL
bnb_0price_removed$host_name=NULL
bnb_0price_removed$neighbourhood = as.numeric(factor(bnb_0price_removed$neighbourhood, levels = unique(bnb_0price_removed$neighbourhood)))
bnb_0price_removed$neighbourhood_group = as.numeric(factor(bnb_0price_removed$neighbourhood_group, levels = unique(bnb_0price_removed$neighbourhood_group)))
bnb_0price_removed$room_type = as.numeric(factor(bnb_0price_removed$room_type, levels = unique(bnb_0price_removed$room_type)))
bnb_0price_removed$bathrooms_text = as.numeric(factor(bnb_0price_removed$bathrooms_text, levels = unique(bnb_0price_removed$bathrooms_text)))
head(bnb_0price_removed)

registerDoParallel(cl)
preProc = preProcess(bnb_0price_removed %>% select(host_since, neighbourhood, neighbourhood_group, latitude, longitude, room_type, accommodates, bathrooms_text, bedrooms, beds, number_of_reviews, last_review), method="knnImpute")
stopCluster(cl)
bnb_0price_removed_imputed = predict(preProc, bnb_0price_removed)
# Use the imputed data only for columns with NAs, for others use the original
bnb_0price_removed$host_since=bnb_0price_removed_imputed$host_since
bnb_0price_removed$bedrooms=bnb_0price_removed_imputed$bedrooms
bnb_0price_removed$beds=bnb_0price_removed_imputed$beds
bnb_0price_removed$last_review=bnb_0price_removed_imputed$last_review
sum(is.na(bnb_0price_removed)) #0 yeah

library(caTools)
set.seed(100)
split=sample.split(bnb_0price_removed$price, SplitRatio=0.7)
Train=subset(bnb_0price_removed, split==TRUE)
Test=subset(bnb_0price_removed, split==FALSE)

# 3.1
lin.mod = lm(price~., data=Train)
summary(lin.mod)
lin.mod.pred = predict(lin.mod, newdata=Test)
plot(Test$price, lin.mod.pred)+abline(coef=c(0,1), col="blue")
table(Test$price, lin.mod.pred>500)
lin.mod.SSE=sum((Test$price - lin.mod.pred)^2)
lin.mod.RMSE=sqrt(lin.mod.SSE/nrow(Test))
lin.mod.RMSE

#3.2 
#for (i in ncol(Train)){
#  Train[,i] = as.factor(Train[,i])
#  Test[,i] = as.factor(Test[,i])
#}

#log.mod = glm(price~., data=Train, family=binomial)
#summary(log.mod)
# log model does not want to work

# 3.2 CART Tree
# classification tree

library(rpart)
tree.CART=rpart(price~., data=Train, method="class")
tree.CART.pred = predict(tree.CART, newdata=Test, type="class")
library(rpart.plot)
prp(tree.CART)
plot(Test$price, tree.CART.pred)+abline(coef=c(0,1), col="blue")
# regression tree
tree.CART=rpart(price~., data=Train, method="anova")
tree.CART.pred = predict(tree.CART, newdata=Test)
prp(tree.CART)
plot(Test$price, tree.CART.pred)+abline(coef=c(0,1), col="blue")

CART.SSE=sum((Test$price - tree.CART.pred)^2)
CART.RMSE=sqrt(CART.SSE/nrow(Test))
CART.RMSE

# 3.3 Cross validation (finding best CART-Tree)  
library(caret)
library(e1071)
tr.control=trainControl(method="cv", number=15) # k=15 fold scheme
cp.grid = expand.grid(.cp=(0:100)*0.001)
set.seed(100)
cl<-makePSOCKcluster(4)
registerDoParallel(cl)
all.trees=train(price~., data=Train, method="rpart", trControl=tr.control, tuneGrid=cp.grid)
stopCluster(cl)                
all.trees # find best cp, e.g. cp=0.01
best.tree = all.trees$finalModel
best.tree.pred = predict(best.tree, newdata=Test)
prp(best.tree)
plot(Test$price, best.tree.pred)+abline(coef=c(0,1), col="blue")

best.tree.SSE=sum((Test$price - best.tree.pred)^2)
best.tree.RMSE=sqrt(best.tree.SSE/nrow(Test))
best.tree.RMSE

# 3.4 random Forest
library(randomForest)
library(caret)
library(e1071)

registerDoParallel(cl) 

set.seed(100)
forest.mod=randomForest(price~., data=Train, method="class")
forest.pred = predict(forest.mod, newdata=Test, type="class")

stopCluster(cl)

plot(Test$price, forest.pred)+abline(coef=c(0,1), col="blue")

forest.SSE=sum((Test$price - forest.pred)^2)
forest.RMSE=sqrt(forest.SSE/nrow(Test))
forest.RMSE


## tuned RF not possible due to extreme runtime

#Train_noprice = Train
#Train_noprice$price = NULL

#cl<-makePSOCKcluster(8) 
#registerDoParallel(cl) 
#forest.pred.tuned = tuneRF(x=Train_noprice, y=Train$price, ntree=5000, mtryStart=4, stepFactor=1.5, improve=0.01, trace=FALSE)
#stopCluster(cl)

#forest.tuned.SSE=sum((Test$price - forest.pred.tuned)^2)
#forest.tuned.RMSE=sqrt(forest.tuned.SSE/nrow(Test))
#forest.tuned.RMSE


# Normalize
library(caret)
preproc = preProcess(Train)
normTrain = predict(preproc, Train)
normTest = predict(preproc, Test)                    

set.seed(100)
KMC=kmeans(normTrain, centers=3)
table(KMC$cluster)

library(flexclust)
km.kcca=as.kcca(KMC, normTrain)
clusterTrain=predict(km.kcca)
clusterTest=predict(km.kcca)

# remake submodels
Train1=subset(Train, clusterTrain==1)
Train2=subset(Train, clusterTrain==2)
Train3=subset(Train, clusterTrain==3)

Test1=subset(Test, clusterTest==1)
Test2=subset(Test, clusterTest==2)
Test3=subset(Test, clusterTest==3)


cl2=makePSOCKcluster(8)
registerDoParallel(cl2) 
set.seed(100)
forest.mod1=randomForest(price~., data=Train1, method="class")
forest.pred1 = predict(forest.mod1, newdata=Test1, type="class")
forest.mod2=randomForest(price~., data=Train1, method="class")
forest.pred2 = predict(forest.mod1, newdata=Test1, type="class")
forest.mod3=randomForest(price~., data=Train3, method="class")
forest.pred3 = predict(forest.mod1, newdata=Test3, type="class")
stopCluster(cl2)


forest1.SSE=sum((Test1$price - forest.pred1)^2)
forest1.RMSE=sqrt(forest1.SSE/nrow(Test1))
forest1.RMSE

forest2.SSE=sum((Test2$price - forest.pred2)^2)
forest2.RMSE=sqrt(forest2.SSE/nrow(Test2))
forest2.RMSE

forest3.SSE=sum((Test3$price - forest.pred3)^2)
forest3.RMSE=sqrt(forest3.SSE/nrow(Test3))
forest3.RMSE

mean(c(forest1.RMSE, forest2.RMSE, forest3.RMSE))



# 3.6 final model
# use whole given dataset to train
# preprocess Training set TBB
TBB = subset(TBB, TBB$price!=0)
TBB$host_since = as.numeric(as.Date(TBB$host_since, "%m/%d/%Y"))
TBB$last_review = as.numeric(as.Date(TBB$last_review, "%m/%d/%Y"))
TBB$id=NULL
TBB$name=NULL
TBB$host_id=NULL
TBB$host_name=NULL
TBB$neighbourhood = as.numeric(factor(TBB$neighbourhood, levels = unique(TBB$neighbourhood)))
TBB$neighbourhood_group = as.numeric(factor(TBB$neighbourhood_group, levels = unique(TBB$neighbourhood_group)))
TBB$room_type = as.numeric(factor(TBB$room_type, levels = unique(TBB$room_type)))
TBB$bathrooms_text = as.numeric(factor(TBB$bathrooms_text, levels = unique(TBB$bathrooms_text)))

preProc = preProcess(TBB %>% select(host_since, neighbourhood, neighbourhood_group, latitude, longitude, room_type, accommodates, bathrooms_text, bedrooms, beds, number_of_reviews, last_review), method="knnImpute")
TBB_imputed_models = predict(preProc, TBB)
TBB$host_since=TBB_imputed_models$host_since
TBB$bedrooms=TBB_imputed_models$bedrooms
TBB$beds=TBB_imputed_models$beds
TBB$last_review=TBB_imputed_models$last_review


# load evalset
Evaluate_original = read.csv("evalset_rev.csv")
Evaluate = Evaluate_original
head(Evaluate)
# preprocess evalset
Evaluate$host_since = as.numeric(as.Date(Evaluate$host_since, "%Y-%m-%d"))
Evaluate$last_review = as.numeric(as.Date(Evaluate$last_review, "%Y-%m-%d"))
Evaluate$id=NULL
Evaluate$name=NULL
Evaluate$host_id=NULL
Evaluate$host_name=NULL
Evaluate$neighbourhood = as.numeric(factor(Evaluate$neighbourhood, levels = unique(Evaluate$neighbourhood)))
Evaluate$neighbourhood_group = as.numeric(factor(Evaluate$neighbourhood_group, levels = unique(Evaluate$neighbourhood_group)))
Evaluate$room_type = as.numeric(factor(Evaluate$room_type, levels = unique(Evaluate$room_type)))
Evaluate$bathrooms_text = as.numeric(factor(Evaluate$bathrooms_text, levels = unique(Evaluate$bathrooms_text)))

preProc = preProcess(Evaluate %>% select(host_since, neighbourhood, neighbourhood_group, latitude, longitude, room_type, accommodates, bathrooms_text, bedrooms, beds, number_of_reviews, last_review), method="knnImpute")
Evaluate_imputed_models = predict(preProc, Evaluate)
Evaluate$host_since=Evaluate_imputed_models$host_since
Evaluate$bedrooms=Evaluate_imputed_models$bedrooms
Evaluate$beds=Evaluate_imputed_models$beds
Evaluate$last_review=Evaluate_imputed_models$last_review

set.seed(100)
final.mod=randomForest(price~., data=Train, method="class")
final.mod.pred = predict(final.mod, newdata=Evaluate, type="class")

Evaluate_original$Prediction = final.mod.pred
write.csv(Evaluate_original, file="Evaluate.csv")


