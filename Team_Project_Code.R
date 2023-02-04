LoadLibraries = function()
{
  library(caret)
  library(cluster)
  library(dplyr)
  library(ggplot2)
  library(rpart)
  library(rpart.plot)
  library(randomForest)
  library(gbm)
  library(xgboost)
  library(lars)
  library(Matrix)  
  library(ROCR)
  install.packages('fastDummies')
  library(fastDummies)
  install.packages("corpus")
  library("corpus")
  install.packages("tidytext")
  library(tm)
  install.packages("softImpute")
  library(softImpute)
  print("libraries have been loaded")
  library("magrittr")
  install.packages("anytime")   # Install anytime package
  library("anytime") 
}

InitialiseR_StartWork = function(){
  
  setwd("~/Documents/MIT-Home/Fall/Analytics Edge/R CodeBase/project_AE")# make the command work
  LoadLibraries()
  #### remove all loaded functions and variables
  rm(list=ls())
  ###ls()%>%rm() ## can do same using pipe operator Magritte 
  print("All saved functions and variabels reset")
}

calc.OSR2 <- function(actuals, model.preds, baseline.preds) {
  return( 1 - sum((actuals - model.preds)^2) / sum((actuals - baseline.preds)^2) )
}

OSR2 = function(predictions, test, train) {
  SSE = sum((test - predictions)^2)
  SST = sum((test - mean(train))^2)
  r2 = 1 - SSE/SST
  return(r2)
}

InitialiseR_StartWork()  ## Call this function to perform all Init

####Read main file with all data####################
nftSales = read.csv("nft_sales_trim.csv")
str(nftSales)

######Creating subset from main file with required fields and preparing data###########
nftSalesTrim<-subset(nftSales, select = c(collection_name,asset_id,asset_name,event_date,asset_age_tos_yrs,event_auction_type,asset_unit_price_usd))
str(nftSalesTrim)
nftSalesTrim$event_auction_type = as.factor(nftSalesTrim$event_auction_type) ## changing auction type to factor as it has 2 types 
nftSalesTrim$event_date = as.Date(nftSalesTrim$event_date)
##########Assigning Rank of NFT Collection#######################
nftCollectionRank = read.csv("nft_collection_sales.csv")
str(nftCollectionRank)
nftCollectionRank$Rank <-nrow(nftCollectionRank):1
head(nftCollectionRank)

################Checking Collection name in nft sales and assigning rank, we have only few collections which are in both files so other collections are ranked equally###

Match_Count<-length(intersect(nftCollectionRank$Collections,nftSalesTrim$collection_name))
Match_Count<-nrow(nftCollectionRank)- Match_Count
print(Match_Count)
nftSalesTrim$Rank <-Match_Count:Match_Count
head(nftSalesTrim)

for (i in 1:nrow(nftSalesTrim))
{
  for (j in 1:nrow(nftCollectionRank))
  {
    if(nftSalesTrim$collection_name[i]==nftCollectionRank$Collections[j])
    {
      nftSalesTrim$Rank[i] <-nftCollectionRank$Rank[j]
    }
  }
}

nlevels(as.factor(nftSalesTrim$Rank))


####################Getting Maket Signal Data from BTC/ETH Price and overall NFT Sales############################

BTCPrice <- read.csv("BTC-USD.csv")
ETHPrice<- read.csv("ETH-USD.csv")
ETHPrice$Open = as.numeric(ETHPrice$Open)
ETHPrice$Close = as.numeric(ETHPrice$Close)
ETHPrice$Date = as.Date(ETHPrice$Date)

BTCPrice$Open = as.numeric(BTCPrice$Open)
BTCPrice$Close = as.numeric(BTCPrice$Close)
BTCPrice$Date = as.Date(BTCPrice$Date)

rows<-nrow(BTCPrice)
for (i in 1:rows)
{
  
  BTCPrice$BTCSignal[i]=BTCPrice$Open[i]
}

rows<-nrow(ETHPrice)
for (i in 1:rows)
{
    BTCPrice$ETHSignal[i]= ETHPrice$Open[i]
}


MarketCondition <- subset(BTCPrice,select = c(BTCSignal,ETHSignal,Overall.Market.Log,Date))
str(MarketCondition)


###############Adding rank and signal to nftSales############
nftSalesTrim$BTCSignal<-1:1
nftSalesTrim$ETHSignal<-1:1
nftSalesTrim$overallMarket<-1:1
str(nftSalesTrim)

for ( i in 1:nrow(nftSalesTrim))
{
  for(j in 1:nrow(MarketCondition))
  {
    if(nftSalesTrim$event_date[i]==MarketCondition$Date[j])
    {
      nftSalesTrim$BTCSignal[i] <-MarketCondition$BTCSignal[j]
      nftSalesTrim$ETHSignal[i]<-MarketCondition$ETHSignal[j]
      nftSalesTrim$overallMarket[i]<-MarketCondition$Overall.Market.Log[j]
      
    }
  }
}

# nftSalesTrim$BTCSignal= factor(nftSalesTrim$BTCSignal)
# nftSalesTrim$ETHSignal= factor(nftSalesTrim$ETHSignal)

str(nftSalesTrim)

###########Basic Visualization################################################

### Visualization 1 ----Assest by auction_type######
nft_grp_collection = nftSalesTrim %>% group_by(event_date,event_auction_type)  %>%
  summarise(total_quantity = length(event_auction_type),
  .groups = 'drop')
nft_grp_collection
ggplot(nft_grp_collection, aes(x = event_date , y = total_quantity, fill = event_auction_type)) + 
  geom_bar(stat = "identity", position = position_dodge(width=0.5))+
  xlab("Date")+ ylab("Volume(units)") +
  scale_fill_discrete(name = "Auction Types", 
  labels = c("Dutch", "Timed"))+
  ggtitle("Auction wise transaction data per date")+
  theme(axis.title.y = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 20))+
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))+
  theme(plot.title = element_text(size = 20))+
  scale_x_date(breaks="4 days", minor_breaks="1 day")+ theme_bw()
  
## remove dodge to get vertical 


#Visualization 2----Number of different collections in dataset########
nft_grp_collection = nftSalesTrim %>% group_by(collection_name)  %>%
  summarise(total_quantity = length(asset_id),
            .groups = 'drop')

#####Visualization 2.1 -----Top 10 collection####################
nft_grp_collection_sort<-head(nft_grp_collection[order(-nft_grp_collection$total_quantity),],10)
print(nft_grp_collection_sort)

ggplot(nft_grp_collection_sort, aes(x=collection_name, y=total_quantity)) +
        geom_point(aes(size=total_quantity,color=collection_name)) + guides(size ="none")+
        theme(axis.title=element_text(size=10), axis.text=element_text(size=10))+
          scale_fill_brewer(palette="Paired")

## Vialuaziation 2.2 -----Barplot is better for color we used scale_fill_brewer Qualitative colors to show distinct categories####################
ggplot(nft_grp_collection_sort,aes(collection_name,total_quantity,fill=collection_name))+geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired")+xlab("Collection")+ ylab("Volume(units)") + 
  ggtitle("Top 10 collections by transaction volume")+
  theme(axis.title.y = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 20))+
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))+ theme_bw()
  
        
#######Visuliazation 3 ----Datewise visualization for overall sales in NFT Market##########
str(BTCPrice)

ggplot(BTCPrice, aes(x=Date, y=Overall.Market)) +
  geom_point(aes(size=Overall.Market),colour="blue") + 
  geom_line(aes(y=Overall.Market),colour="blue")+
  guides(size ="none",color="none")+
  xlab("Date")+ ylab("Price") + 
  ggtitle("Overall NFT Market Sales date wise")+
  theme(axis.title.y = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 20))+
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))+
  scale_x_date(breaks="4 days", minor_breaks="1 day")+ theme_bw()

#### Visualization 5 payment symbol wise ###########
# bar <- ggplot(nftSales, aes(x=event_payment_symbol)) + geom_bar(aes(color=event_payment_symbol))
# bar
nft_grp_collection = nftSales %>% group_by(event_payment_symbol)  %>%
  summarise(total_quantity = length(asset_id),
            .groups = 'drop')
length(nft_grp_collection$event_payment_symbol)
nft_grp_collection<-head(nft_grp_collection[order(-nft_grp_collection$total_quantity),],10)
print(nft_grp_collection)
ggplot(nft_grp_collection,aes(event_payment_symbol,log(total_quantity),fill=event_payment_symbol))+geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired")+coord_flip()+
  labs(fill="Cryptocurrency")+labs(title="Top 10 Cryptcurrency projects by volume")+
  labs(x="Cryptocurrency",y="log(Quantity)")+ theme_bw()
##################################################################
str(nftSalesTrim)


nft_grp_collection = nftSalesTrim %>% group_by(event_date)  %>%
  summarise(total_quantity = length(asset_id),
            .groups = 'drop')
class(nft_grp_collection$event_date)
nft_grp_collection<-head(nft_grp_collection[order(-nft_grp_collection$total_quantity),],28)
print(nft_grp_collection)
ggplot(nft_grp_collection,aes(event_date,total_quantity))+
  geom_point(aes(y=total_quantity),color="darkgreen")+
  geom_line(color="darkgreen")+
  ggtitle("Transaction Volume date wise")+
  xlab("")+ylab("Quantity")+
  theme(axis.title.y = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 20))+
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))+
  scale_x_date(breaks="4 days", minor_breaks="1 day")+ theme_bw()


# +
#   scale_fill_brewer(palette="Paired")

####All market signals #####
library("reshape2")
marketsignaldf<-subset(BTCPrice,select = c(Date,Close,Overall.Market.Log))
marketsignaldf$Ethclose <-ETHPrice$Close
colnames(marketsignaldf)<-c("Date","BTCClosingPrice","OverallMarket","ETHClosingPrice")


max(marketsignaldf$BTCClosingPrice)

ggplot(marketsignaldf,aes(x=Date))+
  geom_line(aes(y=scale(BTCClosingPrice),color="red"))+
  geom_line(aes(y=scale(ETHClosingPrice),color="darkgreen"))+
  geom_line(aes(y=scale(OverallMarket),color="blue"))+
  scale_x_date(breaks="4 days", minor_breaks="1 day")+
  ggtitle("Market Signals")+
  xlab("")+ylab("Price")+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.title.x = element_text(size = 20))+
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))+
  theme_bw()+ scale_color_discrete(name = "Data Points", labels = c("BTC", "ETH", "OverallMkt"))

####Preparing Training and test data#####################################
nftSalesTrimTrain <- subset(nftSalesTrim, event_date <= "2021-02-20",select = -c(1,2,3,4))
nftSalesTrimTest <- subset(nftSalesTrim, event_date > "2021-02-20",select = -c(1,2,3,4))
                            #2002, + select=c(3,4,5,6,7,8,9,10,11)
str(nftSalesTrimTest)

#### Linear Regression#########
###############################Iteration 1 --First Model with all variables#########################

pricePredictionModel<-lm(asset_unit_price_usd~asset_age_tos_yrs+event_auction_type+Rank+scale(BTCSignal)+scale(ETHSignal)+overallMarket,data=nftSalesTrimTrain)
summary(pricePredictionModel)
preds <- predict(pricePredictionModel, newdata=nftSalesTrimTest)
summary(preds)
calc.OSR2(nftSalesTrimTest$asset_unit_price_usd, preds, mean(nftSalesTrimTrain$asset_unit_price_usd))

### Iteration 2 -- removing ETHSignalUp as it is not significant###
pricePredictionModel<-lm(asset_unit_price_usd~asset_age_tos_yrs+event_auction_type+Rank+BTCSignal+overallMarket,data=nftSalesTrim)
summary(pricePredictionModel)
preds <- predict(pricePredictionModel, newdata=nftSalesTrimTest)
summary(preds)
calc.OSR2(nftSalesTrimTest$asset_unit_price_usd, preds, mean(nftSalesTrimTrain$asset_unit_price_usd))

### Iteration 3 --removing ETHSignalUp and BTC signalup as it is not significant###
pricePredictionModel<-lm(asset_unit_price_usd~asset_age_tos_yrs+event_auction_type+Rank+overallMarket,data=nftSalesTrim)
summary(pricePredictionModel)
preds <- predict(pricePredictionModel, newdata=nftSalesTrimTest)
summary(preds)
calc.OSR2(nftSalesTrimTest$asset_unit_price_usd, preds, mean(nftSalesTrimTrain$asset_unit_price_usd))

### Iteration 4 --- removing Asset age as it is not significant###
pricePredictionModel<-lm(asset_unit_price_usd~event_auction_type+Rank+overallMarket,data=nftSalesTrim)
summary(pricePredictionModel)
preds <- predict(pricePredictionModel, newdata=nftSalesTrimTest)
summary(preds)
calc.OSR2(nftSalesTrimTest$asset_unit_price_usd, preds, mean(nftSalesTrimTrain$asset_unit_price_usd))
##################Not much imprvement, best was iteration2 hence we stick to it#######

######Polynomial Regression#######################
degrees <- 1:10
n <- length(degrees)
summary.model.fit <- nftSalesTrimTrain
R2.all <- rep(NA,n)
OSR2.all <- rep(NA,n)
for (i in 1:n){
  degree <- degrees[i]
  model=lm(asset_unit_price_usd~poly(overallMarket, degree),data=nftSalesTrimTrain)
  summary.model.fit[[paste0('poly', degree)]] <- predict(model,nftSalesTrimTrain)
  R2.all[i] <- calc.OSR2(nftSalesTrimTrain$asset_unit_price_usd, predict(model,nftSalesTrimTrain), mean(nftSalesTrimTrain$asset_unit_price_usd))
  OSR2.all[i] <- calc.OSR2(nftSalesTrimTest$asset_unit_price_usd, predict(model,nftSalesTrimTest), mean(nftSalesTrimTrain$asset_unit_price_usd))
}

R2.all
OSR2.all

ggplot(data=summary.model.fit,aes(x=overallMarket)) +
  geom_line(aes(y=poly1,col='1'),lwd=2) +
  geom_line(aes(y=poly2,col='2'),lwd=2) +
  geom_line(aes(y=poly3,col='3'),lwd=2) +
  geom_line(aes(y=poly4,col='4'),lwd=2) +
  geom_line(aes(y=poly5,col='5'),lwd=2) +
  geom_line(aes(y=poly6,col='6'),lwd=2) +
  theme_bw() +
  xlab('overallMarket') +
  ylab("Asset Unit Price") +
  theme(axis.title=element_text(size=18), 
        axis.text=element_text(size=18), 
        legend.text=element_text(size=18)) + 
  scale_color_brewer(name='',
                     labels=paste0('degree ',c(1,2,3,4,5,6)), 
                     palette="Set1")

#####################################not good results###################


######## Using these data sources and others we built another dataset ######
####Read main file with all data####################
nftnewData = read.csv("train-data.csv")
str(nftnewData)

set.seed(123)
train.obs <- sort(sample(seq_len(nrow(nftnewData)), 0.7*nrow(nftnewData)))
train <- nftnewData[train.obs,]
test <- nftnewData[-train.obs,]


#######################Linear Regression ###############################################
pricePredictionModel<-lm(predicted_price~.,data=train)
summary(pricePredictionModel)
preds <- predict(pricePredictionModel, newdata=test)
summary(preds)
lrosr2<-calc.OSR2(test$predicted_price, preds, mean(train$predicted_price))
lrosr2
#################################Poly on Last Sale Price##########################################

degrees <- 1:10
n <- length(degrees)
summary.model.fit <- train
R2.all <- rep(NA,n)
OSR2.all <- rep(NA,n)
for (i in 1:n){
  degree <- degrees[i]
  model=lm(predicted_price~poly(last_sale_price, degree),data=train)
  summary.model.fit[[paste0('poly', degree)]] <- predict(model,train)
  R2.all[i] <- calc.OSR2(train$predicted_price, predict(model,train), mean(train$predicted_price))
  OSR2.all[i] <- calc.OSR2(test$predicted_price, predict(model,test), mean(train$predicted_price))
}

R2.all
OSR2.all

#######Not good results ###########################

#################################Poly on Rarity##########################################
degrees <- 1:10
n <- length(degrees)
summary.model.fit <- train
R2.all <- rep(NA,n)
OSR2.all <- rep(NA,n)
for (i in 1:n){
  degree <- degrees[i]
  model=lm(predicted_price~poly(rarity, degree),data=train)
  summary.model.fit[[paste0('poly', degree)]] <- predict(model,train)
  R2.all[i] <- calc.OSR2(train$predicted_price, predict(model,train), mean(train$predicted_price))
  OSR2.all[i] <- calc.OSR2(test$predicted_price, predict(model,test), mean(train$predicted_price))
}

R2.all
OSR2.all

ggplot(data=summary.model.fit,aes(x=rarity)) +
  geom_line(aes(y=poly1,col='1'),lwd=2) +
  geom_line(aes(y=poly2,col='2'),lwd=2) +
  geom_line(aes(y=poly3,col='3'),lwd=2) +
  geom_line(aes(y=poly4,col='4'),lwd=2) +
  geom_line(aes(y=poly5,col='5'),lwd=2) +
  geom_line(aes(y=poly6,col='6'),lwd=2) +
  theme_bw() +
  xlab('Rarity') +
  ylab("NFT Price") +
  theme(axis.title=element_text(size=18), 
        axis.text=element_text(size=18), 
        legend.text=element_text(size=18)) + 
  scale_color_brewer(name='',
                     labels=paste0('degree ',c(1,2,3,4,5,6)), 
                     palette="Set1")

##### between 5 to 7 is giving good performance but can't say if because of increase in # of variables####

### Boosting#####
mod.boost1 = gbm(predicted_price ~ .,
                 data = train,
                 distribution = "gaussian",
                 n.trees = 10000,
                 shrinkage = .01,
                 interaction.depth = 9, bag.fraction=1)

set.seed(232)
tGrid = expand.grid(n.trees = seq.int(1000, 35000, by = 1000), interaction.depth = 4:12,
                    shrinkage = 0.001, n.minobsinnode = 10)

system.time(train.boost <- train(predicted_price ~ .,
                                 data = train,
                                 method = "gbm",
                                 tuneGrid = tGrid,
                                 trControl = trainControl(method="cv", number=10),
                                 metric = "RMSE",
                                 distribution = "gaussian", verbose = F))
#took me 5+ hours

train.boost$bestTune
#n.trees interaction.depth shrinkage n.minobsinnode
#315   35000                12     0.001             10

set.seed(235)
mod.boost = gbm(predicted_price ~ .,
                data = train,
                distribution = "gaussian",
                n.trees = 35000,
                shrinkage = 0.001,
                interaction.depth = 12)



pred.boost = predict(mod.boost, newdata = test, n.trees = 35000)

mae.boost <- mean(abs(pred.boost-test$predicted_price))
#0.03034
rmse.boost <- sqrt(mean((pred.boost - test$predicted_price)^2))
#0.0524
OSR2.boost  <- OSR2(pred.boost, test$predicted_price, train$predicted_price)
#0.5294


###############
#XGBoost
#(eXtreme Gradient Boosting)
###############
str(train)
x.train <- train[,-4]
y.train <- train[,4]
x.train <- lm(y.train~., data = x.train,x=T)$x

x.test <- test[,-4]
y.test <- test[,4]
x.test <- lm(y.test~., data = x.test,x=T)$x

library(xgboost)
library(xgboost)
tg = expand.grid(max_depth = 3:12,
                 eta = seq(.001, .002, by=0.001), 
                 subsample=.5, 
                 min_child_weight=1, 
                 gamma=c(0,.02, .04),  
                 colsample_bytree=1, 
                 alpha=c(.5,1), 
                 lambda=c(0))       


round.best = rep(0, nrow(tg))
b =    Sys.time() 
RMSE.cv = rep(0, nrow(tg))
set.seed(250)

for(i in 1:nrow(tg))
{
  params.new = split(t(tg[i,]), colnames(tg))
  eval = xgb.cv(data = as.matrix(x.train), label = y.train, params = params.new, nrounds = 15000, nfold = 5, verbose = F)$evaluation_log$test_rmse_mean
  round.best[i] = which.min(eval)
  RMSE.cv[i] = eval[round.best[i]]
  
  print(i)
}
Sys.time()
RMSE.cv
winner = which.min(RMSE.cv)
tg[winner,]
round.best[winner]
round.best
# tg[winner,]
# max_depth   eta subsample min_child_weight gamma colsample_bytree alpha lambda
# 7        9 0.001       0.5                1     0                1   0.5      0
# > round.best[winner]
# [1] 14933

set.seed(1988)
params.winner = list(max_depth = 9, eta = .001, subsample = .5, min_child_weight = 1, gamma = 0, colsample_bytree=1, alpha=.5, lambda = 0)
round.winner = 14993
mod.xgboost <- xgboost(data = as.matrix(x.train), label = y.train, params = params.winner, nrounds = round.winner, verbose = F)
pred.xgboost <- predict(mod.xgboost, newdata=as.matrix(x.test), rounds=rounds.winner)
SST = sum((y.test - mean(y.train))^2)

OSR2.xgboost <- 1 - sum((pred.xgboost - y.test)^2)/SST
OSR2.xgboost
#[1] 0.5324156
RMSE.xgboost <- sqrt(mean((pred.xgboost - y.test)^2))
RMSE.xgboost
#[1] 0.05225879
MAE.xgboost <- mean(abs(pred.xgboost-y.test))
MAE.xgboost
#[1] 0.02999403

################CART###################################################
claimsTree = rpart(predicted_price ~ ., 
                   data=train, 
                   method="anova",
                   minbucket = 25, cp=0.002)
pdf('initial_all_var_tree.pdf',12,6)
prp(claimsTree, digits = 3, varlen = 0, faclen = 0)
dev.off()

minbucket.base = 50
cp.all <- c(0.001,0.002,0.005)

for (cp in cp.all){
  claimsTree = rpart(predicted_price ~ ., 
                     data=train, 
                     method="anova",
                     minbucket = minbucket.base,
                     cp=cp)
  pdf(paste0('tree_',minbucket.base,'_',cp,'.pdf'),6,8)
  prp(claimsTree, digits = 3, varlen = 0, faclen = 0)
  dev.off()
}

# Final tree


treeFinal <- rpart(predicted_price ~ ., data=train, minbucket = 50, cp=0.00001)
pdf("tree_final_with_best_cp.pdf",8,8)
prp(treeFinal, digits = 3, varlen = 0, faclen = 0)
dev.off()

treeSmaller <- rpart(predicted_price ~ ., data=train, minbucket = 50, cp=0.001)
pdf("tree_smaller.pdf",8,8)
prp(treeSmaller, digits = 3, varlen = 0, faclen = 0)
dev.off()

# Let's assess performance on the test set

# Slide 25: Make predictions on test and train sets with treeFinal
PredictTrain.treeFinal = predict(treeFinal, newdata = train)
PredictTest.treeFinal = predict(treeFinal, newdata = test)

# Make predictions on test and train sets with treeSmaller
PredictTrain.treeSmaller = predict(treeSmaller, newdata = train)
PredictTest.treeSmaller = predict(treeSmaller, newdata = test)

# Slide 26
# Calculate R-Squared and OSR-Squared with treeFinal
SSTTrain = sum((train$predicted_price - mean(train$predicted_price))^2)
SSETrain = sum((PredictTrain.treeFinal - train$predicted_price)^2)
R2_CART_treeFinal <- 1 - SSETrain/SSTTrain
SSTTest = sum((test$predicted_price - mean(train$predicted_price))^2)
SSETest = sum((PredictTest.treeFinal - test$predicted_price)^2)
OSR2_CART_treeFinal <- 1 - SSETest/SSTTest

# Calculate R-Squared and OSR-Squared with treeSmaller
SSTTrain = sum((train$predicted_price - mean(train$predicted_price))^2)
SSETrain = sum((PredictTrain.treeSmaller - train$predicted_price)^2)
R2_CART_treeSmaller <- 1 - SSETrain/SSTTrain
SSTTest = sum((test$predicted_price - mean(train$predicted_price))^2)
SSETest = sum((PredictTest.treeSmaller - test$predicted_price)^2)
OSR2_CART_treeSmaller <- 1 - SSETest/SSTTest



# Slide 28: Compare all the models
results <- data.frame(
                      `CART (cp=0.001)` = c(R2_CART_treeSmaller, OSR2_CART_treeSmaller),
                      `CART (cp=0.00001)` = c(R2_CART_treeFinal, OSR2_CART_treeFinal),
                      `Linear Regression` = c(summary(pricePredictionModel)$r.squared, lrosr2))
rownames(results) <- c('R-Squared', 'OSR-Squared')
results


######################GAM ######################
#Plotting the data
ggplot(data=nftnewData,aes(x=rarity)) + geom_histogram(fill = "cornflowerblue", 
                                                  color = "white",binwidth=50)+theme_bw()  
nrow(nftnewData)

nftnewData$rarity_class<-1:1

for (i in 1:nrow(nftnewData))
{
  if(nftnewData$rarity[i]<150 && nftnewData$rarity[i]>10)
    nftnewData$rarity_class[i]<-"LOW"
  else if(nftnewData$rarity[i]<350 && nftnewData$rarity[i]>=150 )
  nftnewData$rarity_class[i]<-"MEDIUM"
  else if(nftnewData$rarity[i]<600 && nftnewData$rarity[i]>=350)
  nftnewData$rarity_class[i]<-"HIGH"
}

ggplot(nftnewData, 
       aes(x = sale_count, 
           fill = rarity_class)) +
  geom_density(alpha = 0.4) +
  labs(title = "Price distribution by rarity")






