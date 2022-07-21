# import packages
library(tidyverse)
library(dlookr)
library(GGally)
library(kableExtra)
library(caret)

setwd("C:/Users/tsuba/Downloads")

ames = read.table("train.csv", header = TRUE, sep = ",")
head(ames)

names(ames)
dim(ames)

######################## EDA

describe(ames)
# examine nulls values 
ames %>% filter(is.na(LotFrontage) |
                  is.na(MasVnrArea) |
                  is.na(GarageYrBlt))

# numerical vars

ames.num <- select_if(ames, is.numeric)
ames.num_clean <- na.omit(ames.num)
ames.corr = cor(ames.num_clean)
ames.corr


#most significant
# overall quality
# YearBuilt
# YearRemodAdd
# MasVnrArea
# BsmtFinSF1
# totalbsmt
# x1
# GroungLivingArea
# FullBath
# AboveGrough
#fireplaces
# garage year built
# garagecars
# garagearea

ut <- upper.tri(ames.corr)
flatten_corr <- data.frame(
  row = rownames(ames.corr)[row(ames.corr)[ut]],
  column = rownames(ames.corr)[col(ames.corr)[ut]],
  cor  =(ames.corr)[ut])
flatten_corr %>% filter(column == "SalePrice" & cor > 0.3)

selected_numvars <- flatten_corr %>% filter(column == "SalePrice" & cor > 0.3) %>% select(row)
index_num <- which(names(ames.num_clean) %in% selected_numvars$row)
ggpairs(ames.num_clean, columns = index_num)

# Plot distributions of numerical predictors 
p1 <- ames.num_clean %>%
  select(which(names(ames.num_clean) %in% selected_numvars$row)) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

ggsave("p1.png", # the name of the file where it will be save
       plot = p1, # what plot to save
       height=6, width=10, units="in") 

ind <- which(names(ames.num_clean) %in% c("YearRemodAdd","YearBuilt",
                                          "X1stFlrSF", "TotRmsAbvGrd"
                                          ,"TotalBsmtSF",
                                          "OverallQual",
                                          "GarageYrBlt",
                                          "GrLivArea",
                                          "LotFrontage",
                                          "GarageArea",
                                          "FullBath", "SalePrice"))

p2 <- ggpairs(ames.num_clean, columns = ind)
ggsave("p2.png", # the name of the file where it will be save
       plot = p2, # what plot to save
       height=6, width=10, units="in") 

# categorical vars

ames %>% keep(is.character) %>% head()

cat_ind <- ames %>% keep(is.character) %>% is.na()

ames_char <- ames %>% keep(is.character) 

for (i in 1:43) {
  print(summary(as.factor(ames_char[, i])))
}

ames_char.merge <- cbind(ames_char, ames$SalePrice)
ames_char.merge

ggpairs(ames_char.merge, columns=c(38:43, 44), cardinality_threshold = 30)

ggpairs(ames_char.merge, columns=c(31:37, 44), cardinality_threshold = 30, bindwith=20)

ggpairs(ames_char.merge, columns=c(24:30, 44), cardinality_threshold = 30)

ggpairs(ames_char.merge, columns=c(17:23, 44), cardinality_threshold = 30)

ggpairs(ames_char.merge, columns=c(11:16, 44), cardinality_threshold = 30)

ggpairs(ames_char.merge, columns=c(5:10, 44), cardinality_threshold = 30)

ggpairs(ames_char.merge, columns=c(1:5, 44), cardinality_threshold = 30)

ames %>% keep(is.character)  %>% 
  filter_all(any_vars(! is.na(.)))

ames %>% keep(is.character) %>% colnames()

sum(is.na(ames_char$Alley))
sum(is.na(ames_char$ExterQual))
sum(is.na(ames_char$MSZoning))
sum(is.na(ames_char$KitchenQual))

ames %>% select(YearRemodAdd, TotRmsAbvGrd, TotalBsmtSF, OverallQual, GrLivArea, GarageArea, FullBath, ExterQual, MSZoning,
                KitchenQual, SalePrice) %>% filter_all(any_vars(! is.na(.)))

ames_subset <- ames %>% select(YearRemodAdd, TotRmsAbvGrd, TotalBsmtSF, OverallQual, GrLivArea, GarageArea, FullBath, ExterQual, MSZoning,
                               KitchenQual, SalePrice) 

ggpairs(ames_subset)



# Data Preparation

ames_subset <- ames_subset %>% mutate(logBsmt = log(TotalBsmtSF + 1),
                                      logLivArea = log(GrLivArea +1),
                                      logSale = log(SalePrice +1),
                                      ExterQual = as.factor(ExterQual),
                                      MSZoning = as.factor(MSZoning),
                                      KitchenQual = as.factor(KitchenQual))

trCrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)


# Fitting Models

#################### Stepwise

library(leaps)
set.seed(05112022)
subset.fit <- train(logSale ~ YearRemodAdd + TotRmsAbvGrd + OverallQual + 
                      + GarageArea + FullBath + ExterQual + MSZoning + 
                      + KitchenQual + logBsmt + logLivArea,
                    data = ames_subset,
                    method = 'leapSeq',
                    # backward selection on full data has 10 variables so we only find max variable = 10
                    tuneGrid = data.frame(nvmax = 1:10),
                    preProcess= c("center","scale"),
                    trControl = trCrl
)
subset.fit$bestTune
linear.rmse <- subset.fit$results$RMSE[9]
subset.fit$finalModel
plot(subset.fit)
summary(subset.fit$finalModel)
coef(subset.fit$finalModel, 9) %>% kbl() %>%
  kable_material(c("striped", "hover"))


####################### KNN Regression 

set.seed(05112022)
knn.fit <- train(logSale ~ YearRemodAdd + TotRmsAbvGrd + OverallQual + 
                   + GarageArea + FullBath + ExterQual + MSZoning + 
                   + KitchenQual + logBsmt + logLivArea,
                 data = ames_subset,
                 method = "knn", 
                 trControl = trCrl,
                 preProcess = c("center","scale"),
                 tuneGrid = data.frame(k= c(3:10)))
knn.fit$results

knn.fit$finalModel
plot(knn.fit)
knn.rmse <- knn.fit$results$RMSE[3]

##################### Elastic Net

library(glmnet)
set.seed(05112022)
elastic.fit <- train(logSale ~ YearRemodAdd + TotRmsAbvGrd + OverallQual + 
                       + GarageArea + FullBath + ExterQual + MSZoning + 
                       + KitchenQual + logBsmt + logLivArea,
                     data = ames_subset,
                     method="glmnet",
                     trControl=trCrl,
                     preProcess= c("center","scale"),
                     ## LASSO: alpha=1 (oposite of notes)
                     tuneGrid = expand.grid(alpha=seq(0, 1, by=0.1), lambda=seq(0, 0.3, by=0.05)))
elastic.fit

plot(elastic.fit$finalModel, xvar="lambda")

elastic.fit$bestTune

coef(elastic.fit$finalModel, 8)

enet.rmse <- elastic.fit$results$RMSE[8]

############################### Random Forest

set.seed(05112022)
rf.fit <- train(logSale ~ YearRemodAdd + TotRmsAbvGrd + OverallQual + 
                  + GarageArea + FullBath + ExterQual + MSZoning + 
                  + KitchenQual + logBsmt + logLivArea,
                data = ames_subset,
                method="rf",
                trControl=trainControl("oob"), # Out-of-bag MSE
                tuneGrid=expand.grid(mtry = 1:10),
                ntree=100)

rf.fit
rf.fit$finalModel
plot(rf.fit)
plot()

varImp(rf.fit, scale=FALSE)

plot(varImp(rf.fit, scale=FALSE))

rf.fit.rmse <- rf.fit$results$RMSE[6]


####################### PCR

set.seed(05112022)
pcr.fit <- train(logSale ~ YearRemodAdd + TotRmsAbvGrd + OverallQual + 
                   + GarageArea + FullBath + ExterQual + MSZoning + 
                   + KitchenQual + logBsmt + logLivArea,
                 data = ames_subset,
                 method="pcr", # pcr() in pls library
                 preProcess=c("center", "scale"),
                 trControl = trCrl,
                 tuneGrid=data.frame(ncomp=1:10))
pcr.fit
pcr.fit$finalModel
plot(pcr.fit)
pcr.fit.rmse <- pcr.fit$results$RMSE[9]


coef(pcr.fit$finalModel, pcr.fit$bestTune$ncomp)


# Model Comparison

data.frame(model = c('Stepwise regression', 'KNN regression',
                     'ElasticNet', 'Random Forest', 'PCR'),
           rmse = c(linear.rmse, knn.rmse, enet.rmse, rf.fit.rmse, pcr.fit.rmse)) %>%
  kbl(col.names = c("Model", "Test MSE")) %>%
  kable_material(c("striped", "hover"))


