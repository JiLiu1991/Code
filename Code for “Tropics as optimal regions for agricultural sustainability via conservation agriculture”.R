################################################开始训练模型
load("E:/r/r1/数据/插补完数据.RData")
str(da1)
#####################################################################################算模型，分割版本
library(randomForest)
library(rfPermute)
library(missForest)
library(caret)
####################################TC
names(da1)
da <- da1[, c(1, 6:29)]
da=na.omit(da)
str(da)
names(da)

set.seed(113)
rf_model <- randomForest(RRTC ~ ., data = da, ntree = 500)
rf_pred <- predict(rf_model, da)
residuals <- da$RRTC - rf_pred
summary(residuals)
threshold <- quantile(abs(residuals), 0.95) 
outliers <- which(abs(residuals) > threshold)
da_TC <- da[-outliers, ]




set.seed(113)
train <- sample(nrow(da_TC), nrow(da_TC)*0.8)
otu_train <- da_TC[train, ]
otu_test <- da_TC[-train, ]
names(otu_train)
# 预测变量矩阵         MAOCmax
x <-  otu_train[, c(2:25)]
# 响应变量向量
y <- otu_train$RRTC

rf_grid <- expand.grid(mtry = seq(1, 24, by = 1))
control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
num_cores <- 12  # 假设选择 6 个核心
# 注册并行计算后端
cl <- makeCluster(num_cores)
registerDoParallel(cl)
set.seed(113)
rf_model <- train(RRTC ~ ., data = otu_train, method = "rf", trControl = control, tuneGrid = rf_grid, importance = TRUE)
stopCluster(cl)
registerDoSEQ()  # 恢复到顺序计算

print(rf_model$bestTune)
print(rf_model$results)

mtry_TC <- as.data.frame(rf_model$results)
best_mtry<- mtry_TC$mtry[which.min(mtry_TC$RMSE)]


###################
set.seed(113)
folds <- createFolds(otu_train$RRTC, k=10)
accuracies <- vector()

for (ntree in seq(50, 2000, by=50)) {
  fold_accuracies <- vector()
  
  for (j in 1:10) {
    fold_train <- otu_train[-folds[[j]], ]
    fold_test <- otu_train[folds[[j]], ]
    
    set.seed(113)
    fold_model <- randomForest(RRTC ~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                                 pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                                 Frequency+Input.depth+Operation+
                                 Input.N+Input.P+RR+Duration+Soil.depth,
                               data=fold_train, mtry=best_mtry, ntree=ntree)
    fold_pred <- predict(fold_model, fold_test)
    fold_accuracy <- cor(fold_pred, fold_test$RRTC)
    fold_accuracies <- c(fold_accuracies, fold_accuracy)
  }
  
  accuracies <- c(accuracies, mean(fold_accuracies))
}

# 保存结果
accuracies_RRTC <- accuracies


set.seed(113)
train <- sample(nrow(da_TC), nrow(da_TC)*0.8)
otu_train <- da_TC[train, ]
str(otu_train)
otu_test <- da_TC[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)

set.seed(113)
rf_results1<-rfPermute(RRTC~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                         pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                         Frequency+Input.depth+Operation+
                         Input.N+Input.P+RR+Duration+Soil.depth,
                       data = otu_train,importance=T, mtry=18, ntree = 1500,trControl = cv_folds)
rf_results1$rf

####################################TN
names(da1)
da <- da1[, c(2, 6:29)]
da=na.omit(da)
str(da)
names(da)

set.seed(112)
rf_model <- randomForest(RRTN ~ ., data = da, ntree = 500)
rf_pred <- predict(rf_model, da)
residuals <- da$RRTN - rf_pred
summary(residuals)
threshold <- quantile(abs(residuals), 0.95) 
outliers <- which(abs(residuals) > threshold)
da_TN <- da[-outliers, ]




set.seed(112)
train <- sample(nrow(da_TN), nrow(da_TN)*0.8)
otu_train <- da_TN[train, ]
str(otu_train)
otu_test <- da_TN[-train, ]
names(otu_train)
# 预测变量矩阵         MAOCmax
x <-  otu_train[, c(2:25)]
# 响应变量向量
y <- otu_train$RRTN

rf_grid <- expand.grid(mtry = seq(1, 24, by = 1))
control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
num_cores <- 12  # 假设选择 6 个核心
# 注册并行计算后端
cl <- makeCluster(num_cores)
registerDoParallel(cl)
set.seed(112)
rf_model <- train(RRTN ~ ., data = otu_train, method = "rf", trControl = control, tuneGrid = rf_grid, importance = TRUE)
stopCluster(cl)
registerDoSEQ()  # 恢复到顺序计算

print(rf_model$bestTune)
print(rf_model$results)

mtry_TN <- as.data.frame(rf_model$results)
best_mtry<- mtry_TN$mtry[which.min(mtry_TN$RMSE)]


###################
set.seed(112)
folds <- createFolds(otu_train$RRTN, k=10)
accuracies <- vector()

for (ntree in seq(50, 2000, by=50)) {
  fold_accuracies <- vector()
  
  for (j in 1:10) {
    fold_train <- otu_train[-folds[[j]], ]
    fold_test <- otu_train[folds[[j]], ]
    
    set.seed(112)
    fold_model <- randomForest(RRTN ~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                                 pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                                 Frequency+Input.depth+Operation+
                                 Input.N+Input.P+RR+Duration+Soil.depth,
                               data=fold_train, mtry=best_mtry, ntree=ntree)
    fold_pred <- predict(fold_model, fold_test)
    fold_accuracy <- cor(fold_pred, fold_test$RRTN)
    fold_accuracies <- c(fold_accuracies, fold_accuracy)
  }
  
  accuracies <- c(accuracies, mean(fold_accuracies))
}

# 保存结果
accuracies_RRTN <- accuracies



set.seed(112)
train <- sample(nrow(da_TN), nrow(da_TN)*0.8)
otu_train <- da_TN[train, ]
otu_test <- da_TN[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)

set.seed(112)
rf_results2<-rfPermute(RRTN~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                         pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                         Frequency+Input.depth+Operation+
                         Input.N+Input.P+RR+Duration+Soil.depth,
                       data = otu_train,importance=T, mtry=16, ntree = 1000,trControl = cv_folds)
rf_results2$rf
###################################TP
names(da1)
da <- da1[, c(3, 6:29)]
da=na.omit(da)
str(da)
names(da)

set.seed(110)
rf_model <- randomForest(RRTP ~ ., data = da, ntree = 500)
rf_pred <- predict(rf_model, da)
residuals <- da$RRTP - rf_pred
summary(residuals)
threshold <- quantile(abs(residuals), 0.95) 
outliers <- which(abs(residuals) > threshold)
da_TP <- da[-outliers, ]




set.seed(110)
train <- sample(nrow(da_TP), nrow(da_TP)*0.8)
otu_train <- da_TP[train, ]
str(otu_train)
otu_test <- da_TP[-train, ]
names(otu_train)
# 预测变量矩阵         MAOCmax
x <-  otu_train[, c(2:25)]
# 响应变量向量
y <- otu_train$RRTP

rf_grid <- expand.grid(mtry = seq(1, 24, by = 1))
control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
num_cores <- 12  # 假设选择 6 个核心
# 注册并行计算后端
cl <- makeCluster(num_cores)
registerDoParallel(cl)
set.seed(110)
rf_model <- train(RRTP ~ ., data = otu_train, method = "rf", trControl = control, tuneGrid = rf_grid, importance = TRUE)
stopCluster(cl)
registerDoSEQ()  # 恢复到顺序计算

print(rf_model$bestTune)
print(rf_model$results)

mtry_TP <- as.data.frame(rf_model$results)
best_mtry<- mtry_TP$mtry[which.min(mtry_TP$RMSE)]


###################
set.seed(110)
folds <- createFolds(otu_train$RRTP, k=10)
accuracies <- vector()

for (ntree in seq(50, 2000, by=50)) {
  fold_accuracies <- vector()
  
  for (j in 1:10) {
    fold_train <- otu_train[-folds[[j]], ]
    fold_test <- otu_train[folds[[j]], ]
    
    set.seed(110)
    fold_model <- randomForest(RRTP ~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                                 pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                                 Frequency+Input.depth+Operation+
                                 Input.N+Input.P+RR+Duration+Soil.depth,
                               data=fold_train, mtry=best_mtry, ntree=ntree)
    fold_pred <- predict(fold_model, fold_test)
    fold_accuracy <- cor(fold_pred, fold_test$RRTP)
    fold_accuracies <- c(fold_accuracies, fold_accuracy)
  }
  
  accuracies <- c(accuracies, mean(fold_accuracies))
}
accuraciesy_RRTP<- accuracies


set.seed(110)
train <- sample(nrow(da_TP), nrow(da_TP)*0.8)
otu_train <- da_TP[train, ]
otu_test <- da_TP[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)

set.seed(110)
rf_results3<-rfPermute(RRTP ~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                         pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                         Frequency+Input.depth+Operation+
                         Input.N+Input.P+RR+Duration+Soil.depth,
                       data = otu_train,importance=T, mtry=2, ntree = 1000,trControl = cv_folds)
rf_results3$rf
####################################pH
names(da1)
da <- da1[, c(4, 6:29)]
da=na.omit(da)
str(da)
names(da)

set.seed(122)
rf_model <- randomForest(RRpH ~ ., data = da, ntree = 800)
rf_pred <- predict(rf_model, da)
residuals <- da$RRpH - rf_pred
summary(residuals)
threshold <- quantile(abs(residuals), 0.95) 
outliers <- which(abs(residuals) > threshold)
da_pH <- da[-outliers, ]




set.seed(122)
train <- sample(nrow(da_pH), nrow(da_pH)*0.8)
otu_train <- da_pH[train, ]
str(otu_train)
otu_test <- da_pH[-train, ]
names(otu_train)
# 预测变量矩阵         MAOCmax
x <-  otu_train[, c(2:25)]
# 响应变量向量
y <- otu_train$RRpH

rf_grid <- expand.grid(mtry = seq(1, 24, by = 1))
control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
num_cores <- 12  # 假设选择 6 个核心
# 注册并行计算后端
cl <- makeCluster(num_cores)
registerDoParallel(cl)
set.seed(122)
rf_model <- train(RRpH ~ ., data = otu_train, method = "rf", trControl = control, tuneGrid = rf_grid, importance = TRUE)
stopCluster(cl)
registerDoSEQ()  # 恢复到顺序计算

print(rf_model$bestTune)
print(rf_model$results)

mtry_pH <- as.data.frame(rf_model$results)
best_mtry<- mtry_pH$mtry[which.min(mtry_pH$RMSE)]


###################
set.seed(122)
folds <- createFolds(otu_train$RRpH, k=10)
accuracies <- vector()

for (ntree in seq(50, 2000, by=50)) {
  fold_accuracies <- vector()
  
  for (j in 1:10) {
    fold_train <- otu_train[-folds[[j]], ]
    fold_test <- otu_train[folds[[j]], ]
    
    set.seed(122)
    fold_model <- randomForest(RRpH ~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                                 pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                                 Frequency+Input.depth+Operation+
                                 Input.N+Input.P+RR+Duration+Soil.depth,
                               data=fold_train, mtry=best_mtry, ntree=ntree)
    fold_pred <- predict(fold_model, fold_test)
    fold_accuracy <- cor(fold_pred, fold_test$RRpH)
    fold_accuracies <- c(fold_accuracies, fold_accuracy)
  }
  
  accuracies <- c(accuracies, mean(fold_accuracies))
}

# 保存结果
accuracies_RRpH <- accuracies




set.seed(122)
train <- sample(nrow(da_pH), nrow(da_pH)*0.8)
otu_train <- da_pH[train, ]
otu_test <- da_pH[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)

set.seed(122)
rf_results4<-rfPermute(RRpH~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                         pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                         Frequency+Input.depth+Operation+
                         Input.N+Input.P+RR+Duration+Soil.depth,
                       data = otu_train,importance=T, mtry=8, ntree = 1000,trControl = cv_folds)
rf_results4$rf

####################################Yield
names(da1)
da <- da1[, c(5, 6:27, 29)]
da=na.omit(da)
str(da)
names(da)

set.seed(123)
rf_model <- randomForest(RRYield ~ ., data = da, ntree = 500)
rf_pred <- predict(rf_model, da)
residuals <- da$RRYield - rf_pred
summary(residuals)
threshold <- quantile(abs(residuals), 0.95) 
outliers <- which(abs(residuals) > threshold)
da_Yield <- da[-outliers, ]




set.seed(123)
train <- sample(nrow(da_Yield), nrow(da_Yield)*0.8)
otu_train <- da_Yield[train, ]
str(otu_train)
otu_test <- da_Yield[-train, ]
names(otu_train)
# 预测变量矩阵         MAOCmax
x <-  otu_train[, c(2:24)]
# 响应变量向量
y <- otu_train$RRYield

rf_grid <- expand.grid(mtry = seq(1, 24, by = 1))
control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
num_cores <- 8  # 假设选择 6 个核心
# 注册并行计算后端
cl <- makeCluster(num_cores)
registerDoParallel(cl)
set.seed(123)
rf_model <- train(RRYield ~ ., data = otu_train, method = "rf", trControl = control, tuneGrid = rf_grid, importance = TRUE)
stopCluster(cl)
registerDoSEQ()  # 恢复到顺序计算

print(rf_model$bestTune)
print(rf_model$results)

mtry_Yield <- as.data.frame(rf_model$results)
best_mtry<- mtry_Yield$mtry[which.min(mtry_Yield$RMSE)]


###################
set.seed(123)
folds <- createFolds(otu_train$RRYield, k=10)
accuracies <- vector()

for (ntree in seq(50, 2000, by=50)) {
  fold_accuracies <- vector()
  
  for (j in 1:10) {
    fold_train <- otu_train[-folds[[j]], ]
    fold_test <- otu_train[folds[[j]], ]
    
    set.seed(123)
    fold_model <- randomForest(RRYield ~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                                 pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                                 Frequency+Input.depth+Operation+
                                 Input.N+Input.P+RR+Duration,
                               data=fold_train, mtry=best_mtry, ntree=ntree)
    fold_pred <- predict(fold_model, fold_test)
    fold_accuracy <- cor(fold_pred, fold_test$RRYield)
    fold_accuracies <- c(fold_accuracies, fold_accuracy)
  }
  
  accuracies <- c(accuracies, mean(fold_accuracies))
}

# 保存结果
accuracies_RRYield <- accuracies


set.seed(123)
train <- sample(nrow(da_Yield), nrow(da_Yield)*0.8)
otu_train <- da_Yield[train, ]
otu_test <- da_Yield[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)

set.seed(123)
rf_results5<-rfPermute(RRYield~ Precipitation+Temperate+Evaporation+AI+Sand+Silt+Clay+
                         pH+BD+TC+TN+TP+AP+Landuse+Plant.type+TR+
                         Frequency+Input.depth+Operation+
                         Input.N+Input.P+RR+Duration,
                       data = otu_train,importance=T, mtry=23, ntree = 1000,trControl = cv_folds)
rf_results5$rf
rf_results4$rf


save.image("E:/r/r1/相关性与模型/5个模型.RData")
load("E:/r/r1/相关性与模型/5个模型.RData")

####################################################tu

rf_results1$rf
predictor_var1<- data.frame(importance(rf_results1, scale = TRUE), check.names = FALSE)
predictor_sig1<-as.data.frame((rf_results1$pval)[,,2])
colnames(predictor_sig1)<-c("sig","other")
df_pre1<-cbind(predictor_var1,predictor_sig1)
df_pre1

df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.05&df_pre1$`%IncMSE.pval`<0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.05]<-"*"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.01]<-"**"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.001]<-"***"

rankImportance <- df_pre1 %>%
  mutate(Rank = paste0('#',dense_rank(desc(df_pre1$`%IncMSE`))))
varImportance <- data.frame(Variables = row.names(rankImportance))
rankImportance1 <- cbind.data.frame(rankImportance,varImportance)
rankImportance1

col <- colorRampPalette(c("grey", "#4477AA"))
x1 = reorder(rankImportance1$Variables, rankImportance1$`%IncMSE`)

pdf("E:/r/r1/相关性与模型/TC.pdf", width = 8, height = 4.5)
ggplot(rankImportance1, aes(x = x1, y = `%IncMSE`,fill=x1))+
  geom_bar(stat='identity',alpha=1,width=0.75)+
  scale_fill_manual(values = col(length(unique(x1))))+
  labs(x = ' ') +
  geom_text(aes(label = rankImportance1$`sig`,y= rankImportance1$`%IncMSE`+2, x =Variables),vjust = 0.6,size=7)+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=16),axis.text.x = element_text(color="black",size=16),axis.title.y = element_text(size=16),axis.title.x = element_text(size=16))+
  theme(panel.grid=element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
  ylab('MSE (%)')+
  annotate("text",label="Var. explained: 49.4 %",x=8,y=40,size=6)+
  annotate("text",label="MSR: 0.00549",x=8,y=36,size=6)+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))+
  #coord_flip()+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))
dev.off()
#使用训练集，查看预测精度

set.seed(113)
train <- sample(nrow(da_TC), nrow(da_TC)*0.8)
otu_train <- da_TC[train, ]
otu_test <- da_TC[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)
predict_TC1 <- predict(rf_results1, otu_train)
otu_train$predictTC<- predict_TC1
otu_train$type <- "A"
predict_TC2 <- predict(rf_results1, otu_test)
otu_test$predictTC <- predict_TC2
otu_test$type <- "B"
da<- bind_rows(otu_train, otu_test)
rmse <- sqrt(mean((da$RRTC - da$predictTC)^2))
rmse

pdf("E:/r/r1/相关性与模型/模型评价/XTC.pdf", width = 8, height = 6.3)
ggplot(da,aes(x=RRTC,y=predictTC,group=type,color=type))+
  geom_point(shape=21,alpha = 0.3,size=7, stroke = 0.4) +
  scale_color_manual(values = c("#C71000FF", "#008EA0FF")) +
  scale_fill_manual(values = c("#C71000FF", "#008EA0FF")) +
  geom_smooth(aes(color = type), method = "lm", size = 1)+
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed", size = 1) + 
  #stat_poly_eq(aes(x=MAOCstock,y=predictMAOC,label = paste(..rr.label..,sep="*\", \"*")), parse = T,size=6.8)+ 
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.95, label.x = 0.05,
               color = "#C71000FF", 
               data = da[da$type == "A", ]) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.85, label.x = 0.05,
               color = "#008EA0FF", 
               data = da[da$type == "B", ]) +
  scale_x_continuous(breaks = c(0,0.4,0.8) )+
  #                 labels = c(0,20,40,60)) +
  xlab(expression("RRTC"))+
  ylab(expression("Predicted RRTC"))+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=24),axis.text.x = element_text(color="black",size=24),axis.title.y = element_text(size=24),axis.title.x = element_text(size=24))+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))
#12*6
dev.off()


rf_results2$rf
predictor_var1<- data.frame(importance(rf_results2, scale = TRUE), check.names = FALSE)
predictor_sig1<-as.data.frame((rf_results2$pval)[,,2])
colnames(predictor_sig1)<-c("sig","other")
df_pre1<-cbind(predictor_var1,predictor_sig1)
df_pre1

df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.05&df_pre1$`%IncMSE.pval`<0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.05]<-"*"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.01]<-"**"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.001]<-"***"

rankImportance <- df_pre1 %>%
  mutate(Rank = paste0('#',dense_rank(desc(df_pre1$`%IncMSE`))))
varImportance <- data.frame(Variables = row.names(rankImportance))
rankImportance1 <- cbind.data.frame(rankImportance,varImportance)
rankImportance1

col <- colorRampPalette(c("grey", "#4477AA"))
x1 = reorder(rankImportance1$Variables, rankImportance1$`%IncMSE`)

pdf("E:/r/r1/相关性与模型/TN.pdf", width = 8, height = 4.5)
ggplot(rankImportance1, aes(x = x1, y = `%IncMSE`,fill=x1))+
  geom_bar(stat='identity',alpha=1,width=0.75)+
  scale_fill_manual(values = col(length(unique(x1))))+
  labs(x = ' ') +
  geom_text(aes(label = rankImportance1$`sig`,y= rankImportance1$`%IncMSE`+1.2, x =Variables),vjust = 0.6,size=7)+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=16),axis.text.x = element_text(color="black",size=16),axis.title.y = element_text(size=16),axis.title.x = element_text(size=16))+
  theme(panel.grid=element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
  ylab('MSE (%)')+
  annotate("text",label="Var. explained: 47.6 %",x=8,y=60,size=6)+
  annotate("text",label="MSR: 0.00560",x=8,y=50,size=6)+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))+
  #coord_flip()+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))
dev.off()
#使用训练集，查看预测精度

set.seed(112)
train <- sample(nrow(da_TN), nrow(da_TN)*0.8)
otu_train <- da_TN[train, ]
otu_test <- da_TN[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)
predict_TN1 <- predict(rf_results2, otu_train)
otu_train$predictTN<- predict_TN1
otu_train$type <- "A"
predict_TN2 <- predict(rf_results2, otu_test)
otu_test$predictTN <- predict_TN2
otu_test$type <- "B"
da<- bind_rows(otu_train, otu_test)
rmse <- sqrt(mean((da$RRTN - da$predictTN)^2))
rmse
pdf("E:/r/r1/相关性与模型/模型评价/XTN.pdf", width = 8, height = 6.3)
ggplot(da,aes(x=RRTN,y=predictTN,group=type,color=type))+
  geom_point(shape=21,alpha = 0.3,size=7, stroke = 0.4) +
  scale_color_manual(values = c("#C71000FF", "#008EA0FF")) +
  scale_fill_manual(values = c("#C71000FF", "#008EA0FF")) +
  geom_smooth(aes(color = type), method = "lm", size = 1)+
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed", size = 1) + 
  #stat_poly_eq(aes(x=MAOCstock,y=predictMAOC,label = paste(..rr.label..,sep="*\", \"*")), parse = T,size=6.8)+ 
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.95, label.x = 0.05,
               color = "#C71000FF", 
               data = da[da$type == "A", ]) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.85, label.x = 0.05,
               color = "#008EA0FF", 
               data = da[da$type == "B", ]) +
  #scale_x_continuous(lim=c(0,1),breaks = c(0,20,40,60), 
  #                 labels = c(0,20,40,60)) +
  xlab(expression("RRTN"))+
  ylab(expression("Predicted RRTN"))+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=24),axis.text.x = element_text(color="black",size=24),axis.title.y = element_text(size=24),axis.title.x = element_text(size=24))+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))
#12*6
dev.off()









rf_results3$rf
predictor_var1<- data.frame(importance(rf_results3, scale = TRUE), check.names = FALSE)
predictor_sig1<-as.data.frame((rf_results3$pval)[,,2])
colnames(predictor_sig1)<-c("sig","other")
df_pre1<-cbind(predictor_var1,predictor_sig1)
df_pre1

df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.05&df_pre1$`%IncMSE.pval`<0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.05]<-"*"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.01]<-"**"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.001]<-"***"

rankImportance <- df_pre1 %>%
  mutate(Rank = paste0('#',dense_rank(desc(df_pre1$`%IncMSE`))))
varImportance <- data.frame(Variables = row.names(rankImportance))
rankImportance1 <- cbind.data.frame(rankImportance,varImportance)
rankImportance1

col <- colorRampPalette(c("grey", "#4477AA"))
x1 = reorder(rankImportance1$Variables, rankImportance1$`%IncMSE`)

pdf("E:/r/r1/相关性与模型/TP.pdf", width = 8, height = 4.5)
ggplot(rankImportance1, aes(x = x1, y = `%IncMSE`,fill=x1))+
  geom_bar(stat='identity',alpha=1,width=0.75)+
  scale_fill_manual(values = col(length(unique(x1))))+
  labs(x = ' ') +
  geom_text(aes(label = rankImportance1$`sig`,y= rankImportance1$`%IncMSE`+1, x =Variables),vjust = 0.6,size=7)+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=16),axis.text.x = element_text(color="black",size=16),axis.title.y = element_text(size=16),axis.title.x = element_text(size=16))+
  theme(panel.grid=element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
  ylab('MSE (%)')+
  annotate("text",label="Var. explained: 37.0 %",x=8,y=42,size=6)+
  annotate("text",label="MSR: 0.0100",x=8,y=35,size=6)+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))+
  #coord_flip()+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))
dev.off()
#使用训练集，查看预测精度

set.seed(110)
train <- sample(nrow(da_TP), nrow(da_TP)*0.8)
otu_train <- da_TP[train, ]
otu_test <- da_TP[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)
predict_TP1 <- predict(rf_results3, otu_train)
otu_train$predictTP<- predict_TP1
otu_train$type <- "A"
predict_TP2 <- predict(rf_results3, otu_test)
otu_test$predictTP <- predict_TP2
otu_test$type <- "B"
da<- bind_rows(otu_train, otu_test)
rmse <- sqrt(mean((da$RRTP - da$predictTP)^2))
rmse
pdf("E:/r/r1/相关性与模型/模型评价/XTP.pdf", width = 8, height = 6.3)
ggplot(da,aes(x=RRTP,y=predictTP,group=type,color=type))+
  geom_point(shape=21,alpha = 0.3,size=7, stroke = 0.4) +
  scale_color_manual(values = c("#C71000FF", "#008EA0FF")) +
  scale_fill_manual(values = c("#C71000FF", "#008EA0FF")) +
  geom_smooth(aes(color = type), method = "lm", size = 1)+
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed", size = 1) + 
  #stat_poly_eq(aes(x=MAOCstock,y=predictMAOC,label = paste(..rr.label..,sep="*\", \"*")), parse = T,size=6.8)+ 
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.95, label.x = 0.05,
               color = "#C71000FF", 
               data = da[da$type == "A", ]) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.85, label.x = 0.05,
               color = "#008EA0FF", 
               data = da[da$type == "B", ]) +
  #scale_x_continuous(lim=c(0,1),breaks = c(0,20,40,60), 
  #                 labels = c(0,20,40,60)) +
  xlab(expression("RRTP"))+
  ylab(expression("Predicted RRTP"))+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=24),axis.text.x = element_text(color="black",size=24),axis.title.y = element_text(size=24),axis.title.x = element_text(size=24))+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))
#12*6
dev.off()




rf_results4$rf
predictor_var1<- data.frame(importance(rf_results4, scale = TRUE), check.names = FALSE)
predictor_sig1<-as.data.frame((rf_results4$pval)[,,2])
colnames(predictor_sig1)<-c("sig","other")
df_pre1<-cbind(predictor_var1,predictor_sig1)
df_pre1

df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.05&df_pre1$`%IncMSE.pval`<0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.05]<-"*"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.01]<-"**"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.001]<-"***"

rankImportance <- df_pre1 %>%
  mutate(Rank = paste0('#',dense_rank(desc(df_pre1$`%IncMSE`))))
varImportance <- data.frame(Variables = row.names(rankImportance))
rankImportance1 <- cbind.data.frame(rankImportance,varImportance)
rankImportance1

col <- colorRampPalette(c("grey", "#4477AA"))
x1 = reorder(rankImportance1$Variables, rankImportance1$`%IncMSE`)

pdf("E:/r/r1/相关性与模型/pH.pdf", width = 8, height = 4.5)
ggplot(rankImportance1, aes(x = x1, y = `%IncMSE`,fill=x1))+
  geom_bar(stat='identity',alpha=1,width=0.75)+
  scale_fill_manual(values = col(length(unique(x1))))+
  labs(x = ' ') +
  geom_text(aes(label = rankImportance1$`sig`,y= rankImportance1$`%IncMSE`+1.2, x =Variables),vjust = 0.6,size=7)+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=16),axis.text.x = element_text(color="black",size=16),axis.title.y = element_text(size=16),axis.title.x = element_text(size=16))+
  theme(panel.grid=element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
  ylab('MSE (%)')+
  annotate("text",label="Var. explained: 47.0 %",x=8,y=32,size=6)+
  annotate("text",label="MSR: 0.000649",x=8,y=26,size=6)+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))+
  #coord_flip()+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))
dev.off()
#使用训练集，查看预测精度

set.seed(122)
train <- sample(nrow(da_pH), nrow(da_pH)*0.8)
otu_train <- da_pH[train, ]
otu_test <- da_pH[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)
predict_pH1 <- predict(rf_results4, otu_train)
otu_train$predictpH<- predict_pH1
otu_train$type <- "A"
predict_pH2 <- predict(rf_results4, otu_test)
otu_test$predictpH <- predict_pH2
otu_test$type <- "B"
da<- bind_rows(otu_train, otu_test)
rmse <- sqrt(mean((da$RRpH - da$predictpH)^2))
rmse
pdf("E:/r/r1/相关性与模型/模型评价/XpH.pdf", width = 8, height = 6.3)
ggplot(da,aes(x=RRpH,y=predictpH,group=type,color=type))+
  geom_point(shape=21,alpha = 0.3,size=7, stroke = 0.4) +
  scale_color_manual(values = c("#C71000FF", "#008EA0FF")) +
  scale_fill_manual(values = c("#C71000FF", "#008EA0FF")) +
  geom_smooth(aes(color = type), method = "lm", size = 1)+
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed", size = 1) + 
  #stat_poly_eq(aes(x=MAOCstock,y=predictMAOC,label = paste(..rr.label..,sep="*\", \"*")), parse = T,size=6.8)+ 
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.95, label.x = 0.05,
               color = "#C71000FF", 
               data = da[da$type == "A", ]) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.85, label.x = 0.05,
               color = "#008EA0FF", 
               data = da[da$type == "B", ]) +
  #scale_x_continuous(lim=c(0,1),breaks = c(0,20,40,60), 
  #                 labels = c(0,20,40,60)) +
  xlab(expression("RRpH"))+
  ylab(expression("Predicted RRpH"))+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=24),axis.text.x = element_text(color="black",size=24),axis.title.y = element_text(size=24),axis.title.x = element_text(size=24))+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))
#12*6
dev.off()




rf_results5$rf
predictor_var1<- data.frame(importance(rf_results5, scale = TRUE), check.names = FALSE)
predictor_sig1<-as.data.frame((rf_results5$pval)[,,2])
colnames(predictor_sig1)<-c("sig","other")
df_pre1<-cbind(predictor_var1,predictor_sig1)
df_pre1

df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`>=0.05&df_pre1$`%IncMSE.pval`<0.1]<-" "
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.05]<-"*"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.01]<-"**"
df_pre1$`sig`[df_pre1$`%IncMSE.pval`<0.001]<-"***"

rankImportance <- df_pre1 %>%
  mutate(Rank = paste0('#',dense_rank(desc(df_pre1$`%IncMSE`))))
varImportance <- data.frame(Variables = row.names(rankImportance))
rankImportance1 <- cbind.data.frame(rankImportance,varImportance)
rankImportance1

col <- colorRampPalette(c("grey", "#4477AA"))
x1 = reorder(rankImportance1$Variables, rankImportance1$`%IncMSE`)

pdf("E:/r/r1/相关性与模型/Yield.pdf", width = 8, height = 4.5)
ggplot(rankImportance1, aes(x = x1, y = `%IncMSE`,fill=x1))+
  geom_bar(stat='identity',alpha=1,width=0.75)+
  scale_fill_manual(values = col(length(unique(x1))))+
  labs(x = ' ') +
  geom_text(aes(label = rankImportance1$`sig`,y= rankImportance1$`%IncMSE`+2, x =Variables),vjust = 0.6,size=7)+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=16),axis.text.x = element_text(color="black",size=16),axis.title.y = element_text(size=16),axis.title.x = element_text(size=16))+
  theme(panel.grid=element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
  ylab('MSE (%)')+
  annotate("text",label="Var. explained: 66.6 %",x=8,y=85,size=6)+
  annotate("text",label="MSR: 0.00300",x=8,y=75,size=6)+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))+
  #coord_flip()+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))
dev.off()
#使用训练集，查看预测精度

set.seed(123)
train <- sample(nrow(da_Yield), nrow(da_Yield)*0.8)
otu_train <- da_Yield[train, ]
otu_test <- da_Yield[-train, ]
cv_folds <- trainControl(method = "cv", 10,allowParallel = TRUE)
predict_Yield1 <- predict(rf_results5, otu_train)
otu_train$predictYield<- predict_Yield1
otu_train$type <- "A"
predict_Yield2 <- predict(rf_results5, otu_test)
otu_test$predictYield <- predict_Yield2
otu_test$type <- "B"
da<- bind_rows(otu_train, otu_test)
rmse <- sqrt(mean((da$RRYield - da$predictYield)^2))
rmse

pdf("E:/r/r1/相关性与模型/模型评价/XYield.pdf", width = 8, height = 6.3)
ggplot(da,aes(x=RRYield,y=predictYield,group=type,color=type))+
  geom_point(shape=21,alpha = 0.3,size=7, stroke = 0.4) +
  scale_color_manual(values = c("#C71000FF", "#008EA0FF")) +
  scale_fill_manual(values = c("#C71000FF", "#008EA0FF")) +
  geom_smooth(aes(color = type), method = "lm", size = 1)+
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed", size = 1) + 
  #stat_poly_eq(aes(x=MAOCstock,y=predictMAOC,label = paste(..rr.label..,sep="*\", \"*")), parse = T,size=6.8)+ 
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.95, label.x = 0.05,
               color = "#C71000FF", 
               data = da[da$type == "A", ]) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "*\", \"*")), 
               formula = y ~ x, parse = TRUE, size = 9, 
               label.y = 0.85, label.x = 0.05,
               color = "#008EA0FF", 
               data = da[da$type == "B", ]) +
  #scale_x_continuous(lim=c(0,1),breaks = c(0,20,40,60), 
  #                 labels = c(0,20,40,60)) +
  xlab(expression("RRYield"))+
  ylab(expression("Predicted RRYield"))+
  theme_bw()+
  theme(axis.text.y = element_text(color="black",size=24),axis.text.x = element_text(color="black",size=24),axis.title.y = element_text(size=24),axis.title.x = element_text(size=24))+
  theme(legend.position="")+theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  theme(axis.ticks = element_line(size = 0.8, color = "black"), axis.ticks.length = unit(0.15, "cm"))
#12*6
dev.off()
