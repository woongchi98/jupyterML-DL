data <- iris
data$Species[data$Species == "versicolor"] <- "virginica" ## Versicolor를 virginica로
data$Species <- factor(data$Species)  ## 데이터 프레임의 범주 조절

lda_error = c() 
qda_error = c()
knn_error = c()
knncv_error = c()
glm_error = c() # 각 오차율 저장할 리스트

library(MASS)

for (i in 1:50){ # 훈련 및 테스트 50번 반복시행

train <- sample(1:150, 100) #train 데이터 분리

###### LDA
ld <- lda(formula = Species ~., data= data, subset = train) # lda 적합
predict(ld, data[-train,])$class #훈련데이터로 예측
(tt <- table(data$Species[-train], predict(ld, data[-train,])$class)) #confusion matrix
lda_error[i] <- (1-sum(tt[row(tt) == col(tt)])/sum(tt)) #오분류율

###### QDA
qda.fits <- qda(Species ~., data= data, subset=train)
predict(qda.fits, data[-train,])$class #훈련데이터로 예측
(tt3 <- table(data$Species[-train], predict(qda.fits, data[-train,])$class)) #confusion matrix
qda_error[i] <- (1-sum(tt3[row(tt3) == col(tt3)])/sum(tt3)) #오분류율

###### KNN
idx <- sample(x = c("train","test"), size = nrow(data), replace = TRUE, prob = c(2,1))
train <- data[idx=="train",]
test <- data[idx=="test",]

train_x <- (train[,-5]) #5번째 행만 제거
test_x <- test[,-5]

train_y <- (train[,5]) # 5번째 행만 
test_y <- test[,5]  ##Train test set 분리

library(class)
knn.fits <- knn(train=train_x, test=test_x, cl=train_y, k=1)
tt2 <- table(knn.fits, test_y)
knn_error[i] <- (1-sum(tt2[row(tt2) == col(tt2)])/sum(tt2)) #오분류율


######로지스틱
glm.fits <- glm(Species~., data = train, family = "binomial") #로지스틱 이진분류 적합

logit_p <- predict(glm.fits, test, type="response") # 테스트 데이터

is_correct_p <- as.numeric(test$Species) == ifelse(logit_p <0.5, 1, 2) #로지스틱 정확도 계산
glm_error[i] <- (1 - sum(is_correct_p) / length(is_correct_p)) 


######KNN-CV
## library(caret)
trControl <- trainControl(method  = "cv", number  = 5) #5개로 나눠서 cv
fit <- train(Species ~ ., method = "knn", tuneGrid = expand.grid(k = 1:10),
             trControl  = trControl, metric= "Accuracy", data = data) #k갯수 1~10까지 비교
MAX <- max(fit[4]$results$Accuracy) #가장 높은값을 갖는 accuracy
knncv_error[i] = 1- MAX
}

boxplot(qda_error,glm_error,lda_error,knn_error,knncv_error, 
        main = "Error Rate", names=c("qda","logistic", "lda", "knn","knn_cv"))