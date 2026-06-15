install.packages("randomForest")
library(randomForest)

install.packages("dplyr")
library(dplyr)

install.packages("pls")
library(pls)

install.packages("keras")
reticulate::install_miniconda()
keras::install_keras(method = "conda", python_version = "3.10")

install.packages("doParallel")
library(doParallel)


#Simulation for Poly Model

# number of simulations to run (100 is just an example!)
nsim <- 50
# set parallelization
# detect the number of Cores available in the system
nCores <- parallel::detectCores()
cl <- parallel::makeCluster(nCores);
doParallel::registerDoParallel(cl)
# Note: foreach is different than the traditional for loop
# You need to include the packages in the foreach loop!
results <- foreach(i=1:nsim, .combine=rbind, .packages = c('randomForest', 'dplyr')) %dopar% {
  #Drawing 5 random variables
  set.seed(NULL)
  
  n <- 1000
  x1 = rgamma(n,2,1); x2 = rnorm(n,0,2);
  x3 = rweibull(n,2,2); x4 = rlogis(n,2,1);
  x5 = rbeta(n,2,1);
  x = cbind(x1,x2,x3,x4,x5)
  ###############################################
  #transform into independent random variables
  # find the current correlation matrix
  c1 <- var(x)
  # cholesky decomposition to get independence
  chol1 <- solve(chol(c1))
  x <- x %*% chol1
  ###############################################
  #generate random correlation matrix
  R <- matrix(runif(ncol(x)^2,-1,1), ncol=ncol(x))
  RtR <- R %*% t(R)
  corr <- cov2cor(RtR)
  # check that it is positive definite
  sum((eigen(corr)$values>0))==ncol(x)
  ################################################
  #transform according to this correlation matrix
  x <- x %*% chol(corr)
  datam <- as.data.frame(x)
  datam <- datam %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  datam_Poly <- as.data.frame(x)
  datam_Poly <- datam_Poly %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  
  #Specification
  y <- datam$x1 + ((datam$x2)^2 * datam$x3) + ((datam$x4 *  datam$x1 *  datam$x5)/10)
  datam_Poly$y <- datam_Poly$x1 + ((datam_Poly$x2)^2 * datam_Poly$x3) + ((datam_Poly$x4 *  datam_Poly$x1 *  datam_Poly$x5)/10) 
  
  
  datam <- scale(datam)
  
  
  #Creating a Test Sample
  set.seed(0)
  Test_Sample <- sample(1:n, n/2)
  
  Test <- datam[Test_Sample, ]
  
  #Poly Model
  
  Poly <- lm(y ~ poly(x1, 3) + poly(x2, 3) + poly(x3, 3) + poly(x4, 3) + poly(x5, 3), data = datam_Poly[-Test_Sample, ] )
  summary(Poly)
  
  pred_Poly <- predict(Poly, datam_Poly[Test_Sample, ])
  
  MSE_Poly <- (mean((y[Test_Sample] - pred_Poly)^2))
  
  c(MSE_Poly)
  
  
                                                            }
#cleanUp
parallel::stopCluster(cl)
rm(cl)
# report results
AvgMSE_Poly <- c(mean(results))



#Simulation for Random Forest

nsim <- 50
# set parallelization
# detect the number of Cores available in the system
nCores <- parallel::detectCores()
cl <- parallel::makeCluster(nCores);
doParallel::registerDoParallel(cl)
# Note: foreach is different than the traditional for loop
# You need to include the packages in the foreach loop!
results <- foreach(i=1:nsim, .combine=rbind, .packages = c('randomForest', 'dplyr')) %dopar% {
  #Drawing 5 random variables
  set.seed(NULL)
  
  
  n <- 1000
  x1 = rgamma(n,2,1); x2 = rnorm(n,0,2);
  x3 = rweibull(n,2,2); x4 = rlogis(n,2,1);
  x5 = rbeta(n,2,1);
  x = cbind(x1,x2,x3,x4,x5)
  ###############################################
  #transform into independent random variables
  # find the current correlation matrix
  c1 <- var(x)
  # cholesky decomposition to get independence
  chol1 <- solve(chol(c1))
  x <- x %*% chol1
  ###############################################
  #generate random correlation matrix
  R <- matrix(runif(ncol(x)^2,-1,1), ncol=ncol(x))
  RtR <- R %*% t(R)
  corr <- cov2cor(RtR)
  # check that it is positive definite
  sum((eigen(corr)$values>0))==ncol(x)
  ################################################
  #transform according to this correlation matrix
  x <- x %*% chol(corr)
  datam <- as.data.frame(x)
  datam <- datam %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  datam_Poly <- as.data.frame(x)
  datam_Poly <- datam_Poly %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  
  #Specification
  y <- datam$x1 + ((datam$x2)^2 * datam$x3) + ((datam$x4 *  datam$x1 *  datam$x5)/10)
  datam_Poly$y <- datam_Poly$x1 + ((datam_Poly$x2)^2 * datam_Poly$x3) + ((datam_Poly$x4 *  datam_Poly$x1 *  datam_Poly$x5)/10) 
  
  
  datam <- scale(datam)
  
  
  #Creating a Test Sample
  set.seed(0)
  Test_Sample <- sample(1:n, n/2)
  
  Test <- datam[Test_Sample, ]
  
  #RF Model
  
  RF_Model <- randomForest(y ~ ., data = datam_Poly[-Test_Sample, ], ntree = 1000, mtry = 4, importance = TRUE)
  
  pred_RF <- predict(RF_Model, newdata = datam_Poly[Test_Sample, ])
  
  MSE_RF <- mean((pred_RF - y[Test_Sample])^2)
  
  c(MSE_RF)
  
}
#cleanUp
parallel::stopCluster(cl)
rm(cl)
# report results
AvgMSE_RF <- c(mean(results))



#Simulation for Neural Net

NN_Sim_MSE <- matrix(nrow = 50, ncol = 1)

for (i in 1:50){

print(i)

set.seed(NULL)
  
  
n <- 1000
x1 = rgamma(n,2,1); x2 = rnorm(n,0,2);
x3 = rweibull(n,2,2); x4 = rlogis(n,2,1);
x5 = rbeta(n,2,1);
x = cbind(x1,x2,x3,x4,x5)
###############################################
#transform into independent random variables
# find the current correlation matrix
c1 <- var(x)
# cholesky decomposition to get independence
chol1 <- solve(chol(c1))
x <- x %*% chol1
###############################################
#generate random correlation matrix
R <- matrix(runif(ncol(x)^2,-1,1), ncol=ncol(x))
RtR <- R %*% t(R)
corr <- cov2cor(RtR)
# check that it is positive definite
sum((eigen(corr)$values>0))==ncol(x)
################################################
#transform according to this correlation matrix
x <- x %*% chol(corr)
datam <- as.data.frame(x)
datam <- datam %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)

datam_Poly <- as.data.frame(x)
datam_Poly <- datam_Poly %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)


#Specification
y <- datam$x1 + ((datam$x2)^2 * datam$x3) + ((datam$x4 *  datam$x1 *  datam$x5)/10)
datam_Poly$y <- datam_Poly$x1 + ((datam_Poly$x2)^2 * datam_Poly$x3) + ((datam_Poly$x4 *  datam_Poly$x1 *  datam_Poly$x5)/10) 


datam <- scale(datam)


#Creating a Test Sample
set.seed(0)
Test_Sample <- sample(1:n, n/2)

Test <- datam[Test_Sample, ]

#Neural nets

library(keras)

modnn <- keras_model_sequential() %>%  layer_dense(units = 64, activation = "sigmoid", 
    input_shape = ncol(x)) %>%  layer_dense(units = 32, 
    activation = "sigmoid") %>% layer_dense(units = 16, 
    activation = "sigmoid")

summary(modnn)

modnn %>% compile(loss = "mse", optimizer = optimizer_rmsprop (), metrics = list("mean_absolute_error"))

Neural <- modnn %>% fit(datam[-Test_Sample , ], y[-Test_Sample], batch_size = 64, epochs = 300, validation_data = list(datam[Test_Sample, ], y[Test_Sample])
)

pred <- predict(modnn, datam[Test_Sample, ])

MSE_Neural <- mean((y[Test_Sample] - pred)^2)

NN_Sim_MSE[i] <- MSE_Neural

}

mean(NN_Sim_MSE)








#Simulation for Neural Net Spec #2

NN_Sim_MSE <- matrix(nrow = 50, ncol = 1)

for (i in 1:50){
  
  print(i)
  
  set.seed(NULL)
  
  
  n <- 1000
  x1 = rgamma(n,2,1); x2 = rnorm(n,0,2);
  x3 = rweibull(n,2,2); x4 = rlogis(n,2,1);
  x5 = rbeta(n,2,1);
  x = cbind(x1,x2,x3,x4,x5)
  ###############################################
  #transform into independent random variables
  # find the current correlation matrix
  c1 <- var(x)
  # cholesky decomposition to get independence
  chol1 <- solve(chol(c1))
  x <- x %*% chol1
  ###############################################
  #generate random correlation matrix
  R <- matrix(runif(ncol(x)^2,-1,1), ncol=ncol(x))
  RtR <- R %*% t(R)
  corr <- cov2cor(RtR)
  # check that it is positive definite
  sum((eigen(corr)$values>0))==ncol(x)
  ################################################
  #transform according to this correlation matrix
  x <- x %*% chol(corr)
  datam <- as.data.frame(x)
  datam <- datam %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  datam_Poly <- as.data.frame(x)
  datam_Poly <- datam_Poly %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  
  #Specification
  y <- log(((datam$x1)^4)/(10) + abs(datam$x2) + (datam$x3)^2) + (datam$x4 * datam$x2 *
              sin(datam$x5)) + rnorm(1,0,1)

  
  datam <- scale(datam)
  
  
  #Creating a Test Sample
  set.seed(0)
  Test_Sample <- sample(1:n, n/2)
  
  Test <- datam[Test_Sample, ]
  
  #Neural nets
  
  library(keras)
  
  modnn <- keras_model_sequential() %>%  layer_dense(units = 64, activation = "sigmoid", 
                                                     input_shape = ncol(x)) %>%  layer_dense(units = 32, 
                                                                                             activation = "sigmoid") %>% layer_dense(units = 16, 
                                                                                                                                     activation = "sigmoid")
  
  summary(modnn)
  
  modnn %>% compile(loss = "mse", optimizer = optimizer_rmsprop (), metrics = list("mean_absolute_error"))
  
  Neural <- modnn %>% fit(datam[-Test_Sample , ], y[-Test_Sample], batch_size = 64, epochs = 300, validation_data = list(datam[Test_Sample, ], y[Test_Sample])
  )
  
  pred <- predict(modnn, datam[Test_Sample, ])
  
  MSE_Neural <- mean((y[Test_Sample] - pred)^2)
  
  NN_Sim_MSE[i] <- MSE_Neural
  
}

mean(NN_Sim_MSE)






#Simulation for Random Forest, Spec #2

nsim <- 50
# set parallelization
# detect the number of Cores available in the system
nCores <- parallel::detectCores()
cl <- parallel::makeCluster(nCores);
doParallel::registerDoParallel(cl)
# Note: foreach is different than the traditional for loop
# You need to include the packages in the foreach loop!
results <- foreach(i=1:nsim, .combine=rbind, .packages = c('randomForest', 'dplyr')) %dopar% {
  #Drawing 5 random variables
  set.seed(NULL)
  
  
  n <- 1000
  x1 = rgamma(n,2,1); x2 = rnorm(n,0,2);
  x3 = rweibull(n,2,2); x4 = rlogis(n,2,1);
  x5 = rbeta(n,2,1);
  x = cbind(x1,x2,x3,x4,x5)
  ###############################################
  #transform into independent random variables
  # find the current correlation matrix
  c1 <- var(x)
  # cholesky decomposition to get independence
  chol1 <- solve(chol(c1))
  x <- x %*% chol1
  ###############################################
  #generate random correlation matrix
  R <- matrix(runif(ncol(x)^2,-1,1), ncol=ncol(x))
  RtR <- R %*% t(R)
  corr <- cov2cor(RtR)
  # check that it is positive definite
  sum((eigen(corr)$values>0))==ncol(x)
  ################################################
  #transform according to this correlation matrix
  x <- x %*% chol(corr)
  datam <- as.data.frame(x)
  datam <- datam %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  datam_Poly <- as.data.frame(x)
  datam_Poly <- datam_Poly %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  
  #Specification
  y <- datam$x1 + log(((datam$x1)^4)/(10) + abs(datam$x2) + (datam$x3)^2) + (datam$x4 * datam$x2 *
                   sin(datam$x5)) + rnorm(1,0,1) 
  datam_Poly$y <- log(((datam$x1)^4)/(10) + abs(datam$x2) + (datam$x3)^2) + (datam$x4 * datam$x2 *
                  sin(datam$x5)) + rnorm(1,0,1) 
  
  
  datam <- scale(datam)
  
  
  #Creating a Test Sample
  set.seed(0)
  Test_Sample <- sample(1:n, n/2)
  
  Test <- datam[Test_Sample, ]
  
  #RF Model
  
  RF_Model <- randomForest(y ~ ., data = datam_Poly[-Test_Sample, ], ntree = 1000, mtry = 4, importance = TRUE)
  
  pred_RF <- predict(RF_Model, newdata = datam_Poly[Test_Sample, ])
  
  MSE_RF <- mean((pred_RF - y[Test_Sample])^2)
  
  c(MSE_RF)
  
}
#cleanUp
parallel::stopCluster(cl)
rm(cl)
# report results
AvgMSE_RF <- c(mean(results))






#Simulation for Poly Model, Spec 2

# number of simulations to run (100 is just an example!)
nsim <- 50
# set parallelization
# detect the number of Cores available in the system
nCores <- parallel::detectCores()
cl <- parallel::makeCluster(nCores);
doParallel::registerDoParallel(cl)
# Note: foreach is different than the traditional for loop
# You need to include the packages in the foreach loop!
results <- foreach(i=1:nsim, .combine=rbind, .packages = c('randomForest', 'dplyr')) %dopar% {
  #Drawing 5 random variables
  set.seed(NULL)
  
  n <- 1000
  x1 = rgamma(n,2,1); x2 = rnorm(n,0,2);
  x3 = rweibull(n,2,2); x4 = rlogis(n,2,1);
  x5 = rbeta(n,2,1);
  x = cbind(x1,x2,x3,x4,x5)
  ###############################################
  #transform into independent random variables
  # find the current correlation matrix
  c1 <- var(x)
  # cholesky decomposition to get independence
  chol1 <- solve(chol(c1))
  x <- x %*% chol1
  ###############################################
  #generate random correlation matrix
  R <- matrix(runif(ncol(x)^2,-1,1), ncol=ncol(x))
  RtR <- R %*% t(R)
  corr <- cov2cor(RtR)
  # check that it is positive definite
  sum((eigen(corr)$values>0))==ncol(x)
  ################################################
  #transform according to this correlation matrix
  x <- x %*% chol(corr)
  datam <- as.data.frame(x)
  datam <- datam %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  datam_Poly <- as.data.frame(x)
  datam_Poly <- datam_Poly %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)
  
  
  #Specification
  y <- datam$x1 + log(((datam$x1)^4)/(10) + abs(datam$x2) + (datam$x3)^2) + (datam$x4 * datam$x2 *
       sin(datam$x5)) + rnorm(1,0,1) 
  datam_Poly$y <- log(((datam$x1)^4)/(10) + abs(datam$x2) + (datam$x3)^2) + (datam$x4 * datam$x2 *
                  sin(datam$x5)) + rnorm(1,0,1) 
  
  
  datam <- scale(datam)
  
  
  #Creating a Test Sample
  set.seed(0)
  Test_Sample <- sample(1:n, n/2)
  
  Test <- datam[Test_Sample, ]
  
  #Poly Model
  
  Poly <- lm(y ~ poly(x1, 3) + poly(x2, 3) + poly(x3, 3) + poly(x4, 3) + poly(x5, 3), data = datam_Poly[-Test_Sample, ] )
  summary(Poly)
  
  pred_Poly <- predict(Poly, datam_Poly[Test_Sample, ])
  
  MSE_Poly <- (mean((y[Test_Sample] - pred_Poly)^2))
  
  c(MSE_Poly)
  
  
}
#cleanUp
parallel::stopCluster(cl)
rm(cl)
# report results
AvgMSE_Poly <- c(mean(results))







#Problem #2

airbnb_data_original <- read.csv("~/ML Work/airbnb_data.csv")

head(airbnb_data_original)

dim(airbnb_data_original)


airbnb_data_clean <- airbnb_data_original

#Remove NA values
airbnb_data_clean <- airbnb_data_clean %>%
  filter(!is.na(airbnb_data_clean$price))

airbnb_data_clean <- airbnb_data_clean %>%
  filter(!is.na(accommodates), !is.na(beds), !is.na(number_of_reviews), !is.na(review_scores_rating))


#Check if NA values were deleted 
sum(is.na(airbnb_data_original$price))
length(airbnb_data_original$price) - length(airbnb_data_clean$price)

#Create host_experience variable
date <- as.Date("2023-06-05")
airbnb_data_clean$host_since <- as.Date(airbnb_data_clean$host_since)

host_experience <- (date - airbnb_data_clean$host_since)
airbnb_data_clean$host_experience <- as.numeric(host_experience/365)

airbnb_data_clean <- airbnb_data_clean %>%
  filter(!is.na(host_experience))

#Create entire_apt variable
airbnb_data_clean$entire_apt <- airbnb_data_clean$room_type

for(i in 1:nrow(airbnb_data_clean)){
  
  if (airbnb_data_clean$room_type[i] == "Entire home/apt"){
    airbnb_data_clean$entire_apt[i] <- 1
    
  }
  
  else if (is.na(airbnb_data_clean$entire_apt[i]) == TRUE) {
    airbnb_data_clean$entire_apt[i] <-  airbnb_data_clean$entire_apt[i]
    
    
  }
  
  else {
    airbnb_data_clean$entire_apt[i] <- 0
    
  }
  
}




airbnb_data_clean <- airbnb_data_clean %>%
  filter(!is.na(entire_apt))

#Prepare Superhost data
airbnb_data_clean <- airbnb_data_clean %>%
  filter(!is.na(host_is_superhost))


#Sorting Data
airbnb_data_Total <- arrange(airbnb_data_clean, id)

#Test Sample
set.seed(0)
TS <- sample(nrow(airbnb_data_Total), 0.90 * nrow(airbnb_data_Total))

airbnb_data_Train <- airbnb_data_Total[TS, ]
airbnb_data_Test <- airbnb_data_Total[-TS, ]



#PCA Analysis

airbnb_data_Ordered <-
  subset(airbnb_data_Train, select = c("accommodates", "beds", "host_experience", 
          "host_is_superhost", "entire_apt", "number_of_reviews", "review_scores_rating")) 

airbnb_data_Ordered$accommodates <- as.numeric(airbnb_data_Ordered$accommodates)
airbnb_data_Ordered$beds <- as.numeric(airbnb_data_Ordered$beds)
airbnb_data_Ordered$host_experience <- as.numeric(airbnb_data_Ordered$host_experience)
airbnb_data_Ordered$host_is_superhost <- as.numeric(airbnb_data_Ordered$host_is_superhost)
airbnb_data_Ordered$entire_apt <- as.numeric(airbnb_data_Ordered$entire_apt)
airbnb_data_Ordered$number_of_reviews <- as.numeric(airbnb_data_Ordered$number_of_reviews)
airbnb_data_Ordered$review_scores_rating <- as.numeric(airbnb_data_Ordered$review_scores_rating)


accommodatesSq <- c(as.numeric((airbnb_data_Ordered$accommodates)^2)) 
bedsSq <- c(as.numeric((airbnb_data_Ordered$beds)^2))
host_experienceSq <- c(as.numeric((airbnb_data_Ordered$host_experience)^2))
number_of_reviewsSq <- c(as.numeric((airbnb_data_Ordered$number_of_reviews)^2))
review_scores_ratingSq <- c(as.numeric((airbnb_data_Ordered$review_scores_rating)^2))

accommodates_W_host_is_superhost <- c(airbnb_data_Ordered$accommodates * airbnb_data_Ordered$host_is_superhost)
beds_W_host_is_superhost <- c(airbnb_data_Ordered$beds * airbnb_data_Ordered$host_is_superhost)
host_experience_W_host_is_superhost <- c(airbnb_data_Ordered$host_experience * airbnb_data_Ordered$host_is_superhost)
number_of_reviews_W_host_is_superhost <- c(airbnb_data_Ordered$number_of_reviews * airbnb_data_Ordered$host_is_superhost)
review_scores_rating_W_host_is_superhost <- c(airbnb_data_Ordered$review_scores_rating * airbnb_data_Ordered$host_is_superhost)

accommodates_W_entire_apt <- c(airbnb_data_Ordered$accommodates * as.numeric(airbnb_data_Ordered$entire_apt))
beds_W_entire_apt <- c(airbnb_data_Ordered$beds * as.numeric(airbnb_data_Ordered$entire_apt))
host_experience_W_entire_apt <- c(airbnb_data_Ordered$host_experience * as.numeric(airbnb_data_Ordered$entire_apt))
number_of_reviews_W_entire_apt <- c(airbnb_data_Ordered$number_of_reviews * as.numeric(airbnb_data_Ordered$entire_apt))
review_scores_rating_W_entire_apt <- c(airbnb_data_Ordered$review_scores_rating * as.numeric(airbnb_data_Ordered$entire_apt))


accommodates_W_host_is_superhost <- as.numeric(accommodates_W_host_is_superhost)
beds_W_host_is_superhost <-  as.numeric(beds_W_host_is_superhost)
host_experience_W_host_is_superhost <-  as.numeric(host_experience_W_host_is_superhost)
number_of_reviews_W_host_is_superhost <-  as.numeric(number_of_reviews_W_host_is_superhost)
review_scores_rating_W_host_is_superhost <-  as.numeric(review_scores_rating_W_host_is_superhost)

accommodates_W_entire_apt <- as.numeric(accommodates_W_entire_apt)
beds_W_entire_apt <- as.numeric(beds_W_entire_apt)
host_experience_W_entire_apt <- as.numeric(host_experience_W_entire_apt)
number_of_reviews_W_entire_apt <- as.numeric(number_of_reviews_W_entire_apt)
review_scores_rating_W_entire_apt <- as.numeric(review_scores_rating_W_entire_apt)






airbnb_data_PCA_1 <- cbind(airbnb_data_Ordered, accommodatesSq, bedsSq, host_experienceSq, number_of_reviewsSq, review_scores_ratingSq,
                             accommodates_W_host_is_superhost, beds_W_host_is_superhost, host_experience_W_host_is_superhost
                             ,number_of_reviews_W_host_is_superhost, review_scores_rating_W_host_is_superhost,
                             accommodates_W_entire_apt, beds_W_entire_apt, host_experience_W_entire_apt, 
                             number_of_reviews_W_entire_apt, review_scores_rating_W_entire_apt)



PCA <- prcomp(airbnb_data_PCA_1, scale = TRUE)

PCA_Var <- PCA$sdev

PVE <-  PCA$sdev/sum(PCA_Var)

cumsum(PVE[1:4])

PVE_COV <- PCA$rotation
PVE_COV <- subset(PVE_COV, select = c("PC1", "PC2", "PC3", "PC4", "PC5")) 

PVE_COV <- cbind(PVE_COV, airbnb_data_Train$price)
colnames(PVE_COV)[6] <- c("price")

PVE_Data <- data.frame(PVE_COV)


#Regression with PCA

set.seed(0)
PCA_Model <- pcr(price ~ I(accommodates^2) + I(beds^2) + I(host_experience^2) + 
                   + I(number_of_reviews^2) + I(review_scores_rating^2) + accommodates * host_is_superhost +
                   beds * host_is_superhost + host_experience * host_is_superhost + number_of_reviews * host_is_superhost +
                   review_scores_rating * host_is_superhost + entire_apt * accommodates + entire_apt * beds +
                   entire_apt * host_experience + entire_apt * number_of_reviews + entire_apt * review_scores_rating, data = airbnb_data_Train,
                 scale = TRUE,  ncomp = 4)

summary(PCA_Model)

pred <- predict(PCA_Model, airbnb_data_Test)

MSE_PCA <- mean((airbnb_data_Test$price - pred)^2)

#Random Forest

RF_Model_2 <- randomForest(price ~ I(accommodates^2) + I(beds^2) + I(host_experience^2) + 
                             + I(number_of_reviews^2) + I(review_scores_rating^2) + accommodates * host_is_superhost +
                             beds * host_is_superhost + host_experience * host_is_superhost + number_of_reviews * host_is_superhost +
                             review_scores_rating * host_is_superhost + entire_apt * accommodates + entire_apt * beds +
                             entire_apt * host_experience + entire_apt * number_of_reviews + entire_apt * review_scores_rating, 
                           data = airbnb_data_Train, ntree = 1000, mtry = sqrt(22), importance = TRUE)

pred_RF <- predict(RF_Model_2, newdata = airbnb_data_Test)

MSE_RF_2 <- mean((pred_RF - airbnb_data_Test$price)^2)

#Ridge
install.packages("glmnet")
library(glmnet)

x <- model.matrix(price ~ I(accommodates^2) + I(beds^2) + I(host_experience^2) + 
                    + I(number_of_reviews^2) + I(review_scores_rating^2) + accommodates * host_is_superhost +
                    beds * host_is_superhost + host_experience * host_is_superhost + number_of_reviews * host_is_superhost +
                    review_scores_rating * host_is_superhost + entire_apt * accommodates + entire_apt * beds +
                    entire_apt * host_experience + entire_apt * number_of_reviews + entire_apt * review_scores_rating, 
                  data = airbnb_data_Train)

x2 <- model.matrix(price ~ I(accommodates^2) + I(beds^2) + I(host_experience^2) + 
                     + I(number_of_reviews^2) + I(review_scores_rating^2) + accommodates * host_is_superhost +
                     beds * host_is_superhost + host_experience * host_is_superhost + number_of_reviews * host_is_superhost +
                     review_scores_rating * host_is_superhost + entire_apt * accommodates + entire_apt * beds +
                     entire_apt * host_experience + entire_apt * number_of_reviews + entire_apt * review_scores_rating, 
                   data = airbnb_data_Test)



y <- airbnb_data_Train$price


#Ridge

set.seed(0)
Cross_ridge <- cv.glmnet(x, y, alpha = 0, nfolds = 10 )

lambda_ridge <- Cross_ridge$lambda.min

Ridge_cv <- glmnet(x, y, alpha = 0, lambda = c(lambda_ridge, 10, 5, 0))

Ridge_pred_cv <- predict(Ridge_cv, s = lambda_ridge, newx = x2 )

MSE_ridge_cv <- mean((airbnb_data_Test$price - Ridge_pred_cv)^2)


#Lasso
Cross_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10 )

lambda_lasso <- Cross_lasso$lambda.min

Lasso_cv <- glmnet(x, y, alpha = 1, lambda = c(lambda_lasso, 10, 5, 0))

Lasso_pred_cv <- predict(Ridge_cv, s = lambda_lasso, newx = x2)

MSE_lasso_cv <- mean((airbnb_data_Test$price - Lasso_pred_cv)^2)


#Polynomials

Lin_Reg_Polynomials <- lm(price ~ I(accommodates^2) + I(beds^2) + I(host_experience^2) + 
                            + I(number_of_reviews^2) + I(review_scores_rating^2) + accommodates * host_is_superhost +
                            beds * host_is_superhost + host_experience * host_is_superhost + number_of_reviews * host_is_superhost +
                            review_scores_rating * host_is_superhost + entire_apt * accommodates + entire_apt * beds +
                            entire_apt * host_experience + entire_apt * number_of_reviews + entire_apt * review_scores_rating, 
                          data = airbnb_data_Train)

summary(Lin_Reg_Polynomials)

pred <- predict(Lin_Reg_Polynomials, airbnb_data_Test)

MSE_LinRegPolynomials <- mean((airbnb_data_Test$price - pred)^2)


sum(is.na(airbnb_data_Total$price))

