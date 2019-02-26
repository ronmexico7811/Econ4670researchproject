library(readr)
shelter_skim <- read_csv("Econ4670researchproject/shelter_skim.csv")

#Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#Import Library
library(e1071) #Contains the SVM 
Train <- shelter_skim[1:742,]
Test <- shelter_skim[743:1060,]
# there are various options associated with SVM training; like changing kernel, gamma and C value.

#model linear
model_l <- svm(StayedInShelterMoreThanOnce ~.,data=Train, kernel = "linear", type = "C-classification", gamma=0.2, cost=100)

#Predict Output
preds_l <- predict(model_l,Test)
table(preds_l, Test$StayedInShelterMoreThanOnce)

#rmse linear
actual <- as.integer(Test$StayedInShelterMoreThanOnce)
preds_sl <- as.integer(preds_l)
error_l <- preds_sl - actual
rmse(error_l)

#model radial
model_r <- svm(StayedInShelterMoreThanOnce ~.,data=Train, kernel = "radial", type = "C-classification", gamma=0.2, cost=100)

#Predict Output
preds_r <- predict(model_r,Test)
table(preds_r, Test$StayedInShelterMoreThanOnce)

#rmse radial
actual <- as.integer(Test$StayedInShelterMoreThanOnce)
preds_sr <- as.integer(preds_r)
error_r <- preds_sr - actual
rmse(error_r)

#model polynomial
model_p <- svm(StayedInShelterMoreThanOnce ~.,data=Train, kernel = "polynomial", type = "C-classification", gamma=0.2, cost=100)

#Predict Output
preds_p <- predict(model_p,Test)
table(preds_p, Test$StayedInShelterMoreThanOnce)

#rmse linear
actual <- as.integer(Test$StayedInShelterMoreThanOnce)
preds_sp <- as.integer(preds_p)
error_p <- preds_sp - actual
rmse(error_p)

summary(model_l)
