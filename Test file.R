library(readr)
shelter_skim <- read_csv("Econ4670researchproject/shelter_skim.csv")
shelter_skim$TotalEnrollments = NULL

shelter_return <- subset(shelter_skim , StayedInShelterMoreThanOnce == 1)
shelter_no_return <- subset(shelter_skim , StayedInShelterMoreThanOnce == 0)
shelter_no <- shelter_no_return[1:328,]
shelter_sample <- merge(shelter_no , shelter_return , all = TRUE)
summary(shelter_sample)
shelter_random <- shelter_sample[sample(nrow(shelter_sample)),]

#Import Library
library(e1071) #Contains the SVM 
Train <- shelter_random[1:460,]
Test <- shelter_random[461:656,]

#model linear
model_l <- svm(StayedInShelterMoreThanOnce ~.,data=Train, kernel = "linear", type = "C-classification", gamma=0.2, cost=100)
summary(model_l)

#Predict Output
preds_l <- predict(model_l,Test)
table(preds_l, Test$StayedInShelterMoreThanOnce)

#model radial
model_r <- svm(StayedInShelterMoreThanOnce ~.,data=Train, kernel = "radial", type = "C-classification", gamma=0.2, cost=100)
summary(model_r)

#Predict Output
preds_r <- predict(model_r,Test)
table(preds_r, Test$StayedInShelterMoreThanOnce)

#model polynomial
model_p <- svm(StayedInShelterMoreThanOnce ~.,data=Train, kernel = "polynomial", type = "C-classification", gamma=0.2, cost=100)
summary(model_p)

#Predict Output
preds_p <- predict(model_p,Test)
table(preds_p, Test$StayedInShelterMoreThanOnce)

count <- length(unique(shelter_skim$StayedInShelterMoreThanOnce))
count

model_l$coefs


#plot(model_l , shelter_random , )
