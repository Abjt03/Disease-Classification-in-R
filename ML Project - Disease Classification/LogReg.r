install.packages("dplyr")
library(dplyr)
install.packages("caTools") 
library(caTools)
install.packages("ROCR") 
library(ROCR) 
library(readr)
library(readxl)
install.packages("VGAM") 
library(VGAM)
install.packages("tidymodels") 
library(tidymodels)
library(nnet)
install.packages("caret") 
library(caret)

train <- read_csv("New_Training.csv")
test <- read_csv("New_Testing.csv")

logistic_model <- multinom(prognosis ~ unsteadiness + spinning_movements
                      + nausea+ loss_of_balance
                      + muscle_pain + headache
                      + yellowing_of_eyes + chest_pain
                      + high_fever + vomiting
                      + loss_of_appetite + mild_fever
                      + yellowish_skin + diarrhoea
                      + chills + fatigue
                      + abdominal_pain + dark_urine
                      + weight_loss + itching
                      + family_history + malaise
                      + irritability + cough
                      + sweating + phlegm
                      + breathlessness + muscle_weakness
                      + skin_rash + red_spots_over_body
                      + joint_pain + fast_heart_rate
                      + lethargy, data = train, MaxNWts=4000)

summary(logistic_model)

predicted_probs <- predict(logistic_model, test, type="probs")
predicted_classes <- predict(logistic_model, test, type="class")

print(predicted_probs)
print(predicted_classes)

table(test$prognosis, predicted)

reference_classes <- factor(test$prognosis, levels = levels(predicted_classes))

cm <- confusionMatrix(data=predicted_classes, reference=reference_classes,  mode="everything")

print(cm$overall["Accuracy"])