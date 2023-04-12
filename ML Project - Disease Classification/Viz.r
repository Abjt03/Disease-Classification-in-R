# Libraries
library(visdat)
library(corrplot)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(installr)

# Get Data Set
traindf = read.csv('New_Training.csv')
str(traindf)

colnames(traindf)

# Data Visualization

# Bar Plot - Symptoms

symptom_counts <- colSums(traindf[, 1:33])
barplot(symptom_counts, xlab = "Symptom", ylab = "Number of Patients", main = "Symptom Frequency in Dataset", las=2, cex.names = 0.5)

# Bar Plot - Disease

prognosis_counts <- table(traindf$prognosis)
barplot(prognosis_counts, xlab = "Prognosis", ylab = "Count", main = "Prognosis Frequency in Dataset", las=2, cex.names = 0.5)

# Stacked Bar Plot - Hepatitis

hep_a_df <- traindf[traindf$prognosis == "hepatitis A", 1:33]
hep_b_df <- traindf[traindf$prognosis == "Hepatitis B", 1:33]
hep_c_df <- traindf[traindf$prognosis == "Hepatitis C", 1:33]
hep_d_df <- traindf[traindf$prognosis == "Hepatitis D", 1:33]
hep_e_df <- traindf[traindf$prognosis == "Hepatitis E", 1:33]

hep_a_counts <- colSums(hep_a_df)
hep_b_counts <- colSums(hep_b_df)
hep_c_counts <- colSums(hep_c_df)
hep_d_counts <- colSums(hep_d_df)
hep_e_counts <- colSums(hep_e_df)

barplot(rbind(hep_a_counts, hep_b_counts, hep_c_counts, hep_d_counts, hep_e_counts),
        xlab = "Hepatitis", ylab = "Number of Patients",
        main = "Symptom Frequency by Disease for Hepatitis",
        legend.text = c("hepatitis A", "Hepatitis B", "Hepatitis C", "Hepatitis D", "Hepatitis E"),
        args.legend = list(x = "topright", bty = "n"),
        beside = FALSE, las=2, cex.names = 0.5)

# Stacked Bar Plot - Thyroid

hypo_df <- traindf[traindf$prognosis == "Hypothyroidism", 1:33]
hyper_df <- traindf[traindf$prognosis == "Hyperthyroidism", 1:33]

hypo_counts <- colSums(hypo_df)
hyper_counts <- colSums(hyper_df)

barplot(rbind(hypo_counts, hyper_counts),
        xlab = "Thyroid", ylab = "Number of Patients",
        main = "Symptom Frequency by Disease for Thyroid",
        legend.text = c("Hypothyroidism", "Hyperthyroidism"),
        args.legend = list(x = "topright", bty = "n"),
        beside = FALSE, las=2, cex.names = 0.5)

# Scatter Plot - Vomiting vs Fatigue

new_df <- data.frame(fatigue = traindf$fatigue, vomiting = traindf$vomiting)

plot(x = new_df$fatigue, y = new_df$vomiting, type = "n", xlab = "Fatigue", ylab = "Vomiting")

points(x = new_df$fatigue[new_df$fatigue == 1 & new_df$vomiting == 0], y = new_df$vomiting[new_df$fatigue == 1 & new_df$vomiting == 0], col = "red", pch = 16)
points(x = new_df$fatigue[new_df$fatigue == 0 & new_df$vomiting == 1], y = new_df$vomiting[new_df$fatigue == 0 & new_df$vomiting == 1], col = "blue", pch = 16)
points(x = new_df$fatigue[new_df$fatigue == 1 & new_df$vomiting == 1], y = new_df$vomiting[new_df$fatigue == 1 & new_df$vomiting == 1], col = "green", pch = 16)

# Missing Values Graph

visdat::vis_miss(traindf)

# Correlation Matrix

traindf_numeric <- select(traindf, -prognosis)
cor_matrix <- cor(traindf_numeric)
ggcorrplot(cor_matrix, type = "lower", lab = FALSE, lab_size = 3, colors = c("#6D9EC1", "white", "#E46726", textsize = 3, label = "none"))+
        theme(axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 7))+ ggtitle("Correlation Matrix")
