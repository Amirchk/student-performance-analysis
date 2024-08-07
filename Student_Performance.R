# Load necessary libraries
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("corrplot")
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

# Load the dataset
data <- read.csv("Student_performance_data.csv")

# Summary of the dataset
summary(data)

# Check for missing values
colSums(is.na(data))

# Basic statistics
str(data)

#Demographic Analysis
#Age distribution
ggplot(data, aes(x=Age)) +
  geom_bar() +
  labs(title="Age Distribution", x="Age", y="Count")

#Gender dist
ggplot(data, aes(x=factor(Gender, labels=c("Male", "Female")))) +
  geom_bar() +
  labs(title="Gender Distribution", x="Gender", y="Count")

#Ethnicity dist
ggplot(data, aes(x=factor(Ethnicity, labels=c("Caucasian", "African American", "Asian", "Other")))) +
  geom_bar() +
  labs(title="Ethnicity Distribution", x="Ethnicity", y="Count")

#Parental education impoact on GPA
ggplot(data, aes(x=factor(ParentalEducation, labels=c("None", "High School", "Some College", "Bachelor's", "Higher")), y=GPA)) +
  geom_boxplot() +
  labs(title="Impact of Parental Education on GPA", x="Parental Education", y="GPA")

#Weekly Study Time vs. GPA
ggplot(data, aes(x=StudyTimeWeekly, y=GPA)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  labs(title="Weekly Study Time vs. GPA", x="Weekly Study Time (hours)", y="GPA")


#Parental Support vs. GPA
ggplot(data, aes(x=factor(ParentalSupport, labels=c("None", "Low", "Moderate", "High", "Very High")), y=GPA)) +
  geom_boxplot() +
  labs(title="Parental Support vs. GPA", x="Parental Support", y="GPA")


#Extracurricular Activities and GPA
ggplot(data, aes(x=factor(Extracurricular, labels=c("No", "Yes")), y=GPA)) +
  geom_boxplot() +
  labs(title="Extracurricular Activities and GPA", x="Extracurricular Activities", y="GPA")


#Absences vs. GPA
ggplot(data, aes(x=Absences, y=GPA)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  labs(title="Absences vs. GPA", x="Number of Absences", y="GPA")


#Tutoring and GPA
ggplot(data, aes(x=factor(Tutoring, labels=c("No", "Yes")), y=GPA)) +
  geom_boxplot() +
  labs(title="Tutoring and GPA", x="Tutoring", y="GPA")


#Grade Distribution
ggplot(data, aes(x=factor(GradeClass, labels=c("A", "B", "C", "D", "F")))) +
  geom_bar() +
  labs(title="Grade Distribution", x="Grade Class", y="Count")


#Correlation Matrix
# Calculate correlation matrix
correlation_matrix <- cor(data[,sapply(data, is.numeric)])
# Plot correlation matrix
corrplot(correlation_matrix, method="color", tl.col="black", tl.srt=45)
