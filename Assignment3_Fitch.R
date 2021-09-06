# This program contains all the R code used to analyze the Pima Diabetes dataset using decision tree analysis
# Data 630 Week 6 Module 4 Assignment 3
# Ted Fitch
# Last updated 06JUL21

# Section 1 - Introduction /////
# Install packages
install.packages("party")
library("party")
# Set working directory and read the data
# Read the CSV file.  
setwd("C:/Users/soari/Documents/Assignments/Data Analytics/UMGC/Summer 2021 Data 630/Assignment 3")
# Turn file into an object
PD <- read.csv("pima_diabetes.csv", head =TRUE, sep=",", as.is=FALSE)
str(PD)
summary(PD)
View(PD)

# 2. Data Pre-processing
# Remove key variable
# No key to remove

# Make plots
# Histogram of insulin
hist(PD$insu,main="Distribution of Insulin Levels",
     xlab="Insulin Levels",
     ylab="Frequency",col="brown")

# Histogram of plasma
hist(PD$plas,main="Distribution of Plasma Glucose Levels",
     xlab="Plasma Glucose Levels",
     ylab="Frequency",col="brown")

# Histogram of skin
hist(PD$skin,main="Distribution of Skin Thickness",
     xlab="Skin Thickness",
     ylab="Frequency",col="brown")

# Histogram of skin
hist(PD$mass,main="Distribution of BMI",
     xlab="BMI",
     ylab="Frequency",col="brown")


# Replace neg/pos with 0/1
PD$class <- as.character(PD$class)
PD$class[PD$class == "tested_negative"] <- "0"
PD$class[PD$class == "tested_positive"] <- "1"
# Check replacement worked
View(PD)
# Return to factor
PD$class <- as.factor(PD$class)
str(PD)

# MODEL 1 ///////

# 3. Split the data into training and test set
set.seed(1234)
ind <- sample(2, nrow(PD), replace = TRUE, prob = c(0.7, 0.3))
train.PD <- PD[ind == 1, ]
test.PD <- PD[ind == 2, ]
str(train.PD)
str(test.PD)
View(train.PD)

# 4. Run the method on a training data
myFormula<-class~.
pima_ctree <- ctree(myFormula, data = train.PD)
# Output the tree structure
print(pima_ctree) 

# 5. Visualize the tree
plot(pima_ctree)
plot(pima_ctree, type="simple")

# 6. Confusion matrix
table(predict(pima_ctree), train.PD$class)
prop.table(table(predict(pima_ctree), train.PD$class))

# 7. Evaluate the model on a test data
testPred <- predict(pima_ctree, newdata = test.PD)
table (testPred, test.PD$class)
prop.table(table (testPred, test.PD$class))

# MODEL 2 /////// (steps 3-7 still listed as 3-7 to show comparability)
# Remove outliers
PD<-PD[PD$mass > 1,]
PD<-PD[PD$skin < 80,]
# Shows breakdown / that outlier removal worked
str(PD)
View(PD)

# 3. Split the data into training and test set
set.seed(1234)
ind <- sample(2, nrow(PD), replace = TRUE, prob = c(0.7, 0.3))
train.PD <- PD[ind == 1, ]
test.PD <- PD[ind == 2, ]
str(train.PD)
str(test.PD)
View(train.PD)

# 4. Run the method on a training data
myFormula<-class~.
pima_ctree <- ctree(myFormula, data = train.PD)
# Output the tree structure
print(pima_ctree) 

# 5. Visualize the tree
plot(pima_ctree)
plot(pima_ctree, type="simple")

# 6. Confusion matrix
table(predict(pima_ctree), train.PD$class)
prop.table(table(predict(pima_ctree), train.PD$class))

# 7. Evaluate the model on a test data
testPred <- predict(pima_ctree, newdata = test.PD)
table (testPred, test.PD$class)
prop.table(table (testPred, test.PD$class))


# End of script