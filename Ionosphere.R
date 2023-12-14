suppressMessages(library(tidyverse))
suppressMessages(library(pander))
suppressMessages(library(class))

df <- read.csv("https://query.data.world/s/a7daatpbbqbht722grkkm3ws522yhg?dws=00000",
               header = TRUE, stringsAsFactors = FALSE)

# Display the structure of the underlying data
str(df)

# Display the first six lines
head(df)

# Display the field headers
names(df)

# Display a summary of various results for each field
summary(df)

# Assigning the values "g" and "b" in field AI to "good" and "bad" respectively

df$g <- factor(df$g, levels = c("g", "b"), labels = c("good", "bad"))

# Creating a method to normalize data between 0 and 1

max_min_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Testing the previous function against a first group of numbers

v1 <- c(1, 2, 3, 4, 5)
max_min_normalize(v1)

# Testing the previous function against a second group of numbers

v2 <- c(10, 20, 30, 40, 50)
max_min_normalize(v2)

# Removing field B because the data adds no value

df_n <- select(df, -X0)

# Creating a new data frame without field AI and applying the normalization function

df_n2 <- as.data.frame(lapply(df_n[-34], max_min_normalize))

# Checking to see if the normalization is working correctly
summary(df_n2)

# Assigning field AI from the original data set to the variable g

g <- df[,35]

# Combining the data set with the normalized data back with field AI

dat <- cbind(g, df_n2)

# Setting the size of the training data

train_ratio <- .90

# Finding the number of rows to use for training based on the ratio and size of the data set

train_N <- round(train_ratio * nrow(dat)) +1

# Setting the seed value

set.seed(813)

# Finding the rows that will be used to train the model

train_IDs_dat <- sample(1:nrow(dat), size = train_N, replace = FALSE)

# Creating a set of data that will be used to train the model

train_dat <- dat[train_IDs_dat,]

# Creating a set of data that will be used to test the model.  Any row that's not in the train data will be in the test data.

test_dat <- dat[-train_IDs_dat,]

# Creating a function that will let us create a frequency table

frqtab <- function(x,caption) { round(100*prop.table(table(x)),2) }

# Checking the frequency of field g in the dat table

ft_whole <- frqtab(dat$g)

# Checking the frequency of field g in the train data

ft_train <- frqtab(train_dat$g)

# Checking the frequency of field g in the test table

ft_test <- frqtab(test_dat$g)

#Combining the previous three values in to one data set and naming the fields

ftcmp_df <- as.data.frame(cbind(ft_whole, ft_train, ft_test))
colnames(ftcmp_df) <- c("Whole", "Training Set", "Test Set")

#Using the pander package to display the frequency of the g field from all three tables

pander(ftcmp_df, style="rmarkdown", caption = "Comparison of good and bad frequency in %")

# Setting the K value equal to the square root of the number of rows plus 1

k0 <- round(sqrt(nrow(dat))) +1

# Displaying the number of k values

k0

# Using the class package to do KNN classification

knn.k0 <- knn(train = train_dat[,-1],  test = test_dat[,-1], cl = train_dat[,1], k=k0)

# Determine the Accuracy of the previous classification

accuracy <- round(100*mean(test_dat$g==knn.k0),digits = 2)
cat('\n', 'The overall accuracy is', accuracy, '%')

df_z <- as.data.frame(lapply(df[-35], scale))
df_z2 <- select(df_z, -X0)

g <- df[, 35]
dat <- cbind(g, df_z2)

train_ratio <- .9
train_N <- round(train_ratio * nrow(dat)) + 1
set.seed(813)
train_ID_dat <- sample(1:nrow(dat), size = train_N, replace = FALSE)
train_dat <- dat[train_ID_dat,]
test_dat <- dat[-train_ID_dat,]

k0 <- round(sqrt(nrow(dat))) + 1
knn.k0 <- knn(train = train_dat[, -1], test = test_dat[,-1], cl=train_dat[,1], k=k0)

accuracy_z <- round(100*mean(test_dat$g==knn.k0),digits=2)

accur_cmp.df <- as.data.frame(cbind(accuracy, accuracy_z))
colnames(accur_cmp.df) <- c("Max-Min Normalization", "Z-Score Normalization")
pander(accur_cmp.df, style="rmarkdown", caption="Comparison of normailzation methods: Kappa Coefficient (in %)")

df_n2 <- as.data.frame(lapply(df[-35], max_min_normalize))
type <- df[, 35]
dat <- cbind(type, df_z2)

train_ratio <- 813
Accuracy.list <- 813   #
i <- 1
for (train_ratio in seq(from=0.05, to=0.95, by=0.05)){
  
  train_N <- round(train_ratio * nrow(dat)) + 1
  set.seed(813)
  train_ID_dat <- sample(1:nrow(dat), size = train_N, replace = FALSE) 
  train_dat <- dat[train_ID_dat, ]
  test_dat <- dat[-train_ID_dat, ]

  k0 <- round(sqrt(nrow(dat))) + 1
  knn.k0 <- knn(train = train_dat[,-1],  test = test_dat[,-1], cl=train_dat[, 1], k=k0)

  Accuracy.list[i] <- round(100*mean(test_dat$type==knn.k0), digits = 2)
  cat('The overall accuracy is ', Accuracy.list[i], '% when the percentage of training samples is', train_ratio*100, '%', '\n')       # to print % accuracy 
  i <- i+1
}

plot(x = seq(from=0.05, to=0.95, by=0.05)*100, y = Accuracy.list, type="b", xlab="Percentage of training samples (%)",ylab="Overall accuracy (%)")

df_n <- as.data.frame(lapply(df[-35], max_min_normalize))
type <- df[, 10]
dat <- cbind(type, df_z2)

train_ratio <- 0.85 
train_N <- round(train_ratio * nrow(dat)) + 1
set.seed(813)
train_ID_dat <- sample(1:nrow(dat), size = train_N, replace = FALSE) 
train_dat <- dat[train_ID_dat, ]
test_dat <- dat[-train_ID_dat, ]

k_try <- 813                      
Accuracy.list <- 813      
for (k_try in 1 : (k0*2)){  
    knn.mod <-  knn(train = train_dat[,-1],  test = test_dat[,-1], cl=train_dat[, 1], k = k_try)
    Accuracy.list[k_try] <- round(100*mean(test_dat$type==knn.mod), digits = 2)
    cat('When k =', k_try, 'the overall accuracy =', Accuracy.list[k_try], '%\n')       # to print % accuracy 
}

plot(x = 1 : (k0*2), y = Accuracy.list, type="b", xlab="k-Value",ylab="Overall accuracy (%)") 
