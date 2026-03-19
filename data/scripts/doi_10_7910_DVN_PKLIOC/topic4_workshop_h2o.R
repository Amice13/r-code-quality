# If you do not have 64-bit Java installed, the code below will not run
# Java x64 .exe: https://download.oracle.com/java/17/latest/jdk-17_windows-x64_bin.exe

# After installing Java, install the package with the code below:
# install.packages("h2o")

# Load in the libraries to be used in this R script
library(h2o)
library(dplyr)


# Load and format Data --------------------------------------------------------------

load("geo_and_wq_24hr_C.rda")
# Create new data object that we can edit
data <- geo_and_wq_24hr_C 

## Clean data ##
# Remove "SampleNumber"
data <- data[,-1] 
# Remove any observations that are missing Geosmin
data <- data[!is.na(data$GeosminEntry),] 

# Save and remove "Datetime" column
dates <- data[,1]
data <- data[,-1]
# If missing depth, replace with 0
data$Depth[is.na(data$Depth)] <- 0

## Define geosmin events ##
data$event <- ifelse(data$GeosminEntry <= 5, "low", "high")
# Reformat
data$event <- factor(data$event)


# Select 'p' predictors (inputs) and 'r' response (output)
# for prediction '.p' or categorization '.c'

# Prediction / Response
r.p <- which(colnames(data)=="GeosminEntry") 
# Prediction / Predictor
p.p <- seq(1,ncol(data))
p.p <- p.p[-c(r.p, which(colnames(data)=="event"))]
# Categorization / Response
r.c <- which(colnames(data)=="event") 
# Categorization / Predictor
p.c <- seq(1,ncol(data))
p.c <- p.c[-c(r.c, which(colnames(data)=="GeosminEntry"))] 


# Convert to factor
data$Origin <- factor(data$Origin)
str(data$Origin)
#  Factor w/ 2 levels "Grab_2021","Plankton": 1 1 1 1 1 1 1 1 1 1 ...

# Create integer values for use in prediction
data$Origin <- sapply(data$Origin, function(x) which(x==levels(x)))


# Set up train and test data sets (both scaled and unscaled)

# Use 70% of all observations for training
per.train <- 0.7 
# Total number of training observations (round up)
n.train <- ceiling(nrow(data)*per.train) 

## Random train/test split ##
# Possible numbers from 1 - nrow(data)
n <- sample(x=1:nrow(data), 
            # Number of samples
            size=n.train, 
            # No sample can be selected twice
            replace=F) 
# Subset data to training obs
n <- sort(n)
training.data <- data[n,]
# Subset data to everything but the training obs
testing.data <- data[-n,]


# Unscaled
X.train <- training.data[,p.p]
X.train <- as.matrix(X.train, ncol(X.train))

Y.train <- as.numeric(training.data[,r.p])
Y.train <- as.array(Y.train)

X.test <- testing.data[,p.p]
X.test <- as.matrix(X.test, ncol(X.test))

Y.test <- as.numeric(testing.data[,r.p])
Y.test <- as.array(Y.test)

# Scaled
X.train.s <- scale(X.train)

Y.train.s <- scale(Y.train)

# Using the mean and standard deviation of the training data, scale the testing data
X.test.s <- scale(X.test, 
                  center=attr(X.train.s, 'scaled:center'),
                  scale=attr(X.train.s, 'scaled:scale'))

Y.test.s <- scale(Y.test, 
                  center=attr(Y.train.s, 'scaled:center'),
                  scale=attr(Y.train.s, 'scaled:scale'))

# Unscale
unscale <- function(data) {
  x <- sapply(data, function(r) {
    y <- r*attr(Y.train.s,'scaled:scale') + attr(Y.train.s, 'scaled:center')
    if(y<0) y<-0
    return(y)
  })
  return(x)
}


# Deep learning with h2o ---------------------------------------------------------------------

# Combine the X rows with the Y rows to create a single train data frame
# and a single test data frame
full_train <- cbind(X.train.s, Y.train.s)
full_test <- cbind(X.test.s, Y.test.s)

colnames(full_train)[9] <- colnames(full_test)[9] <- "GeosminEntry"

# Initialize h2o server
localH2O = h2o.init()

# Convert our data frame to h2o frames:
full_train_h2o <- as.h2o(full_train, destination_frame = "full_train")
full_test_h2o <- as.h2o(full_test, destination_frame = "full_test")

# Look at the list h2o objects in the current instance of h2o
h2o.ls()

# Variables containing the names of all predictor variables and the response variable
predictor_vars <- colnames(full_train)[-9]
response_var <- "GeosminEntry"

# Fit the model
model = h2o.deeplearning(x = predictor_vars, 
                         y = response_var, 
                         training_frame = full_train_h2o, # Full training data frame
                         validation_frame = full_test_h2o, # Full testing data frame
                         activation = "Tanh",
                         loss = "Quadratic",
                         # nfolds = 10, # Number of folds for k-fold cross-validation
                         hidden = c(8), # Number of nodes in each hidden layer
                         epochs = 50)

print(model)

preds <- as.data.frame(h2o.predict(model, full_test_h2o))
preds <- unscale(preds$predict)

(rmse <- sqrt(mean((preds - full_test[, 9])^2)))


plot(Y.test, preds, ylim = c(0, 25), pch = 20,
     xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "firebrick", lty = 2)
abline(lm(preds ~ Y.test), col = 'steelblue')

# Plot for comparison with random forest and ANN plots in day 4 material under "conclusions"
plot(Y.test, preds, ylim = c(0, 70), pch = 20,
     xlab = "Actual", ylab = "Predicted",
     main = "Artificial Neural Network")
abline(a = 0, b = 1, col = "firebrick", lty = 2)
abline(lm(preds ~ Y.test), col = 'steelblue')
mtext(paste("RSME =", round(rmse, 2)), adj=0.01, line=-1)



# Remove all elements of the h2o cluster
# h2o.removeAll(timeout_secs = 0, retained_elements = c())
