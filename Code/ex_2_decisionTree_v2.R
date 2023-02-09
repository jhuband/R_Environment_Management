library(rpart)

#*****************************************
#  Read in data
data_file <- 'cancer_data.csv'
target_file <- 'cancer_target.csv'
cancer_data <- read.csv(data_file, header=FALSE,
                                          stringsAsFactors=FALSE)
cancer_target <- read.csv(target_file, header=FALSE,
                                          stringsAsFactors=FALSE)

headers <- 'cancer_feature_names.csv'
column_names <- readLines(headers)
column_names <- gsub(" ", ".", column_names)
names(cancer_data) <- column_names
names(cancer_target) <- "Diagnosis"

cancer_data$Diagnosis <- cancer_target$Diagnosis

#*****************************************
#  Divide data training set and test set
N <- nrow(cancer_data)
sample_size <- as.integer(0.80*N)    # We'll use 70% for training

set.seed(191506) #Set for reproducibility only
ndx <- sort(sample(1:N, sample_size))

train_data <- cancer_data[ndx, ]
test_data <- cancer_data[-ndx, ] 


#*****************************************
#  Fit decision tree model
fit <- rpart(Diagnosis ~ ., 
             data = train_data, method = "class")


#*****************************************
#  Apply the model to the test data
prediction <- predict(fit, test_data, 
                      type = "class")


#*****************************************
#  Compute confusion matrix
cm <- table(Diagnosis=test_data$Diagnosis,
            Predicted=prediction)
row.names(cm) <- c("Malignant", "Benign")
colnames(cm) <- row.names(cm)
cat("Results as a Confusion Matrix \n\n")
print(cm)



