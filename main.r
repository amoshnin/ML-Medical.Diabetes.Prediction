# Importing the Libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(ROCR)
library(rpart)
library(rpart.plot)
library(e1071)
library(class)
library(corrplot)

# Importing the Dataset
df = read.csv("R_Data/diabetes_5050.csv")
set.seed(1101)

# ------ Part 1 : Exploring the Dataset ------

# --- 1.1 General Exploration of the Dataset Structure --- 
dim(df)
names(df)
str(df)
head(df)
summary(df)

# Checking (Number of Unique Values in Different Variables)
unique_counts <- sapply(df, function(x) length(unique(x)))
print(unique_counts)

# Checking (Number of Missing Values in Different Variables)
missing_counts <- colSums(is.na(df))
print(missing_counts)

# Checking and Eliminating (Duplicate Rows)
duplicate_rows <- duplicated(df, fromLast=TRUE)
print(sum(duplicate_rows))
df <- df[!duplicate_rows, ]

# Plotting Histograms for each Variable to better Understand the Dataset
for (col in names(df)) {
  hist(df[[col]], main=col)
}

# Analysing the Correlation between Columns of the Dataset
correlation_matrix <- cor(df)
correlation_melted <- melt(correlation_matrix)

ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

corrplot(correlation_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", 
         tl.srt = 45, tl.cex = 0.8, number.cex = 0.7,
         col = colorRampPalette(c("white", "blue"))(100), 
         title = "Correlation Matrix",
         width = 600, height = 600)

# Analysing the Correlation of (Independent Variables with Dependent Variable)
correlation_data <- df %>%
  select(-Diabetes_binary) %>%
  sapply(function(col) cor(col, df$Diabetes_binary))

correlation_df <- data.frame(Feature = names(correlation_data), Correlation = correlation_data)

ggplot(correlation_df, aes(x = Feature, y = Correlation)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Correlation with Diabetes_binary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  # To make the x-axis labels vertical

# Analysing the (Dependent/Target Variable)
# - Replacing 0 into Non-Diabetic and 1 into Diabetic 
# - Adding new column Diabetes_binary_String

# Create a Counts Table of Values for Diabetes_binary_String
df = df %>%
  mutate(Diabetes_binary_String = ifelse(Diabetes_binary == 0, "Non-Diabetic", "Diabetic"))
data_counts = table(df$Diabetes_binary_String)
data_counts

# Create a count plot for Diabetes_binary_String
ggplot(df, aes(x = Diabetes_binary_String)) +
  geom_bar() +
  labs(title = "Count of Diabetes_binary_String")

# Create a pie chart for Diabetic and non-Diabetic people
labels <- c("Non-Diabetic", "Diabetic")
pie_chart_data <- data.frame(labels, data_counts)

ggplot(pie_chart_data, aes(x = "", y = data_counts, fill = labels)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Diabetic and Non-Diabetic People") +
  theme_minimal()

# --- 1.2 Specific Exploration of Relationships between (Diabetes_binary) and (Major Factors that cause Diabetes)

relationship_analysis <- function(variable_name) {
  # Create a vector of labels
  labels <- c(paste("Non", variable_name), variable_name)
  
  # P1
  # Calculate the counts of HighBP and non HighBP people and create a vector
  counts <- table(df[, variable_name])
  # Create a pie chart
  pie(counts, labels = labels, col = c("blue", "red"), main = paste(variable_name, "vs. non", variable_name, "People"))
  
  # P2
  # Create a cross-tabulation (contingency table) of HighBP and Diabetes_binary_str
  cross_table <- table(df[, variable_name], df$Diabetes_binary_String)
  # Print the cross-tabulation
  print("Contingency Table in Values")
  print(cross_table)
  
  # P3
  # Create a cross-tabulation (contingency table) of HighBP and Diabetes_binary_str
  cross_table <- table(df[, variable_name], df$Diabetes_binary_String)
  # Create a bar plot
  barplot(cross_table, beside = TRUE, col = c("blue", "red"), 
          names.arg = labels, 
          main = paste("Diabetes Disease Frequency for", variable_name),
          xlab = variable_name,
          ylab = "Frequency",
          legend.text = rownames(cross_table))
  # Add a legend
  legend("topright", legend = colnames(cross_table), fill = c("blue", "red"))
  
  # P4
  # Create a contingency table of Diabetes_binary_str and HighBP
  cross_table <- table(df$Diabetes_binary_String, df[, variable_name])
  # Calculate the percentages of HighBP and NO HighBP within each Diabetes category
  percentage_table <- 100 * prop.table(cross_table, margin = 1)
  # Print the resulting table
  print("Contingency Table in Percentages")
  print(percentage_table)
}

# 1.2.1 Exploring Relationship between (HighBP) and (Diabetes_binary)
relationship_analysis("HighBP")

# 1.2.2 Exploring Relationship between (HighChol) and (Diabetes_binary)
relationship_analysis("HighChol")

# 1.2.3 Exploring Combined Effect of (HighBP and HighChol) on  (Diabetes_binary)
result <- df %>%
  group_by(HighBP, HighChol, Diabetes_binary_String) %>%
  summarise(count = n()) %>%
  group_by(HighBP, HighChol) %>%
  mutate(total_count = sum(count)) %>%
  mutate(percentage = (count / total_count) * 100) %>%
  select(HighBP, HighChol, Diabetes_binary_String, percentage)

print(result)

ggplot(df, aes(x = HighBP, fill = Diabetes_binary_String)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ HighChol) +
  labs(title = "Relation b/w HighBP, HighChol and Diabetes")

# 1.2.4 Exploring Relationship between (BMI) and (Diabetes_binary)

# Graphing the (Count Plot) of (BMI)
ggplot(data = df, aes(x = BMI, fill = factor(Diabetes_binary))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "red", "1" = "green")) +
  labs(fill = "Diabetes") +
  ggtitle("Relation b/w BMI and Diabetes") +
  theme(legend.position = "top")

# Graping the (Box Plot) of (BMI)
ggplot(data = df, aes(y = BMI)) +
  geom_boxplot()

# Range of BMI values
range(df$BMI) # Min = 12 | Max = 98

# Making Three BMI Category Groups 
# Category 1 : (0 < BMI <= 20) 
category1 <- df$Diabetes_binary[df$BMI > 0 & df$BMI <= 20]
table(category1)

# Category 2 : (20 < BMI <= 50)
category2 <- df$Diabetes_binary[df$BMI > 20 & df$BMI <= 50]
table(category2)

# Category 3 : (50 < BMI <= 100)
category3 <- df$Diabetes_binary[df$BMI > 50 & df$BMI <= 100]
table(category3)

# ------ Part 2 : Feature Selections Process ------

# Dropping Independent Variables with Small Influence on Target Variable
columns_to_drop = c("Fruits", "AnyHealthcare", "NoDocbcCost", "Sex", "Veggies", "CholCheck", "Diabetes_binary_String")
df = df %>% select(-columns_to_drop)

# Convert Categorical Variables into Factors
categorical_columns = c("Diabetes_binary", "HighBP", "HighChol", "Smoker", "Stroke", "HeartDiseaseorAttack", "DiffWalk", "PhysActivity", "HvyAlcoholConsump")
df[categorical_columns] <- lapply(df[categorical_columns], as.factor)
print(str(df[categorical_columns]))

# Convert Ordinal Variables as Numericals (as requested in the assignment)
ordinal_columns = c("GenHlth", "Age", "Education", "Income")
df[ordinal_columns] <- lapply(df[ordinal_columns], as.numeric)
print(str(df[ordinal_columns]))

# Convert Numerical Variables as Numericals
numerical_columns = names(df)[!(names(df) %in% c(categorical_columns, ordinal_columns))]
df[numerical_columns] <- lapply(df[numerical_columns], as.numeric)
print(str(df[numerical_columns]))

# ------ Part 3 : Building the Classification Model ------
n_folds = 10

# Splitting the Dataset
target_variable_idx = which(names(df) == "Diabetes_binary")
train_indices <- sample(1:nrow(df), 0.7 * nrow(df))

df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

df_train_x <- df_train[, -target_variable_idx]
df_train_y <- df_train[, target_variable_idx]

df_test_x <- df_test[,-target_variable_idx]
df_test_y <- df_test[,target_variable_idx]

print(names(df_train))

# Function for Performance Evaluation of a Classification Model (ROC & AUC)
evaluate_performance <- function(model_name, predictions) {
  comparison = prediction(predictions, df_test_y)
  roc = performance(comparison, "tpr", "fpr")
  auc = performance(comparison, measure ="auc")@y.values[[1]]
  
  plot(roc , col = "red", main = paste(" Area Under Curve (AUC) of", model_name, ":", round(auc ,4)))
}

# Classification Model 1 : (Logistic Regression) Model
log_model = glm(Diabetes_binary ~ ., data=df_train, family=binomial(link ="logit"))
summary(log_model)

log_preds = predict(log_model, newdata=df_test_x, type="response")
evaluate_performance("Logistic Regression", log_preds)

threshold <- 0.5
log_preds_class <- ifelse(log_preds > threshold, 1, 0)

log_confusion = table(Actual=df_test_y, Predicted=log_preds_class)
log_confusion_prop = prop.table(log_confusion)

log_confusion
log_confusion_prop

# Classification Model 2 : (Decision Tree) Model
x_cols = c(
  "HighBP",              
  "HighChol",
  "BMI",
  "Smoker",
  "Stroke",         
  "HeartDiseaseorAttack",
  "PhysActivity",
  "HvyAlcoholConsump", 
  "GenHlth",
  "MentHlth",
  "PhysHlth",
  "DiffWalk",
  "Age",
  "Education",
  "Income"    
)

y_cols = "Diabetes_binary"

n_folds = 10
indexes_by_folds <- sample(rep(1:n_folds, length.out = dim(df)[1]))

cp_values = 10^(-2:10)
tree_results = rep(0, length(cp_values))

for (cp_idx in 1:length(cp_values)) {
  miss_classification = 0
  print(cp_idx)
  for (fold_idx in 1:n_folds) {
    test_indexes = which(indexes_by_folds == fold_idx)
    test = df[test_indexes,]
    train = df[-test_indexes,]
    
    test_y = test[, y_cols]
    test_x = test[, x_cols]
    
    # cat("Test Dim: ", dim(test), "\n") # Test Dim:  200 9 
    # cat("Train Dim: ", dim(train), "\n") # Train Dim:  1800 9 
    
    tree_model = rpart(data=train,
                       formula=Diabetes_binary ~ .,
                       method="class",
                       control=rpart.control(cp=cp_values[cp_idx]),
                       parms=list(split='information'))
    
    predictions = predict(tree_model, test_x, type="class")
    miss_classification = miss_classification + sum(predictions != test_y)
  }
  tree_results[cp_idx] = miss_classification / dim(df)[1]
}

print(tree_results)
best_cp = cp_values[which(tree_results == min(tree_results))]
print(best_cp)

tree_model = rpart(data=df_train,
                   formula=Diabetes_binary ~ .,
                   method="class",
                   control=rpart.control(cp=best_cp),
                   parms=list(split='information'))

tree_preds_prob = predict(tree_model, newdata=df_test_x, type="prob")
tree_preds_class = predict(tree_model, newdata=df_test_x, type="class")
evaluate_performance("Decision Tree", tree_preds_prob[, "1"])

tree_confusion = table(Actual=df_test_y, Predicted=tree_preds_class)
tree_confusion_prop = prop.table(tree_confusion)

tree_confusion
tree_confusion_prop

rpart.plot(
  tree_model,
  type =4, 
  extra =2, 
  clip.right.labs = FALSE, 
  varlen =0,
  faclen =0
)

# Classification Model 3 : (KNN) Model
n_folds = 2
k_values = c(1:30)
knn_results = data.frame(
  K=k_values,
  accuracy=k_values
)

folds = sample(rep(1:n_folds, length.out = nrow(df)))

# Defining N-Fold Cross-Validation Function
folding <- function(k, fold) {
  test_idx = which(folds == fold)
  
  # Extract the (Test Fold) and the (Train Fold)
  test_df <- df[test_idx, ]
  train_df <- df[-test_idx, ]
  
  # Display the training (Training and Testing) Sets
  head(test_df)
  head(train_df)
  
  # Splitting between (Dependent Variable & Independent Variables)
  train_x = train_df[,-target_variable_idx]
  train_y = train_df[,target_variable_idx]
  
  test_x = test_df[,-target_variable_idx]
  test_y = test_df[,target_variable_idx]
  
  # Forming KNN (K-Nearest Neighbor) Algorithm with (K=1)
  predictions = knn(train_x, test_x, train_y, k=k)
  
  # Setting up the Confusion Matrix
  confusion_matrix <- table(test_y, predictions)
  
  conf_cols = colnames(confusion_matrix)
  conf_rows = rownames(confusion_matrix)
  
  TP <- ifelse("1" %in% conf_rows && "1" %in% conf_cols, confusion_matrix["1", "1"], 0)
  TN <- ifelse("0" %in% conf_rows && "0" %in% conf_cols, confusion_matrix["0", "0"], 0)
  FP <- ifelse("0" %in% conf_rows && "1" %in% conf_cols, confusion_matrix["0", "1"], 0)
  FN <- ifelse("1" %in% conf_rows && "0" %in% conf_cols, confusion_matrix["1", "0"], 0)
  
  # Computing Accuracy of the Fitted KNN Algorithm
  compute_accuracy <- function(TP, FP, TN, FN) {
    result = (TP + TN) / (TP + FP + FN + TN)
    return(result)
  }
  
  accuracy = compute_accuracy(TP, FP, TN, FN)
  return(accuracy)
}

for (k_value in k_values) {
  total_accuracy = 0
  
  for (fold in 1:n_folds) {
    accuracy <- folding(k_value, fold)
    total_accuracy = total_accuracy + accuracy
  }
  
  # Compute (Average Accuracy) based on (All N-Folds) for Specific (K)
  average_accuracy = total_accuracy / n_folds
  
  # Obtained Results for this (k_value) get (Saved into Vector) & (Printed)
  knn_results <- knn_results %>%
    mutate(accuracy = ifelse(K == k_value, average_accuracy, accuracy))
  cat("For k-value: ", k_value, " the average accuracy is: ", average_accuracy, "\n")
}

best_k = as.numeric(knn_results[k_values[which(knn_results[,"accuracy"] == max(knn_results[, "accuracy"]))],]["K"])
print(best_k)

knn_model = knn(df_train_x, df_test_x, df_train_y, k=best_k, prob = TRUE)
knn_preds <- attributes(knn_model)$prob
evaluate_performance("K-Nearest Neighbors", knn_preds)

threshold <- 0.5
knn_preds_class <- ifelse(knn_preds > threshold, 1, 0)

knn_confusion = table(Actual=df_test_y, Predicted=knn_preds_class)
knn_confusion_prop = prop.table(knn_confusion)

knn_confusion
knn_confusion_prop

# Classification Model 4 : (Naive Bayes) Model
naive_model <- naiveBayes(Diabetes_binary ~ ., df_train)

naive_preds_class = predict(naive_model, df_test_x, type="class")
naive_preds_prob = predict(naive_model, df_test_x, type="raw")[, "1"]

evaluate_performance("Naive Bayes", naive_preds_prob)

naive_confusion = table(Actual=df_test_y, Predicted=naive_preds_class)
naive_confusion_prop = prop.table(naive_confusion)

naive_confusion
naive_confusion_prop
