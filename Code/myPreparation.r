# Student Identity : 44814377
# File Description : this file is for the task 1 of the assignment, which is about the 
#                    data preperation and file formating.



# Task1
# 1.1 Extract the data into an R data frame

# read the data from raw file, with seperator as commar
breast_cancer_data <- read.table("./data/breast-cancer-wisconsin.data",sep = ',')


# 1.2 Assign the following names to the 11 different columns in your dataset

# generate out a name list
names <- list("Sample code number","Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape",
         "Marginal Adhesion","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli",
         "Mitoses","Class")

# change the space of names list elements to dot, and assign it to the title of table 'breast_cancer_data'
for(i in 1:ncol(breast_cancer_data)){
  names(breast_cancer_data)[i] <- gsub(" ", ".", names[i], fixed=TRUE)
}


# 1.3. Remove all rows with missing values. Notice that R might define a column
# with missing data as “factor”. If such column is supposed to be integer,
# then your need to change the column type from factor to integer.

# change the '?' to NA, and remove rows with data NA
breast_cancer_data[breast_cancer_data=="?"] <- NA
breast_cancer_data <- na.omit(breast_cancer_data)

# change the 'factor' column to 'integer'
for(i in 1:ncol(breast_cancer_data)){
  if(class(breast_cancer_data[,i])=='factor'){
    breast_cancer_data[,i] = as.integer(breast_cancer_data[,i])
  }
}
for(i in 1:ncol(breast_cancer_data)){print(class(breast_cancer_data[,i]))}


# 1.4 Remove the first column (Sample code number) as it is not useful for our next tasks

# choose all the row and remove column 1
breast_cancer_data <- breast_cancer_data[,-c(1)]
  

# 1.5. Notice that R might define the “class” column as integer. In that case,
# change its type from integer to factor.

# change the 'class' type from integer to factor
breast_cancer_data$Class = as.factor(breast_cancer_data$Class)
for(i in 1:ncol(breast_cancer_data)){print(class(breast_cancer_data[,i]))}


# 1.6. Save the dataframe into a file with filename bcw_processed.Rda
# save file
saveRDS(breast_cancer_data,  file = "./Data/bcw_processed.Rda")
