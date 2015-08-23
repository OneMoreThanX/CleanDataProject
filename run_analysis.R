run_analysis <- function(folder = "C:/A-nnealing/Courses/Clean Data/Project/UCI_HAR_Dataset"){
  
  setwd(folder)
  
  train_act <- read.table("train/y_train.txt")
  train_data <- read.table("train/X_train.txt")
  train_sub <- read.table("train/subject_train.txt")
  
  Train <- cbind(train_sub,train_act,train_data)
  
  test_act <- read.table("test/y_test.txt")
  test_data <- read.table("test/X_test.txt")
  test_sub <- read.table("test/subject_test.txt")
  
  Test <- cbind(test_sub,test_act,test_data)
  
  if(ncol(Train) != ncol(Test)){
    message("Can't merge two datasets,number of features is not consistent")
    return
  }
  else{
    ## Merge training set and test set as Comb_Data
    Comb_Data <- rbind(Train,Test)
    Feature <- read.table("features.txt")
    f_name <- as.character(Feature[,2])
    
    ## Label the merged data set
    Label <- c("Sub","Act",f_name)
    colnames(Comb_Data) <- Label
    
    ## Extract features named with "mean" and "std"
    Select_Data <- Comb_Data[,c(1,2,grep("mean()",colnames(Comb_Data)),grep("std()",colnames(Comb_Data)))]
    
    ## Independent data set with the average of each variable for each activity and each subject
    library(reshape)
    G <- melt(Select_Data,id.vars = c("Act","Sub"))
    Tidy_Data <- cast(Act + Sub ~ variable, data = G, fun = mean)
    
    ## Write the result into file
    write.table(Tidy_Data,"tidy_data.txt",row.name = FALSE)
    
    ## Output tidy data set
    Tidy_Data
  }
  
  
}
  