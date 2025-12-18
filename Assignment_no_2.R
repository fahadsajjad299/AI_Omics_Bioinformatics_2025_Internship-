#########################  AI_Omics_Internship_2025  #########################
##############################################################################
##########################      Assignment No. 2    ##########################


#setting up the Working Directory 

getwd()
setwd("C:/Users/DELL/Documents/AI_Omics_Internship_2025/Module_2")
getwd()

#working directory Setted

#making Folders conditionally 

# assigning the variables 
input_dir  <- "Raw_Data"
output_dir <- "Results"
script_dir <- "Script"

# Apply If Condition 
if (!dir.exists(input_dir))  dir.create(input_dir)
if (!dir.exists(output_dir)) dir.create(output_dir)
if (!dir.exists(script_dir)) dir.create(script_dir)

# using a varibale for two input files 
files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")

# Initializing  empty list to store last results
result_list <- list()

# making classification function 
gene_classification <- function(logFC, padj){
  ifelse(padj < 0.05 & logFC < -1, "Down-regulated",
         ifelse(padj < 0.05 & logFC > 1, "Up-regulated",
                "Not-significant"))
}

#  Looping  over files to done in once 
for(files in files_to_process){
  cat("\nProcessing:", files, "\n")
  
  # Import file
  file_path <- file.path(input_dir, files)
  deg_data <- read.csv(file_path, header = TRUE)
  cat("File successfully imported! Checking for missing values...\n")
  
  # Replace missing padj values with 1
  if("padj" %in% names(deg_data)){
    missing_count <- sum(is.na(deg_data$padj))
    cat("Missing values in padj:", missing_count, "\n")
    deg_data$padj[is.na(deg_data$padj)] <- 1
  }
  
  # Replace missing logFC with mean
  if("logFC" %in% names(deg_data)){
    missing_values <- sum(is.na(deg_data$logFC))
    cat("Missing values in logFC:", missing_values, "\n")
    deg_data$logFC[is.na(deg_data$logFC)] <- mean(deg_data$logFC, na.rm = TRUE)
  }
  
  # Classify each gene
  deg_data$Gene_Class <- gene_classification(deg_data$logFC, deg_data$padj)
  
  # Saving  to result_list
  result_list[[files]] <- deg_data
  
  # Saving classified file
  output_file_path <- file.path(output_dir, paste0("Classification_", files))
  write.csv(deg_data, output_file_path, row.names = FALSE)
  cat("Results saved to:", output_file_path, "\n")
  
  # Summarization
  gene_counts <- table(deg_data$Gene_Class)
  cat("Summary counts for", files, ":\n")
  print(gene_counts)
}

 #Accessing  results
result_1 <- result_list[["DEGs_Data_1.csv"]]
result_2 <- result_list[["DEGs_Data_2.csv"]]

# Saving  environment
save.image(file = "Fahad_Sajjad_Assignment(2).RData")



