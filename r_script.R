########################  AI_Omics_Internship_2025 ##########################
########################        ASSIGNMENT_1        #########################

#lets start 


# Creating common subfolders for project organization


# For storing raw or cleaned data files
dir.create("data")    

# For saving R scripts
dir.create("script") 

# For saving analysis outputs
dir.create("results") 

# For clean Data 
dir.create("clean_data")

# For Plot 
dir.create("plots")

# For raw Data
dir.create("raw data")

# importing the csv file
data <-read.csv(file.choose())

# Only for View in excel sheet type to check the loaded file 
View(data)

# Now for veiwing the structure of the data 
str(data)

#for selected search ,i will give me data upto a range(5)
head(data, 5)

#Creating new column 
data$gender_fac <- as.factor(data$gender)
str(data)


# Now view the level 
levels(data$gender_fac)

#Now Reodering the factor 
data$gender_fac <- factor(data$gender_fac, levels = c("Male", "Female"))


#view the factor column
str(data$gender_fac)
View(data)

#Now Diagnosis
data$diagnosis_fac <- as.factor(data$diagnosis)


#To check the levels
levels(data$diagnosis_fac) 


#view the diagnonsis factor column 
View(data)


#Now Smoker 
data$smoker_fac <- as.factor(data$smoker)


#view the levels
levels(data$smoker_fac)


#Re-ordering the factor
data$smoker_fac <- factor(data$smoker_fac, levels = c("Yes", "No"))



#now converting into binary 
data$smoker_num <- ifelse(data$smoker_fac == "Yes", 1, 0)


#view 
View(data)

#loading the data into clean_data folder 
write.csv(data, "clean_data/patient_info_clean.csv")

#Assigning clean_data variable
clean_data <- read.csv("clean_data/patient_info_clean.csv")


#creating workspace
save.image(file = "fahadsajjad_Class_Ib_Assignment.RData")

#loading 
load("fahadsajjad_Class_Ib_Assignment.RData")
ls()


# Assuming 'gender', 'diagnosis', and 'smoker' columns exist 
gender_fac <- as.factor(data$gender)
diagnosis_fac <- as.factor(data$diagnosis)
smoker_fac <- as.factor(data$smoker)
smoker_num <- as.numeric(smoker_fac)

#saving
save(clean_data, gender_fac, diagnosis_fac, smoker_fac, smoker_num, file = "fahadsajjad_Class_Ib_Assignment.RData")

#double Checking it 
load("fahadsajjad_Class_Ib_Assignment.RData")
ls()

#All good thank you !!!!
