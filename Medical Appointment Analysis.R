#load Libraries if not available

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(summarytools)) install.packages("summarytools", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")

# Read the dataset from csv file using the read_csv function
ds_apptnoshow <- read_csv("https://raw.githubusercontent.com/varora9/EDX_Project/master/KaggleV2-May-2016.csv", 
                          col_names = c("PatientId","AppointmentID","Gender","ScheduledDay","AppointmentDay","Age",
                                        "Neighbourhood","Scholarship","Hipertension","Diabetes","Alcoholism",
                                        "Handcap","SMS_received","No-show"))
# remove the duplicate header row
ds_apptnoshow <- ds_apptnoshow[-1,]

# Covert columns which has numeric data from chr into double. This is required for training the model.
ds_apptnoshow$Age = as.double(ds_apptnoshow$Age)
ds_apptnoshow$Scholarship = as.double(ds_apptnoshow$Scholarship)
ds_apptnoshow$Hipertension = as.double(ds_apptnoshow$Hipertension)
ds_apptnoshow$Diabetes = as.double(ds_apptnoshow$Diabetes)
ds_apptnoshow$Alcoholism = as.double(ds_apptnoshow$Alcoholism)
ds_apptnoshow$Handcap = as.double(ds_apptnoshow$Handcap)
ds_apptnoshow$SMS_received = as.double(ds_apptnoshow$SMS_received)

#Rename No-Show Column Name
names(ds_apptnoshow)[names(ds_apptnoshow) == 'No-show'] <- 'NoShow'

# Defining function to generate graph based on columnname
plotGraphbasedonColumnname <- function(title, colname){
  ds_apptnoshow %>% ggplot(aes(get(colname))) +
    geom_bar(stat='count',colour="darkgreen", fill="lightgreen") +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))), 
              stat = "count", vjust = -0.5, colour="black" ) +
    labs(title = title,
         x = colname, 
         y = "Count")
}


# Defining function to generate graph based on columnname in facet
plotGraphbasedonNoShow <- function(title, colname){
  cols<- c('lightgreen','darkred')
  ds_apptnoshow %>% ggplot(aes(NoShow, fill=NoShow)) +
    geom_bar(stat='count',colour="darkgreen") +
    facet_grid(~get(colname)) +
    geom_text(aes(label = 
                    scales::percent((..count..)/sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))),
              stat = "count", vjust = -0.5, hjust= 0.5, colour="black" ) +
    scale_fill_manual(name="Bars",values=cols) +
    labs(title = title,
         x = "No-Show", 
         y = "Count")
}


# 2. Data Analysis and Visualization

#a. Quick glance on the Appointment No-Show Dataset

#Print Top 6 rows of the dataset
head(ds_apptnoshow)


#b. Quick glance of the Datset's Summary
#summary of Dataset
summary(ds_apptnoshow)


#c. Medical Appointment No-Show dataset contain
dim(ds_apptnoshow)

#d. Total count of unique Patients 
length(unique(ds_apptnoshow$PatientId))


#e. Data segregation based on Gender. Females patients are significantly higher than Male patients.

# Print Dataset no. based on Gender
ds_apptnoshow %>% group_by(Gender) %>% summarize(Gender_Count= n()) 

# plot graph to depict Gender %
ds_apptnoshow %>% ggplot(aes(Gender)) +
  geom_bar(stat='count', fill="lightgreen", color="darkgreen") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.45, hjust = 0.5, colour="black" ) +
  labs(title = "Gender Ratio",
       x = "Gender", 
       y = "Count")


#f. Multiple patients have taken appointment multiple times and 
#here is quick glance of the top 10 appointments# Maximum appointments taken by a Patient is 88.

# No. of appointment taken by same patients
AppointCnt_Per_Patient <- ds_apptnoshow %>% 
  group_by(PatientId) %>% 
  summarize(AppointCnt = n())

# Top 10 Rows based on highest appointment count
top_n(AppointCnt_Per_Patient %>% arrange(desc(AppointCnt)),10)


#g. 3.2% Age data in dataset is incorrect where Age is documented as less than or equal to 0.#
#Here is the quick review of the data.
# Review Age Data and fount incorrect Age data based on Gender.
ds_apptnoshow %>% filter(Age <= 0) %>% 
  group_by(Gender) %>% summarize(AgeErrorCount = n() )


#h. Only 32% Patients received SMS of their appointments and out of that 28% patients didn't Show-Up.#
#Recommendation:# Doctor's office must need to look into this % to improve #No-Show# appointment rate.

# SMS Received by Patients of the appointments in Percentage
# 0 = No, 1 = Yes
summarytools::freq(ds_apptnoshow$SMS_received, order = "freq")[1:2, 4]

#Plot a graph to depict the no. of patients and No-Show by SMS Received #
plotGraphbasedonColumnname("No. of Patients received SMS","SMS_received")
plotGraphbasedonNoShow("Patients received SMS with Show/No-Show %", "SMS_received")

#i. ~20% of the Patients didn't Show-Up post taking the appointments.# Probably, reminders would have helped them.
# No Show Frequency in Percentage
# 0 = Show Up, 1 = Didn't Show up
summarytools::freq(ds_apptnoshow$NoShow, order = "freq")[1:2, 4]

#j. Maximum Appointments observed on Wednesdays, followed by Tuesdays and minimal on Saturdays#.
# Adding Appointment Day in the main Dataset
ds_apptnoshow <- ds_apptnoshow %>% 
  mutate(apptdayoftheweek = weekdays(as.Date(AppointmentDay)), 
         apptday = as.numeric(format(as.Date(AppointmentDay), format = "%u"))) 

# Appointment Count on day wise
ds_apptnoshow %>% 
  group_by(apptdayoftheweek, apptday) %>% 
  summarize(daywiseAppts=n()) %>% arrange(apptday) %>% 
  select(apptdayoftheweek, daywiseAppts)

#Plot Appointment Graph Day wise with % scale
ds_apptnoshow %>%
  ggplot(aes(apptdayoftheweek)) +
  geom_bar(stat='count',fill="lightgreen", color="darkgreen") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
            stat = "count", vjust = -0.25, colour="black" ) +
  labs(title = "Daywise Appointments in % scale",
       x = "Week Days", 
       y = "Appointment Count")


#k. % of No-Shows observed same on Mondays and Fridays but total count of Patients on Mondays are higher than Fridays.

#l. Overall percentage of No-Shows throughout week days is 19% or above.

#m. No-Shows % on Saturdays is higher than other days but the Patients count is significantly low.


# Appointment on day wise with Show/No-Show
plotGraphbasedonNoShow("Appointment on daywise with Show/No-Show ", "apptdayoftheweek")

# Data Clean-up - Set Age to 0 where it is stored as <0
ds_apptnoshow[ds_apptnoshow$Age <0,]$Age <- 0

# Create the Age Buckets to depict Age Pattern
ds_apptnoshow$AgeBuckets <- cut(ds_apptnoshow$Age, c(seq(0, 120, 10)), 
                                include.lowest=TRUE, right = FALSE)


#n. Maximum no. of Patients observed in the Age Bucket of 0-10 followed by 50-60.


#Plot a graph to depict the no. of patients and No-Show by Age Buckets #
ds_apptnoshow %>% 
  group_by(AgeBuckets) %>% summarise(Total=n()) %>%
  ggplot(aes(AgeBuckets, Total)) +
  geom_bar(stat='identity',colour="darkgreen", fill="lightgreen") + 
  geom_text(aes(label = Total),  vjust = -0.45) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "No. of Appointments as per Age Buckets",
       x = "AgeBuckets", 
       y = "Count")


#o. Maximum % of No-Show Patients observed in the Age Bucket of 110-120 but the no.of patients in that is significantly low hence we can ignore that No-Show ratio,
#Patients in age bucket of 10-20 has maximum rate of No-Show followed by 20-30.

ds_apptnoshow  %>% 
  group_by(AgeBuckets) %>% summarise(percentNoShow=round(sum(NoShow=='Yes')/n(),3)) %>%
  ggplot(aes(AgeBuckets, percentNoShow)) +
  geom_bar(stat='identity',colour="darkgreen", fill="lightgreen") + 
  geom_text(aes(label = paste(percentNoShow*100, "%")),  vjust = -0.45) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "% No-Show by Age Bucket",
       x = "AgeBuckets", 
       y = "% No-Show") +
  geom_hline(yintercept = mean(ds_apptnoshow$NoShow=='Yes'), col='darkred', size=1)


#p. 10% of the total Patients count has taken social aid from government for their illness expense and out of the that 24% of the Patients didn't Show-Up as per their Scheduled Appointments.


#Plot a graph to depict the no. of patients and No-Show by Scholarship #
plotGraphbasedonColumnname("No. of Patients with Scholarship", "Scholarship")
plotGraphbasedonNoShow("Scholarship Patients with Show/No-Show %","Scholarship")


#q. 20% of the total Patients observed with Hipertension and out of the that 17% of the Patients didn't Show-Up as per their Scheduled Appointments..


#Plot a graph to depict the no. of patients and No-Show by Hipertension #
plotGraphbasedonColumnname("No. of Patients with Hipertension","Hipertension")
plotGraphbasedonNoShow("Hipertension Patients with Show/No-Show %","Hipertension")


#r. 7% of the total Patients observed with Diabetes and out of the that 18% of the Patients didn't Show-Up as per their Scheduled Appointments.


#Plot a graph to depict the no. of patients and No-Show by Diabetes #
plotGraphbasedonColumnname("No. of Patients with Diabetes","Diabetes")
plotGraphbasedonNoShow("Diabetes Patients with Show/No-Show %","Diabetes")


#s. 3% of the total Patients observed with Acholism and out of the that 20% of the Patients didn't Show-Up as per their Scheduled Appointments.


#Plot a graph to depict the no. of patients and No-Show by Alcoholism #
plotGraphbasedonColumnname("No. of Patients with Alcoholism","Alcoholism") 
plotGraphbasedonNoShow("Alcoholism Patients with Show/No-Show %","Alcoholism")


#t. 2% of the total Patients observed with Handicap and average 23% of the Patients didn't Show-Up as per their Scheduled Appointments.

#Plot a graph to depict the no. of patients and No-Show by Handcap #
plotGraphbasedonColumnname("No. of Patients with Handcap", "Handcap")
plotGraphbasedonNoShow("Handicap Patients with Show/No-Show %","Handcap")

#Data Cleanup
# Converting No-Show data in binary format in column NoShow; 1 = Yes (Didn't Show-Up), 0 = No (Show-Up)
ds_apptnoshow$NoShow <- ifelse(ds_apptnoshow$NoShow =='Yes', 1, 0)
ds_apptnoshow$NoShow <- as.factor(ds_apptnoshow$NoShow)

# Removing some columns (non relevant) from dataset to shorten the overall time to train the data.
ds_apptnoshow <- ds_apptnoshow %>% select(-PatientId,-AppointmentID,
                                          -ScheduledDay,-AppointmentDay, 
                                          -Age, -Neighbourhood,  apptdayoftheweek, 
                                          -AgeBuckets, -apptdayoftheweek, -apptday)


## Quick view of the Dataset columns post cleanup.
names(ds_apptnoshow)


##Top 6 rows of the Dataset post cleanup#
head(ds_apptnoshow)


#3. Training Models
#Splitting Appointment NoShow Dataset into Train (90%) / Test (10%) dataset
set.seed(1)
test_index <- createDataPartition(ds_apptnoshow$NoShow, times = 1, p = 0.1, list = FALSE)
train <- ds_apptnoshow[-test_index,]
test <- ds_apptnoshow[test_index,]


dim(train)

dim(test)

#Model 1: Latent Dirichlet Allocation (LDA) Model#
set.seed(1)
#set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
#Linear Discriminant Analysis (LDA)
train_lda <- train(NoShow ~ ., data=train, method = "lda")
#to display Accuracy of the trained dataset
train_lda$results
#predict Accuracy by comparing it with test dataset
lda_preds <- predict(train_lda, test)
test_accuracy <- mean(lda_preds == test$NoShow)
#Add a result in Model Result table
model_results <- tibble(Models="LDA", 
                        Trained_Set_Accuracy = max(train_lda$results$Accuracy), 
                        Test_Set_Accuracy = test_accuracy)



#Model 2: Quadratic Discriminant Analysis (QDA) Model#
set.seed(1)
#set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
#Quadratic Discriminant Analysis (QDA)
train_qda <- train(NoShow ~ ., data=train, method = "qda")
#to display Accuracy of the trained dataset
train_qda$results
#predict Accuracy by comparing it with test dataset
qda_preds <- predict(train_qda, test)
test_accuracy <-mean(qda_preds == test$NoShow)
#Add a result in Model Result table
model_results <- bind_rows(model_results,
                           tibble(Models="QDA", 
                                  Trained_Set_Accuracy = max(train_qda$results$Accuracy), 
                                  Test_Set_Accuracy = test_accuracy))


#Model 3: Linear Regression Model (GLM) Model#
set.seed(1)
#set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
#Linear Regression Model
train_glm <- train(NoShow ~ ., data=train, method = "glm")
#to display Accuracy of the trained dataset
train_glm$results
#predict Accuracy by comparing it with test dataset
glm_preds <- predict(train_glm, test)
test_accuracy <- mean(glm_preds == test$NoShow)
#Add a result in Model Result table
model_results <- bind_rows(model_results,
                           tibble(Models="GLM", 
                                  Trained_Set_Accuracy = max(train_glm$results$Accuracy), 
                                  Test_Set_Accuracy = test_accuracy))


#Model 4: Gradient Boosting Machine Model (GBM) Model#
#set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
# gradient boosting machine
train_gbm <- train(NoShow~., data=train, method="gbm", verbose=FALSE)
#to display Accuracy of the trained dataset
train_gbm$results

#predict Accuracy by comparing it with test dataset
gbm_preds <- predict(train_gbm, test)
test_accuracy <- mean(gbm_preds == test$NoShow)
#Add a result in Model Result table
model_results <- bind_rows(model_results,
                           tibble(Models="GBM", 
                                  Trained_Set_Accuracy = max(train_gbm$results$Accuracy), 
                                  Test_Set_Accuracy = test_accuracy))


##Model 5: TREE (RPART) Model#

# Convert all objects of train dataset into factor to train the data with tree model
train$Gender <- as.factor(train$Gender)
train$Scholarship <- as.factor(train$Scholarship)
train$Hipertension <- as.factor(train$Hipertension)
train$Diabetes <- as.factor(train$Diabetes)
train$Alcoholism <- as.factor(train$Alcoholism)
train$Handcap <- as.factor(train$Handcap)
train$SMS_received <- as.factor(train$SMS_received)
train$NoShow <- as.factor(train$NoShow)

# Convert all objects of test dataset into factor for predicting a tree value
test$Gender <- as.factor(test$Gender)
test$Scholarship <- as.factor(test$Scholarship)
test$Hipertension <- as.factor(test$Hipertension)
test$Diabetes <- as.factor(test$Diabetes)
test$Alcoholism <- as.factor(test$Alcoholism)
test$Handcap <- as.factor(test$Handcap)
test$SMS_received <- as.factor(test$SMS_received)
test$NoShow <- as.factor(test$NoShow)

set.seed(1)
#set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
# Tree Based Model
train_rpart <- train(NoShow ~ ., data=train, method = "rpart")
#to display Accuracy of the trained dataset
train_rpart$results


#predict Accuracy by comparing it with test dataset
rpart_preds <- predict(train_rpart, test)
test_accuracy <- mean(rpart_preds == test$NoShow)
#Add a result in Model Result table
model_results <- bind_rows(model_results,
                           tibble(Models="RPART", 
                                  Trained_Set_Accuracy = max(train_rpart$results$Accuracy), 
                                  Test_Set_Accuracy = test_accuracy))


# 4. Result
#to display consolidated accuracy of trained and test data set
model_results %>% knitr::kable()


#Using Ensemble method to combine the predicted values to decrease variance
ensemble <- cbind(glm = glm_preds == "0", lda = lda_preds == "0", 
                  qda = qda_preds == "0", gbm = gbm_preds == "0", 
                  rpart = rpart_preds =="0")

#predict Accuracy by comparing it with test dataset
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "0", "1")
mean(ensemble_preds == test$NoShow)