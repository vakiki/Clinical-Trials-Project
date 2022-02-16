rm(list=ls())

###############################################################
# WHO CLINICAL PORTAL PROJECT - Vincent AKIKI - December 2021 #
###############################################################

# Packages installation

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(doSNOW)) install.packages("doSNOW", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(irlba)) install.packages("irlba", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(lsa)) install.packages("lsa", repos = "http://cran.us.r-project.org")
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")
if(!require(quanteda)) install.packages("quanteda", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(SnowballC)) install.packages("SnowballC", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(xml2)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# Packages loading

library(caret)
library(data.table)
library(doSNOW)
library(e1071)
library(ggplot2)
library(ggrepel)
library(irlba)
library(kableExtra)
library(lubridate)
library(lsa)
library(openxlsx)
library(quanteda)
library(randomForest)
library(readxl)
library(rvest)
library(SnowballC)
library(stringr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(xml2)


###################################################
#            DATA COLLECTION PROCESS              #
###################################################


# Raw data collection & structure

dlfile <- file.path("Q:/Cours/Data Science Harvard/A4-Project (capstone)/Clinical Trials/ICTRP/ICTRP-Results_mi2018.xml")

# Read the Xml in R
dlfile <- read_xml(dlfile)

# Get the wanted information :
# Internal_Number ; TrialID ; Public_title ; Scientific_title ; Primary_sponsor ;
# Date_registration ; Source_Register ; Study_type ; Countries ; Condition ; Intervention.

ICTRP_TrialID <- xml_find_all(dlfile,"//Internal_Number")
ICTRP_TrialID <- xml_text(ICTRP_TrialID, trim = TRUE)
ICTRP_TrialID <- as.data.frame(ICTRP_TrialID)

Original_TrialID <- xml_find_all(dlfile,"//TrialID")
Original_TrialID <- xml_text(Original_TrialID, trim = TRUE)
Original_TrialID <- as.data.frame(Original_TrialID)

Public_title <- xml_find_all(dlfile,"//Public_title")
Public_title <- xml_text(Public_title, trim = TRUE)
Public_title <- as.data.frame(Public_title)

Scientific_title <- xml_find_all(dlfile,"//Scientific_title")
Scientific_title <- xml_text(Scientific_title, trim = TRUE)
Scientific_title <- as.data.frame(Scientific_title)

Study_Source <- xml_find_all(dlfile,"//Source_Register")
Study_Source <- xml_text(Study_Source, trim = TRUE)
Study_Source <- as.data.frame(Study_Source)

Study_type <- xml_find_all(dlfile,"//Study_type")
Study_type <- xml_text(Study_type, trim = TRUE)
Study_type <- as.data.frame(Study_type)

Study_Condition <- xml_find_all(dlfile,"//Condition")
Study_Condition <- xml_text(Study_Condition, trim = TRUE)
Study_Condition <- as.data.frame(Study_Condition)


#merge all the previous variable into one table

ICTRP_table <- as.data.frame(c(ICTRP_TrialID,
                               Original_TrialID,
                               Study_Source,
                               Public_title,
                               Scientific_title,
                               Study_Condition,
                               Study_type))


#remove all unnecessary objects
rm(list = c("dlfile","ICTRP_TrialID","Original_TrialID","Study_Source","Public_title","Scientific_title","Study_Condition","Study_type"))


###################################################
#            DATA CLEANSING PROCESS               #
###################################################


# Check if there are missing values in our dataset
length(which(!complete.cases(ICTRP_table)))

# Total number of records within the original dataset
N_before <- nrow(ICTRP_table)

# Delete duplicates within clinical trials records

# 1/ Based on the "Original trial ID"
ICTRP_table <- distinct(ICTRP_table, Original_TrialID, .keep_all = TRUE)
# 2/ Based on the "Scientific title" text
ICTRP_table <- distinct(ICTRP_table, Scientific_title, .keep_all = TRUE)
# 3/ Based on the "Public title" text
ICTRP_table <- distinct(ICTRP_table, Public_title, .keep_all = TRUE)

# Total number of records after deleting the previous duplicates
N_After <- nrow(ICTRP_table)

# Number and percentage of duplicates in the original dataset
Delta = N_before - N_After

perc = (Delta/N_before) * 100

# Dataset "Study Type" categories distribution
length(unique(ICTRP_table$Study_type))
StudyTypeCategories_before <- ICTRP_table %>% group_by(Study_type) %>% count()


#  "Study Type" categories standardization : uppercase and lowercase
ICTRP_table$Study_type <- str_to_title(ICTRP_table$Study_type)

# Dataset "Study Type" categories distribution after standardization
length(unique(ICTRP_table$Study_type))
StudyTypeCategories_after <- ICTRP_table %>% group_by(Study_type) %>% count()

# Create an object containing all the Clinical trials (= Interventional)
Interventional = c("Intervention", "Interventional", "Interventional Clinical Trial Of Medicinal Product", "Interventional Study")

# Get only the clinical trials (= Interventional) from our dataset
ICTRP_table <- ICTRP_table %>% filter(Study_type %in% Interventional)

# Number of records in the dataset after duplicates deletion and Interventional Study selection
N_Interventional <- nrow(ICTRP_table)


########################################################################################################################################
# Call my file from Github
#library(readr)
#library(knitr)
#myfile <- "https://github.com/datasciencedojo/IntroToTextAnalyticsWithR/blob/master/spam.csv"
#spam.raw <- read_csv(myfile)

########################################################################################################################################
# Export table to Xls
# Create the workbook
# wb <- createWorkbook("ICTRP_table")
# create the tabs
# addWorksheet(wb, "ICTRP_table_export")
# import the data into the created tab and customize the table appearance
# MyStyle <- createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD")
# writeData(wb, sheet = "ICTRP_table_export", ICTRP_table, startCol = 1, startRow = 1,withFilter= FALSE, headerStyle = MyStyle, borders = "all", keepNA = TRUE)
# Adjust the column width
# setColWidths(wb, sheet = 1, cols = 1:7, widths = "auto")
# Save the Excel file
# saveWorkbook(wb, "Q:/Cours/Data Science Harvard/A4-Project (capstone)/Clinical Trials/My project/ICTRP_table.xlsx", overwrite = TRUE)
########################################################################################################################################


#####################################################
# DATA INTEGRATION AND EXPLORATORY ANALYSIS PROCESS #
#####################################################


# Call the Excel file with the "Cancer / Not-Cancer" labels for studies
CancerLabels_ReferenceTable <- read_excel("Q:/Cours/Data Science Harvard/A4-Project (capstone)/Clinical Trials/My project/Cancer Not Cancer_table.xlsx", sheet = "List")
CancerLabels_ReferenceTable <- CancerLabels_ReferenceTable %>% select(Cancer,ICTRP_TrialID)

# Integrate the "Cancer / Not-Cancer" labels to our dataset
ICTRP_table <- inner_join(ICTRP_table,CancerLabels_ReferenceTable,by = "ICTRP_TrialID")

# Convert the class label into a factor (useful for ggplot2 per example)
ICTRP_table$Cancer <- as.factor(ICTRP_table$Cancer)

ICTRP_table %>% group_by(Cancer) %>% count()

# Take a look of our Cancer labels distribution
CancerLabelsDistribution <- prop.table(table(ICTRP_table$Cancer))
CancerLabelsDistribution

# Clean up our dataset by keeping only the necessary column for the upcoming text analysis process
ICTRP_table <- ICTRP_table %>% select(ICTRP_TrialID,Public_title,Scientific_title,Study_Condition,Cancer)

# Analyze the text lengths of "Public_title", "Scientific_title", "Study_Condition"
# Create 3 new column that gives the text lenghth of "Public_title", "Scientific_title", "Study_Condition"
ICTRP_table$Public_title_TextLength <- nchar(ICTRP_table$Public_title)
ICTRP_table$Scientific_title_TextLength <- nchar(ICTRP_table$Scientific_title)
ICTRP_table$Study_Condition_TextLength <- nchar(ICTRP_table$Study_Condition)

# Quartile distribution of the 3 text length 
summary(ICTRP_table$Public_title_TextLength)
summary(ICTRP_table$Scientific_title_TextLength)
summary(ICTRP_table$Study_Condition_TextLength)

# Distribution of "Public_title" text length with Class Labels
ggplot(ICTRP_table, aes(x = Public_title_TextLength, fill = Cancer)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Public_title text length with Class Labels")

# Distribution of "Scientific_title" text length with Class Labels
ggplot(ICTRP_table, aes(x = Scientific_title_TextLength, fill = Cancer)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Scientific_title text length with Class Labels")

# Distribution of "Study_Condition" text length with Class Labels
ggplot(ICTRP_table, aes(x = Study_Condition_TextLength, fill = Cancer)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Study_Condition text length with Class Labels")

# Concatenate the 3 columns "Public_title", "Scientific_title", "Study_Condition" into 1 column "text" 
ICTRP_table <- ICTRP_table %>% unite("Text", Public_title:Study_Condition, sep= " ", na.rm = TRUE, remove = TRUE)

# Concatenate operation - Quality Control : Delta_length = 0 => QC OK 
ICTRP_table$Text_TextLength <- nchar(ICTRP_table$Text)

ICTRP_table <- ICTRP_table %>% mutate(Delta_length=
                                        Text_TextLength - 2 -
                                        Public_title_TextLength -
                                        Scientific_title_TextLength -
                                        Study_Condition_TextLength)
sum(ICTRP_table$Delta_length)

#remove all unnecessary columns for the data analysis process
ICTRP_table <- ICTRP_table %>% select(ICTRP_TrialID,Text,Public_title_TextLength,Scientific_title_TextLength,Study_Condition_TextLength,Cancer)

# Rename columns of our dataset
names(ICTRP_table) <- c("ID", "Text", "Pub_TextLength", "Sci_TextLength", "Cond_TextLength", "Label")

# Convert our class label into a factor.
ICTRP_table$Label <- as.factor(ICTRP_table$Label)

#######################################################################
#              TEXT ANALYSIS PROCESS  AND MACHINE LEARNING            #
#######################################################################

#STEP 1 : CREATE A 70%/30% STRATIFIED SPLIT

# Seting the random seed for reproducibility
set.seed(21)

# Create a 70% /30% stratified split dataset.
indexes <- createDataPartition(ICTRP_table$Label, times = 1, p =0.7, list = FALSE)

# Identify our train and test datasets.
train <- ICTRP_table[indexes,]
test <- ICTRP_table[-indexes,]

# Train and Test proportion verification compared to our original dataset
prop.table(table(train$Label))
prop.table(table(test$Label))

#STEP 2 : TRAIN TEXT "TOKENIZATION"

# Tokenize our text
train.tokens <- tokens(train$Text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)

# Lower case the tokens.
train.tokens <- tokens_tolower(train.tokens)

# Remove "stop words" using quanteda's built-in English stopword list.
train.tokens <- tokens_select(train.tokens, stopwords(), 
                              selection = "remove")
# Perform stemming on the tokens.
train.tokens <- tokens_wordstem(train.tokens, language = "english")

#STEP 3 : CREATE THE "BAG OF WORDS" MODEL => "Document Feature Matrix" (DFM)

# Transform our tokens into a "Document Feature Matrix" (DFM) : the "bag of words" model
train.tokens.DFM <- dfm(train.tokens, tolower = FALSE)

# dfm in matrix format (for data exploration purpose)
train.tokens.DFM_matrix <- as.matrix(train.tokens.DFM)

dim(train.tokens.DFM_matrix)

# Setup a the feature data frame with labels.
train.tokens.DFM.labels_df <- cbind(Label = train$Label, data.frame(train.tokens.DFM))

# Cleanup column names.
names(train.tokens.DFM.labels_df) <- make.names(names(train.tokens.DFM.labels_df))

#STEP 4 : ENHANCE THE DFM MATRIX BY COMBINING TERM FREQUENCY (TF) and INVERSE DOCUMENT FREQUENCY (IDF)

# Calculate the TF-IDF of our train dfm
train.tokens.DFM.TFIDF <- dfm_tfidf(x = train.tokens.DFM,
                                scheme_tf = "count",
                                scheme_df = "inverse",
                                base = 10,
                                force = FALSE)

# Transform the TF-IDF in matrix format
train.tokens.DFM.TFIDF_matrix <- as.matrix(train.tokens.DFM.TFIDF)

dim(train.tokens.DFM.TFIDF_matrix)

# Check for incomplete cases
incomplete.cases <- which(!complete.cases(train.tokens.DFM.TFIDF_matrix))
train$Text[incomplete.cases]

# Fix incomplete cases
train.tokens.DFM.TFIDF_matrix[incomplete.cases,] <- rep(0.0, ncol(train.tokens.DFM.TFIDF_matrix))
dim(train.tokens.DFM.TFIDF_matrix)
sum(which(!complete.cases(train.tokens.DFM.TFIDF_matrix)))

# Make a clean data frame using the same process as before.
train.tokens.DFM.TFIDF.labels_df <- cbind(Label = train$Label, data.frame(train.tokens.DFM.TFIDF))
names(train.tokens.DFM.TFIDF.labels_df) <- make.names(names(train.tokens.DFM.TFIDF.labels_df))

#STEP 5 : PERFORM SINGULAR VALUE DECOMPOSITION (SVD) ON THE TF-IDF TO REDUCE FEATURES NUMBER TO 300 FOR OUR LATENT SEMANTIC ANALYSIS (LSA).

# Time the code execution
start.time <- Sys.time()

# Perform SVD to reduce dimensionality down to 300 columns
train.irlba <- irlba(t(train.tokens.DFM.TFIDF_matrix), nv = 300, maxit = 600)

# Total time of execution on workstation was 
total.time <- Sys.time() - start.time
total.time

# Take a look at the new feature for our documents.
View(train.irlba$v)

# As with TF-IDF, we will need to project new data (e.g., the test data) into the SVD semantic space.
# The following code illustrates how to do this using a row of the training data that has already been transformed by TF-IDF using the mathematic formula
sigma.inverse <- 1 / train.irlba$d
u.transpose <- t(train.irlba$u)
document <- train.tokens.DFM.TFIDF_matrix[1,]
document.hat <- sigma.inverse * u.transpose %*% document

# Look at the first 15 components of projected document and the corresponding row in our document semantic space (i.e., the v matrix)
document.hat[1:15]
train.irlba$v[1, 1:15]

# Create new feature data frame using our document semantic space of 300 features
train.svd <- data.frame(Label = train$Label, train.irlba$v)

# STEP 6 : PERFORM ON THE TEST DATA OUR PREVIOUS PREPROCESSING PIPELINE OF :
#         1/Tokenization ; 2/Lower casing ; 3/Stopword removal ; 4/Stemming ;
#         5/Adding bigrams (if needed) ; 6/Transform to dfm et 7/Ensure test dfm has same features as train dfm Tokenization.
test.tokens <- tokens(test$Text, what = "word", 
                      remove_numbers = TRUE, remove_punct = TRUE,
                      remove_symbols = TRUE, remove_hyphens = TRUE)

# Lower case the tokens
test.tokens <- tokens_tolower(test.tokens)

# Stopword removal from the tokens
test.tokens <- tokens_select(test.tokens, stopwords(), 
                             selection = "remove")

# Stemming the tokens
test.tokens <- tokens_wordstem(test.tokens, language = "english")

# Add bigrams
# test.tokens <- tokens_ngrams(test.tokens, n = 1:2)

# Convert n-grams to document-term frequency matrix.
test.tokens.DFM <- dfm(test.tokens, tolower = FALSE)

# Explore the train and test dfm objects.
train.tokens.DFM
test.tokens.DFM

# Ensure the test dfm has the same n-grams as the training dfm.
test.tokens.DFM <- dfm_match(test.tokens.DFM, features = featnames(train.tokens.DFM))
test.tokens.DFM_matrix <- as.matrix(test.tokens.DFM)

# Calculate the TF-IDF of our test dfm that has now the same features as the train dfm
test.tokens.DFM.TFIDF <- dfm_tfidf(x = test.tokens.DFM,
                                    scheme_tf = "count",
                                    scheme_df = "inverse",
                                    base = 10,
                                    force = FALSE)

# Transform the test TF-IDF into a matrix format
test.tokens.DFM.TFIDF_matrix <- as.matrix(test.tokens.DFM.TFIDF)

# Fix incomplete cases
summary(test.tokens.DFM.TFIDF_matrix[1,])
test.tokens.DFM.TFIDF_matrix[is.na(test.tokens.DFM.TFIDF_matrix)] <- 0.0
summary(test.tokens.DFM.TFIDF_matrix[1,])

# STEP 7 : PROJECT THE TEST TF-IDF MATRIX USING THE SVD MATRIX FACTORIZATION PROJECTION

# TEST DATA final projection into the training LSA semantic space, the SVD matrix factorization
test.svd.raw <- t(sigma.inverse * u.transpose %*% t(test.tokens.DFM.TFIDF_matrix))

# Build the test data frame to feed into our trained machine learning model for predictions
test.svd <- data.frame(Label = test$Label, test.svd.raw)

# saveRDS(test.svd, "./M1_test.svd.rds")

# STEP 8 : MACHINE LEARNING PREDICTION

# Create 30 random stratified samples : 10-fold cross validation repeated 3 times
set.seed(21)
cv.folds <- createMultiFolds(train$Label, k = 10, times = 3)

cv.cntrl <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 3, index = cv.folds)


# Computing time optimization : create a cluster to work on "3" logical cores, if you have a machine a Quad-Core (4 cores) processor.
# 3 cores allowed to R to run our machine learning algorithm and the last core to keep other application on your machine running.
# PS: if you have a more performance processor you can improve the number of processor allowed.
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

# Time the code execution
start.time <- Sys.time()

# Re-run the training process with the additional feature.
# It takes more that 7 hours when allowing 3 cores. 
rf.cv.unigram.nv300.nolength <- train(Label ~ ., data = train.svd, method = "rf",
                 trControl = cv.cntrl, tuneLength = 7,
                 importance = TRUE)

# Processing is done, stop cluster.
stopCluster(cl)

# Total time of execution on workstation was
total.time <- Sys.time() - start.time
total.time

# save the model to the R working directory
saveRDS(rf.cv.unigram.nv300.nolength, "./rf.cv.unigram.nv300.nolength.rds")

# You can load the model next time without running again the machine learning step.
# rf.cv.unigram.nv300.nolength <- readRDS("./rf.cv.unigram.nv300.nolength.rds")
# print(rf.cv.unigram.nv300.nolength)

# Prediction result
rf.cv.unigram.nv300.nolength

preds <- predict(rf.cv.unigram.nv300.nolength, test.svd)
confusionMatrix(preds, test.svd$Label)

