###################  TEXT CLASSIFICATION   ####################
##input = data
##output = data_cleaned

rm(list = ls())
setwd("E:/My documents/Sem-3/D/wd")

####################    Load Packages   #####################

library("dplyr")
library("caret")
library("tm")
library("stringr")
library("wordcloud")
library("Matrix")
library("slam")
library("glmnet")
library("SnowballC")
library("tidytext")

###################     Load data into R    ##################

data <- read.csv("TextClassification_Data.csv", header = TRUE, na.strings = c(""," "))

str(data)       #observe structure of data

data$fileid <- NULL
data$ID <- NULL
data$SUMMARY <- as.character(data$SUMMARY)
data$DATA <- as.character(data$DATA)

data$previous_appointment <- tolower(data$previous_appointment)
data$previous_appointment <- gsub('yes', 1, as.character(data$previous_appointment), ignore.case = FALSE, perl = FALSE)
data$previous_appointment <- gsub('no', 0, as.character(data$previous_appointment), ignore.case = FALSE, perl = FALSE)
data$previous_appointment <- as.factor(data$previous_appointment)

data$categories <- gsub('as', 'AS', as.character(data$categories), ignore.case = FALSE, perl = FALSE)
data$categories <- gsub('m', 'M', as.character(data$categories), ignore.case = FALSE, perl = FALSE)
data$categories <- gsub('JUNK', NA, as.character(data$categories), ignore.case = FALSE, perl = FALSE)

data$sub_categories <- gsub('m', 'M', as.character(data$sub_categories), ignore.case = FALSE, perl = FALSE)
data$sub_categories <- gsub('JUNK', NA, as.character(data$sub_categories), ignore.case = FALSE, perl = FALSE)

data$SUMMARY <- str_trim(data$SUMMARY)
data$DATA <- str_trim(data$DATA)

sum(is.na(data))           #check no.of missing values
data <- data[complete.cases(data),]   #Remove all rows with missing values

str(data)      #Check the structure of data

################   Cleaning the DATA variable   #####################

for(i in c(1:53911)){ 
  data$DATA[i]=gsub("\\\\[^\\s]+\\s"," ",data$DATA[i], perl=T) 
  data$DATA[i]=gsub("\\\\par[^}]+}"," ",data$DATA[i], perl=T) 
  data$DATA[i]=gsub("{[^}]+}"," ",data$DATA[i], perl=T) 
  data$DATA[i]=gsub("xxxx-xxxx"," ",data$DATA[i], perl=T)
  data$DATA[i]=gsub("}"," ",data$DATA[i], perl=T)
}
rm(i)

##########################    Preparing necessary functions    #############################

DF <- function(data1, print = TRUE){
  corpus <- Corpus(VectorSource(data1))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords('english'), 'arial', 'xxx', 'xx'))   #Remove stop words
  corpus <- tm_map(corpus, stripWhitespace)    #Remove unnecessary spaces
  dict <- corpus
  corpus <- tm_map(corpus, stemDocument)            
  # tokenize the corpus
  corpusTokenized <- lapply(corpus, scan_tokenizer)
  # stem complete each token vector
  myTokensStemCompleted <- lapply(corpusTokenized, stemCompletion, dict)
  # concatenate tokens by document, create data frame
  myDf <- data.frame(text = sapply(myTokensStemCompleted, paste, collapse = " "), stringsAsFactors = FALSE)
  return(myDf)
}  

tdm <- function(data1, print = TRUE){
  corpus <- Corpus(VectorSource(data1))
  tdm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf, min_docfreq = 3))
  return(tdm)
}  

############################   Preparing data_cleaned(cleaned data frame)    ###################

df <- DF(data$DATA)
ds <- DF(data$SUMMARY)

data_cleaned <- as.data.frame(cbind(summary = ds$text, data = df$text, previous_appointment = data$previous_appointment, categories = data$categories, sub_categories = data$sub_categories))
write.csv(data_cleaned, "data_cleaned.csv")
rm(df, ds)

############################   Preparing tdm's for visualizations    ########################

tdm_sum <- tdm(data_cleaned$summary)
tdm_data <- tdm(data_cleaned$data)

#Appointments
data_appointments <- data_cleaned %>% filter(data_cleaned$categories == "APPOINTMENTS")
table(data_appointments$sub_categories)
tdm_appointments <- tdm(data_appointments$data)
rm(data_appointments)

#Ask_a_doctor
data_ask_a_doctor <- data_cleaned %>% filter(data_cleaned$categories == "ASK_A_DOCTOR")
table(data_ask_a_doctor$sub_categories)
tdm_ask_a_doctor <- tdm(data_ask_a_doctor$data)
rm(data_ask_a_doctor)

#lab
data_lab <- data_cleaned %>% filter(data_cleaned$categories == "LAB")
table(data_lab$sub_categories)
tdm_lab <- tdm(data_lab$data)
rm(data_lab)

#prescreption
data_prescreption <- data_cleaned %>% filter(data_cleaned$categories == "PRESCRIPTION")
table(data_prescreption$sub_categories)
tdm_prescreption <- tdm(data_prescreption$data)
rm(data_prescreption)

#miscellaneous
data_miscellaneous <- data_cleaned %>% filter(data_cleaned$categories == "MISCELLANEOUS")
table(data_miscellaneous$sub_categories)
tdm_miscellaneous <- tdm(data_miscellaneous$data)
rm(data_miscellaneous)

#############################  Visualizations   #############################

library("ggplot2")

#word cloud for "SUMMARY" and "DATA" variables

a <- tidy(tdm_sum) %>% 
  count(term, sort = TRUE) %>%
  top_n(1000)
pal <- c("cyan3", "indianred1", "gold1", "burlywood2", "chartreuse2")
wordcloud(a$term, a$n, max.words = 200, colors = pal)
rm(a, pal)

b <- tidy(tdm_data) %>% 
  count(term, sort = TRUE) %>%
  top_n(1000)
pal <- c("cyan3", "indianred1", "gold1", "burlywood2", "chartreuse2")
wordcloud(b$term, b$n, max.words = 200, colors = pal)
rm(b, pal)

#bar plot for most repeated words in each category

count_bar <- function(df, term, fill, title, print = TRUE){
  a <- df %>%
    count(term, sort = TRUE) %>%
    top_n(10) %>%
    mutate(word = reorder(term, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill = fill) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust=0.5)) +
    xlab(NULL) +
    coord_flip()
  return(a)
}

tidy_appointments <- tidy(tdm_appointments)
count_bar(tidy_appointments, term, "chartreuse2", "Appointments")
rm(tidy_appointments)

tidy_ask_a_doctor <- tidy(tdm_ask_a_doctor)
count_bar(tidy_ask_a_doctor, term, "cyan3", "Ask_a_doctor")
rm(tidy_ask_a_doctor)

tidy_lab <- tidy(tdm_lab)
count_bar(tidy_lab, term, "blue2", "Lab")
rm(tidy_lab)

tidy_prescreption <- tidy(tdm_prescreption)
count_bar(tidy_prescreption, term, "burlywood4", "Prescreption")
rm(tidy_prescreption)

tidy_miscellaneous <- tidy(tdm_miscellaneous)
count_bar(tidy_miscellaneous, term, "deeppink1", "Miscellaneous")
rm(tidy_miscellaneous)

#################################################################################