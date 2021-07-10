## ----debugging_flags, include=FALSE---------------------------------------------------------------------------------------------------------------------------------
#https://bookdown.org/yihui/rmarkdown-cookbook/
#options(tinytex.verbose = TRUE)  # in case of errors switch on for debugging


## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# cleanup
rm(list=ls(all=T))

#----------------------------------------------------------------------------------------------------------------------------\
### set working dir, prepare download dir, generate on demand the ".R" file
#----------------------------------------------------------------------------------------------------------------------------/
#wd = "D:\\Courses\\dsbook-master\\capstone\\covid_finance"
#dwd = paste(wd,"\\","data",sep="")
#setwd(wd)
#subDir <- "data"
#ifelse(!dir.exists(file.path(".", subDir)), dir.create(file.path(".", subDir)), FALSE)

#Sys.setenv(R_GSCMD="C:\\Program Files\\gs\\gs9.54.0\\bin\\gswin64.exe")

# knitr::purl("covid_finance.Rmd") # generate the ".R" script on demand

#----------------------------------------------------------------------------------------------------------------------------\
### List of packages required for this analysis - explanation see below (1) 
#----------------------------------------------------------------------------------------------------------------------------/
pkg <- c("dplyr"   , "tidyverse"  , "tidytext"      , "tidyquant" ,"data.table" ,   
         "ggplot2" , "DiagrammeR" , "ggwordcloud"   , "wordcloud" , "gridExtra", 
         "readxl"  , 
         "stringi" , "countrycode",
         "class"   , "e1071"      , "fastNaiveBayes", "RTextTools", "tm", "SparseM", "caret"
         )
new.pkg <- pkg[!(pkg %in% installed.packages())] # if not installed => install
if (length(new.pkg)) { install.packages(new.pkg, repos = "https://cran.rstudio.com") }

# (1) Libraries and what they are used for:
library(dplyr, warn.conflicts = FALSE) # grammar to manipulate tables
library(tidyverse)   # the connection design
library(tidytext)    # textual analysis
library(tidyquant)   # financial analysis, tidy access to quantmod = financial analysis
library(data.table)  # helper for data tables / classes to data frames
# 
library(ggplot2)     # the standard to plot data (normally in tidyverse)
library(DiagrammeR)  # draw some overview diagrams via grViz
library(ggwordcloud) # draw word clouds - facet possible - geom_text_wordcloud
library(wordcloud)   # draw word clouds, simpler
library(gridExtra)   # group plots together in a grid - as per course book chapter 7.14 - grid.arrange
# 
library(readxl)      # read excel files
#library(writexl)    # write excel files, sometimes needed for debugging / short crosschecks
# 
library(stringi)     # stri_trans_tolower - better than tolower because considers itâ€™s / itÃ¢â‚¬â„¢s
library(textdata)    # helper for textual analysis, stopwords et al.
# 
library(countrycode) # big helper for mapping different syntax of country names to standardized ISO3-code
# 
library("e1071")     # for Naive-Bayes
library("fastNaiveBayes")  
library("RTextTools") # for create_container = DocTermMatrix
# 
library(tm)          # for text mining - VCorpus
library(class)       # for KNN
library(SparseM)     # for SVN
library(caret)       # for traincontrol, confusionmatrix
#??gmodels

#tinytex::install_tinytex() # 100MB!
#tinytex::tlmgr_install("pdfcrop")

# Suppress summarise info - annoying in reports and does not help
options(dplyr.summarise.inform = FALSE)

#---------------------------------------------------------------------------------------------------------------------------------\
## ----load_Data_all, echo=FALSE, results = 'hide'--------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/


############################################################################################################################
############################################################################################################################
# DATA LOAD PART
############################################################################################################################
############################################################################################################################

#---------------------------------------------------------------------------------------------------------------------------------\
### my_download = wrapper for downloading to temp file
#---------------------------------------------------------------------------------------------------------------------------------/
my_download <- function(burl,bfile,bdest="") {
  url  <- paste(burl,bfile,sep="")
  tmp  <- tempfile()
  download.file(url, tmp, mode="wb")
  return(tmp)
}

#---------------------------------------------------------------------------------------------------------------------------------\
## ----load_FinData, echo=FALSE, results = 'hide'---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/

#---------------------------------------------------------------------------------------------------------------------------------\
## ----load_FCI0, echo=FALSE, message=FALSE, cache=TRUE---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
dest <- my_download("https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/936261/",
                       "covid-fci-data.xlsx")
fci0    <- readxl::read_excel(dest,sheet=2,trim_ws = TRUE)
#dim(fci0)  # 3723x14
as_tibble(head(fci0))
print(paste("FCI-Data:",nrow(fci0),"rows x",length(fci0),"columns"))

#---------------------------------------------------------------------------------------------------------------------------------\
## ----load_Fiscal0, echo=FALSE, message=FALSE, warn=FALSE, cache=TRUE------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
file    <- "revised-april-2021-fiscal-measures-response-database-publication-april-2021-v3"
dest <- my_download("https://www.imf.org/en/Topics/imf-and-covid19/~/media/Files/Topics/COVID/FM-Database/SM21/",
                       paste(file,".ashx",sep=""),
                       paste(file,".xlsx",sep=""))
fiscal0  <- readxl::read_excel(dest,sheet=1,skip=4)
#dim(fiscal0) # 193 25
as_tibble(head(fiscal0))
print(paste("Fiscal:",nrow(fiscal0),"rows x",length(fiscal0),"columns"))
#---------------------------------------------------------------------------------------------------------------------------------\
## ----load_Country0, echo=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/

#---------------------------------------------------------------------------------------------------------------------------------\
## ----load_Country1, echo=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
dest  <- my_download("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/",
                        "all.csv")
country0 <- read_csv(dest)
#spec(country0) 
print(paste("Country:",nrow(country0),"rows x",length(country0),"columns"))

#---------------------------------------------------------------------------------------------------------------------------------\
## ----load_Class0, echo=FALSE, message=FALSE, cache=TRUE-------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
dest  <- my_download("http://databank.worldbank.org/data/download/site-content/",
                      "CLASS.xls")
class0 <- readxl::read_excel(dest,sheet=1,skip=4)
#as_tibble(head(class0)) 
class0%>%sample_n(5)
print(paste("Country Class:",nrow(class0),"rows x",length(class0),"columns"))



## ----load_Sent0, echo=FALSE, message=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/

#---------------------------------------------------------------------------------------------------------------------------------\
## ----load_Sent, echo=FALSE, message=FALSE, cache=TRUE---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
# https://sraf.nd.edu/textual-analysis/resources/#LM%20Sentiment%20Word%20Lists
# Download the file "LoughranMcDonald_SentimentWordLists_2018.xlsx" 
# not needed, using builtin list of R

#?get_sentiments
#get_sentiments("bing")  # big differences in result using other lists!
#get_sentiments("nrc")
Sentiment_Classifier <- get_sentiments("loughran") 

#str(Sentiment_Classifier) 
Sentiment_Classifier%>%sample_n(5)
print(paste("Sentiment_Classifier:",nrow(Sentiment_Classifier),"rows x",length(Sentiment_Classifier),"columns"))

############################################################################################################################
############################################################################################################################
# DATA PREPARE PART
############################################################################################################################
############################################################################################################################
#---------------------------------------------------------------------------------------------------------------------------------\
## ----preparation, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/

#---------------------------------------------------------------------------------------------------------------------------------\
## ----prep_FCI1, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/

#fci0[['Country Name']][0:2]
#dim(fci0)  # 3788x14
#fci0 %>% head(2) 
# rename a bit more handy and drop unused columns
colnames(fci0)        <- c('ID','Country','ISO3','IncomeLevel','Authority','Date',
                           'Measure1','Measure2','Measure3','Detail','Reference','TermDate','ModParent','Parent')
fci1 <- fci0 %>% select(-c('ID',                                     'Reference','TermDate','ModParent','Parent'))
fci1$ym <- format(fci1$Date, "%y-%m")
#fci1 %>% head(2) 
#dim(fci1)  # 3788x9
fci2 <- fci1[!(is.na(fci1$Measure1   ) | fci1$Measure1    =="" | # cleanup whitespace(!!)
               is.na(fci1$Measure2   ) | fci1$Measure2    =="" | 
               is.na(fci1$Detail     ) | fci1$Detail      =="" | 
               is.na(fci1$Date       ) | fci1$Date > "2021-03-31" |   # no empty date or funny date in future (Italy!)
               is.na(fci1$IncomeLevel) | fci1$IncomeLevel =="" | fci1$IncomeLevel  =="Aggregates"| 
               is.na(fci1$Country    ) | fci1$Country     =="" | fci1$Country      =="G20" ),]
#dim(fci2)  # 3771x9
# rename cryptic / too short / too long titles / content
fci2$Measure1[fci2$Measure1  == 'Liquidity/Funding']  <-  'Liquidity/ Funding'
fci2$Measure1[fci2$Measure1  == 'Financial Markets/Nbfi']  <-  'Fin. Markets/ Nbfi'
fci2$Measure2[fci2$Measure2  == 'NBFI']  <-  'Non-Bank Financial Intermediaries'
fci2$Measure2[fci2$Measure2  == 'Promoting and ensuring availability of digital payment mechanisms'] <- 'Ensure digital payment mechanisms'
fci2$Measure2[fci2$Measure2  == 'Consumer protection measures and ensuring availability and acceptance of cash'] <- 'Consumer protection/ avail+accept cash'
fci2$Measure2[fci2$Measure2  == 'Enhancing tools for out-of-court debt restructuring and workouts'] <- 'Enhance tools: Debt Restructuring+Workouts'

fci2$Country[fci2$Country  == 'United Kingdom of Great Britain and Northern Ireland'] <- 'United Kingdom'

# normalize mixed writing style (cap./non cap/camel style
fci2$IncomeLevel <- str_to_title(fci2$IncomeLevel)
fci2$Measure1    <- str_to_title(fci2$Measure1)
fci2$Measure2    <- str_to_title(fci2$Measure2)
no_c <- fci2$Country %>% sort() %>% unique() # check that joins do not filter out lines
#length(no_c) # 156

FCI2 <- fci2 # central result of FCI-preparation

#```
#After cleaning - here a subset:
#\tiny 
#```{r prep_FCI2a, echo=FALSE}
#knitr::kable(fci2[1:5,1:5], caption = "\\label{tab:fci2}FCI data")
FCI2 %>% sample_n(5) #%>% select(c('ISO3','Measure1','Detail')) %>% knitr::kable()  #[1:5,1:5]
#```
#\normalsize 
#```{r prep_FCI2b, echo=FALSE}

print(paste("FCI data:",nrow(FCI2),"rows x",length(FCI2),"columns"))

if(FALSE) { # for debug - Save datasets to a file
  saveRDS(FCI2   , "FCI2.rds")
}
rm(fci0,fci1,fci2)
#---------------------------------------------------------------------------------------------------------------------------------\
## ----prep_Fiscal2, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------
fiscal2 %>% sample_n(5) #%>% select(c('Country','ISO3','Unit1','Budget','AddHealth','AddNonHealth'))%>%knitr::kable()
#knitr::kable(fiscal2[1:5,1:5], caption = "\\label{tab:fiscal0}Country Fiscal data")
#fiscal2[1:5,1:5]
print(paste("Fiscal data:",nrow(fiscal2),"rows x",length(fiscal2),"columns"))

rm(fiscal1)

#---------------------------------------------------------------------------------------------------------------------------------\
## ----prep_Country1, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
# country seems ok - we just need it for the ISO3-Code - Region - map 

#country0 %>% head(4)
# rename a bit more handy and drop unused / non-needed columns
colnames(country0) <-            c('Country','a1','ISO3','a3','ccode','Region','SubRegion','intRegion','Regioncode','subregcode','intregcode')
country1 <- country0 %>% select(-c(          'a1',       'a3','ccode',                     'intRegion','Regioncode','subregcode','intregcode')) 
# normalize the country names
country1$Country[country1$Country == 'United Kingdom of Great Britain and Northern Ireland'] <- 'United Kingdom'
country1$Country[country1$Country == 'United Kingdom'                    ] <- 'UK'
country1$Country[country1$Country == 'United States of America'          ] <- 'USA'

# add Income Level from FCI data to country - to summarize 
fciInc <- FCI2[c('ISO3','IncomeLevel')]# ,'Country')]
fciInc$IncomeLevel <- gsub(' Income', '', fciInc$IncomeLevel)
country1 <- merge(x=country1,y=fciInc,by="ISO3",all=TRUE) %>% unique()

# add fin.region from country class table
class1 <- class0[!(is.na(class0$Region) | class0$Region == "" | class0$Region == "x"),]  # cleanup whitespace or noise(!!)
class2 <- class1 %>% select(c('Code','Economy','Region','Income group'))
colnames(class2) <- c('ISO3','Economy','FinRegion','Incomegroup')
class2$Incomegroup = gsub(" income", "",class2$Incomegroup)
class2$Incomegroup <- str_to_title(class2$Incomegroup)
country1 <- merge(x=country1,y=class2,by="ISO3",all=TRUE) %>% unique()
# IncomeLevel not well filled, only 155 => IncomeGroup is much better filled, but also not always -> coalesce!
country1 <- country1 %>% 
  mutate(IncomeLevel = coalesce(IncomeLevel,Incomegroup)) # take the best of both worlds - whatever has a value: grab it

# cleanup data
country1 <- country1[!(is.na(country1$Region)     |country1$Region     =="" |
                       is.na(country1$SubRegion)  |country1$SubRegion  =="" |
                       is.na(country1$IncomeLevel)|country1$IncomeLevel==""),]
# 218 instead of 156 without CLASS

## ----prep_Country2, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------



## ----prep_Country3, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------
COUNTRY2 <- country1
COUNTRY2 %>% head(5) %>% knitr::kable(caption="\\label{tab:country2}Country <-> Region/SubRegion/IncomeLevel")


## ----sent_fci2, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------
FCI2 %>% sample_n(5) %>% select(Detail) %>% knitr::kable()

t0 <- FCI2[c('Country','Detail','ISO3')]  # filter needed columns out of FCI data

t1 <- t0 %>%                              # tokenize / group / count detail
  group_by(ISO3) %>%
  summarise(word=paste0(Detail, collapse=' '))  %>% 
  mutate(linenumber = row_number())       # linenumber to be able to rejoin later on

t1 <- data.frame(t1)                      # change grouped_df to df

# remove stop words - not absolutely needed cause the sentiment is an inner join - but to get an overview!  
t2 <- t1 %>%  
  unnest_tokens(word, word) %>%
  anti_join(stop_words) %>%          # filter out stop words (built_in list)
  filter(!grepl('[0-9]', word)) %>%  # filter out numbers
  #filter(n() > 10) %>%              # filter out very rare words
  ungroup()
#str(t2) # [89,526 x 2] , rare words out / n>10 => [87,971 x 4], numbers out => tibble [79,899 x 4] 

# core of sentiment analysis = inner join to sentiments
t3 <- t2 %>%
  inner_join(Sentiment_Classifier) #%>% unique() # not unique because words can be in different sentences per country

#---------------------------------------------------------------------------------------------------------------------------------\
## ----sent_country1, echo=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
t3  <- t3 %>% # pull out only sentiment words
  group_by(linenumber) %>% 
  count(sentiment) 

# merge back the beginning - here we have the country-word-sentiment-count
t4 <- merge(x=t1,y=t3,by="linenumber") #,all=TRUE)  #outer join
t4 <- t4 %>% select(-c(word))
t4[is.na(t4)] <- 0 # replace NAs with 0

# count c-ountries per r-egion (=cr), s-ubRegion(=cs), i-ncomelevel(=ci) 
# this is to normalize counts of words per countries
cr <- COUNTRY2 %>% group_by(     Region) %>% count(); colnames(cr)[colnames(cr) == "n"] <- "nr"
cs <- COUNTRY2 %>% group_by(  SubRegion) %>% count(); colnames(cs)[colnames(cs) == "n"] <- "ns"
ci <- COUNTRY2 %>% group_by(IncomeLevel) %>% count(); colnames(ci)[colnames(ci) == "n"] <- "ni"

# now merge back to other tables
t5 <- merge(x=t4,y=COUNTRY2,by="ISO3"  ) %>% unique() #,all=TRUE)  # country-word-sentiment-count => Region/SubRegion/Income
t5 <- t5 %>% filter(!is.na(FinRegion))           # remove these without finregion
t5 <- t5 %>% filter(sentiment != "superfluous")  # do not use superfluous, only very rare

t6 <- merge(x=t5,y=fiscal3 ,by="Country"    ) %>% unique() #,all=TRUE)  #   => GDP
low_n <- 10  # low water limit to keep it readable
t7 <- t6 %>% # filter a few out to have it readable
  filter(n>low_n) %>%
  mutate(linenumber = row_number())  # linenumber to be able to rejoin later on


# merge comparing Regions -----------------------------------------\
## ----sent_COUNTRY3, echo=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------/

# merge comparing Regions -----------------------------------------\
t7 <- merge(x=t6,y=cr      ,by="Region"     ,all=TRUE) %>% unique() #,all=TRUE)  #   => countries per Region
t8 <- merge(x=t7,y=cs      ,by="SubRegion"  ,all=TRUE) %>% unique() #,all=TRUE)  #   => countries per SubRegion
t9 <- merge(x=t8,y=ci      ,by="IncomeLevel",all=TRUE) %>% unique() #,all=TRUE)  #   => countries per IncomeLevel

###############################################\
SENT1 <- t9  # t9 is the central result of sentiment analysis, all in
###############################################/

if(FALSE) { # for debug - Save datasets to a file
   saveRDS(fci0   , ".\\data\\fci0.rds")
   saveRDS(t2     , ".\\data\\t2.rds")
   saveRDS(t5     , ".\\data\\t5.rds")
   saveRDS(t7     , ".\\data\\t7.rds")
   saveRDS(t8     , ".\\data\\t8.rds")
   saveRDS(t9     , ".\\data\\t9.rds")
   ## load objects from file if downloaded / prepared before
   #t9   <- readRDS(".\\data\\t9.rds")
   #fci0 <- readRDS(".\\data\\fci0.rds")
}

# count sentiment per country per line - get first maximum of line as maximum sentiment... as per (*1*) ... weak point!!
SENT2 <- t5 %>% 
  spread(sentiment, n, fill = 0) 

# evaluate the maximal sentiment word list of the country
SENT2$max_sent  <- pmax(SENT2$negative,SENT2$constraining,SENT2$litigious,SENT2$uncertainty,SENT2$positive)# ,SENT2$superfluous) 
SENT2$max_sentx <- case_when(
  SENT2$max_sent    ==0              ~ 'no',
  SENT2$negative    ==SENT2$max_sent ~ 'negative'    ,
  SENT2$constraining==SENT2$max_sent ~ 'constraining',
  SENT2$litigious   ==SENT2$max_sent ~ 'litigious'   ,
  SENT2$positive    ==SENT2$max_sent ~ 'positive'    ,
  SENT2$uncertainty ==SENT2$max_sent ~ 'uncertainty' ,
  #ENT2$superfluous ==SENT2$max_sent ~ 'superfluous' , # filtered out above
  TRUE                               ~ 'no')


############################################################################################################################
############################################################################################################################
# MACHINE LEARNING PART
############################################################################################################################
############################################################################################################################

## ----ml0a, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------
# evaluate number of sentiments
tss <- t(colSums(SENT2[c("negative","constraining","litigious","uncertainty","positive")]))
knitr::kable(tss, caption="\\label{tab:sent_country5}maximal sentiment counts over all words")
#| negative| constraining| litigious| uncertainty| positive| superfluous|
#|--------:|------------:|---------:|-----------:|--------:|-----------:|
#|     2796|         1481|      1232|         788|      544|          11|
# Rank   1            2          3             4         5             6

if(FALSE) { # for debug - Save datasets to a file
  saveRDS(SENT2     , "SENT2.rds")
}

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml0, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/

####################################################\
# merge sentiment to country via ISO3
fci.qual     <- SENT2[c('ISO3','max_sentx')]     # max sent per country # ,'word'
fci.txt.qual <- merge(x=t0,y=fci.qual,by="ISO3") # merge back to starting point / the multi-texts per country

# =: ISO3:country:Detailtext:max.sentiment

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml1a, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
fci.corpus <- VCorpus(VectorSource(fci.txt.qual$Detail))
#typeof(fci.corpus) # list
#print(fci.corpus)  # 3728 documents / 152 countries / 16.6MB
#object.size(fci.corpus) # 16600568 bytes

fci.corpus.clean <- fci.corpus %>%
  tm_map(content_transformer(stri_trans_tolower)) %>%   # better than tolower because considers it’s / itâ€™s
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords()) %>%
  tm_map(removePunctuation) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)

for(i in 1:3){
    print(as.character(fci.corpus[[i]]))
    print(paste("  ",as.character(fci.corpus.clean[[i]])))
}


#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml2, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
#?TermDocumentMatrix # simpler
#?DocumentTermMatrix # more parameter, weighting function
fci.dtm <- DocumentTermMatrix(fci.corpus.clean)
#object.size(fci.dtm) # 1727816 bytes

#?removeSparseTerms 
fci.dtm <- removeSparseTerms(fci.dtm,0.99)
#object.size(fci.dtm) # 0.5=240528 0.9=386056 0.99=1114296 0.99=1114296 0.995=1259192 bytes

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml3a, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
set.seed(123L)
random <- sample(nrow(fci.dtm), round(nrow(fci.dtm) * 0.8), replace = FALSE) 
fci.dtm.train        <- fci.dtm[random,  ] # 80%
fci.dtm.test         <- fci.dtm[-random, ] # 20%
fci.dtm.train.labels <- as.factor(fci.txt.qual[random,  ]$max_sentx)
fci.dtm.test.labels  <- as.factor(fci.txt.qual[-random, ]$max_sentx)

# check that test and train data are representative = have similar distribution
a = fci.dtm.train.labels %>% table %>% prop.table
b = fci.dtm.test.labels  %>% table %>% prop.table
df <- data.frame(train=a,test=b) %>%
  mutate(delta_percent = round(10*(a-b)*100)/10)
knitr::kable(df, caption="\\label{tab:test_train15}Frequency of test and train data are similar")

# representative - no big delta!

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml4, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
min_freq=5   #50
fci.dtm.train.freq <- fci.dtm.train %>% findFreqTerms(min_freq) %>% fci.dtm.train[ , .]
fci.dtm.test.freq  <- fci.dtm.test  %>% findFreqTerms(min_freq) %>% fci.dtm.test[ , .]

#object.size(fci.dtm)            # 1.7MB 2982 x 737 = 48447
#object.size(fci.dtm.train)      # 1.4MB 48447 
#object.size(fci.dtm.train.freq) # 1.2MB 50: 2982 x 322 = 37840


# result structure to find the maximum / best algorithm with best parameter
V <- data.frame(t=NA, max=0, acc=0, timing=0)

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml5a, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/

# Naive-Bayes without using laplace (= 0).
V[1,c("t","max","acc","timing")] <- c("Naive-Bayes e1071   no laplace",0,0,0)
#print(V[1,]$t)

# prepare train and test data
#  Note that, e1071 asks the response variable to be numeric or factor. 
#  Thus, we convert characters to factors here. This is a little trick.
convert_counts <- function(x) {   x <- ifelse(x > 0, "Yes", "No") }
x  <- fci.dtm.train.freq %>% apply(MARGIN = 2, convert_counts)
xl <- fci.dtm.train.labels
t  <- fci.dtm.test.freq  %>% apply(MARGIN = 2, convert_counts)
tl <- fci.dtm.test.labels

# train model and check accuracy on test data (and measure time)
max_par <- max_acc <- 0
start_time   <- Sys.time()   # for timing\
model          <- naiveBayes(x, xl) 
pred           <- predict(model, t, type="class")
confusionMatrix(pred, tl, dnn = c("Predicted", "Actual"),mode = "sens_spec")
max_acc        <- confusionMatrix(pred, tl, dnn = c("Predicted", "Actual"),mode = "sens_spec")$overall[1]#$Accuracy
end_time     <- Sys.time()   # for timing/

print(sprintf("   best parameter:%02.2f - accuracy: %02.3f%%",max_par, 100*max_acc))
V[1,c("max","acc","timing")] <- c(max_par,round(100*max_acc,2),round(difftime(end_time,start_time,units="secs"),2))

if(FALSE) {
  ct <- CrossTable(pred, tl, prop.chisq = FALSE, chisq = FALSE, 
                   prop.t = FALSE,prop.r=FALSE,prop.c=FALSE,
                   dnn = c("Predicted", "Actual"))
  print("The diagonal shows the 'hit'  = correct prediction")
  sum(diag(ct$t)) / sum(ct$t) # same as accuracy
  #0.7292225 #0.8243968
}

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml5b, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
# Naive-Bayes with laplace - some manual assumptions / trainings
V[2,c("t","max","acc","timing")] <- c("Naive-Bayes e1071 with laplace",0,0,0)
print(V[2,]$t)

# train model and check accuracy on test data (and measure time)
max_par <- max_acc <- 0
start_time <- Sys.time()   # for timing\
loop <- 0 # count in seq below for measure time
for (par in c(0.01,0.02,0.05,0.10,0.20,0.50,0.75,1.00)) {  # we do not need 0.00 - that is done above - do an 8-loop to have it comparable
  model <- naiveBayes(x, xl, laplace = par) # 0=0.6863 // 0.01=0.6970509 // 0.02=0.69437 // 0.1=0.655496 // 0.5=0.099)
  pred  <- as.factor(predict(model, t, type="class"))
  acc   <- confusionMatrix(pred,tl,dnn = c("Predicted", "Actual"),mode = "sens_spec")$overall[1] #$Accuracy
  print(sprintf("        parameter:%2.2f - accuracy: %02.3f%%",par, 100*acc))
  if(acc > max_acc) { 
    max_par <- par
    max_acc <- acc
  }
  loop <- loop + 1
}
end_time  <- Sys.time()     # for timing/
#warnings()
print(sprintf(  "   best parameter:%02.2f - accuracy: %02.3f%%", max_par,100*max_acc))
V[2,c("max","acc","timing")] <- c(max_par,round(100*max_acc,2),round(difftime(end_time,start_time,units="secs")/1,2))  # 8-loop - check total time, /1 not /loop

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml6, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
V[3,c("t","max","acc","timing")] <- c("Fast Naive-Bayes multinomial",0,0,0)
print(V[2,]$t)

# prepare train and test data
x  <- fci.dtm.train.freq # .freq  
xl <- fci.dtm.train.labels
t  <- fci.dtm.test.freq # .freq
tl <- fci.dtm.test.labels

# train model and check accuracy on test data (and measure time)
max_par <- max_acc <- 0
start_time <- Sys.time()   # for timing\
loop <- 0 # count in seq below for measure time
for (par in c(0.01,0.02,0.05,0.10,0.20,0.50,0.75,1.00)) {  # we do not need 0.00 - that is done above - do an 8-loop to have it comparable
  model <- fnb.multinomial(x, xl, priors = NULL, laplace = par, sparse = FALSE)#, check = TRUE)
  pred  <- suppressWarnings(predict(model, t))
  acc   <- confusionMatrix(pred, tl, dnn = c("Predicted", "Actual"),mode = "sens_spec")$overall[1]
  print(sprintf("        parameter:%02.2f - accuracy: %02.3f%%",par, 100*acc))
  if(acc > max_acc) { 
    max_par <- par
    max_acc <- acc
  }
  loop <- loop + 1
}
end_time <- Sys.time()     # for timing/
print(sprintf(  "   best parameter:%02.2f - accuracy: %02.3f%%", max_par,100*max_acc))
V[3,c("max","acc","timing")] <- c(max_par,round(100*max_acc,2),round(difftime(end_time,start_time,units="secs")/1,2))  # 8-loop - check total time, /1 not /loop

V[4,c("t","max","acc","timing")] <- c("Fast Naive-Bayes Bernoulli",0,0,0)
print(V[3,]$t)

# train model and check accuracy on test data (and measure time)
max_par <- max_acc <- 0
start_time <- Sys.time()   # for timing\
loop <- 0 # count in seq below for measure time
for (par in c(0.01,0.02,0.05,0.10,0.20,0.50,0.75,1.00)) {  # we do not need 0.00 - that is done above - do an 8-loop to have it comparable, take total time
  model <- fnb.bernoulli( x, xl,               laplace = par)
  pred  <- suppressWarnings(predict(model, t))
  acc <- confusionMatrix(pred, tl, dnn = c("Predicted", "Actual"),mode = "sens_spec")$overall[1]
  print(sprintf("        parameter:%02.2f - accuracy: %02.3f%%",par, 100*acc))
  if(acc > max_acc) { 
    max_par <- par
    max_acc <- acc
  }
  loop <- loop + 1
}
end_time <- Sys.time()     # for timing/
print(sprintf(  "   best parameter:%02.2f - accuracy: %02.3f%%", max_par,100*max_acc))
V[4,c("max","acc","timing")] <- c(max_par,round(100*max_acc,2),round(difftime(end_time,start_time,units="secs")/1,2)) # 8-loop - check total time, /1 not /loop

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml7, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
# Use the document term matrix. 
#?e1071::svm
V[5,c("t","max","acc","timing")] <- c("Support vector machines",0,0,0)
print(V[5,]$t)

# prepare train and test data
x  <- fci.dtm.train#.freq # using freq does not work - "test data does not match model"
xl <- fci.dtm.train.labels
t  <- fci.dtm.test#.freq # .freq
tl <- fci.dtm.test.labels

# train model and check accuracy on test data (and measure time)
max_par    <- max_acc <- 0
start_time <- Sys.time()   # for timing\
model        <- e1071::svm(x, xl, kernal = "linear")
pred         <- predict(model, t)
confusionMatrix(pred,tl,dnn = c("Predicted", "Actual"),mode = "sens_spec")
max_acc      <- confusionMatrix(pred,tl,dnn = c("Predicted", "Actual"),mode = "sens_spec")$overall[1]
end_time   <- Sys.time()   # for timing/

print(sprintf(  "   best parameter:         accuracy: %02.3f%%",          100*max_acc))

V[5,c("max","acc","timing")] <- c(max_par,round(100*max_acc,2),round(difftime(end_time,start_time,units="secs"),2))

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml8, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
V[6,c("t","max","acc","timing")] <- c("K nearest neighbor loop approach",0,0,0)
print(V[6,]$t)

# prepare train and test data
x  <- fci.dtm.train#.freq # using freq does not work - "test data does not match model"
xl <- fci.dtm.train.labels
t  <- fci.dtm.test#.freq # .freq
tl <- fci.dtm.test.labels

# train model and check accuracy on test data (and measure time)
max_par    <- max_acc <- 0
start_time <- Sys.time()   # for timing\
loop <- 0 # count in seq below for measure time
for (par in seq(1,15,2)) { # all other takes ages seq(1,15,4)) { # loop=4 * 240sec = 16min
  pred <- knn(x, t, xl, k=par)  # parameter = number of neighbors
  acc  <- confusionMatrix(pred, tl, dnn = c("Predicted", "Actual"),mode = "sens_spec")$overall[1]
  print(sprintf("neighbours:%02d - accuracy: %02.3f%%", par, 100*acc))
  if(acc > max_acc) { 
    max_par <- par
    max_acc <- acc
  }
  loop <- loop + 1
}
end_time <- Sys.time()     # for timing/
print(sprintf(  "   best parameter:%02.2f - accuracy: %02.3f%%", max_par,100*max_acc))
V[6,c("max","acc","timing")] <- c(max_par,round(100*max_acc,2),round(difftime(end_time,start_time,units="secs")/1,2))  # 8-loop - check total time, /1 not /loop

# 13 is the optimum
#[1] "neighbours:01 - accuracy: 78.150%"  #
#[1] "neighbours:03 - accuracy: 80.161%"
#[1] "neighbours:05 - accuracy: 81.099%"  #
#[1] "neighbours:07 - accuracy: 80.697%"
#[1] "neighbours:09 - accuracy: 80.563%"  #
#[1] "neighbours:11 - accuracy: 80.697%"
#[1] "neighbours:13 - accuracy: 81.367%"  #
#[1] "neighbours:15 - accuracy: 81.233%"

#---------------------------------------------------------------------------------------------------------------------------------\
## ----ml9, fig.width=6, fig.height=3, fig.align='center', fig.cap="knnFit-Results: optimal k value", tidy=F, echo=FALSE, warning=FALSE-------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/
V[7,c("t","max","acc","timing")] <- c("K nearest neighbor caret optimizer",0,0,0)
print(V[7,]$t)

set.seed(400)

# prepare train and test data and the algorithms data structure
# ?train
convert_counts <- function(x) {  x <- ifelse(x > 0, 1, 0) }
fci.train <- fci.dtm.train.freq %>% apply(MARGIN = 2, convert_counts)
fci.test  <- fci.dtm.test.freq  %>% apply(MARGIN = 2, convert_counts)

fci.train.1 <- cbind(cat=factor(fci.dtm.train.labels), fci.train)
fci.train.1 <- fci.train.1[,-c(2,3,4,5,6,7)]  # remove funny words where alg. fails
x  <- as.data.frame(fci.train.1)
xl <- fci.dtm.train.labels

# fit model and check accuracy on test data (and measure time)
start_time <- Sys.time()   # for timing\
knnFit <- train(x, xl, 
                method = "knn", 
                trControl = trainControl(method="cv"), 
                preProcess = c("center","scale"),tuneLength = 5)
end_time <- Sys.time()     # for timing/

knnFit #%>% knitr::kable(caption="\\label{tab:knnfit}knnFit-Results: optimal k value")

#  k  Accuracy   Kappa     
#  5  0.8360085  0.24460417
#  7  0.8397077  0.20628362
#  9  0.8407144  0.16819036
# 11  0.8383688  0.10909614
# 13  0.8350175  0.05937574
#
# Accuracy was used to select the optimal model using the largest value.
max_par <- knnFit$bestTune$k
max_acc <- max(knnFit$results$Accuracy)
plot(knnFit)

V[7,c("max","acc","timing")] <- c(max_par,round(100*max_acc,2),round(difftime(end_time,start_time,units="secs"),2))
colnames(V) <- c('Type of Machine learning','best parameter','Accuracy','timing(sec)')

############################################################################################################################
############################################################################################################################
# RESULT PART
############################################################################################################################
############################################################################################################################

#---------------------------------------------------------------------------------------------------------------------------------\
## ----sent_country3, fig.width=6, fig.height=6.5, fig.align='center', tidy=F, echo=FALSE, warning=FALSE, fig.cap="Sentiment types per country group"-----------------
#---------------------------------------------------------------------------------------------------------------------------------/

A <- theme(axis.text.x = element_text(angle = 0, size = 9, hjust=1), # A-xis text
           legend.position=c(0.99, 0.99)    , legend.justification = c("right","top"),
           legend.text=element_text(size=8) , legend.key.size = unit(0.3, 'cm'),
           plot.title = element_blank()) # element_text(size = 12))
B <- geom_boxplot()
C <- geom_jitter(width=0.03,alpha=0.3) # jitter throws warnings unfort.
L <- c("Sentiment type","count")
T <- c("Word counts of sentiment type per","Ordered left to right by count")

p1 <- ggplot(t5,aes(x=fct_reorder(sentiment,-n), y=n, fill=Region     )) + labs(x = element_blank(), y = L[2])
# p2 # check per subregion makes it very noisy
#p2 <- ggplot(tb,aes(x=fct_reorder(sentiment,-n), y=n, fill=SubRegion  ))+ labs(x = element_blank(), y = L[2])
p3 <- ggplot(t5,aes(x=fct_reorder(sentiment,-n), y=n, fill=IncomeLevel)) + labs(x = element_blank(), y = L[2])
p4 <- ggplot(t5,aes(x=fct_reorder(sentiment,-n), y=n, fill=FinRegion  )) + labs(x = L[1]           , y = L[2])

grid.arrange(p1+A+B,p3+A+B,p4+A+B,ncol=1)

rm(A,B,C,L,T,p1,p3,p4) 

#---------------------------------------------------------------------------------------------------------------------------------\
## ----results, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------/

V %>% knitr::kable()

