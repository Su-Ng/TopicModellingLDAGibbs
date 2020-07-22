#Text Analytics Workshop - Topic Modeling 

setwd("D:/hover/PCP-TA/Day2/Day2_R_Workshop")
#install.packages("topicmodels")

library(topicmodels)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)  

my_stopwords <- c(stopwords("english"), "will", "also", "etc", "else", "can", "even", "within", "without", 
                  "well", "say", "year", "must", "need", "never", "now", "want", "still", 
                  "time", "therefore", "send", "today", "may", "many", "make", "whose",
                  "however", "get", "have", "just", "him")

#======create document term matrix=====
#read in file
textdata <- read.delim("MovieStories.utf8.txt", header=TRUE, sep="\t", quote = "", stringsAsFactors = FALSE)


#get the movie stories
text <- textdata[, 3]
atext <- textdata[1, 3]

#help(sapply)
#a little data exploration - how long are the stories?
doclen <- sapply(text, function(x) length(strsplit(x, " ")[[1]]))
str(doclen)
table(doclen)
hist(doclen)

#For MacOS
#text<-iconv(text,"UTF-8") 

#create dtm using TF indexing
corpus <- VCorpus(VectorSource(text))

dtm <- DocumentTermMatrix(corpus, control = list (removeNumbers = TRUE,
                                                  tolower = TRUE,
                                                  stopwords = my_stopwords,
                                                  removePunctuation = TRUE,
                                                  stemming = TRUE))

dtm

#word cloud to check roughly what's in the data
tf <-sort(colSums(as.matrix(dtm)), decreasing=TRUE)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(tf), tf, max.words=20, scale=c(2, .04), colors=dark2)

#====topic modeling=========
#LDA is a Bayesian mixture model. 
#Two estimation methods are available for LDA: VEM and Gibbs.
#let's use Gibbs method.
lda_5_g <- LDA(dtm, 5, method="Gibbs")

#---What are in the topics--------
#Look at the most frequent terms for each topic
terms(lda_5_g, 10)

#Function logLik() gives us the log-likelihood of the model, 
#which is the sum over the log-likelihoods of all documents,
#maximized during maximum likelihood estimation of the model
logLik(lda_5_g)

#use "@terms" to find out the terms in columns
#beta, logarithmized parameters of the word distribution for topic N
lda_5_g@terms[1:10]
lda_5_g@beta[3, 1:10]

#We can produce wordcloud for more intuitive visualisation of the topics.

#Let's create a helper function which takes in a topic model and the index 
#of a topic and generates a wordcloud for this topic.

#- get the matrix of probabilities of words over topics - the beta
#- name the columns of the matrix with the corresponding terms
#- get the ith topic (a vector of word probabilities) and sort them in decreasing order
#- display the top 20 most frequent words in wordcloud
showcloud = function (m, i) {
  tt <- m@beta
  colnames(tt) <- m@terms
  top <- sort(tt[i, ], decreasing = TRUE)
  wordcloud(names(top[1:20]), 2^top[1:20],scale=c(2, .04),rot.per=0.3, colors=dark2)
}

showcloud(lda_5_g, 4) #show cloud for topic_1

#Now how do we know which document belongs to which topic?
#Let us get the 3 most likely topics for the first ten documents. 
t(topics(lda_5_g, 3))[1:10,]
#which topic has largest number of documents?
which.max(tabulate(topics(lda_5_g)))
tabulate(topics(lda_5_g))

#gamma, posterior topic distribution for each document, gives the actual probabilities
#let's look at the probabilities of the first document belonging to each of the topics
lda_5_g@gamma[1,]
barplot(lda_5_g@gamma[1,], names.arg=1:5, main="Topic distribution of Story 1")

######################################################
#We may also build topic models using a matrix with its sparsity reduced
dtm_slim <- removeSparseTerms(dtm, 0.998)
dtm
dtm_slim
lda_5_g_s <- LDA(dtm_slim, 5, method="Gibbs")
#Are you still getting the same topics?
terms(lda_5_g_s, 10)

#However, with more sparse terms removed(e.g. at 0.995), some documents with fewer words may end
#up having no non-zero values in the row, causing LDA() unable to run
dtm_skinny <- removeSparseTerms(dtm, 0.995)
dtm_skinny
LDA(dtm_skinny, 5, method="Gibbs")

#Error might be observed 
  #Error in LDA(dtm_skinny, 5, method = "Gibbs") : 
  #Each row of the input matrix needs to contain at least one non-zero entry
#Therefore, we need to remove those 'empty' rows from the matrix first
#This is what you can do:

#- Find the total count of words in each Document
rowTotals <- apply(dtm_skinny, 1, sum) 
#- remove all docs with 0 words due to RemoveSparse
dtm_skinny   <- dtm_skinny[rowTotals> 0, ] 
dtm_skinny
#Now try 
#1. build the topic models again using Gibbs method. This should be much faster than just now.
#Since it's much faster, let's try getting more topics. 
lda_5_g_sk <- LDA(dtm_skinny, 5, method="Gibbs")
lda_7_g_sk <- LDA(dtm_skinny, 7, method="Gibbs")
lda_10_g_sk <- LDA(dtm_skinny, 10, method="Gibbs")

#2. check out the topics. Do they still make sense? 
terms(lda_5_g_sk, 10)
terms(lda_7_g_sk, 10)
terms(lda_10_g_sk, 10)

barplot(lda_10_g_sk@gamma[1,], names.arg=1:10, main="Topic distribution of Story 1")
barplot(lda_5_g_sk@gamma[1,], names.arg=1:5, main="Topic distribution of Story 1")
barplot(lda_7_g_sk@gamma[1,], names.arg=1:7, main="Topic distribution of Story 1")
text[1]



