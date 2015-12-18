library(tm) 
# tutorial from https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
# full documentation: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf


# create file path to corpus consisting of txt files saved individually, view files 
cname <- "C:/Users/sdevine/Desktop/texts"
dir(cname)   
list.files(cname)

# create the corpus
docs <- Corpus(DirSource(cname))
docs
summary(docs)   

# inspect docs
inspect(docs[1])

# the inspect command combines these two commands
docs[[1]]
docs[1]

# to view individual document
as.character(docs[1]) # this has some meta data mixed in
as.character(docs[[1]]) # this is cleaner version of text
writeLines(as.character(docs[[1]])) # this is even cleaner

# preprocess corpus to clean it up
# tm_map just applies a function to all the texts in the corpus - in this case the removePunctuation function
docs <- tm_map(docs, removePunctuation) 
# test to confirm punctuation is removed - it worked except for apostrophes
writeLines(as.character(docs[[3]]))

# can remove other custom punctuation
for(j in seq(docs))   
{   
        docs[[j]] <- gsub("/", " ", docs[[j]])   
        docs[[j]] <- gsub("@", " ", docs[[j]])   
        docs[[j]] <- gsub("\\|", " ", docs[[j]])   
} 

# remove numbers
docs <- tm_map(docs, removeNumbers)   
# test to confirm - it worked
writeLines(as.character(docs[[3]]))

# convert to lowercase
docs <- tm_map(docs, tolower) 
# test to confirm - it worked
writeLines(as.character(docs[[3]]))

# remove "stopwords" that usually have no analytic value
# For a list of the stopwords, see:   
length(stopwords("english"))   
stopwords("english")   
docs <- tm_map(docs, removeWords, stopwords("english"))
# test to confirm - it worked
writeLines(as.character(docs[[3]]))

# remove particular words of your choosing
# note that you've already processed the original text, so exclude punctuation, use lowercase, etc
docs <- tm_map(docs, removeWords, c("selfdoubt", "baseball")) 
# test to confirm - it worked
writeLines(as.character(docs[[3]]))

# combining words that should stay together
for (j in seq(docs))
{
        docs[[j]] <- gsub("export industries", "export_term", docs[[j]])
        docs[[j]] <- gsub("net exports", "export_term", docs[[j]])
}
# test to confirm - it worked
writeLines(as.character(docs[[3]]))

# remove common word endings aka stemming; (eg "ing", "es", "s")
library(SnowballC)   
docs <- tm_map(docs, stemDocument)   
# test to confirm - it worked
writeLines(as.character(docs[[3]]))

# strip unnecessary whitespace produced by processing
docs <- tm_map(docs, stripWhitespace) 
# test to confirm - it worked
writeLines(as.character(docs[[3]]))

# once finished pre-processing, run this to tell R to treat processed docs as text docs
docs <- tm_map(docs, PlainTextDocument)   




# create a document-term matrix
dtm <- DocumentTermMatrix(docs)   
dtm   

# inspect dtm
inspect(dtm)
# inspect only a subset of dtm, first two docs and first 20 terms only
inspect(dtm[1:2, 1:20])

# create a document-term matrix
tdm <- TermDocumentMatrix(docs)   
tdm   

# inspect tdm
inspect(tdm)
inspect(tdm[1:5, 1:2])

# explore data
freq <- colSums(as.matrix(dtm))   
length(freq) 
freq
names(freq)
freq[1:10]
# create ordered list of index positions of the words in freq
ord <- order(freq) 
# so most frequest words are
freq[tail(ord)]

# remove sparse terms from dtm
# This makes a matrix that is 10% empty space, maximum. 
dtms <- removeSparseTerms(dtm, 0.7)   
inspect(dtms)
# dtms <- removeSparseTerms(dtm, 0.1)  
# inspect(dtms)

# word frequency
freq[head(ord)]   
freq[tail(ord)]   

# frequency of frequencies
head(table(freq), 20) 
# the top number of output is the frequency that words appear
# the bottom number is the number of words appearing that frequently

# look at frequnecies of just the non-sparse terms
freq2 <- colSums(as.matrix(dtms))   
freq2 
freq2 <- sort(freq2, decreasing = TRUE)

# alternative way to find frequent terms
findFreqTerms(dtm, lowfreq=5)
findFreqTerms(dtm, lowfreq=10)

# another alternative involving dataframe
freq3 <- data.frame(term_var = names(freq), freq_var = freq)
head(freq3)

# plot word frequencies
library(ggplot2)   
p <- ggplot(subset(freq3, freq > 5), aes(term_var, freq_var))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x = element_text(angle=45, hjust=1))   
p   

# find term correlations for specific terms
findAssocs(dtm, c("people" , "think"), corlimit=0.95)
findAssocs(dtm, c("people" , "think"), corlimit=0.8)

# word clouds
library(wordcloud) 
set.seed(142)   
wordcloud(names(freq), freq, min.freq=7)
wordcloud(names(freq), freq, min.freq=5)

set.seed(142)   
wordcloud(names(freq), freq, max.words=100)

# add color to word cloud
set.seed(142)   
wordcloud(names(freq), freq, min.freq=5, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))



# clustering by term similarity
inspect(dtms)
dtms

# make a very sparse dtm for clarity 
dtmss <- removeSparseTerms(dtm, 0.5) # This makes a matrix that is only 50% empty space, maximum.   
inspect(dtmss)  
dtmss

library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d = d, method = "ward")   
fit   
plot(fit, hang=-1) 

# to get boxes around clusters
term_clusters <- cutree(fit, k=5)   # this tells you what cluster each term is in    

# this will overlay rectangular boxes on clusters, but you need to have plot already displayed in viewing pane before running
rect.hclust(fit, k=4, border="red") # draw dendogram with rectangular red borders around the 5 clusters 


# k-means clustering
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   




