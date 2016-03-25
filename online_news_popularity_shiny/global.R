library(dplyr)
library(tm)
library(wordcloud)
source("helper.R")


# load data
articles = read.csv("./data/articles.csv", na.strings = "NA", stringsAsFactors = FALSE)

articles.reduced = select(articles, -content, -title, -topics, -link)

# text for word clound (topics, keywords, content)
all.topics.text = paste(articles$topics, collapse = ', ')
all.titles.text = paste(removePunctuation(articles$title), collapse = ', ')
#all.content.text = paste(articles$content, collapse = ', ')

# split topics into list and load into data frame and get counts
#topic.df = as.data.frame(strsplit(all.topics.text, ', '), stringsAsFactors = FALSE)
#colnames(topic.df) = c('topic')
# by_topic = group_by(topic.df,topic)
# topic.counts = summarise(by_topic, total = n())
# topic.counts = head(arrange(topic.counts, desc(total)), 20)


# split titles into list and load into data frame and get counts
title.word.df = as.data.frame(strsplit(removePunctuation(all.titles.text), ' '), stringsAsFactors = FALSE)
colnames(title.word.df) = c('title')
by_title_word = group_by(title.word.df, title)
title.word.counts = summarise(by_title_word, total = n())
title.word.counts = head(arrange(title.word.counts, desc(total)), 20)

# split content into list and load into data frame and get counts
# content.word.df = as.data.frame(strsplit(all.content.text, ', '), stringsAsFactors = FALSE)
# colnames(content.word.df) = c('title')
# by_content_word = group_by(title.word.df, title)
# content.word.counts = summarise(by_content_word, total = n())
# content.word.counts = head(arrange(content.word.counts, desc(total)), 20)

# creaet a data frame for authors and counts
by_author = group_by(articles.reduced, author)
top.authors = arrange(summarise(by_author, count = n()), desc(count))
top.authors = head(top.authors, 10)
articles.top.authors = filter(articles.reduced, author %in% top.authors$author)
# top.authors.by.shares = arrange(summarise(by_author, avg.shares = mean(shares)), desc(avg.shares))
# top.authors.by.shares = head(top.authors.by.shares, 10)
# articles.top.authors.by.shares = filter(articles.reduced, author %in% top.authors.by.shares$author)


#arrange(summarise(by_author, count = n()), desc(count))

# create a data frame based on the scaled variables (except 'shares')
articles.numeric = select(articles.reduced, -id, -author, -channel, -post_date,-type)
#articles.scaled = as.data.frame(scale(select(articles.numeric, -shares))) 
#articles.scaled = as.data.frame(normalize(select(articles.numeric, -shares, -contains("sentiment"), -contains("subjectivity")))) 
#articles.scaled$shares = articles.reduced$shares

#articles.scaled = cbind(articles.scaled, select(articles.reduced, shares, contains("sentiment"), contains("subjectivity")))



