#install.packages("rjson")
#install.packages('jsonlite')
library("jsonlite")
#library("rjson")
library(dplyr)

setwd('/Users/binlin/Documents/DataScience/projects/web_scraping/online_news_popularity/data/')
articles = read.csv("./articles.csv", na.strings = "NA", stringsAsFactors = FALSE)
articles.reduced = select(articles, -content, -title, -topics, -link)


# top author
by_author = group_by(articles.reduced, author)
top.authors = arrange(summarise(by_author, count = n()), desc(count))
top.authors = head(top.authors, 10)
articles.top.authors = filter(articles.reduced, author %in% top.authors$author)

head(arrange(summarise(by_author, avg.shares = mean(shares)), desc(avg.shares)), 10)

top.authors.by.shares = arrange(summarise(by_author, avg.shares = mean(shares)), desc(avg.shares))
top.authors.by.shares = head(top.authors.by.shares, 10)
articles.top.authors.by.shares = filter(articles.reduced, author %in% top.authors.by.shares$author)

top.authors
top.authors.by.shares
articles.top.authors.by.shares

articles.top.authors.by.shares = filter(articles.reduced, author == 'Amanda Wills')

(11266+693+415)/3

mean(c(11266,500,415))

median(c(11266,500, 415))

?median

ggplot(articles.top.authors, aes(factor(author), shares, fill = factor(author))) +
  geom_boxplot() + geom_point() +  scale_y_log10() + xlab("Author") + ylab("log10(shares)")


ggplot(articles.top.authors.by.shares, aes(factor(author), shares, fill = factor(author))) +
  geom_boxplot(aes(middle = median(articles.top.authors.by.shares$shares)), stat = "identity") 



+ geom_point() +  xlab("Author") + ylab("log10(shares)")


?geom_boxplot

View(articles.top.authors)



articles.reduced$channel = factor(articles.reduced$channel, ordered = TRUE)

articles.reduced 

head(top.authors, 10)



summary(articles.reduced)

articles.filtered = articles.reduced[articles.reduced$type %in% c('new'), ]


articles.reduced$channel2 = factor(articles.reduced$channel, levels=names(sort(table(articles.reduced$channel), decreasing=TRUE)))



ggplot(articles.reduced, aes(channel2, fill=channel)) + geom_bar() 


articles.numeric = select(articles.reduced, -id, -author, -channel, -post_date, -shares, -type)

View(articles)


articles.scaled = as.data.frame(scale(articles.numeric))

articles.scaled$shares = articles.reduced$shares

plot_share = ggplot(articles, aes(y = shares))
plot_share = plot_share + geom_line(aes(x = timedelta, colour = "timedelta"))

plot_share

complete.cases(articles.scaled$timedelta)

View(articles.scaled)
       
all.topics =paste(articles$topics, collapse = ', ')

strsplit(all.topics, ', ')

topic.df = as.data.frame(strsplit(all.topics, ', '), stringsAsFactors = FALSE)

colnames(topic.df) = c('topic')

by_topic = group_by(topic.df,topic)

topic.counts = summarise(by_topic, total = n())

topic.counts = arrange(topic.counts, desc(total)) 

topic.counts

c('test', 'test2')

paste('2', paste(c('test', 'test2'), '1'))

removePunctuation(c('test,lsdkjflds.sdfksld', 'test2,....lkdfjd')
)

stopwords("SMART")

