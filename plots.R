
df<-arrange(unigram,Count,decreasing=TRUE)[10:1,]
df$f <- factor(df$String, levels=df$String)
plot1 <-ggplot(df, aes(x = f, y = Count)) +
        geom_bar(stat = "identity", color="gray55", fill="ivory1") +
        geom_text(data=df,aes(x=String,y=(Count*0.8),label=Count),vjust=0, size=3) +
        xlab("UniGrams") + ylab("Count") + ggtitle("Top 10 UniGrams Frequency") +
        theme(plot.title = element_text(lineheight=.8, face="bold")) +
        coord_flip()
plot1

df<-arrange(bigram,Count,decreasing=TRUE)[10:1,]
df$f <- factor(df$String, levels=df$String)
plot2 <-ggplot(df, aes(x = f, y = Count)) +
        geom_bar(stat = "identity", color="gray55", fill="palegreen1") +
        geom_text(data=df,aes(x=String,y=(Count*0.8),label=Count),vjust=0, size=3) +
        xlab("BiGrams") + ylab("Count") + ggtitle("Top 10 BiGrams Frequency") +
        theme(plot.title = element_text(lineheight=.8, face="bold")) +
        coord_flip()

df<-arrange(trigram[1:10,],Count,decreasing=TRUE)[10:1,]
df$f <- factor(df$String, levels=df$String)
plot3 <-ggplot(df, aes(x = f, y = Count)) +
        geom_bar(stat = "identity", color="gray55", fill="steelblue2") +
        geom_text(data=df,aes(x=String,y=(Count*0.8),label=Count),vjust=0, size=3) +
        xlab("TriGrams") + ylab("Count") + ggtitle("Top 10 TriGrams Frequency") +
        theme(plot.title = element_text(lineheight=.8, face="bold")) +
        coord_flip()

df<-arrange(quadgram[1:10,],Count,decreasing=TRUE)[10:1,]
df$f <- factor(df$String, levels=df$String)
plot4 <-ggplot(df, aes(x = f, y = Count)) +
        geom_bar(stat = "identity", color="gray55", fill="khaki2") +
        geom_text(data=df,aes(x=String,y=(Count*0.8),label=Count),vjust=0, size=3) +
        xlab("QuadGrams") + ylab("Count") + ggtitle("Top 10 QuadGrams Frequency") +
        theme(plot.title = element_text(lineheight=.8, face="bold")) +
        coord_flip()

wordcloud(words = unigram$String,
          freq = unigram$Count, 
          scale=c(5,0.5), max.words=100, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8,"Dark2"))
wordcloud(words = bigram$String,
          freq = bigram$Count, 
          scale=c(5,0.5), max.words=100, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8,"Dark2"))
wordcloud(words = trigram$String,
          freq = trigram$Count, 
          scale=c(5,0.5), max.words=100, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8,"Dark2"))
wordcloud(words = quadgram$String,
          freq = quadgram$Count, 
          scale=c(5,0.5), max.words=100, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8,"Dark2"))
