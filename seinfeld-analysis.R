# import libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(grid)
library(viridis)
library(lubridate)
library(igraph)
library(ggraph)
library(tm)
library(tidytext)
library(wordcloud)

# read data
episodes <- read.csv("episode_info.csv",sep=',',stringsAsFactors=F)
scripts <- read.csv("scripts.csv",sep=',',stringsAsFactors=F)


# data preparation
mymonths <- c("January","February","March","April","May","June","July","August","September","October","November","December")
episodes[episodes$X1==81,'AirDate'] <- 'April 28, 1994'
episodes$AirYear<-sapply(episodes$AirDate, function(x) as.numeric(trimws(strsplit(x,',')[[1]][2])))
episodes$AirDay<-sapply(episodes$AirDate, function(x) as.numeric(strsplit(strsplit(x,',')[[1]][1]," ")[[1]][2]))
episodes$AirMonthName<-sapply(episodes$AirDate, function(x) strsplit(strsplit(x,',')[[1]][1]," ")[[1]][1])
episodes$AirMonthNum<- sapply(episodes$AirDate, function(x) match(strsplit(strsplit(x,',')[[1]][1]," ")[[1]][1], mymonths))
episodes$AirDateTS<-as.Date(paste0(episodes$AirYear,'-',episodes$AirMonthNum,'-',episodes$AirDay), format="%Y-%m-%d")

episodes$weekdays<-weekdays(episodes$AirDateTS)
episodes$week<-week(episodes$AirDateTS)
episodes$weekdays <- factor(episodes$weekdays, levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")))

# when were eps broadcasted?
per_week <- episodes %>% select(AirYear, week, weekdays) %>% na.omit() %>% group_by(AirYear,week,weekdays) %>% summarize(week_count=n()) 

ggplot(per_week, aes(x = week, y = weekdays, fill=factor(week_count))) +
  scale_fill_manual(name='',values = viridis::viridis(2)) + 
  geom_tile(color = "white", size = 0.2) + facet_wrap("AirYear", ncol = 1) + 
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  theme_fivethirtyeight() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size=10),
        panel.grid.major.y= element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(.5, "cm"),
        strip.text = element_text(hjust = 0.01, face = "bold", size = 10)) + 
  ggtitle('# of episodes per week')


# top characters by season
top<-data.frame(scripts %>% filter(!grepl("^\\[|^\\(", Character)) %>% group_by(Season, Character) %>% summarise(count=n()) %>% arrange(-count) %>% top_n(20))
top$Char<-ifelse(top$Character %in% c('JERRY','GEORGE','ELAINE','KRAMER','NEWMAN','PUDDY','PETERMAN','ESTELLE','SUSAN','HELEN','MORTY','FRANK'),top$Character,'OTHER')

top %>% 
  ggplot(aes(x=factor(Season),y=count,fill=Char)) + 
  geom_histogram(stat='identity',color='white',size=.2) + theme_fivethirtyeight() +
  scale_fill_manual(name='',values=colorRampPalette(brewer.pal(11,"Paired"))(13)) +
  guides(fill=guide_legend(ncol=8)) + 
  labs(title='Number of lines per character and season')

data.frame(scripts %>% filter(SEID==unique(scripts$SEID)[1]) %>% select(Character, Dialogue) %>% slice(110))

# function to count the number of occurrences of x in a given pattern
# here x --> current dialogue line ; pattern --> vector of top characters
strcount <- function(x, pattern, split){
  unlist(lapply(
    strsplit(x, split),
    function(z) na.omit(length(grep(pattern, z)))
  ))
}

printEdges = FALSE
prepareNetwork <- function(currentEpisodeDF, topChars){
  currentEpisode<-currentEpisodeDF
  N<-topChars
  
  # select the top names in that current episode
  top<-currentEpisode %>% select(Character) %>% group_by(Character) %>% summarise(count=n()) %>% arrange(-count) %>% top_n(N) %>% as.vector
  totNames <- top$Character
  # remove whitespaces and regroup the unique values after
  totNames <- unique(trimws(totNames))
  
  # loop over all the top names
  
  # make a blank dataframe
  res<-data.frame(sourceName = character(), targetName = character(), value = integer(), frac = double())
  for (name in totNames){
    # select the lines related to the current character(i.e when he/she speaks)
    nameDF<-data.frame(currentEpisode %>% filter(Character==name) %>% select(Character, Dialogue,X) %>% select(Dialogue) %>% as.vector)
    
    cnt<-0
    sourceNameVec<-c()
    targetNameVec<-c()
    valueVec<-c()
    for (secondName in totNames){
      # loop over all rows
      curCount<-0
      for(line in 1:nrow(nameDF)){
        curCount<- curCount + strcount(tolower(nameDF$Dialogue[line]), tolower(secondName), " ")
      }
      if(curCount>0 & printEdges==TRUE){
        print(paste0(name,' --> ', secondName,' : ',curCount))
      }
      if(curCount>0){
        cnt<-cnt+1
        valueVec[cnt]<-curCount
        sourceNameVec[cnt]<-name
        targetNameVec[cnt]<-secondName
      }
      temp<-data.frame(sourceName=sourceNameVec, targetName=targetNameVec, value = valueVec)
      temp$frac<-temp$value / sum(valueVec)
    }
    res<-rbind(res,temp)
  }
  return(res)
}


#regexp is to remove lines for a character action, not a quote
topChars<-15
makeNetwork<-function(currentNetwork){
  currentNetwork$sourceName<-as.character(currentNetwork$sourceName)
  currentNetwork$targetName<-as.character(currentNetwork$targetName)
  currentNetwork <- currentNetwork %>% filter(!(sourceName == targetName)) %>% select(-frac)
  allNames<-data.frame(name=unique(c(unique(currentNetwork$sourceName), unique(currentNetwork$targetName))))
  curLinks<-data.frame(currentNetwork)
  return(graph_from_data_frame(curLinks,vertices = allNames))
}

mylist<-list()
for(i in 1:9){
  temp <- prepareNetwork(data.frame(scripts %>% dplyr::filter(Season==i) %>% filter(!grepl("^\\[|^\\(", Character))),topChars)
  mylist[[i]]<-makeNetwork(temp)
}

ggraph(mylist[[1]], layout = 'kk') + 
  geom_edge_arc(aes(color=value, width=value,
                    start_cap = label_rect(node1.name),
                    end_cap = label_rect(node2.name)), 
                arrow = arrow(length = unit(4, 'mm')),
                alpha=.5) + geom_node_text(aes(label = name)) + theme_fivethirtyeight()  + 
  theme(legend.position='None',
        panel.grid=element_blank(), 
        axis.text=element_blank()) + labs(title=paste0('Dialogues\'s network for Season ',1))

ggraph(mylist[[2]], layout = 'kk') + 
  geom_edge_arc(aes(color=value, width=value,
                    start_cap = label_rect(node1.name),
                    end_cap = label_rect(node2.name)), 
                arrow = arrow(length = unit(4, 'mm')),
                alpha=.5) + geom_node_text(aes(label = name)) + theme_fivethirtyeight()  + 
  theme(legend.position='None',
        panel.grid=element_blank(), 
        axis.text=element_blank()) + labs(title=paste0('Dialogues\'s network for Season ',2))

ggraph(mylist[[3]], layout = 'kk') + 
  geom_edge_arc(aes(color=value, width=value,
                    start_cap = label_rect(node1.name),
                    end_cap = label_rect(node2.name)), 
                arrow = arrow(length = unit(4, 'mm')),
                alpha=.5) + geom_node_text(aes(label = name)) + theme_fivethirtyeight()  + 
  theme(legend.position='None',
        panel.grid=element_blank(), 
        axis.text=element_blank()) + labs(title=paste0('Dialogues\'s network for Season ',3))

ggraph(mylist[[4]], layout = 'kk') + 
  geom_edge_arc(aes(color=value, width=value,
                    start_cap = label_rect(node1.name),
                    end_cap = label_rect(node2.name)), 
                arrow = arrow(length = unit(4, 'mm')),
                alpha=.5) + geom_node_text(aes(label = name)) + theme_fivethirtyeight()  + 
  theme(legend.position='None',
        panel.grid=element_blank(), 
        axis.text=element_blank()) + labs(title=paste0('Dialogues\'s network for Season ',4))

ggraph(mylist[[5]], layout = 'kk') + 
  geom_edge_arc(aes(color=value, width=value,
                    start_cap = label_rect(node1.name),
                    end_cap = label_rect(node2.name)), 
                arrow = arrow(length = unit(4, 'mm')),
                alpha=.5) + geom_node_text(aes(label = name)) + theme_fivethirtyeight()  + 
  theme(legend.position='None',
        panel.grid=element_blank(), 
        axis.text=element_blank()) + labs(title=paste0('Dialogues\'s network for Season ',5))

ggraph(mylist[[6]], layout = 'kk') + 
  geom_edge_arc(aes(color=value, width=value,
                    start_cap = label_rect(node1.name),
                    end_cap = label_rect(node2.name)), 
                arrow = arrow(length = unit(4, 'mm')),
                alpha=.5) + geom_node_text(aes(label = name)) + theme_fivethirtyeight()  + 
  theme(legend.position='None',
        panel.grid=element_blank(), 
        axis.text=element_blank()) + labs(title=paste0('Dialogues\'s network for Season ',6))

ggraph(mylist[[7]], layout = 'kk') + 
  geom_edge_arc(aes(color=value, width=value,
                    start_cap = label_rect(node1.name),
                    end_cap = label_rect(node2.name)), 
                arrow = arrow(length = unit(4, 'mm')),
                alpha=.5) + geom_node_text(aes(label = name)) + theme_fivethirtyeight()  + 
  theme(legend.position='None',
        panel.grid=element_blank(), 
        axis.text=element_blank()) + labs(title=paste0('Dialogues\'s network for Season ',7))

ggraph(mylist[[8]], layout = 'kk') + 
  geom_edge_arc(aes(color=value, width=value,
                    start_cap = label_rect(node1.name),
                    end_cap = label_rect(node2.name)), 
                arrow = arrow(length = unit(4, 'mm')),
                alpha=.5) + geom_node_text(aes(label = name)) + theme_fivethirtyeight()  + 
  theme(legend.position='None',
        panel.grid=element_blank(), 
        axis.text=element_blank()) + labs(title=paste0('Dialogues\'s network for Season ',8))

ggraph(mylist[[9]], layout = 'kk') + 
  geom_edge_arc(aes(color=value, width=value,
                    start_cap = label_rect(node1.name),
                    end_cap = label_rect(node2.name)), 
                arrow = arrow(length = unit(4, 'mm')),
                alpha=.5) + geom_node_text(aes(label = name)) + theme_fivethirtyeight()  + 
  theme(legend.position='None',
        panel.grid=element_blank(), 
        axis.text=element_blank()) + labs(title=paste0('Dialogues\'s network for Season ',9))

# what characters have said (over 9 years)
makeCorpus<-function(mydf){
  wordVec<-c()
  temp<-Corpus(VectorSource(mydf$Dialogue))
  for(i in 1:length(temp)){
    temp2 <- Corpus(VectorSource(temp[i]$content))
    temp_clean<-temp2 %>% 
      tm_map(content_transformer(tolower)) %>% 
      tm_map(content_transformer(removeNumbers)) %>% 
      tm_map(content_transformer(removePunctuation)) %>% 
      tm_map(content_transformer(removeWords),c(tidytext::stop_words$word,"dont","youre","ill","theyre","hes","didnt","shes","ive")) %>%
      tm_map(content_transformer(trimws)) %>% 
      tm_map(content_transformer(stripWhitespace))
    
    current<-sort(strsplit(as.character(temp_clean$content),' ')[[1]])
    wordVec<-append(wordVec,current,length(current)) 
  }
  return(wordVec)
}

# wordclouds
freqNames <-list()
cnt<-0
for(name in c('JERRY','GEORGE','ELAINE','KRAMER')){
  print(name)
  cnt<-cnt+1
  temp <- makeCorpus(scripts %>% dplyr::filter(Character==name) %>% dplyr::select(Dialogue))
  tdm <- TermDocumentMatrix(Corpus(VectorSource(temp)))
  m_tdm <-as.matrix(tdm)
  freqNames[[cnt]]<-sort(rowSums(m_tdm), decreasing=T)
}

set.seed(1234)
par(mfrow=c(1, 1),bg="gray90")
wordcloud(words=names(freqNames[[1]]),freq = freqNames[[1]],random.order=F,colors=viridis::viridis(150),scale=c(5,1),max.words=150)
title('Most frequent words by JERRY SEINFELD',col.main='black',cex.main=1.)