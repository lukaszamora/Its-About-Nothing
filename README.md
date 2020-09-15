# It's About Nothing!

![](/images/800px-Seinfeld_logo.svg.png)

## Introduction

Not sure if it needs an intro but in any case:

> Seinfeld is an American television sitcom that ran from 1989 to 1998. It was created by Larry David and Jerry Seinfeld. Set predominantly in an apartment building in Manhattan’s Upper West Side in New York City, the show features a handful of Jerry’s friends and acquaintances, particularly best friend George Costanza (Jason Alexander), former girlfriend Elaine Benes (Julia Louis-Dreyfus), and neighbor across the hall Cosmo Kramer (Michael Richards). It is often described as being a show about nothing, as many of its episodes are about the minutiae of daily life.

The dataset I'm using includes 2 files:

* `scripts.csv`: contains the full script for each episode, line by line. Thus, 1 line of the dataframe has the `Character` diaglouge, and the `Dialogue` itself.
* `episode_info.csv`: has the details of each 174 epides, including the `Date` it was released, the `Title`, writers, etc.

The goal of this project is to explore the data for the details of each episode. As well as making a network using the dialogue.

## Data Preparation

```
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
```
I need to make a `Date` field in the `episodes` dataframe that contains variables such as `Year` and `Month` from the `AirDate` field. I also noticed that there is only one row where `AirDate` was written incorrectly.

```
mymonths<-c("January","February","March","April","May","June","July","August","September","October","November","December")
episodes[episodes$X==81,'AirDate']<-'April 28, 1994'
episodes$AirYear<-sapply(episodes$AirDate, function(x) as.numeric(trimws(strsplit(x,',')[[1]][2])))
episodes$AirDay<-sapply(episodes$AirDate, function(x) as.numeric(strsplit(strsplit(x,',')[[1]][1]," ")[[1]][2]))
episodes$AirMonthName<-sapply(episodes$AirDate, function(x) strsplit(strsplit(x,',')[[1]][1]," ")[[1]][1])
episodes$AirMonthNum<- sapply(episodes$AirDate, function(x) match(strsplit(strsplit(x,',')[[1]][1]," ")[[1]][1], mymonths))
episodes$AirDateTS<-as.Date(paste0(episodes$AirYear,'-',episodes$AirMonthNum,'-',episodes$AirDay), format="%Y-%m-%d")

episodes$weekdays<-weekdays(episodes$AirDateTS)
episodes$week<-week(episodes$AirDateTS)
episodes$weekdays <- factor(episodes$weekdays, levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")))
```

## When were Episodes Broadcasted?

```
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
```

![](/images/num_eps.png)

*Seinfeld* started airing in 1989 with the initial pilot episode. We can also see that June-September was the crews off-season. It's also interesting to note that, except for the pilot episode and the first season, the show broadcasted on Wednesdays and Thursdays, with only a few instances where two episodes aired on the same day.

## Characters

If you're not familiar with the cast of characters from *Seinfeld*, here's a helpful guide (from [wikipedia](https://en.wikipedia.org/wiki/List_of_Seinfeld_characters):

1. Main Characters
  * Jerry Seinfeld: Jerry is a "minor celeb" stand-up comedian, living in New York
  * George Costanza: Jerry's best friend, definition of loser
  * Elaine Benes: Jerry's ex-girlfriend and later friend
  * Cosmo Kramer: Jerry's *wacky* neighbor
2. Family
  * Morty Seinfeld: Jerry's father, has strong convictions about business and the way of the world.
  * Helen Seinfeld: Jerry's mother, often overprotective of Jerry
  * Uncle Leo: Jerry's Uncle, old coot
  * Nana: Jerry's grandmother and mother of Helen and Leo
  * Frank Costanza: George's father, quick to anger, invented the holiday Festivus
  * Estelle Costanza: George's mother, highly obnoxious, constantly squabbles with Frank.
3. Misc
  * J Peterman: Elaine's boss, eccentric adventurer, loves the film *The English Patient*
  * George Steinbrenner: George's boss, owner of the New York Yankees.
  * David Puddy: Elaine's on-again-off-again boyfriend
  * Jackie Chiles: Kramer's eccentric, yet highly efficient, lawyer; has bad luck when representing Kramer
  
  
## Overview of Top Characters by Season

```
top<-data.frame(scripts %>% filter(!grepl("^\\[|^\\(", Character)) %>% group_by(Season, Character) %>% summarise(count=n()) %>% arrange(-count) %>% top_n(20))
top$Char<-ifelse(top$Character %in% c('JERRY','GEORGE','ELAINE','KRAMER','NEWMAN','PUDDY','PETERMAN','ESTELLE','SUSAN','HELEN','MORTY','FRANK'),top$Character,'OTHER')

top %>% 
  ggplot(aes(x=factor(Season),y=count,fill=Char)) + 
  geom_histogram(stat='identity',color='white',size=.2) + theme_fivethirtyeight() +
  scale_fill_manual(name='',values=colorRampPalette(brewer.pal(11,"Paired"))(13)) +
  guides(fill=guide_legend(ncol=8)) + 
  labs(title='Number of lines per character and season')
```

![](/images/num_lines_per_character.png)

We notice an increase of lines from season 1,2 to the rest (indirectly because of more episodes).

## Networks

The idea of making a network comes from the fact that there is a lot of communicating between different characters in *Seinfeld*. Most episodes focus around each of the main characters interacting with side characters and then coming back together towards the end of the episode.

The way I built the network is as follows:
* Loop over each episode
* Find the N top characters by grouping the `Character` feature
* Loop over the top characters, make a dataframe for this character only including the `Dialogue` features
* Find the other names of the top caracters in the `Dialogue` feature and count them

Example:

```
data.frame(scripts %>% filter(SEID==unique(scripts$SEID)[1]) %>% select(Character, Dialogue) %>% slice(110))
```

```
##   Character               Dialogue
## 1     JERRY Hello...Oh, hi, Laura.
```

We can see that Jerry is taking to Laura. Note that I am missing a lot of occurrences since `Dialouge` does not always have the name to the other person.

### Main Function

```
# function to count the number of occurences of x in a given pattern
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
```

### Making nodes/links

Steps to creating nodes in the network:
* Convert factor to character for removing rows having same `sourceName` and `targetName`
* Find unique names needed to define the nodes

```
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
```
### Loop over Seasons

```
mylist<-list()
for(i in 1:9){
  temp <- prepareNetwork(data.frame(scripts %>% dplyr::filter(Season==i) %>% filter(!grepl("^\\[|^\\(", Character))),topChars)
  mylist[[i]]<-makeNetwork(temp)
}
```

## Network Visualization

![](/images/network_s1.png)
![](/images/network_s2.png)
![](/images/network_s3.png)
![](/images/network_s4.png)
![](/images/network_s5.png)
![](/images/network_s6.png)
![](/images/network_s7.png)
![](/images/network_s8.png)
![](/images/network_s9.png)

We often see large connections between the main characters: Jerry, George, Elaine, and Kramer. The main characters are often linked to their own parents (e.g. season 7 for `Jerry -> Helen,Morty`). We also see the relationship with co-workers: `George -> Steinbrenner`, `Elaine -> Peterman`.

## What the main characters have said (over 9 years)

```
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
```

```
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
```

```
## [1] "JERRY"
## [1] "GEORGE"
## [1] "ELAINE"
## [1] "KRAMER"
```

### Wordclouds

![](/images/wordcloud_jerry.png)
![](/images/wordcloud_george.png)
![](/images/wordcloud_elaine.png)
![](/images/wordcloud_kramer.png)

The words `Yeah`, `Hey` are the most common words spoken by the main characters. We can also observe that each character references the other main characters. 

## Conclusion

Seinfeld is a great show that portrays the minutae of life in 1990s New York. This text analysis was a great way to dip my feet into `R`. 
