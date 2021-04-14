# watch out! at the end remove palyers with missing values because they might be lend or some shit
# ranking is removed from the list to simplify
#data managing  (list, clean, reduce) -> check thatd df.complete.fanta nrow correspond to data_fata (i.e. same numbers of players)
#df.complete.serie.a$PLAYER <- sub(".*? (.+)", "\\1", df.complete.serie.a$PLAYER) #remove players name
#for future versions would it be better to scrap also the names otherwise is a huge mess (sometimes they change the name on the website remember babacar)
#remember that until each player doesn't play at least 1 game they will be not listed in the seria website, meaning that the column team will not converge while merging dataframes
#!!!correct dataset removing goals, assists and cards from initial games not played [check this every year to correct the df]
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#LIBRARIES
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rvest)
library(XML)
library(dplyr)
library(tidyverse) 
library(stringr)  
library(rebus)   
library(lubridate)
library(stringr)
library(utils)
library(base)
library(plyr)
library(data.table)
library(ggplot2)
library(Rmisc)
library(ggforce)
library(cowplot)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#http://www.legaseriea.it
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#top scores     
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

url.top.scores <- 'http://www.legaseriea.it/en/serie-a/statistics/Gol'#Specifying the url for desired website to be scraped

top.scores.table <- read_html(url.top.scores)#Reading the HTML code from the website

top.scores.html <- html_nodes(top.scores.table,'td')#Using CSS selectors to scrape the rankings section

top.scores <- html_text(top.scores.html)#Converting the ranking data to text

head(top.scores)#Let's have a look at the rankings

top.scores <- str_replace_all(top.scores, "[\r\n]" , "")#remove all the tags

#transform the values in a dataframe
top.scores <- as.data.frame(top.scores)
top.scores <- data.frame(
              top.scores,
              ind = rep(1:6, nrow(top.scores))) # create a repeated index
top.scores <-unstack(top.scores, top.scores~ind)

names(top.scores) <- c("RANKING", "TEAM", "PLAYER", "GOALS", "PLAYED", "PENALTIES")#name headers

top.scores$TEAM <- gsub('\\s+', '', top.scores$TEAM)# remove unnecessary space from the beginning

top.scores$PLAYER <- gsub(" $","", top.scores$PLAYER, perl=T)# remove unnecessary space from at the end

top.scores

#clean environment
rm(top.scores.html,top.scores.table)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#top asssist   
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

url.top.assist <- 'http://www.legaseriea.it/en/serie-a/statistics/NAssistVin'#Specifying the url for desired website to be scraped

top.assist.table <- read_html(url.top.assist)#Reading the HTML code from the website

top.assist.html <- html_nodes(top.assist.table,'td')#Using CSS selectors to scrape the rankings section

top.assist <- html_text(top.assist.html)#Converting the ranking data to text

head(top.assist)#Let's have a look at the rankings

top.assist <- str_replace_all(top.assist, "[\r\n]" , "")#remove all the tags

#transform the values in a dataframe
top.assist <- as.data.frame(top.assist)
top.assist <- data.frame(
  top.assist,
  ind = rep(1:4, nrow(top.assist))) # create a repeated index
top.assist <-unstack(top.assist, top.assist~ind)

names(top.assist) <- c("RANKING", "TEAM", "PLAYER", "ASSIST")#name headers

top.assist$TEAM <- gsub('\\s+', '', top.assist$TEAM)# remove unnecessary space from the beginning

top.assist$PLAYER <- gsub(" $","", top.assist$PLAYER, perl=T)# remove unnecessary space from at the end

top.assist

#clean environment
rm(top.assist.html,top.assist.table)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#shot  
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

url.shot <- 'http://www.legaseriea.it/en/serie-a/statistics/NTir'#Specifying the url for desired website to be scraped

shot.table <- read_html(url.shot)#Reading the HTML code from the website

shot.html <- html_nodes(shot.table,'td')#Using CSS selectors to scrape the rankings section

shot <- html_text(shot.html)#Converting the ranking data to text

head(shot)#Let's have a look at the rankings

shot <- str_replace_all(shot, "[\r\n]" , "")#remove all the tags

#transform the values in a dataframe
shot <- as.data.frame(shot)
shot <- data.frame(
  shot,
  ind = rep(1:6, nrow(shot))) # create a repeated index
shot <-unstack(shot, shot~ind)

names(shot) <- c("RANKING", "TEAM", "PLAYER", "TOTAL.SHOT", "ON.TARGET", "OFF.TARGET")#name headers

shot$TEAM <- gsub('\\s+', '', shot$TEAM)# remove unnecessary space from the beginning

shot$PLAYER <- gsub(" $","", shot$PLAYER, perl=T)# remove unnecessary space from at the end

shot

#clean environment
rm(shot.html,shot.table)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#key passes   
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

url.key.passes <- 'http://www.legaseriea.it/en/serie-a/statistics/PassChiave'#Specifying the url for desired website to be scraped

key.passes.table <- read_html(url.key.passes)#Reading the HTML code from the website

key.passes.html <- html_nodes(key.passes.table,'td')#Using CSS selectors to scrape the rankings section

key.passes <- html_text(key.passes.html)#Converting the ranking data to text

head(key.passes)#Let's have a look at the rankings

key.passes <- str_replace_all(key.passes, "[\r\n]" , "")#remove all the tags

#transform the values in a dataframe
key.passes <- as.data.frame(key.passes)
key.passes <- data.frame(
  key.passes,
  ind = rep(1:4, nrow(key.passes))) # create a repeated index
key.passes <-unstack(key.passes, key.passes~ind)

names(key.passes) <- c("RANKING", "TEAM", "PLAYER", "KEY.PASSES")#name headers

key.passes$TEAM <- gsub('\\s+', '', key.passes$TEAM)# remove unnecessary space from the beginning

key.passes$PLAYER <- gsub(" $","", key.passes$PLAYER, perl=T)# remove unnecessary space from at the end

key.passes

#clean environment
rm(key.passes.html,key.passes.table)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#recoveries
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

url.recoveries <- 'http://www.legaseriea.it/en/serie-a/statistics/Recuperi'#Specifying the url for desired website to be scraped

recoveries.table <- read_html(url.recoveries)#Reading the HTML code from the website

recoveries.html <- html_nodes(recoveries.table,'td')#Using CSS selectors to scrape the rankings section

recoveries <- html_text(recoveries.html)#Converting the ranking data to text

head(recoveries)#Let's have a look at the rankings

recoveries <- str_replace_all(recoveries, "[\r\n]" , "")#remove all the tags

#transform the values in a dataframe
recoveries <- as.data.frame(recoveries)
recoveries <- data.frame(
  recoveries,
  ind = rep(1:4, nrow(recoveries))) # create a repeated index
recoveries <-unstack(recoveries, recoveries~ind)

names(recoveries) <- c("RANKING", "TEAM", "PLAYER", "RECOVERIES")#name headers

recoveries$TEAM <- gsub('\\s+', '', recoveries$TEAM)# remove unnecessary space from the beginning

recoveries$PLAYER <- gsub(" $","", recoveries$PLAYER, perl=T)# remove unnecessary space from at the end

recoveries

#clean environment
rm(recoveries.html,recoveries.table)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#km
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

url.km <- 'http://www.legaseriea.it/en/serie-a/statistics/MediaKm'#Specifying the url for desired website to be scraped

km.table <- read_html(url.km)#Reading the HTML code from the website

km.html <- html_nodes(km.table,'td')#Using CSS selectors to scrape the rankings section

km <- html_text(km.html)#Converting the ranking data to text

head(km)#Let's have a look at the rankings

km <- str_replace_all(km, "[\r\n]" , "")#remove all the tags

#transform the values in a dataframe
km <- as.data.frame(km)
km <- data.frame(
  km,
  ind = rep(1:5, nrow(km))) # create a repeated index
km <-unstack(km, km~ind)

names(km) <- c("RANKING", "TEAM", "PLAYER", "AVERAGE.km", "MINUTES")#name headers

km$TEAM <- gsub('\\s+', '', km$TEAM)# remove unnecessary space from the beginning

km$PLAYER <- gsub(" $","", km$PLAYER, perl=T)# remove unnecessary space from at the end

km

#clean environment
rm(km.html,km.table)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#fouls against 
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

url.fouls.against <- 'http://www.legaseriea.it/en/serie-a/statistics/NFalSubiti'#Specifying the url for desired website to be scraped

fouls.against.table <- read_html(url.fouls.against)#Reading the HTML code from the website

fouls.against.html <- html_nodes(fouls.against.table,'td')#Using CSS selectors to scrape the rankings section

fouls.against <- html_text(fouls.against.html)#Converting the ranking data to text

head(fouls.against)#Let's have a look at the rankings

fouls.against <- str_replace_all(fouls.against, "[\r\n]" , "")#remove all the tags

#transform the values in a dataframe
fouls.against <- as.data.frame(fouls.against)
fouls.against <- data.frame(
  fouls.against,
  ind = rep(1:4, nrow(fouls.against))) # create a repeated index
fouls.against <-unstack(fouls.against, fouls.against~ind)

names(fouls.against) <- c("RANKING", "TEAM", "PLAYER", "FOULS.AGAINST")#name headers

fouls.against$TEAM <- gsub('\\s+', '', fouls.against$TEAM)# remove unnecessary space from the beginning

fouls.against$PLAYER <- gsub(" $","", fouls.against$PLAYER, perl=T)# remove unnecessary space from at the end

fouls.against

#clean environment
rm(fouls.against.html,fouls.against.table)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#headed goals
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

url.headed.goals <- 'http://www.legaseriea.it/en/serie-a/statistics/GolTesta'#Specifying the url for desired website to be scraped

headed.goals.table <- read_html(url.headed.goals)#Reading the HTML code from the website

headed.goals.html <- html_nodes(headed.goals.table,'td')#Using CSS selectors to scrape the rankings section

headed.goals <- html_text(headed.goals.html)#Converting the ranking data to text

head(headed.goals)#Let's have a look at the rankings

headed.goals <- str_replace_all(headed.goals, "[\r\n]" , "")#remove all the tags

#transform the values in a dataframe
headed.goals <- as.data.frame(headed.goals)
headed.goals <- data.frame(
  headed.goals,
  ind = rep(1:4, nrow(headed.goals))) # create a repeated index
headed.goals <-unstack(headed.goals, headed.goals~ind)

names(headed.goals) <- c("RANKING", "TEAM", "PLAYER", "HEADED.GOALS")#name headers

headed.goals$TEAM <- gsub('\\s+', '', headed.goals$TEAM)# remove unnecessary space from the beginning

headed.goals$PLAYER <- gsub(" $","", headed.goals$PLAYER, perl=T)# remove unnecessary space from at the end

headed.goals

#clean environment
rm(headed.goals.html,headed.goals.table)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#posts/crossbar
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

url.posts.crossbar <- 'http://www.legaseriea.it/en/serie-a/statistics/Palo'#Specifying the url for desired website to be scraped

posts.crossbar.table <- read_html(url.posts.crossbar)#Reading the HTML code from the website

posts.crossbar.html <- html_nodes(posts.crossbar.table,'td')#Using CSS selectors to scrape the rankings section

posts.crossbar <- html_text(posts.crossbar.html)#Converting the ranking data to text

head(posts.crossbar)#Let's have a look at the rankings

posts.crossbar <- str_replace_all(posts.crossbar, "[\r\n]" , "")#remove all the tags

#transform the values in a dataframe
posts.crossbar <- as.data.frame(posts.crossbar)
posts.crossbar <- data.frame(
  posts.crossbar,
  ind = rep(1:4, nrow(posts.crossbar))) # create a repeated index
posts.crossbar <-unstack(posts.crossbar, posts.crossbar~ind)

names(posts.crossbar) <- c("RANKING", "TEAM", "PLAYER", "POSTS.CROSSBAR")#name headers

posts.crossbar$TEAM <- gsub('\\s+', '', posts.crossbar$TEAM)# remove unnecessary space from the beginning

posts.crossbar$PLAYER <- gsub(" $","", posts.crossbar$PLAYER, perl=T)# remove unnecessary space from at the end

posts.crossbar

#clean environment
rm(posts.crossbar.html,posts.crossbar.table)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#data managing  (list, clean, reduce)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
df.list.source <- list(fouls.against = fouls.against, # create a list of dataframes 
                  headed.goals = headed.goals,
                  key.passes = key.passes,
                  km = km,
                  posts.crossbar = posts.crossbar,
                  recoveries = recoveries,
                  shot = shot,
                  top.assist = top.assist,
                  top.scores = top.scores)

df.list.clean <- lapply(df.list.source, function(x) { x["RANKING"] <- NULL; x }) # remove ranking column otherwise is a mess for merging 

rm(fouls.against, headed.goals, key.passes, km, posts.crossbar, recoveries, shot, top.assist, top.scores) #clean environment

rm(url.fouls.against, url.headed.goals, url.key.passes, url.km, url.recoveries, url.posts.crossbar, url.shot, url.top.assist, url.top.scores) # clean environment from urls

#merge all the data set
df.complete.serie.a <- reduce(df.list.clean, full_join)
df.complete.serie.a <- df.complete.serie.a[!duplicated(df.complete.serie.a), ] #remove duplicate rows
team.list <- list("ATALANTA", "BOLOGNA", "BRESCIA", "CAGLIARI", "FIORENTINA", "GENOA", "HELLASVERONA", "INTER", "JUVENTUS", "LAZIO", "LECCE", "MILAN", "NAPOLI", "PARMA", "ROMA", "SAMPDORIA", "SASSUOLO", "SPAL", "TORINO", "UDINESE")
df.complete.serie.a <- df.complete.serie.a[ df.complete.serie.a$TEAM %in% team.list, ] #remove team that are not in the list (for instance transferred players after the first game are reported with the new team even if abroad) 
rm(team.list)
#df.complete.serie.a$PLAYER <- sub(".*? (.+)", "\\1", df.complete.serie.a$PLAYER) #remove players name

#merge with data fanta
data_fanta <- read.csv(file.path ("D:\\My Drive\\fanta 2019_2020","DATA_FANTA.csv"), sep=";")
setnames(data_fanta, "GIOCATORE", "PLAYER")#rename GIOCATORE into player
data_fanta$PLAYER <- gsub(" $","", data_fanta$PLAYER, perl=T)# remove unnecessary space from at the end
data_fanta$SQUADRA <- NULL #remove squadra
df.complete.fanta.uncorrected <- list(data_fanta = data_fanta, df.complete.serie.a = df.complete.serie.a)
df.complete.fanta.uncorrected  <- reduce(df.complete.fanta.uncorrected, full_join)
df.complete.fanta.uncorrected<-df.complete.fanta.uncorrected[-which(is.na(df.complete.fanta.uncorrected$FANTA.TEAM)),]#remove all the players without fanta team

#str dataframe
rm(data_fanta)

#str dataframe
df.complete.fanta.uncorrected$FANTA.TEAM <- as.factor(df.complete.fanta.uncorrected$FANTA.TEAM)
df.complete.fanta.uncorrected$PLAYER <- as.factor(df.complete.fanta.uncorrected$PLAYER)
df.complete.fanta.uncorrected$PREZZO.ASTA <- as.numeric(df.complete.fanta.uncorrected$PREZZO.ASTA)
df.complete.fanta.uncorrected$RUOLO <- as.factor(df.complete.fanta.uncorrected$RUOLO)
df.complete.fanta.uncorrected$TEAM <- as.factor(df.complete.fanta.uncorrected$TEAM)
df.complete.fanta.uncorrected$FOULS.AGAINST <- as.factor(df.complete.fanta.uncorrected$FOULS.AGAINST)
df.complete.fanta.uncorrected$HEADED.GOALS <- as.numeric(df.complete.fanta.uncorrected$HEADED.GOALS)
df.complete.fanta.uncorrected$KEY.PASSES <- as.numeric(df.complete.fanta.uncorrected$KEY.PASSES)
df.complete.fanta.uncorrected$AVERAGE.km <- as.numeric(df.complete.fanta.uncorrected$AVERAGE.km)
df.complete.fanta.uncorrected$MINUTES <- as.numeric(df.complete.fanta.uncorrected$MINUTES)
df.complete.fanta.uncorrected$POSTS.CROSSBAR <- as.numeric(df.complete.fanta.uncorrected$POSTS.CROSSBAR)
df.complete.fanta.uncorrected$RECOVERIES <- as.numeric(df.complete.fanta.uncorrected$RECOVERIES)
df.complete.fanta.uncorrected$TOTAL.SHOT <- as.numeric(df.complete.fanta.uncorrected$TOTAL.SHOT)
df.complete.fanta.uncorrected$ON.TARGET <- as.numeric(df.complete.fanta.uncorrected$ON.TARGET)
df.complete.fanta.uncorrected$OFF.TARGET <- as.numeric(df.complete.fanta.uncorrected$OFF.TARGET)
df.complete.fanta.uncorrected$ASSIST <- as.numeric(df.complete.fanta.uncorrected$ASSIST)
df.complete.fanta.uncorrected$GOALS <- as.numeric(df.complete.fanta.uncorrected$GOALS)
df.complete.fanta.uncorrected$PLAYED <- as.numeric(df.complete.fanta.uncorrected$PLAYED)
df.complete.fanta.uncorrected$PENALTIES <- as.numeric(df.complete.fanta.uncorrected$PENALTIES)

#list all the old df
df.backup <- list(df.complete.serie.a = df.complete.serie.a,
                  df.list.clean = df.list.clean,
                  df.list.source = df.list.source)
rm(df.complete.serie.a, df.list.clean, df.list.source)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#general graphic parameters 
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
position = position_dodge(width = .75) #to have labels centered at graph end in case you need it
width = .75 #to have labels centered at graph end in case you need it
fillpalette <- c("AZTEKA FC" = "orange",# fill color for each team
               "NEVERENDING TURD" = "blue",
               "CSK LA RISSA" = "indianred3",
               "MATTIA TEAM V5" =  "white",
               "PARTIZAN DEGRADO" = "firebrick",
               "LOKOMOTIV SMILLAUS" = "yellow2",
               "REAL GHETTO" = "black",
               "ASSASSIN CRIN" = "forestgreen")

borderspalette <- c("AZTEKA FC" = "blue",# fill color for each team
                   "NEVERENDING TURD" = "black",
                   "CSK LA RISSA" = "gray88",
                   "MATTIA TEAM V5" =  "red",
                   "PARTIZAN DEGRADO" = "black",
                   "LOKOMOTIV SMILLAUS" = "darkgreen",
                   "REAL GHETTO" = "snow2",
                   "ASSASSIN CRIN" = "black")

fillpalette_line <- c("AZTEKA FC" = "orange",# fill color for each team
                 "NEVERENDING TURD" = "blue",
                 "CSK LA RISSA" = "indianred3",
                 "MATTIA TEAM V5" =  "azure3",
                 "PARTIZAN DEGRADO" = "firebrick",
                 "LOKOMOTIV SMILLAUS" = "yellow2",
                 "REAL GHETTO" = "black",
                 "ASSASSIN CRIN" = "forestgreen")
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#plot.top.scores.team
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.top.scores.team <- aggregate(GOALS ~ FANTA.TEAM, df.complete.fanta.uncorrected, sum) #sum per fanta.team
plot.top.scores.team <- plot.top.scores.team[order(plot.top.scores.team$GOALS, decreasing = F), ] #reorder the sum
x = max(plot.top.scores.team$GOALS) #set the maximum value for the graph 
plot.top.scores.team$FANTA.TEAM <- factor(plot.top.scores.team$FANTA.TEAM, levels = plot.top.scores.team$FANTA.TEAM) #use the factor to impose the order in the graph

plot.top.scores.team <- ggplot(plot.top.scores.team, aes(x = FANTA.TEAM, y = GOALS, fill = FANTA.TEAM, color = FANTA.TEAM))+
  stat_summary(fun.y="sum", geom="bar", size = 2)+
  theme(axis.title.y = element_blank(),
         axis.text.y = element_text(size = 12, face = "bold", color = "black"),
         axis.title.x = element_text(size = 15, face = "bold", color = "black"),
         axis.text.x = element_text(size = 12, face = "bold", color = "black"),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black"),
         panel.grid.major.x = element_line(colour = "grey"),
         legend.position = "none")+
  scale_y_continuous(breaks = seq(0, x, by=1))+
  ylab("GOAL SEGNATI")+
  coord_flip()+ 
  scale_fill_manual(values = fillpalette)+ 
  scale_color_manual(values = borderspalette)
plot.top.scores.team
rm(x)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#plot.top.scores.team.role
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#after a long interal debate with myself I relaized that the best solution for this graph is to split it in 3 dataset and then plot everything together
#after a second long internal debate with myself I decided that a dotline plot is more better represent the data. I keep anyway the extended version
plot.top.scores.team.role <- aggregate(GOALS ~ FANTA.TEAM + RUOLO, df.complete.fanta.uncorrected, sum) #sum per fanta.team
plot.top.scores.team.role <- plot.top.scores.team.role[order(plot.top.scores.team.role$RUOLO, plot.top.scores.team.role$GOALS),] # reorder based on goals and role
plot.top.scores.team.role.df <- subset(plot.top.scores.team.role, subset = RUOLO == "DF", select = c(FANTA.TEAM,RUOLO,GOALS)) #subset separately each role
plot.top.scores.team.role.cc <- subset(plot.top.scores.team.role, subset = RUOLO == "CC", select = c(FANTA.TEAM,RUOLO,GOALS)) 
plot.top.scores.team.role.at <- subset(plot.top.scores.team.role, subset = RUOLO == "AT", select = c(FANTA.TEAM,RUOLO,GOALS))
x = max(plot.top.scores.team.role$GOALS) #set the maximum value for the graph
x.df = max(plot.top.scores.team.role.df$GOALS)  
x.cc = max(plot.top.scores.team.role.cc$GOALS)
x.at = max(plot.top.scores.team.role.at$GOALS)
plot.top.scores.team.role.df$FANTA.TEAM <- factor(plot.top.scores.team.role.df$FANTA.TEAM, levels = plot.top.scores.team.role.df$FANTA.TEAM) #use the factor to impose the order in the graph
plot.top.scores.team.role.cc$FANTA.TEAM <- factor(plot.top.scores.team.role.cc$FANTA.TEAM, levels = plot.top.scores.team.role.cc$FANTA.TEAM) #use the factor to impose the order in the graph
plot.top.scores.team.role.at$FANTA.TEAM <- factor(plot.top.scores.team.role.at$FANTA.TEAM, levels = plot.top.scores.team.role.at$FANTA.TEAM) #use the factor to impose the order in the graph

#goals.line.plot
plot.top.scores.team.role$RUOLO <- factor(plot.top.scores.team.role$RUOLO, levels=c("DF", "CC", "AT"))# reorder x axis
goals.line.plot <- ggplot(plot.top.scores.team.role,
                                    aes(x = RUOLO, y = GOALS,
                                        group = FANTA.TEAM))+
  geom_line(size = 2, aes(color=factor(FANTA.TEAM)))+
  geom_point(aes(color=factor(FANTA.TEAM), 
                 fill = factor(FANTA.TEAM)), shape=21, size = 5, stroke = 3)+ 
  scale_fill_manual(values= borderspalette) + #to use my palettes
  scale_colour_manual(values= fillpalette_line)+
  theme(axis.title.y = element_text(size = 15, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="grey88"),
        legend.position = "right",
        legend.title  = element_blank(),
        legend.text = element_text(size = 12, face = "bold"))+
  scale_y_continuous(breaks = seq(0, x, by=1))+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin=0, ymax=Inf, alpha=0.1, fill="cyan")+#color sectors of background
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin=0, ymax=Inf, alpha=0.1, fill="lightgoldenrod")+
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin=0, ymax=Inf, alpha=0.1, fill="tomato3")+
  ylab ("GOAL SEGNATI")
goals.line.plot

rm(x)
rm(plot.top.scores.team.role)


#plot df
plot.top.scores.team.role.df <- ggplot(plot.top.scores.team.role.df, aes(x = FANTA.TEAM, y = GOALS, fill = FANTA.TEAM, color = FANTA.TEAM))+
  stat_summary(fun.y="sum", geom="bar", size = 2)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "grey"),
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(breaks = seq(0, x.df, by=1))+
  ylab("GOAL SEGNATI")+
  ggtitle("DIFESA")+
  coord_flip()+
  scale_fill_manual(values = fillpalette)+
  scale_color_manual(values = borderspalette)
plot.top.scores.team.role.df

rm(x.df)

#plot cc
plot.top.scores.team.role.cc <- ggplot(plot.top.scores.team.role.cc, aes(x = FANTA.TEAM, y = GOALS, fill = FANTA.TEAM, color = FANTA.TEAM))+
  stat_summary(fun.y="sum", geom="bar", size = 2)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "grey"),
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(breaks = seq(0, x.cc, by=1))+
  ylab("GOAL SEGNATI")+
  ggtitle("CENTROCAMPO")+
  coord_flip()+
  scale_fill_manual(values = fillpalette)+
  scale_color_manual(values = borderspalette)
plot.top.scores.team.role.cc

rm(x.cc)

#plot at
plot.top.scores.team.role.at <- ggplot(plot.top.scores.team.role.at, aes(x = FANTA.TEAM, y = GOALS, fill = FANTA.TEAM, color = FANTA.TEAM))+
  stat_summary(fun.y="sum", geom="bar", size = 2)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "grey"),
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(breaks = seq(0, x.at, by=1))+
  ylab("GOAL SEGNATI")+
  ggtitle("ATTACCO")+
  coord_flip()+
  scale_fill_manual(values = fillpalette)+
  scale_color_manual(values = borderspalette)
plot.top.scores.team.role.at

rm(x.at)

grid.goals.role<-plot_grid(plot.top.scores.team.role.df,
             plot.top.scores.team.role.cc,
             plot.top.scores.team.role.at,
             nrow = 1)
grid.goals.role

#create a lsit with all the goals graphs
goals.plot.list <- list(goals.line.plot = goals.line.plot,
                        plot.top.scores.team = plot.top.scores.team,
                        plot.top.scores.team.role.at = plot.top.scores.team.role.at,
                        plot.top.scores.team.role.cc = plot.top.scores.team.role.cc,
                        plot.top.scores.team.role.df = plot.top.scores.team.role.df,
                        grid.goals.role = grid.goals.role)
rm(goals.line.plot, plot.top.scores.team, plot.top.scores.team.role.at, plot.top.scores.team.role.cc, plot.top.scores.team.role.df, grid.goals.role)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#plot.top.assist.team
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.top.assist.team <- aggregate(ASSIST ~ FANTA.TEAM, df.complete.fanta.uncorrected, sum) #sum per fanta.team
plot.top.assist.team <- plot.top.assist.team[order(plot.top.assist.team$ASSIST, decreasing = F), ] #reorder the sum
x = max(plot.top.assist.team$ASSIST) #set the maximum value for the graph 
plot.top.assist.team$FANTA.TEAM <- factor(plot.top.assist.team$FANTA.TEAM, levels = plot.top.assist.team$FANTA.TEAM) #use the factor to impose the order in the graph

plot.top.assist.team <- ggplot(plot.top.assist.team, aes(x = FANTA.TEAM, y = ASSIST, fill = FANTA.TEAM, color = FANTA.TEAM))+
  stat_summary(fun.y="sum", geom="bar", size = 2)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "grey"),
        legend.position = "none")+
  scale_y_continuous(breaks = seq(0, x, by=1))+
  ylab("ASSIST")+
  coord_flip()+
  scale_fill_manual(values = fillpalette)+
  scale_color_manual(values = borderspalette)
plot.top.assist.team 
rm(x)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#plot.top.assist.team.role
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot.top.assist.team.role <- aggregate(ASSIST ~ FANTA.TEAM + RUOLO, df.complete.fanta.uncorrected, sum) #sum per fanta.team
plot.top.assist.team.role <- plot.top.assist.team.role[order(plot.top.assist.team.role$RUOLO, plot.top.assist.team.role$ASSIST),] # reorder based on goals and role
plot.top.assist.team.role.df <- subset(plot.top.assist.team.role, subset = RUOLO == "DF", select = c(FANTA.TEAM,RUOLO,ASSIST)) #subset separately each role
plot.top.assist.team.role.cc <- subset(plot.top.assist.team.role, subset = RUOLO == "CC", select = c(FANTA.TEAM,RUOLO,ASSIST)) 
plot.top.assist.team.role.at <- subset(plot.top.assist.team.role, subset = RUOLO == "AT", select = c(FANTA.TEAM,RUOLO,ASSIST))
x = max(plot.top.assist.team.role$ASSIST) #set the maximum value for the graph
x.df = max(plot.top.assist.team.role.df$ASSIST)  
x.cc = max(plot.top.assist.team.role.cc$ASSIST)
x.at = max(plot.top.assist.team.role.at$ASSIST)
plot.top.assist.team.role.df$FANTA.TEAM <- factor(plot.top.assist.team.role.df$FANTA.TEAM, levels = plot.top.assist.team.role.df$FANTA.TEAM) #use the factor to impose the order in the graph
plot.top.assist.team.role.cc$FANTA.TEAM <- factor(plot.top.assist.team.role.cc$FANTA.TEAM, levels = plot.top.assist.team.role.cc$FANTA.TEAM) #use the factor to impose the order in the graph
plot.top.assist.team.role.at$FANTA.TEAM <- factor(plot.top.assist.team.role.at$FANTA.TEAM, levels = plot.top.assist.team.role.at$FANTA.TEAM) #use the factor to impose the order in the graph

#assist.line.plot
plot.top.assist.team.role$RUOLO <- factor(plot.top.assist.team.role$RUOLO, levels=c("DF", "CC", "AT"))# reorder x axis
assist.line.plot <- ggplot(plot.top.assist.team.role,
                          aes(x = RUOLO, y = ASSIST,
                              group = FANTA.TEAM))+
  geom_line(size = 2, aes(color=factor(FANTA.TEAM)))+
  geom_point(aes(color=factor(FANTA.TEAM), 
                 fill = factor(FANTA.TEAM)), shape=21, size = 5, stroke = 3)+ 
  scale_fill_manual(values= borderspalette) + #to use my palettes
  scale_colour_manual(values= fillpalette_line)+
  theme(axis.title.y = element_text(size = 15, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="grey88"),
        legend.position = "right",
        legend.title  = element_blank(),
        legend.text = element_text(size = 12, face = "bold"))+
  scale_y_continuous(breaks = seq(0, x, by=1))+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin=0, ymax=Inf, alpha=0.1, fill="cyan")+#color sectors of background
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin=0, ymax=Inf, alpha=0.1, fill="lightgoldenrod")+
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin=0, ymax=Inf, alpha=0.1, fill="tomato3")+
  ylab ("ASSIST")
assist.line.plot

rm(x)
rm(plot.top.assist.team.role)


#plot df
plot.top.assist.team.role.df <- ggplot(plot.top.assist.team.role.df, aes(x = FANTA.TEAM, y = ASSIST, fill = FANTA.TEAM, color = FANTA.TEAM))+
  stat_summary(fun.y="sum", geom="bar", size = 2)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "grey"),
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(breaks = seq(0, x.df, by=1))+
  ylab("ASSIST")+
  ggtitle("DIFESA")+
  coord_flip()+
  scale_fill_manual(values = fillpalette)+
  scale_color_manual(values = borderspalette)
plot.top.assist.team.role.df

rm(x.df)

#plot cc
plot.top.assist.team.role.cc <- ggplot(plot.top.assist.team.role.cc, aes(x = FANTA.TEAM, y = ASSIST, fill = FANTA.TEAM, color = FANTA.TEAM))+
  stat_summary(fun.y="sum", geom="bar", size = 2)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "grey"),
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(breaks = seq(0, x.cc, by=1))+
  ylab("ASSIST")+
  ggtitle("CENTROCAMPO")+
  coord_flip()+
  scale_fill_manual(values = fillpalette)+
  scale_color_manual(values = borderspalette)
plot.top.assist.team.role.cc

rm(x.cc)

#plot at
plot.top.assist.team.role.at <- ggplot(plot.top.assist.team.role.at, aes(x = FANTA.TEAM, y = ASSIST, fill = FANTA.TEAM, color = FANTA.TEAM))+
  stat_summary(fun.y="sum", geom="bar", size = 2)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "grey"),
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(breaks = seq(0, x.at, by=1))+
  ylab("ASSIST")+
  ggtitle("ATTACCO")+
  coord_flip()+
  scale_fill_manual(values = fillpalette)+
  scale_color_manual(values = borderspalette)
plot.top.assist.team.role.at

rm(x.at)

grid.assist.role<-plot_grid(plot.top.assist.team.role.df,
                           plot.top.assist.team.role.cc,
                           plot.top.assist.team.role.at,
                           nrow = 1)
grid.assist.role

#create a lsit with all the assist graphs
assist.plot.list <- list(assist.line.plot = assist.line.plot,
                        plot.top.assist.team = plot.top.assist.team,
                        plot.top.assist.team.role.at = plot.top.assist.team.role.at,
                        plot.top.assist.team.role.cc = plot.top.assist.team.role.cc,
                        plot.top.assist.team.role.df = plot.top.assist.team.role.df,
                        grid.assist.role = grid.assist.role)
rm(assist.line.plot, plot.top.assist.team, plot.top.assist.team.role.at, plot.top.assist.team.role.cc, plot.top.assist.team.role.df, grid.assist.role)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#km plots
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
