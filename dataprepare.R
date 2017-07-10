#data.table, dplyr, ggplot2 libraries
library(data.table)
library(dplyr)
library(ggplot2)

data.load<-function(){
  #manuallly file loading
  #setwd("/Big Data/final project/data")
  #file_list = list.files(pattern="*.csv")
  
  if (country == "eng")
    league="E0"
  else if (country == "esp")
    league="SP1"
  else if (country == "ita")
    league="I1"
  else if (country == "ger")
    league="D1"
  else if (country == "fra")
    league="F1"
  
  #automatically file loading
  file_list <- list()
  for (i in years){
    season<- paste0(substr(i-1,3,4),substr(i,3,4))
    file_list[length(file_list)+1]<-paste0("http://www.football-data.co.uk/mmz4281/",season,"/",league,".csv")
  }
  #list as tables
  csvs <- lapply(file_list, read.csv , sep=",")
  #tables binding
  database <- rbindlist( csvs , fill= TRUE)
  
  return (database)
}


#table for weekly stats
data.prepare<-function(database){
  retval = copy(database)
  #weekly stats for team
  #date manipulation
  retval[,nDate:=ifelse(nchar(substr(Date,1,10))>8,paste0(substr(Date,1,6),substr(Date,9,10)),substr(Date,1,10))]
  retval[,Date:=as.Date(nDate,format="%d/%m/%y")]
  #flush
  retval <- retval[!is.na(Date),]
  #add season column
  retval[,ss:=ifelse( 7 < month(Date) & month(Date) < 13,year(Date),year(Date)-1)]
  retval[,sn:=ifelse( 7 < month(Date) & month(Date) < 13,year(Date)+1,year(Date))]
  retval[,Season:=paste0(ss,"-",sn)]
  
  week_counter=copy(retval[,.(Season,HomeTeam)])
  week_counter=unique(week_counter)
  week_counter[,NumTeams:=.N,by=Season]
  week_counter[,NumMatches:=NumTeams/2]
  week_counter=unique(week_counter[,.(Season,NumMatches)])
  setkey(retval,Season)
  retval = merge(retval,week_counter)
  retval[ , count := 1:.N, by = Season]
  retval[,Week:=(ceiling(count/NumMatches))]
  
  #flush
  retval=retval[,.(Season,Week,Date,HomeTeam,AwayTeam,FTHG,FTAG)]
  
  return(retval)
}

#table for yearly(seasonly) stats
data.seasonly <- function(database){
  
  seasonly <- copy(database)
  
  home_filter <- c("Season,HomeTeam")
  away_filter <- c("Season,AwayTeam")
  
  seasonly[,HWIN:=sum(FTHG>FTAG), by=home_filter]
  seasonly[,HTIE:=sum(FTHG==FTAG),by=home_filter] 
  seasonly[,HLOSS:=sum(FTHG<FTAG), by=home_filter]
  seasonly[,HGOALS:=sum(FTHG), by=home_filter]
  seasonly[,HGCONC:=sum(FTAG), by=home_filter]
  
  seasonly[,AWIN:=sum(FTHG<FTAG), by=away_filter]
  seasonly[,ATIE:=sum(FTHG==FTAG), by=away_filter] 
  seasonly[,ALOSS:=sum(FTHG>FTAG), by=away_filter]
  seasonly[,AGOALS:=sum(FTAG), by=away_filter]
  seasonly[,AGCONC:=sum(FTHG), by=away_filter]
  
  seasonly.away = unique(seasonly[,.(Season,AwayTeam,AWIN,ATIE,ALOSS,AGOALS,AGCONC)][order(AwayTeam)]) 
  seasonly.home = unique(seasonly[,.(Season,HomeTeam,HWIN,HTIE,HLOSS,HGOALS,HGCONC)][order(HomeTeam)])
  seasonly.away = seasonly.away[,APTS:=3*AWIN+1*ATIE]
  seasonly.home = seasonly.home[,HPTS:=3*HWIN+1*HTIE]

  seasonly.standings=data.table(Season=seasonly.home[,Season],
                          Team=seasonly.home[, HomeTeam],
                          Win=seasonly.home[, HWIN]+seasonly.away[, AWIN],
                          Tie=seasonly.home[, HTIE]+seasonly.away[, ATIE],
                          Loss=seasonly.home[, HLOSS]+seasonly.away[, ALOSS],
                          Scored=seasonly.home[,HGOALS]+seasonly.away[,AGOALS],
                          Conceded=seasonly.home[,HGCONC]+seasonly.away[,AGCONC],
                          Points=seasonly.home[,HPTS]+seasonly.away[,APTS],
                          seasonly.home[,-(1:2)],
                          seasonly.away[,-(1:2)])
  seasonly.standings = seasonly.standings[order(Season,-HPTS)] 
  seasonly.standings[, HPosition := 1:.N, by = Season]
  seasonly.standings = seasonly.standings[order(Season,-APTS)] 
  seasonly.standings[, APosition := 1:.N, by = Season]
  seasonly.standings = seasonly.standings[order(Season,-Points)] 
  seasonly.standings[, Position := 1:.N, by = Season]

  seasonly.standings=seasonly.standings[!is.na(Team),]
  return(seasonly.standings)
  
}

#table for alltime stats
data.accum <- function(database){
  
  at <- copy(database)
  at <- at[,!c("Season","HPosition","APosition","Position")]
  at <- at[,Win:=sum(Win),by=Team] 
  at <- at[,Tie:=sum(Tie),by=Team] 
  at <- at[,Loss:=sum(Loss),by=Team] 
  at <- at[,Scored:=sum(Scored),by=Team] 
  at <- at[,Conceded:=sum(Conceded),by=Team] 
  at <- at[,Points:=sum(Points),by=Team] 
  at <- at[,HWIN:=sum(HWIN,na.rm = TRUE),by=Team] 
  at <- at[,HTIE:=sum(HTIE,na.rm = TRUE),by=Team] 
  at <- at[,HLOSS:=sum(HLOSS,na.rm = TRUE),by=Team] 
  at <- at[,HGOALS:=sum(HGOALS,na.rm = TRUE),by=Team] 
  at <- at[,HGCONC:=sum(HGCONC,na.rm = TRUE),by=Team] 
  at <- at[,HPTS:=sum(HPTS,na.rm = TRUE),by=Team] 
  at <- at[,AWIN:=sum(AWIN,na.rm = TRUE),by=Team] 
  at <- at[,ATIE:=sum(ATIE,na.rm = TRUE),by=Team] 
  at <- at[,ALOSS:=sum(ALOSS,na.rm = TRUE),by=Team] 
  at <- at[,AGOALS:=sum(AGOALS,na.rm = TRUE),by=Team] 
  at <- at[,AGCONC:=sum(AGCONC,na.rm = TRUE),by=Team] 
  at <- at[,APTS:=sum(APTS,na.rm = TRUE),by=Team]
  
  at <- unique(at)
  at[order(-Points)]
  return (at)
}