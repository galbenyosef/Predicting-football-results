#general information we can get out from database
#ggplot(subset(DataSeasonly,Position<3),aes(x=Season,y=Points))+geom_point()+facet_wrap(~Team, ncol = 1)+ggtitle("Top club points")+xlab("Season")
#ggplot(subset(DataSeasonly,Position<3),aes(x=Season,y=Win))+geom_point()+facet_wrap(~Team, ncol = 1)+ggtitle("Top clubs by wins")+xlab("Season")
#ggplot(subset(DataSeasonly,Position<3),aes(x=Season,y=Tie))+geom_point()+facet_wrap(~Team, ncol = 1)+ggtitle("Top clubs by ties")+xlab("Season")
#ggplot(subset(DataSeasonly,Position<3),aes(x=Season,y=Loss))+geom_point()+facet_wrap(~Team, ncol = 1)+ggtitle("Top clubs by loss")+xlab("Season")
#ggplot(subset(DataSeasonly,Position<6),aes(x=Team,y=Points))+geom_point()+facet_wrap(~Season, ncol = 1)+ggtitle("Top clubs points by season")+xlab("Team")
#ggplot(subset(DataSeasonly,Position<6),aes(x=Points,y=Season))+geom_point()+facet_wrap(~Team, ncol = 1)+ggtitle("Top clubs by seasons")+xlab("Points")
#ggplot(subset(DataSeasonly,Position<6),aes(x=HGOALS,y=Team))+geom_point()+facet_wrap(~Season, ncol = 1)+ggtitle("Top clubs home goals by season")+xlab("Home Goals")
#ggplot(subset(DataSeasonly,Position<6),aes(x=AGOALS,y=Team))+geom_point()+facet_wrap(~Season, ncol = 1)+ggtitle("Top clubs away goals by season")+xlab("away goals")
#qplot(Season,Points, data=subset(DataSeasonly,Position<2), fill=Team,geom="auto",size=I(3),color=Team,main="The champions by points", xlab="Season", ylab="Points")

simParams <- makePrediction(DataPrepared,"Man United","Chelsea")
simParams <- makePrediction(DataPrepared,"Chelsea","Southampton")
simParams <- makePrediction(DataPrepared,"Everton","Chelsea")
simParams <- makePrediction(DataPrepared,"Chelsea","Middlesbrough")
simParams <- makePrediction(DataPrepared,"West Brom","Chelsea")

simParams <- makePrediction(DataPrepared,"Tottenham","Bournemouth")
simParams <- makePrediction(DataPrepared,"Crystal Palace","Tottenham")
simParams <- makePrediction(DataPrepared,"Tottenham","Arsenal")
simParams <- makePrediction(DataPrepared,"West Ham","Tottenham")
simParams <- makePrediction(DataPrepared,"Tottenham","Man United")

simParams <- makePrediction(DataPrepared,"Barcelona","Sociedad")
simParams <- makePrediction(DataPrepared,"Real Madrid","Barcelona")
simParams <- makePrediction(DataPrepared,"Barcelona","Osasuna")
simParams <- makePrediction(DataPrepared,"Espanol","Barcelona")
simParams <- makePrediction(DataPrepared,"Barcelona","Villarreal")

simParams <- makePrediction(DataPrepared,"Sp Gijon","Real Madrid")
simParams <- makePrediction(DataPrepared,"Real Madrid","Barcelona")
simParams <- makePrediction(DataPrepared,"La Coruna","Real Madrid")
simParams <- makePrediction(DataPrepared,"Real Madrid","Valencia")
simParams <- makePrediction(DataPrepared,"Granada","Real Madrid")

#plot match prediction
slices <- summarize
lbls <- c("Home","Draw","Away")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)), radius = 20, main="Match Result")
