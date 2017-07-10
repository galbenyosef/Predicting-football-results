
makePrediction <- function(database,homeclub,visitorclub){
  
  dta.predict <- data.frame(Team=as.factor(c(as.character(database$HomeTeam),
                                           as.character(database$AwayTeam))),
                          Opponent=as.factor(c(as.character(database$AwayTeam),
                                               as.character(database$HomeTeam))),
                          Goals=c(database$FTHG, database$FTAG),
                          Home=c(rep(1, dim(database)[1]), rep(0, dim(database)[1])))




  model <- glm(Goals ~ Home + Team + Opponent, data=dta.predict, family=poisson())

  #summary(model)
  
  av_home_goals <- predict(model, 
                         data.frame(Home=1, Team=homeclub, 
                                    Opponent=visitorclub), 
                         type="response")

  av_away_goals <- predict(model, 
                         data.frame(Home=0, Team=visitorclub, 
                                    Opponent=homeclub), 
                         type="response")

  # get probabilities per goal
  home_goals <- dpois(0:9, av_home_goals) 
  away_goals <- dpois(0:9, av_away_goals)
  
  m <- outer(home_goals,away_goals)
  rownames(m) <- 0:9
  colnames(m) <- 0:9
  #class(m) <- c('fboo',class(m))
  attr(m,'row') <- homeclub
  attr(m,'col') <- visitorclub

  
  # get probabilities for home, draw, away win
  home <- sum(m[lower.tri(m)])
  draw <- sum(diag(m))
  away <- sum(m[upper.tri(m)])
  
  summarize = c(home,draw,away)
  names(summarize) <- c(homeclub,'equal',visitorclub)
  print(paste0("Predicting ",homeclub," vs ",visitorclub," full time result"))
  print(summarize)
  
  #print score probabilty matrix
  #goals.difference(m)
  
  #return values to the simulation
  return(c(av_home_goals,av_away_goals))
}
#Simulation
Simulate <- function(params){
  set.seed(735180521)
  nsim <- 10000
  homeGoalsSim <- rpois(nsim, params[1]) 
  awayGoalsSim <- rpois(nsim, params[2])
  goalDiffSim <- homeGoalsSim - awayGoalsSim
  print("Simulation results:")
  print("Home:")
  print(sum(goalDiffSim > 0) / nsim)
  print("Draw:")
  print(sum(goalDiffSim == 0) / nsim)
  print("Away:")
  print(sum(goalDiffSim < 0) / nsim)
}

goals.difference <- function(x) {
  cat(attr(x,'row'),'in rows against',attr(x,'col'),'in columns (goals score) \n')
  class(x) <- class(x)[-1]
  attr(x,'row') <- NULL
  attr(x,'col') <- NULL
  newM <- copy(x)
  newM <- formatC(x,format='f',width=4) # fixed format
  newM <- gsub('\\.0+$','       ',newM)   # replace trailing 0 by ‘ ‘
  newM <- substr(newM,1,6)                # and fix the width
  print(newM,quote=FALSE,justify='left')
}


