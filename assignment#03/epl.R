getAvgMaxData = function(){

  # get max_avg data for 7 seasons using sqldf
  epl_2008_2009_db = sqldf("select * from epl_2008_2009")
  epl_2008_2009_db_max = sqldf("select max(total_score) as max_total_score, max(win_rate) as max_win_rate from epl_2008_2009_db")
  
  epl_2009_2010_db = sqldf("select * from epl_2009_2010")
  epl_2009_2010_db_max = sqldf("select max(total_score) as max_total_score, max(win_rate) as max_win_rate from epl_2009_2010_db")
  
  epl_2010_2011_db = sqldf("select * from epl_2010_2011")
  epl_2010_2011_db_max = sqldf("select max(total_score) as max_total_score, max(win_rate) as max_win_rate from epl_2010_2011_db")
  
  epl_2011_2012_db = sqldf("select * from epl_2011_2012")
  epl_2011_2012_db_max = sqldf("select max(total_score) as max_total_score, max(win_rate) as max_win_rate from epl_2011_2012_db")
  
  epl_2012_2013_db = sqldf("select * from epl_2012_2013")
  epl_2012_2013_db_max = sqldf("select max(total_score) as max_total_score, max(win_rate) as max_win_rate from epl_2012_2013_db")
  
  epl_2013_2014_db = sqldf("select * from epl_2013_2014")
  epl_2013_2014_db_max = sqldf("select max(total_score) as max_total_score, max(win_rate) as max_win_rate from epl_2013_2014_db")
  
  epl_2014_2015_db = sqldf("select * from epl_2014_2015")
  epl_2014_2015_db_max = sqldf("select max(total_score) as max_total_score, max(win_rate) as max_win_rate from epl_2014_2015_db")
  epl_2014_2015_db_max
  
  epl_max_avg = (epl_2008_2009_db_max + epl_2009_2010_db_max + epl_2010_2011_db_max + epl_2011_2012_db_max 
                 + epl_2012_2013_db_max + epl_2013_2014_db_max +epl_2014_2015_db_max)/7
  epl_max_avg = round(epl_max_avg, digits = 3) 

  max_total_score = c(epl_max_avg[1,1])
  max_win_rate = c(epl_max_avg[1,2])
  result = data.frame(max_total_score , max_win_rate)
  return (result)  
  
}

findWinners = function(avg_max_data){
 
  score = avg_max_data[1,1]
  rate = avg_max_data[1,2]
  epl_2008_2009_winners = sqldf(sprintf("select team_name from epl_2008_2009 where (total_Score >= %f and win_rate >= %f)", score, rate))
  epl_2009_2010_winners = sqldf(sprintf("select team_name from epl_2009_2010 where (total_Score >= %f and win_rate >= %f)", score, rate))
  epl_2010_2011_winners = sqldf(sprintf("select team_name from epl_2010_2011 where (total_Score >= %f and win_rate >= %f)", score, rate))
  epl_2011_2012_winners = sqldf(sprintf("select team_name from epl_2011_2012 where (total_Score >= %f and win_rate >= %f)", score, rate))
  epl_2012_2013_winners = sqldf(sprintf("select team_name from epl_2012_2013 where (total_Score >= %f and win_rate >= %f)", score, rate))
  epl_2013_2014_winners = sqldf(sprintf("select team_name from epl_2013_2014 where (total_Score >= %f and win_rate >= %f)", score, rate))
  epl_2014_2015_winners = sqldf(sprintf("select team_name from epl_2014_2015 where (total_Score >= %f and win_rate >= %f)", score, rate))
  
  epl_2008_2009_winners$team_name = as.character(epl_2008_2009_winners$team_name)
  epl_2009_2010_winners$team_name = as.character(epl_2009_2010_winners$team_name)
  epl_2010_2011_winners$team_name = as.character(epl_2010_2011_winners$team_name)
  epl_2011_2012_winners$team_name = as.character(epl_2011_2012_winners$team_name)
  epl_2012_2013_winners$team_name = as.character(epl_2012_2013_winners$team_name)
  epl_2013_2014_winners$team_name = as.character(epl_2013_2014_winners$team_name)
  epl_2014_2015_winners$team_name = as.character(epl_2014_2015_winners$team_name)
  
  if(length(epl_2008_2009_winners$team_name) > 0){
    result = c(epl_2008_2009_winners$team_name)
  }
  if(length(epl_2009_2010_winners$team_name) > 0){
    result = c(result, epl_2009_2010_winners$team_name)
  }
  if(length(epl_2010_2011_winners$team_name) > 0){
    result = c(result, epl_2010_2011_winners$team_name)
  }
  if(length(epl_2011_2012_winners$team_name) > 0){
    result = c(result, epl_2011_2012_winners$team_name)
  }
  if(length(epl_2012_2013_winners$team_name) > 0){
    result = c(result, epl_2012_2013_winners$team_name)
  }
  if(length(epl_2013_2014_winners$team_name) > 0){
    result = c(result, epl_2013_2014_winners$team_name)
  }
  if(length(epl_2014_2015_winners$team_name) > 0){
    result = c(result, epl_2014_2015_winners$team_name)
  }
  return (result)
}

getWinner = function(winners){
  
  for (i in 1:length(winners)) {
    winner_team_name = "NA"
    winner_num = 0
    if(winner_num < length(winners[winners==winners[i]])){
      winner_team_name = winners[i]
    }
  }
  return (winner_team_name)
}

drawData = function(winner){
  
  winner = as.character(winner)

  epl_2008_2009_winner = sqldf(sprintf("select win_rate,total_score from epl_2008_2009 where team_name = '%s'", winner))
  epl_2009_2010_winner = sqldf(sprintf("select win_rate,total_score from epl_2009_2010 where team_name = '%s'", winner))
  epl_2010_2011_winner = sqldf(sprintf("select win_rate,total_score from epl_2010_2011 where team_name = '%s'", winner))
  epl_2011_2012_winner = sqldf(sprintf("select win_rate,total_score from epl_2011_2012 where team_name = '%s'", winner))
  epl_2012_2013_winner = sqldf(sprintf("select win_rate,total_score from epl_2012_2013 where team_name = '%s'", winner))
  epl_2013_2014_winner = sqldf(sprintf("select win_rate,total_score from epl_2013_2014 where team_name = '%s'", winner))
  epl_2014_2015_winner = sqldf(sprintf("select win_rate,total_score from epl_2014_2015 where team_name = '%s'", winner))
  
  total_win = data.frame(year="2008/2009", win_rate = epl_2008_2009_winner$win_rate*100, score=epl_2008_2009_winner$total_score)
  total_win = rbind(total_win, data.frame(year="2009/2010", win_rate = epl_2009_2010_winner$win_rate*100, score=epl_2009_2010_winner$total_score))
  total_win = rbind(total_win, data.frame(year="2010/2011", win_rate = epl_2010_2011_winner$win_rate*100, score=epl_2010_2011_winner$total_score))
  total_win = rbind(total_win, data.frame(year="2011/2012", win_rate = epl_2011_2012_winner$win_rate*100, score=epl_2011_2012_winner$total_score))
  total_win = rbind(total_win, data.frame(year="2012/2013", win_rate = epl_2012_2013_winner$win_rate*100, score=epl_2012_2013_winner$total_score))
  total_win = rbind(total_win, data.frame(year="2013/2014", win_rate = epl_2013_2014_winner$win_rate*100, score=epl_2013_2014_winner$total_score))
  total_win = rbind(total_win, data.frame(year="2014/2015", win_rate = epl_2014_2015_winner$win_rate*100, score=epl_2014_2015_winner$total_score))
  
  png(filename = "total_winners_rate.png")
  rate = paste(winner, "의 역대 승률")
  barplot(total_win$win_rate, names.arg = total_win$year, main=rate, horiz = FALSE, 
          col="blue", xlab="시즌",ylab="승률",density=seq(1:47), space=1)
  dev.off()
  
  score = paste(winner, "의 역대 승점")
  png(filename = "total_winners_score.png")
  barplot(total_win$score, names.arg = total_win$year, main=winner, horiz = FALSE, 
          col="red", xlab="시즌",ylab="승점",density=seq(1:47), space=1)
  dev.off()
  
}

# install packages
# 해당 packages가 설치되지 않았을때 아래 2줄의 주석을 지워주세요. 
# install.packages("sqldf")
# install.packages("png")

# import lib
library(sqldf)
library(png)

# set data
epl_2008_2009 = read.csv("epl_2008_2009.csv", sep = ",", header = TRUE, na.strings = c("NA","") )
epl_2008_2009[is.na(epl_2008_2009)] = c("") # remove "NA"
names(epl_2008_2009) = c("rank", "team_name", "total_game" ,"win", "draw", "lose", "goals", "lose_goals", "win_lose_goals", "total_score", "win_rate")

epl_2009_2010 = read.csv("epl_2009_2010.csv", sep = ",", header = TRUE, na.strings = c("NA","") )
epl_2009_2010[is.na(epl_2009_2010)] = c("") # remove "NA"
names(epl_2009_2010) = c("rank", "team_name", "total_game" ,"win", "draw", "lose", "goals", "lose_goals", "win_lose_goals", "total_score", "win_rate")

epl_2010_2011 = read.csv("epl_2010_2011.csv", sep = ",", header = TRUE, na.strings = c("NA","") )
epl_2010_2011[is.na(epl_2010_2011)] = c("") # remove "NA"
names(epl_2010_2011) = c("rank", "team_name", "total_game" ,"win", "draw", "lose", "goals", "lose_goals", "win_lose_goals", "total_score", "win_rate")

epl_2011_2012 = read.csv("epl_2011_2012.csv", sep = ",", header = TRUE, na.strings = c("NA","") )
epl_2011_2012[is.na(epl_2011_2012)] = c("") # remove "NA"
names(epl_2011_2012) = c("rank", "team_name", "total_game" ,"win", "draw", "lose", "goals", "lose_goals", "win_lose_goals", "total_score", "win_rate")

epl_2012_2013 = read.csv("epl_2012_2013.csv", sep = ",", header = TRUE, na.strings = c("NA","") )
epl_2012_2013[is.na(epl_2012_2013)] = c("") # remove "NA"
names(epl_2012_2013) = c("rank", "team_name", "total_game" ,"win", "draw", "lose", "goals", "lose_goals", "win_lose_goals", "total_score", "win_rate")

epl_2013_2014 = read.csv("epl_2013_2014.csv", sep = ",", header = TRUE, na.strings = c("NA","") )
epl_2013_2014[is.na(epl_2013_2014)] = c("") # remove "NA"
names(epl_2013_2014) = c("rank", "team_name", "total_game" ,"win", "draw", "lose", "goals", "lose_goals", "win_lose_goals", "total_score", "win_rate")

epl_2014_2015 = read.csv("epl_2014_2015.csv", sep = ",", header = TRUE, na.strings = c("NA","") )
epl_2014_2015[is.na(epl_2014_2015)] = c("") # remove "NA"
names(epl_2014_2015) = c("rank", "team_name", "total_game" ,"win", "draw", "lose", "goals", "lose_goals", "win_lose_goals", "total_score", "win_rate")

# call functions

print("기존의 data들을 분석하여 역대 EPL 최강의팀을 보여줍니다.")
avg_max_data = getAvgMaxData()
avg_max_data
winners = findWinners(avg_max_data)
winner = getWinner(winners)
drawData(winner)
print("EPL 최강의 팀")
winner

