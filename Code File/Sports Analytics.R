library(dplyr)
library(tidyr)
library(ggplot2)

bdmain <- main %>% filter(team == "Bangladesh" | team2 == "Bangladesh")

summary(bdmain)

shakib <- main %>% filter(Striker == "Shakib Al Hasan")

shakib <- shakib %>% select(Match_Id, team, team2, winner, Overs, Striker, Non_Striker, Bowler, Run_Scored, Dismissal)

shakib <- shakib %>% separate(Overs, into = c("Over_No", "Ball_no"))

shakib_temp <- shakib %>% gather(key = Role, value = Team, team:team2) %>% filter(Team != "Bangladesh") %>% group_by(Team) %>% summarize(count = n()) %>% ungroup(Team) %>% mutate(Percetange = count/sum(count)*100) 

write.csv(shakib_temp, "countries.csv")

partner <- shakib %>% group_by(Non_Striker) %>% summarise(count = n()) %>% ungroup(Non_Striker) %>% mutate(Percentage = count/sum(count)) %>% arrange(desc(Percentage))

write.csv(partner, "partner.csv")

shakib %>% mutate(runs = as.factor(Run_Scored)) %>% ggplot(aes(x = runs, fill = runs)) + geom_bar()

shakib %>% filter(Dismissal != "") %>% mutate(Dismissal = as.factor(Dismissal)) %>% ggplot(aes(x = Dismissal, fill = Dismissal)) + geom_bar()

bowlers <- outs %>% group_by(Bowler) %>% count(Bowler)

team_outs <- shakib %>% gather(key = Role, value = Team, team:team2) %>% filter(Team != "Bangladesh") %>% filter(Dismissal != "") %>% count(Team, Dismissal) %>% spread(Dismissal, n)

team_outs[is.na(team_outs)] <- 0

write.csv(team_outs, "team_outs.csv")

shakib %>% group_by(Over_No) %>% summarize(run_rate = sum(Run_Scored)/6) %>% arrange(as.numeric(Over_No)) %>% ggplot(aes(x = as.numeric(Over_No), y = run_rate)) + geom_smooth(se = F) 

boundaries <- shakib %>% filter(Run_Scored == 6 | Run_Scored == 4) %>% count(Bowler) %>% arrange(desc(n)) %>% filter(n >= 4)

write.csv(boundaries, "boundaries.csv")

shakib %>% gather(key = Role, value = Team, team:team2) %>% filter(Team != "Bangladesh") %>% filter(Run_Scored == 6 | Run_Scored == 4) %>% group_by(Team) %>% summarise(Avg_Boundaries = mean(Run_Scored, na.rm = T)) %>% arrange(desc(Avg_Boundaries)) %>% filter(Avg_Boundaries >= 4.35) %>% ggplot(aes(x = Team, y= Avg_Boundaries, fill = Team)) + geom_col() 

balls_team <- shakib %>% gather(key = Role, value = Team, team:team2) %>% filter(Team != "Bangladesh") %>% group_by(Match_Id, Team) %>% summarize(Balls = n(), Total_Runs = sum(Run_Scored))

team_runs <- balls_team %>% group_by(Team) %>% summarize(Appearance = n(), Total_Runs = sum(Total_Runs)) %>% mutate(Average_Runs = Total_Runs/Appearance) %>% arrange(desc(Average_Runs)) %>% select(Team, Appearance, Average_Runs)

write.csv(team_runs, "team_runs.csv")

bowler_score <- shakib %>% group_by(Bowler) %>% summarize(Balls_faced = n(), Total_Runs = sum(Run_Scored), Run_rate = Total_Runs/(Balls_faced/6)) %>% filter(Balls_faced > 11, Run_rate > 6)

write.csv(bowler_score, "bowler_score.csv")

#Prediction Modeling

shakib_pred <- shakib %>% gather(key = Role, value = Team, team:team2) %>% filter(Team != "Bangladesh") %>% select(-Match_Id, -winner, -Striker, -Role)

shakib_pred <- shakib_pred %>% mutate(Over_No = as.factor(Over_No), Ball_no = as.factor(Ball_no), Non_Striker = as.factor(Non_Striker), Bowler = as.factor(Bowler), Run = ifelse(Run_Scored <1, "Dot", ifelse(Run_Scored <4, "1, 2 or 3", "4 or 6")), Dismissal = as.factor(Dismissal))

levels(shakib_pred$Dismissal)[1] <- "Not Out"

shakib_pred$Out_NotOut <- ifelse(shakib_pred$Dismissal != "Not Out", "Out", "Not Out")

#Tree model on Runs Scored

library(rpart)
library(rpart.plot)

shakib_pred <- na.omit(shakib_pred)

shakib_pred2 <- shakib_pred %>% filter(Run != "Dot" & Ball_no %in% c("1", "2", "3", "4", "5", "6"))

shakib_pred2 <- shakib_pred2 %>% mutate(Team = as.factor(Team), Run = as.factor(Run), Out_NotOut = as.factor(Out_NotOut))


run_model <- rpart(Run ~ Over_No + Ball_no + Non_Striker + Team + Bowler, data = shakib_pred2)

rpart.plot(run_model)

plotcp(run_model)

run_model2 <- prune(run_model, cp = 0.038)

rpart.plot(run_model2)

shakib_pred3 <- shakib_pred %>% filter(Ball_no %in% c("1", "2", "3", "4", "5", "6"))

shakib_pred3 <- shakib_pred3 %>% mutate(Team = as.factor(Team), Run = as.factor(Run), Out_NotOut = as.factor(Out_NotOut))

out_model <- rpart(Out_NotOut ~ Over_No + Ball_no + Team, data = shakib_pred3, maxdepth = 3)

rpart.plot(out_model)

plotcp(out_model)

out_model2 <- prune(out_model, maxdepth = 3)

rpart.plot(out_model2)

library(RWeka)

out_rule <- OneR(Out_NotOut ~ Over_No + Ball_no + Team ,data = shakib_pred3 )

summary(out_rule)

out_rule

shakib_pred3 <- shakib_pred3 %>% filter(Out_NotOut != "Not Out")

out_model <- rpart(Dismissal ~ Over_No + Ball_no + Team, data = shakib_pred3)

rpart.plot(out_model)

out_rule <- JRip(Dismissal ~ Over_No + Ball_no + Team, data = shakib_pred3)

out_rule
