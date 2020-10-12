library(readxl)
library(tidyverse)
library(expss)

defense <- read_excel('D:\\Personal Files\\School Work 2020 Fall\\Ds 202\\Lab 4\\cyclonesFootball2019.xlsx', sheet='Defensive')
offensive <- read_excel('D:\\Personal Files\\School Work 2020 Fall\\Ds 202\\Lab 4\\cyclonesFootball2019.xlsx', sheet='Offensive')
biography <- read_excel('D:\\Personal Files\\School Work 2020 Fall\\Ds 202\\Lab 4\\cyclonesFootball2019.xlsx', sheet='Biography')

#----Part 1----
#Def
defClean <- defense

defClean$Name <- factor(defClean$Name)
defClean$Opponent_Opponent <- factor(defClean$Opponent_Opponent)

defClean$Tackles_Solo <- as.numeric(defClean$Tackles_Solo)
defClean$Tackles_ASST <- as.numeric(defClean$Tackles_ASST)
defClean$Tackles_TFL <- as.numeric(defClean$Tackles_TFL)
defClean$Tackles_Sack <- as.numeric(defClean$Tackles_Sack)

defClean$Turnover_FF <- as.numeric(defClean$Turnover_FF)
defClean$Turnover_FR <- as.numeric(defClean$Turnover_FR)
defClean$Turnover_INT <- as.numeric(defClean$Turnover_INT)
defClean$Pass_QBH <- as.numeric(defClean$Pass_QBH)
defClean$Pass_PB <- as.numeric(defClean$Pass_PB)

str(defClean)

#Off
offClean <- offensive

offClean$Name <- factor(offClean$Name)
offClean$Opponent_Opponent <- factor(offClean$Opponent_Opponent)

offClean$Rushing_ATT <- as.numeric(offClean$Rushing_ATT)
offClean$Rushing_YDS <- as.numeric(offClean$Rushing_YDS)
offClean$Rushing_TD <- as.numeric(offClean$Rushing_TD)
offClean$Receiving_REC <- as.numeric(offClean$Receiving_REC)
offClean$Receiving_YDS <- as.numeric(offClean$Receiving_YDS)
offClean$Receiving_TD <- as.numeric(offClean$Receiving_TD)
offClean$Passing_YDS <- as.numeric(offClean$Passing_YDS)
offClean$Passing_TD <- as.numeric(offClean$Passing_TD)
offClean$Passing_INT <- as.numeric(offClean$Passing_INT)

str(offClean)

#Bio
bioClean <- biography

bioClean$Name <- factor(bioClean$Name)

bioClean$Weight <- as.numeric(bioClean$Weight)
bioClean$Height <- gsub("-", ".", bioClean$Height)
bioClean$Height <- as.numeric(bioClean$Height)

str(bioClean)

#----Part 2----
#1
defClean2 <- defClean
defClean <- pivot_longer(defClean, cols = c(Tackles_Solo, Tackles_ASST, Tackles_TFL, Tackles_Sack, Turnover_INT, Turnover_FR, Turnover_FF, Pass_PB, Pass_QBH), names_to = "stat", values_to = "count" )

#2
Tackles_Solo <- count_if(gt(0), defClean2$Tackles_Solo)
Tackles_ASST <- count_if(gt(0), defClean2$Tackles_ASST)
Tackles_TFL <- count_if(gt(0), defClean2$Tackles_TFL)
Tackles_Sack <- count_if(gt(0), defClean2$Tackles_Sack)
Turnover_FF <- count_if(gt(0), defClean2$Turnover_FF)
Turnover_FR <- count_if(gt(0), defClean2$Turnover_FR)
Turnover_INT <- count_if(gt(0), defClean2$Turnover_INT)
Pass_QBH <- count_if(gt(0), defClean2$Pass_QBH)
Pass_PB <- count_if(gt(0), defClean2$Pass_PB)
defCount <- data.frame(Tackles_Solo, Tackles_ASST, Tackles_TFL, Tackles_Sack, Turnover_FF, Turnover_FR, Turnover_INT, Pass_QBH, Pass_PB)
defCount <- pivot_longer(defCount, cols = c(Tackles_Solo, Tackles_ASST, Tackles_TFL, Tackles_Sack, Turnover_FF, Turnover_FR, Turnover_INT, Pass_QBH, Pass_PB), names_to = "stat", values_to = "count")
defCount <- defCount %>% arrange(count) %>% mutate(stat=factor(stat, levels=stat))
ggplot(defCount, aes(x = stat, y = count)) + geom_bar(position = 'dodge', stat='identity') + geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25)
#Answer: The most rare defensive skills are Turnover_INT, Turnover_FR, and Turnover_FF due to them having the lowest occurrences for any player.

#3
soloTI <- defClean[defClean$Opponent_Opponent == c("Iowa", "Notre Dame") & defClean$stat == "Tackles_Solo",]
ggplot(soloTI, aes(x = Name, y = count)) + geom_point() + theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) + facet_wrap(~Opponent_Opponent)
#Answer: The defense against Notre Dame was better because there is a higher count of solo tackles more often

#4
bioClean <- separate(bioClean, col = Hometown, into=c("City", "State"), sep = ",")
select(bioClean, City, State) %>% head

#5
numPlayersFromState <- data.frame(table(bioClean$State))
numPlayersFromState <- numPlayersFromState %>% arrange(Freq) %>% mutate(Var1=factor(Var1, levels=Var1))
ggplot(numPlayersFromState, aes(x = Var1, y = Freq)) + geom_bar(position = 'dodge', stat='identity') + geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

#----Part 3----
#1
defCleanStates <- left_join(defClean, bioClean, by = "Name")
defCleanStates <- select(defCleanStates, Name, State)
defCleanStates <- unique(defCleanStates[!is.na(defCleanStates$State),])
defCleanStates <- data.frame(table(defCleanStates$State))
summary(defCleanStates$Var1)

offCleanStates <- left_join(offClean, bioClean, by = "Name")
offCleanStates <- select(offCleanStates, Name, State)
offCleanStates <- unique(offCleanStates[!is.na(offCleanStates$State),])
offCleanStates <- data.frame(table(offCleanStates$State))
summary(offCleanStates$Var1)
#*Answer: 
#*The defense team is from Ark., Calif., Fla., Ga., Iowa, Kan., Mich., Mo,. Ohio., Okla., Texas, Wis.
#*The offense team is from Ariz., Ark., Fla., Ga., Ill., Iowa, Kan., Mo., Ohio, Okla., Texas

#2
offClean <- pivot_longer(offClean, cols = c(Rushing_ATT, Rushing_TD, Rushing_YDS, Receiving_TD, Receiving_YDS, Receiving_REC, Passing_YDS, Passing_TD, Passing_INT), names_to = "stat", values_to = "count" )
BrockPurdy <- offClean[offClean$Name == "Purdy, Brock",]

BPperformance <- BrockPurdy[BrockPurdy$stat == "Passing_TD",]
ISUperformance <- offClean[offClean$stat == "Receiving_TD" & !is.na(offClean$count),]
ISUperformance2 <- defClean[defClean$stat == "Tackles_Solo" & !is.na(defClean$count),]

ggplot(BPperformance, aes(x = Opponent_Opponent, weight = count)) + geom_bar() + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplot(ISUperformance, aes(x = Name, weight = count)) + geom_bar() + theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) + facet_wrap(~Opponent_Opponent)
ggplot(ISUperformance2, aes(x = Name, weight = count)) + geom_bar() + theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) + facet_wrap(~Opponent_Opponent)
#*Answer:
#*You can see in the games where Brock Purdy performs better (more touchdown passes), the rest of the offensive team also performs well (more receiving touch downs) and when Brock performs worse (less touchdown passes), the rest of the offensive team also performs worse (less receiving touch downs)
#*But Brock's performance does not relate to the defensive teams performance as how well the defensive team does does not correlate with Brock's performance.

#3
defense2018 <- read_excel('D:\\Personal Files\\School Work 2020 Fall\\Ds 202\\Lab 4\\cyclonesFootball2018.xlsx', sheet='Defensive')
offensive2018 <- read_excel('D:\\Personal Files\\School Work 2020 Fall\\Ds 202\\Lab 4\\cyclonesFootball2018.xlsx', sheet='Offensive')

#Def
defClean2018 <- defense2018
defClean2018$Name <- factor(defClean2018$Name)
defClean2018$Opponent_Opponent <- factor(defClean2018$Opponent_Opponent)
defClean2018$Tackles_Solo <- as.numeric(defClean2018$Tackles_Solo)
defClean2018$Tackles_ASST <- as.numeric(defClean2018$Tackles_ASST)
defClean2018$Tackles_TFL <- as.numeric(defClean2018$Tackles_TFL)
defClean2018$Tackles_Sack <- as.numeric(defClean2018$Tackles_Sack)
defClean2018$Turnover_FF <- as.numeric(defClean2018$Turnover_FF)
defClean2018$Turnover_FR <- as.numeric(defClean2018$Turnover_FR)
defClean2018$Turnover_INT <- as.numeric(defClean2018$Turnover_INT)
defClean2018$Pass_QBH <- as.numeric(defClean2018$Pass_QBH)
defClean2018$Pass_PB <- as.numeric(defClean2018$Pass_PB)
defClean2018 <- pivot_longer(defClean2018, cols = c(Tackles_Solo, Tackles_ASST, Tackles_TFL, Tackles_Sack, Turnover_INT, Turnover_FR, Turnover_FF, Pass_PB, Pass_QBH), names_to = "stat", values_to = "count" )

defClean2019Stats <- select(defClean, Name, stat, count, Opponent_Opponent)
defClean2018Stats <- select(defClean2018, Name, stat, count, Opponent_Opponent)
defCleanStats <- left_join(defClean2018Stats, defClean2019Stats, by = c("Name", "stat", "Opponent_Opponent"), suffix = c("2018", "2019"))
defCleanStats <- defCleanStats[defCleanStats$stat == "Tackles_Solo",]

defCleanStats <- defCleanStats %>% group_by(Name) %>% summarise(c2018 = mean(count2018), c2019 = mean(count2019, na.rm = TRUE))
defCleanStats <- pivot_longer(defCleanStats, cols = c(c2018, c2019), names_to = "year", values_to = "count")
ggplot(defCleanStats, aes(x = Name, weight = count, fill = year)) + geom_bar(position = 'dodge') + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

#Off
offClean2018 <- offensive2018
offClean2018$Name <- factor(offClean2018$Name)
offClean2018$Opponent_Opponent <- factor(offClean2018$Opponent_Opponent)
offClean2018$Rushing_ATT <- as.numeric(offClean2018$Rushing_ATT)
offClean2018$Rushing_YDS <- as.numeric(offClean2018$Rushing_YDS)
offClean2018$Rushing_TD <- as.numeric(offClean2018$Rushing_TD)
offClean2018$Receiving_REC <- as.numeric(offClean2018$Receiving_REC)
offClean2018$Receiving_YDS <- as.numeric(offClean2018$Receiving_YDS)
offClean2018$Receiving_TD <- as.numeric(offClean2018$Receiving_TD)
offClean2018$Passing_YDS <- as.numeric(offClean2018$Passing_YDS)
offClean2018$Passing_TD <- as.numeric(offClean2018$Passing_TD)
offClean2018$Passing_INT <- as.numeric(offClean2018$Passing_INT)
offClean2018 <- pivot_longer(offClean2018, cols = c(Rushing_ATT, Rushing_TD, Rushing_YDS, Receiving_TD, Receiving_YDS, Receiving_REC, Passing_YDS, Passing_TD, Passing_INT), names_to = "stat", values_to = "count" )

offClean2019Stats <- select(offClean, Name, stat, count, Opponent_Opponent)
offClean2018Stats <- select(offClean2018, Name, stat, count, Opponent_Opponent)
offCleanStats <- left_join(offClean2018Stats, offClean2019Stats, by = c("Name", "stat", "Opponent_Opponent"), suffix = c("2018", "2019"))
offCleanStats <- offCleanStats[offCleanStats$stat == "Receiving_YDS",]
offCleanStats <- offCleanStats %>% group_by(Name) %>% summarise(c2018 = mean(count2018), c2019 = mean(count2019, na.rm = TRUE))
offCleanStats[is.na(offCleanStats)] <- 0
offCleanStats <- pivot_longer(offCleanStats, cols = c(c2018, c2019), names_to = "year", values_to = "count")
ggplot(offCleanStats, aes(x = Name, weight = count, fill = year)) + geom_bar(position = 'dodge') + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))