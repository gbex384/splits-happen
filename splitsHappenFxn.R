##Philip Coyne
##January 6th, 2018
##Splits-Happen Excercise for EITC
##Prompt at https://github.com/EITC-IRPD/splits-happen


##Assumptions:
#--------------------------------------------------------------------------------------------------------------------------------
#1.  Program is intended to prompt a player to give a score, not a text file.  Can be re-written to be take in a text file
#2.  Given that it is considered "Out of Scope" to check for valid rolls, correct number of rolls and frame
#    and that there is no need to update players on scores for intermediate frames, it is assumed that the only importatn
#    data points will be digits, "X", "/", and "-" that follow a distinct pattern.  
#    Therefore, inputs like "One", "Two","three", etc. will be disregarded.
#3.  Given the scoring logic, it's determined that a "-" and a "/" or just a "/" can be considered a miss, followed by knocking
#    all pins down.  This will be coded appropriately.  
#--------------------------------------------------------------------------------------------------------------------------------

##Description:
#--------------------------------------------------------------------------------------------------------------------------------
#1.  Program will take a user input for as long as it is "played".  
#2.  When user finishes, program will parse over input and split into strings, considered "Games"; following logic set forth by the prompt given in the README.txt at https://github.com/gbex384/splits-happen.
#3.  The "Games" will be stored into a data frame, with a "Game Number", "Game Results", and a "Score" for each "Game".
#4.  The "Games" will be split into frames, as appropriate, and these frames will be split into rolls.
#5.  Scoring Logic will be applied to the rolls and stored into a variable that will sum the score.
#6.  The score will then be placed into the data frame noted in Step 4.
#7.  Program will output the score from each "Game".
#8.  Program will end.
#--------------------------------------------------------------------------------------------------------------------------------


splitsHappen<-function(){
##Suppress svDialogs warning of package ‘svDialogs’ was built under R version 3.2.5 
options(warn=-1)
require(svDialogs)
require(stringr)
#1.  Program will take a user input for as long as it is "played".  
#--------------------------------------------------------------------------------------------------------------------------------

library(svDialogs)
scoreKeeper<-as.character()
rollResult<-""

##Prompt Player for Score, store result
while (rollResult!="Exit"){
  rollResult<-dlgInput(message= "Enter a value (type Exit to Exit)")$res
  scoreKeeper<-rbind(scoreKeeper,rollResult)
}
#--------------------------------------------------------------------------------------------------------------------------------
#2.  When user finishes, program will parse over input and split into strings, considered "Games"; 
#    following logic set forth by the prompt given in the README.txt at https://github.com/gbex384/splits-happen.
#--------------------------------------------------------------------------------------------------------------------------------

##Take all results, concatenate into one string
scoreString<-paste(as.vector(scoreKeeper[,1]),collapse = "")

##Load stringr, unload svDialogs
library(stringr)
detach("package:svDialogs",unload=TRUE)

##Find all strings characters related to valid scores, output into vector
allScores<-str_extract_all(scoreString,"(([[:digit:]])|(-)|(\\/)|X)*")
allScoresVect<-paste(allScores[[1]],collapse="")

##Separate into games (One can find test cases used at end of this script)
gameResults<-str_extract_all(allScoresVect,"((([[:digit:]][[:digit:]])|(X)|(\\/)|(-)(-)|[[:digit:]]-|[[:digit:]]\\/|-[[:digit:]]){9})(X\\/|X.{2}|\\/.{1}|([[:digit:]][[:digit:]])|(-)(-)|-[[:digit:]]|[[:digit:]]-|[[:digit:]]\\/.{1})")
#--------------------------------------------------------------------------------------------------------------------------------
#3.  The "Games" will be stored into a data frame, with a "Game Number", "Game Results", and a "Score" for each "Game".
#--------------------------------------------------------------------------------------------------------------------------------
scoreAggr<-as.data.frame(matrix(ncol=3))
colnames(scoreAggr)<-c("Game Number","Game Results","Score")
for(i in 1:length(gameResults[[1]])){
  
  ##Create records to store information for human readability
  scoreAggr[i,1]<-i
  scoreAggr[i,2]<-gameResults[[1]][i]
  
  ##Begin Scoring Logic Section
  #--------------------------------------------------------------------------------------------------------------------------------
  #4.  The "Games" will be split into frames, as appropriate, and these frames will be split into rolls.
  #--------------------------------------------------------------------------------------------------------------------------------
  frameResults<-str_extract_all(list(scoreAggr[i,2]),"(([[:digit:]][[:digit:]])|(X)|(\\/)|(-)(-)|[[:digit:]]-|[[:digit:]]\\/|-[[:digit:]])|[[:digit:]]")[[1]]
  
  ##Make frames human readable
  ##Initialize Vector to hold all rolls made during a game, create for loop to split into single rolls for scoring purposes
  playerRolls<-as.character()
  for (j in 1:length(frameResults)){
    
    ##Initialize dataframe to store roll results
    frameScore<-str_extract_all(frameResults[j],"([[:digit:]]|(X)|(\\/)|(-)|[[:digit:]])")[[1]]
    if(length(frameScore)<2&&frameScore=='/'){
     frameScore<-"0/"
     frameScore<-str_extract_all(frameScore,"([[:digit:]]|(X)|(\\/)|(-)|[[:digit:]])")[[1]]
    }
    playerRolls<-c(playerRolls,frameScore)
  }
  #--------------------------------------------------------------------------------------------------------------------------------
  #5.  Scoring Logic will be applied to the rolls and stored into a variable that will sum the score.
  #--------------------------------------------------------------------------------------------------------------------------------
  
  ##Begin Assign Character Number Values to Characters
  ##"-" will have value of 0
  playerRollsv1<-gsub("-","0",playerRolls)
  ##Add two zeros for case of Strikes in the last frame of game
  playerRollsv1[(length(playerRollsv1)+1):(length(playerRollsv1)+2)]<-0
  totalScore<-as.numeric()
  
  ##Transform characters into values
  for(k in 1:length(playerRollsv1)){
  
   
    ##Include -4 to accomodate for case when a strike is thrown at the 10th frame; 2 for the extra zeros added, 2 for the last free throws.
    if (playerRollsv1[k]=="X"&&k<=length(playerRollsv1)-4){
      ##"X" will have value of 10+Next Roll + Roll After Next Roll
      totalScore<-rbind(totalScore,10+ifelse(playerRollsv1[k+1]=="X",10,as.numeric(playerRollsv1[k+1]))+ifelse(playerRollsv1[k+2]=="/",10-as.numeric(playerRollsv1[k+1]),ifelse(playerRollsv1[k+2]=="X",10,as.numeric(playerRollsv1[k+2]))))
      
      ##Include -3 to accomodate for case when a spare is thrown at the 10th frame; 2 for the extra zeros added, 1 for the last free throw.
          }else if (playerRollsv1[k]=="/"&&k<=length(playerRollsv1)-3){
      
            ##"/" will have value of (10-Previous Numeric Roll)+Next Roll
            totalScore<-rbind(totalScore,10-as.numeric(playerRollsv1[k-1])+ifelse(playerRollsv1[k+1]=="X",10,as.numeric(playerRollsv1[k+1])))
            }else if (!is.na(as.numeric(playerRollsv1[k]))&&k<=length(playerRollsv1)-4){
              ##Numerics will have same value as character representation
              totalScore<-rbind(totalScore,as.numeric(playerRollsv1[k]))
    
              ##Logic for the bonus throws
            }else if (playerRollsv1[k]=="X"){
                totalScore<-rbind(totalScore,0)
            }else if (playerRollsv1[k]=="/"){
              totalScore<-rbind(totalScore,0)
            }else if (!is.na(playerRollsv1[k])||((playerRollsv1[(k-1)]=="X")||(playerRollsv1[(k-2)]=="X"))||((playerRollsv1[(k-2)]=="/")||(playerRollsv1[(k-1)]=="/"))){
                if((((playerRollsv1[(k-1)]=="X")||(playerRollsv1[(k-2)]=="X"))||((playerRollsv1[(k-2)]=="/")||(playerRollsv1[(k-1)]=="/")))==FALSE){
                  totalScore<-rbind(totalScore,as.numeric(playerRollsv1[k]))
                }
              totalScore<-rbind(totalScore,0)
            }else{
              if(playerRollsv1[k-1]=="\\/"){
                totalScore<-rbind(totalScore,0)
              }else{
                totalScore<-rbind(totalScore,as.numeric(playerRollsv1[k]))
              }
              }

  }
  #--------------------------------------------------------------------------------------------------------------------------------
  
  #6.  The score will then be placed into the data frame noted in Step 4.
  #--------------------------------------------------------------------------------------------------------------------------------
  scoreAggr[i,3]<-sum(totalScore)
  #--------------------------------------------------------------------------------------------------------------------------------
  
}

#7.  Program will output the score from each "Game".
#--------------------------------------------------------------------------------------------------------------------------------
print(scoreAggr[,c(1,3)])
#--------------------------------------------------------------------------------------------------------------------------------

#8.  Program will end.
#--------------------------------------------------------------------------------------------------------------------------------
print("Thanks for Playing")
##Turn warnings back on
options(warn=0)
detach("package:stringr",unload=TRUE)
}
#--------------------------------------------------------------------------------------------------------------------------------

##Please run splitsHappen() in R terminal

##Test Cases for reading "Raw Data"
#-----------------------------------------------------------------------------------------------
##Test case 1 (Should read the entire up to X/ (Score of 117)
allScoresVect<-'XX4/9-2340192301X/X'
##Test case 2 (Should read up to the second to last character, IE last / and not the X (Score of 107)
allScoresVect<-'XX4/9-2340192301//X'
##Test case 3 (Should only read up to 8 (Score of 105))
allScoresVect<-'XX4/9-2340192301-8/X'

##Test Case 4 (First game ending at X/ (Score of 117), Second Game ending at 01 (Score of 127), third game ending at 19 (Score of 149))
allScoresVect<-'XX4/9-2340192301X/XXX4/9-2340192301-8/XXX4/9-2340192301//X'

##Test Case 5 (Should split into3 games, first game ends at -8 (score of 105), second game ends at 23 (Score of 146),with the third game ending at 2340 (Score of 142))
allScoresVect<-'XX4/9-2340192301-8/XXX4/9-2340192301//XXX4/9-2340192301X/X'

##Test Case 6 (Should split into 4 games, and should match output in Validation Test Cases)
allScoresVect<-'XXXXXXXXXXXX9-9-9-9-9-9-9-9-9-9-5/5/5/5/5/5/5/5/5/5/5X7/9-X-88/-6XXX81'

#-----------------------------------------------------------------------------------------------









##Misc Notes from beginning of project, moved to the side

#-----------------------------------------------------------------------------------------------

##Determine how many games there "appears to be"
# numberofGames<-length(str_extract_all(allScoresVect,"(([[:digit:]][[:digit:]])|(X)|(\\/)|(-)(-)|[[:digit:]]-|[[:digit:]]\\/|-[[:digit:]]){10}")[[1]])
# 
# for(i in 1:numberofGames){
#   
#   ##Look at first obvious game
#   gameResults<-str_split(allScoresVect,"(?<=(([[:digit:]][[:digit:]])|(X)|(\\/)|(-)(-)|[[:digit:]]-|[[:digit:]]\\/|-[[:digit:]]){10})")[[1]]
# 
#   ##Should the tenth frame have a strike or 
#     if(substr(gameResults[1],nchar(gameResults[1]),nchar(gameResults[1]))=="/"){
#     tallyResults<-paste(gameResults[1],gameResults[1+1],sep="")
#     gameResults<-gameResults[-1:-2]
#   }else if (substr(gameResults[1],nchar(gameResults[1]),nchar(gameResults[1]))=="X"){
#     tallyResults<-paste(gameResults[1],gameResults[1+1],gameResults[1+2],sep="")
#     gameResults[-1:-3]
#   }
#   
# }

##Keep aggregating into games, until end of file



##Use regex or parse through string to find a total of 10 cases, as indicated by rules above (will get tricky regarding 10th frame)

##Alternative idea: Parse through input, one character at a time,
##Construct Program around it to verify numeric, '/', '-', or 'X'
##Given what is found, hold value if '/' or 'X', and double count next read value
##and/or value after.  Sum results, store into a score, discard held values
##This might be considered out of scope though....

#-----------------------------------------------------------------------------------------------





##Initial Notes:

##High-level:  Create a program that  can emulate a single player 
##Bowling.  

##Player will roll twice during a frame, against 10 pins
##If the player hits the strike, then 

##Given Validation Test Cases, the program itself should receive
##an input of type varchar(X,-,/, 1,2,3,4,5,6,7,8,9,10)


##Given Test Case 4, the program only needs to follow 
##the logic set-forth by requirements, even if it doesn't
##follow standard rules of Bowling.

##A Line is a Game
##A Frame is 10 Turns (Is a Turn a Frame?  Assuming
##it is)

##Things that are out of scope
#Check for "Valid Roll" (clarification might be needed)
#Check for correct number of rolls/frames
#Provide Scores before the game is over

##Out of scope to check for correct number of rolls
##indicates that the program should be using current
##value to gauge whether or not the game is over

##Under the impression that it's expected for a player
##to input something like "1", "One", or the beginning paragarph
#of War and Peace.  Given that it is considered "out of scope"
#to check for valid rolls, will design program to store only scores
#that are of type numeric
 