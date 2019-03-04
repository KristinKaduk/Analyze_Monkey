

### KK - april 2017
##########################
library("ggplot2")
library("lme4", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gridExtra")
library("scales", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("dbplyr")
library("dplyr")
library("scales")

rm(list=ls())
starting_folder = "20181217" # 20181107
ending_folder   = "20181217" #"20181122"
monkey          = "Nor"
setwd(paste0("/Users/kristinkaduk/Dropbox/promotion/Projects/Wagering_Monkey/results/",monkey))
table = read.table(paste0(monkey, "M2S_psychophysicTask_since",starting_folder,"_until_",ending_folder,".txt"), sep = ",", header = TRUE)

table$DifficultyLevel = abs(table$Rotation_t1 - table$Rotation_t2)

### 
# training: 20180924 - 
# training: 20181013 20181022 - 

##
head(table)
names(table)


Tabl_Perfor_Date   = c(); 
Tabl_Perfor_Diff   = c(); 
Tabl_Perfor_Sample = c();
Table_Diff_Sample  = c();
Tabl_Diff_Sample_NonMatch= c();




for(i_Sess in 1: length( unique(table$Date))){
Data = c(); NrTrial_Sample = c(); NrTrial_Diff= c();
Data = table[table$Date == unique(table$Date)[i_Sess], ]

#taSample =  Data %>% group_by(Rotation_Sample) %>%  summarise(Accuracy = mean(Accuracy))



if(sum(Data$completed) > 100){ 
  
  ############################################################################
  ## General Performance Per Session
  #############################################################
Accuracy                         = round(sum(Data$success == 1) / sum(Data$completed),3)
Nr_completed                     = sum(Data$completed)
PositionPreference_Left = round(sum(Data$PosTar_ChoosenOption[Data$completed == 1] == 1) /sum(Data$completed,  na.rm = T), 3)
PositionPreference_Right= round(sum(Data$PosTar_ChoosenOption[Data$completed == 1] == 2) /sum(Data$completed,  na.rm = T), 3)
# Error types - Frequency for each type of error code
AbortState = c(); Freq_AbortState= c();
for (i_ErrorTypes in 1: length(unique(table$abort_code))){
  AbortState[i_ErrorTypes] = levels(Data$abort_code)[i_ErrorTypes]
  Freq_AbortState[i_ErrorTypes] = sum(Data$abort_code == levels(Data$abort_code)[i_ErrorTypes])/ length(Data$abort_code)
}

#names(T_Date)[7:length(names(T_Date))]
###############################################################################
## Performance Per Difficulty Level / Samples / Samples per Difficulty Level
###############################################################################
#if(Data$Task_type[1] == 9  Data$Task_type[1] ==  10){ 
  perc_ResponseTime                = mean(Data$M2S_ResponseTime, na.rm = T)
  correct_ResponseTime             = mean(Data$M2S_ResponseTime[Data$completed == 1 & Data$success == 1], na.rm = T)
  error_ResponseTime              = mean(Data$M2S_ResponseTime[Data$completed == 1 & Data$success == 0], na.rm = T)  
  
NrTrial_Sample = matrix(NA,nrow=length(sort(unique(Data$Rotation_Sample))) ,ncol=1)
for (i_Sample in 1: length(unique(Data$Rotation_Sample))){ 
NrTrial_Sample[i_Sample] = sum(Data$Rotation_Sample == sort(unique(Data$Rotation_Sample))[i_Sample])
}

AccuracyPerDifficultyPerSample = matrix(NA,nrow= length(unique(Data$Rotation_Sample))*length(unique(Data$Rot_Diff)),ncol=5)
Acc_DifficultyLevel            =  matrix(NA,nrow=length(unique(Data$Rot_Diff)),ncol=1)
RT_DifficultyLevel             =  matrix(NA,nrow=length(unique(Data$Rot_Diff)),ncol=1)
PositionPreference_Left_DifficutlyLevel =  matrix(NA,nrow=length(unique(Data$Rot_Diff)),ncol=1)
PositionPreference_Right_DifficutlyLevel =  matrix(NA,nrow=length(unique(Data$Rot_Diff)),ncol=1)
RT_Correct_DifficultyLevel          =  matrix(NA,nrow=length(unique(Data$Rot_Diff)),ncol=1)
RT_Incorrect_DifficultyLevel          =  matrix(NA,nrow=length(unique(Data$Rot_Diff)),ncol=1)

DifficultyLevel_rotat          =  matrix(NA,nrow=length(unique(Data$Rot_Diff)),ncol=1)
DifficultyLevel                =  matrix(NA,nrow=length(unique(Data$Rot_Diff)),ncol=1)
AccuracyPerSample              = matrix(NA,nrow=length(unique(Data$Rotation_Sample)),ncol=1)
choice_PreferenceRight_PerSample              = matrix(NA,nrow=length(unique(Data$Rotation_Sample)),ncol=1)


ResponseTimePerSample          = matrix(NA,nrow=length(unique(Data$Rotation_Sample)),ncol=1)
NrTrial_Diff                  = matrix(NA,nrow=length(unique(Data$Rot_Diff)),ncol=1)
# Difficulty Level 1: 20 16 12  8  4 -> ex.: 4 - difficult - difficulty level 5
#i_Diff = 1; i_Sample = 1
C_Sam_Diff = 1


############################################################################
## Performance Per Sample per Non-Match
#############################################################
AccuracyPerSamplePerNonMatch   = matrix(NA,nrow= length(unique(Data$Rotation_Sample))*length(unique(Data$Rot_Diff))*2,ncol=6)
# Difficulty Level 1: 20 16 12  8  4 -> ex.: 4 - difficult - difficulty level 5
#i_Diff = 1; i_Sample = 1;  i_NonMatch = ; 
C_Sam_Diff = 1
for (i_Diff in 1: length(unique(Data$Rot_Diff))){
  NrTrial_Diff[i_Diff]           = sum(Data$Rot_Diff == rev(sort(unique(Data$Rot_Diff)))[i_Diff]) # from easy to difficult
  DataDiff = Data[Data$Rot_Diff == rev(sort(unique(Data$Rot_Diff)))[i_Diff], ]
  
  Acc_DifficultyLevel[i_Diff ]  = round(sum(subset(Data,Data$Rot_Diff == rev(sort(unique(Data$Rot_Diff)))[i_Diff])$success)/sum(subset(Data,Data$Rot_Diff == rev(sort(unique(Data$Rot_Diff)))[i_Diff])$completed),3)  #easy
  RT_DifficultyLevel[i_Diff ]   =  round(mean(DataDiff$M2S_ResponseTime, na.rm = T),3)  #easy
  DifficultyLevel[i_Diff ]      = i_Diff # start with 1 - easy 
  DifficultyLevel_rotat[i_Diff ]= DataDiff$Rot_Diff[1] # start with 1 - easy 
  PositionPreference_Left_DifficutlyLevel[i_Diff ] = round(sum(DataDiff$PosTar_ChoosenOption[DataDiff$completed == 1] == 1) /sum(DataDiff$completed,  na.rm = T), 3)
  PositionPreference_Right_DifficutlyLevel[i_Diff ] = round(sum(DataDiff$PosTar_ChoosenOption[DataDiff$completed == 1] == 2) /sum(DataDiff$completed,  na.rm = T), 3)
  RT_DifficultyLevel[i_Diff ]             =  round(mean(DataDiff$M2S_ResponseTime, na.rm = T),3)  #easy
  RT_Correct_DifficultyLevel[i_Diff ]     =  round(mean(DataDiff$M2S_ResponseTime[DataDiff$success == 1], na.rm = T),3)  #easy
  RT_Incorrect_DifficultyLevel[i_Diff ]   =  round(mean(DataDiff$M2S_ResponseTime[DataDiff$success == 0], na.rm = T),3)  #easy
  
  for (i_Sample in 1: length(sort(unique(DataDiff$Rotation_Sample)))){
    DataSample = DataDiff[DataDiff$Rotation_Sample == rev(unique(sort(DataDiff$Rotation_Sample)))[i_Sample], ]
    choice_PreferenceRight_PerSample[i_Sample, 1 ]   = round(sum(DataSample$target_selected_Hand[DataSample$completed ==1] ==2, na.rm = T)/ sum(DataSample$completed,  na.rm = T),3) # left = 1; right = 2
    
    for (i_NonMatch in 1: length(unique(DataSample$Rotation_t2))){
      
    DataNonMatch = subset(DataSample,DataSample$Rotation_t2 == sort(unique(DataSample$Rotation_t2))[i_NonMatch])
    DataSample_unsuccess = subset(DataNonMatch,DataNonMatch$success == 0)
    
    AccuracyPerSamplePerNonMatch[C_Sam_Diff, 1] = round(sum(DataNonMatch$success)/sum(DataNonMatch$completed),3)
    AccuracyPerSamplePerNonMatch[C_Sam_Diff, 2 ] = unique(DataNonMatch$Rot_Diff)
    AccuracyPerSamplePerNonMatch[C_Sam_Diff, 3 ] = unique(DataNonMatch$Rotation_Sample)
    AccuracyPerSamplePerNonMatch[C_Sam_Diff, 4 ] = unique(DataNonMatch$Rotation_t2)
    AccuracyPerSamplePerNonMatch[C_Sam_Diff, 5] = round(mean(DataNonMatch$M2S_ResponseTime, na.rm = T),3)
    AccuracyPerSamplePerNonMatch[C_Sam_Diff, 6] = length(DataNonMatch$completed) 
    
    
    AccuracyPerSample[i_Sample, 1 ] = round(sum(subset(Data,Data$Rotation_Sample == sort(unique(Data$Rotation_Sample))[i_Sample])$success)/sum(subset(Data,Data$Rotation_Sample == sort(unique(Data$Rotation_Sample))[i_Sample])$completed),3)
    ResponseTimePerSample[i_Sample, 1 ] = round(mean(DataSample$M2S_ResponseTime, na.rm = T),3)  #eas
    choice_PreferenceRight_PerSample[i_Sample, 1 ]   = round(sum(DataSample$target_selected_Hand[DataSample$completed ==1] ==2, na.rm = T)/ sum(DataSample$completed,  na.rm = T),3) # left = 1; right = 2
    
    C_Sam_Diff = C_Sam_Diff+1
    }
  }
}


T_Date   = data.frame(Data$Monkey[1],Data$Date[1], Accuracy, perc_ResponseTime, correct_ResponseTime, error_ResponseTime,Nr_completed, PositionPreference_Right, PositionPreference_Left, AbortState, Freq_AbortState)


T_Diff = data.frame(rep(Data$Monkey[1], length(unique(Data$Rot_Diff))) ,rep(Data$Date[1], length(unique(Data$Rot_Diff))) ,DifficultyLevel ,DifficultyLevel_rotat,  NrTrial_Diff,  Acc_DifficultyLevel, RT_DifficultyLevel, RT_Incorrect_DifficultyLevel, RT_Correct_DifficultyLevel, PositionPreference_Right_DifficutlyLevel, PositionPreference_Left_DifficutlyLevel)
T_Sample     = data.frame(rep(Data$Monkey[1], length(unique(Data$Rotation_Sample))) ,rep(Data$Date[1], length(unique(Data$Rotation_Sample))) ,1:length(sort(unique(Data$Rotation_Sample))) , sort(unique(Data$Rotation_Sample)),  AccuracyPerSample, NrTrial_Sample, ResponseTimePerSample, choice_PreferenceRight_PerSample)
T_Diff_Sample = data.frame(rep(DataDiff$Monkey[1], length(unique(DataDiff$Rotation_Sample))) ,rep(DataDiff$Date[1], length(unique(DataDiff$Rotation_Sample))) ,1:length(sort(unique(DataDiff$Rotation_Sample))) , sort(unique(DataDiff$Rotation_Sample)), AccuracyPerDifficultyPerSample) 
#T_Diff_Sample = melt(T_Diff_Sample, id.vars=c("subject", "sex"))
T_Diff_Sample_NonMatch = data.frame(rep(DataDiff$Monkey[1], length(AccuracyPerSamplePerNonMatch[,1 ])) ,rep(DataDiff$Date[1], length(AccuracyPerSamplePerNonMatch[,1 ])) ,1:length(AccuracyPerSamplePerNonMatch[,1 ]) , AccuracyPerSamplePerNonMatch) 






Tabl_Perfor_Diff = rbind(Tabl_Perfor_Diff, T_Diff) 
Tabl_Perfor_Sample = rbind( Tabl_Perfor_Sample, T_Sample)
Tabl_Diff_Sample_NonMatch = rbind( Tabl_Diff_Sample_NonMatch, T_Diff_Sample_NonMatch)

#} # Task Type 

Tabl_Perfor_Date = rbind( Tabl_Perfor_Date, T_Date)
#Table_Diff_Sample = rbind(Table_Diff_Sample , T_Diff_Sample) 

} #if More than X NR of trials to analysis
}




names(Tabl_Perfor_Diff)[1:11] = c("Monkey", "Date", "DifficultyLevel" ,"RotationDifference" , "NrTrial",  "Accuracy", "percResponseTime","percResponseTimeCorrect", "percResponseTimeIncorrect",   "PositionPreference_Right", "PositionPreference_Left")
names(Tabl_Perfor_Date)[1:9] = c("Monkey", "Date","Accuracy","percResponseTime",  "ResponseTime (correct)", "ResponseTime (error)", "NrTrials_Completed", "PositionPreference_Right", "PositionPreference_Left")

names(Tabl_Perfor_Sample)[1:8] = c("Monkey", "Date", "Sample" , "Sample_Rotation", "Accuracy","NrTrial",  "percResponseTime", 'ChoicePreference_Right')
#names(Table_Diff_Sample)[1:6] = c("Monkey", "Date", "Sample" , "Sample_Rotation", "Accuracy", "Difficulty_Level")
names(Tabl_Diff_Sample_NonMatch)[1:9] = c("Monkey", "Date", "Counter" , "Accuracy", "Difficulty_Level", "Sample_Rotation", "NonMatch_Rotation", "percResponseTime", "NrTrial")

######################################################
# Graph1:  per Date - performance #######
######################################################
Perform_PerDate =   ggplot(data=Tabl_Perfor_Date) +
  geom_bar(aes(x= Date , y= Accuracy , color = "black"), stat="identity", color="black") +
  theme_bw()+ ylab('Accuarcy in %') + xlab('Date') + theme(axis.text.x = element_text(angle = 90)) + ylim(0,1)+ 
  geom_hline( yintercept = 0.5, colour = "red")+
  ggtitle("Blue - ChoicePreference_Right")+
  geom_point(aes(x=Date, y= PositionPreference_Right, color = "blue"), color="blue") 
#  geom_text(x = 1:length(unique(Tabl_Perfor_Date$Date)) ,  y = c(0.6, 0.6,0.6), label = paste0("n=", rev(unique(Tabl_Perfor_Date$NrTrials_Completed)) ), size = 5, colour = "black")+
#  per Date - Error Types #######
######################################################
Perform_PerErrorType =   ggplot(data=Tabl_Perfor_Date) +
  geom_bar(aes(x=AbortState, y= Freq_AbortState , color = "black"), stat="identity", color="black") +
  theme_bw()+ ylab('Frequency in %') + xlab('Error Types') + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Blue - ChoicePreference_Right")+
  facet_grid(  ~ Date )

#################################
### Performance each Difficulty Level
#################################
Perform_DiffLevel =  ggplot(data=Tabl_Perfor_Diff, aes(x= DifficultyLevel, y= Accuracy, group = Date, color = Date)) +
  geom_point(aes(x= DifficultyLevel, y= Accuracy, group = Date, color = Date), size= 2) +
  geom_line(aes(x= DifficultyLevel, y= Accuracy), size= 0.75) + 
  ggtitle("From Easy  to Difficult") + ylim(0.2, 1)+   
  stat_summary(fun.y=mean,geom="point",aes(group=1), size = 4)+ 
  stat_summary(fun.y=mean,geom="line",lwd=1.5, aes(group=1))+ 
  #scale_x_reverse()+ 
  xlab("Difficulty levels (easy to difficult)")+
  theme_bw()+
  theme(text  = element_text(size = 16))

  #scale_x_discrete(brkeas = unique(Tabl_Perfor_Diff$Difficulty_Level), labels = c("1", "2", "3", "4", "5" ))+
PercResponse_DiffLevel =  ggplot(data=Tabl_Perfor_Diff, aes(x= DifficultyLevel, y= percResponseTime, group = Date, color = Date)) +
  geom_point(aes(x= DifficultyLevel, y= percResponseTime, group = Date, color = Date), size= 2) +
  geom_line(aes(x= DifficultyLevel, y= percResponseTime), size= 0.75) + 
  ggtitle("From Easy  to Difficult") +   
  stat_summary(fun.y=mean,geom="point",aes(group=1), size = 4)+ 
  stat_summary(fun.y=mean,geom="line",lwd=1.5, aes(group=1))+ 
  #scale_x_reverse()+ 
  xlab("Difficulty levels (easy to difficult)")+
  theme_bw()+
  theme(text  = element_text(size = 16))

PercResponse_DiffLevel =  ggplot(data=Tabl_Perfor_Diff, aes(x= DifficultyLevel, y= percResponseTime, group = Date, color = Date)) +
  geom_point(aes(x= DifficultyLevel, y= percResponseTime, group = Date, color = Date), size= 2) +
  geom_line(aes(x= DifficultyLevel, y= percResponseTime), size= 0.75) + 
  ggtitle("From Easy  to Difficult") +   
  stat_summary(fun.y=mean,geom="point",aes(group=1), size = 4)+ 
  stat_summary(fun.y=mean,geom="line",lwd=1.5, aes(group=1))+ 
  #scale_x_reverse()+ 
  xlab("Difficulty levels (easy to difficult)")+
  theme_bw()+
  theme(text  = element_text(size = 16))

PercResponseIncorrect_DiffLevel =  ggplot(data=Tabl_Perfor_Diff, aes(x= DifficultyLevel, y= percResponseTimeCorrect, group = Date, color = Date)) +
  geom_point(aes(x= DifficultyLevel, y= percResponseTimeCorrect, group = Date, color = Date), size= 2) +
  geom_line(aes(x= DifficultyLevel, y= percResponseTimeCorrect), size= 0.75) + 
  ggtitle("From Easy  to Difficult") +   
  stat_summary(fun.y=mean,geom="point",aes(group=1), size = 4)+ 
  stat_summary(fun.y=mean,geom="line",lwd=1.5, aes(group=1))+ 
  #scale_x_reverse()+ 
  xlab("Difficulty levels (easy to difficult)")+
  theme_bw()+
  theme(text  = element_text(size = 16))

PercResponseIncorrect_DiffLevel =  ggplot(data=Tabl_Perfor_Diff, aes(x= DifficultyLevel, y= percResponseTimeIncorrect, group = Date, color = Date)) +
  geom_point(aes(x= DifficultyLevel, y= percResponseTimeIncorrect, group = Date, color = Date), size= 2) +
  geom_line(aes(x= DifficultyLevel, y= percResponseTimeIncorrect), size= 0.75) + 
  ggtitle("From Easy  to Difficult") +   
  stat_summary(fun.y=mean,geom="point",aes(group=1), size = 4)+ 
  stat_summary(fun.y=mean,geom="line",lwd=1.5, aes(group=1))+ 
  #scale_x_reverse()+ 
  xlab("Difficulty levels (easy to difficult)")+
  theme_bw()+
  theme(text  = element_text(size = 16))

NrTrial_DiffLevel =  ggplot(data=Tabl_Perfor_Diff, aes(x= DifficultyLevel, y= NrTrial, group = Date, color = Date)) +
  geom_point(aes(x= DifficultyLevel, y= NrTrial, group = Date, color = Date), size= 2) +
  geom_line(aes(x= DifficultyLevel, y= NrTrial), size= 0.75) + 
  ggtitle("From Easy  to Difficult") +   
  stat_summary(fun.y=mean,geom="point",aes(group=1), size = 4)+ 
  stat_summary(fun.y=mean,geom="line",lwd=1.5, aes(group=1))+ 
  #scale_x_reverse()+ 
  xlab("Difficulty levels (easy to difficult)")+
  theme_bw()+
  theme(text  = element_text(size = 16))

# PositionPreference
PositionPreference_DiffLevel =  ggplot(data=Tabl_Perfor_Diff, aes(x= DifficultyLevel, y= PositionPreference_Right, group = Date, color = Date)) +
  geom_point(aes(x= DifficultyLevel, y= PositionPreference_Right, group = Date, color = Date), size= 2) +
  geom_line(aes(x= DifficultyLevel, y= PositionPreference_Right), size= 0.75) + 
  ggtitle("From Easy  to Difficult") +   
  stat_summary(fun.y=mean,geom="point",aes(group=1), size = 4)+ 
  stat_summary(fun.y=mean,geom="line",lwd=1.5, aes(group=1))+ 
  #scale_x_reverse()+ 
  xlab("Difficulty levels (easy to difficult)")+
  theme_bw()+
  theme(text  = element_text(size = 16))+
  ylim(0, 1) 
#################################
### Performance & ReactionTime per Sample
#################################
Perform_Sample =  ggplot(data=Tabl_Perfor_Sample, aes(x= Sample_Rotation, y= Accuracy, group = Date, color = Date)) +
  geom_point(aes(x= Sample_Rotation, y= Accuracy, group = Date, color = Date), size= 3) +
  geom_line(aes(x= Sample_Rotation, y= Accuracy), size= 1) + 
  theme_bw()+ylim(0, 1)  
#  stat_summary(fun.y=mean,geom="point",aes(group=1), size = 3) +
#+stat_summary(fun.y=mean,geom="line",lwd=1.5, aes(group=1))
PercRT_Sample =  ggplot(data=Tabl_Perfor_Sample, aes(x= Sample_Rotation, y= percResponseTime, group = Date, color = Date)) +
  geom_point(aes(x= Sample_Rotation, y= percResponseTime, group = Date, color = Date), size= 3) +
  geom_line(aes(x= Sample_Rotation, y= percResponseTime), size= 1) + 
  theme_bw()
NrTrial_Sample =  ggplot(data=Tabl_Perfor_Sample, aes(x= Sample_Rotation, y= NrTrial, group = Date, color = Date)) +
  geom_point(aes(x= Sample_Rotation, y= NrTrial, group = Date, color = Date), size= 3) +
  geom_line(aes(x= Sample_Rotation, y= NrTrial), size= 1) + 
  theme_bw()

ChoicePreference_Sample =  ggplot(data=Tabl_Perfor_Sample, aes(x= Sample_Rotation, y= ChoicePreference_Right, group = Date, color = Date)) +
  geom_point(aes(x= Sample_Rotation, y= ChoicePreference_Right, group = Date, color = Date), size= 3) +
  geom_line(aes(x= Sample_Rotation, y= ChoicePreference_Right), size= 1) + ylim(0,1)+
  theme_bw()
  ##################################################################
  ### Performance per Sample separated for Difficulty Levels
  ##################################################################
  Perform_Sample_Diff =  ggplot(data=Table_Diff_Sample, aes(x= Sample_Rotation, y= Accuracy, group = Date, color = Date)) +
    geom_point(aes(x= Sample_Rotation, y= Accuracy, group = Date, color = Date), size= 3) +
    geom_line(aes(x= Sample_Rotation, y= Accuracy), size= 1) + 
    facet_grid( . ~ Difficulty_Level ) +theme_bw()+
    stat_summary(fun.y=mean,geom="point",aes(group=1), size = 3)+ 
    stat_summary(fun.y=mean,geom="line",lwd=1.5, aes(group=1)) +ylim(0, 1) 
  
  #################################
  ### Performance per NonMatch per Sample separated for each DifficultyLevel 
  #################################  
# missing 2nd line: 77     Nor 2018-09-24      77    0.545                8             150               142            0.889      11
if (Tabl_Diff_Sample_NonMatch$Date[1] == "2018-09-24"){
  Tabl_Diff_Sample_NonMatch = Tabl_Diff_Sample_NonMatch[ -c(which(is.na(Tabl_Diff_Sample_NonMatch$Accuracy ) )) , ] 
}

  Perform_Diff_Sample_NonMatch =  ggplot(data=Tabl_Diff_Sample_NonMatch, aes(x= NonMatch_Rotation, y= Accuracy, group = Date, color = Date)) +
    geom_point(aes(x= NonMatch_Rotation, y= Accuracy, group = Date, color = Date), size= 3) +
    geom_line(aes(x= NonMatch_Rotation, y= Accuracy), size= 1) + 
    facet_grid(  ~ Sample_Rotation ) +ylim(0, 1) + 
  theme_bw()
  
NrTrials_Diff_Sample_NonMatch =  ggplot(data=Tabl_Diff_Sample_NonMatch, aes(x= NonMatch_Rotation, y= NrTrial, group = Date, color = Date)) +
  geom_point(aes(x= NonMatch_Rotation, y= NrTrial, group = Date, color = Date), size= 3) +
  geom_line(aes(x= NonMatch_Rotation, y= NrTrial), size= 1) +
  facet_grid(  ~ Sample_Rotation )


percRT_Diff_Sample_NonMatch =  ggplot(data=Tabl_Diff_Sample_NonMatch, aes(x= NonMatch_Rotation, y= percResponseTime, group = Date, color = Date)) +
  geom_point(aes(x= NonMatch_Rotation, y= percResponseTime, group = Date, color = Date), size= 3) +
  geom_line(aes(x= NonMatch_Rotation, y= percResponseTime), size= 1) +
  facet_grid(   ~ Sample_Rotation ) 


#################################
### Performance per NonMatch per Sample
#################################  

Perform_Diff_Sample_NonMatch =  ggplot(data=Tabl_Diff_Sample_NonMatch, aes(x= NonMatch_Rotation, y= Accuracy, group = Date, color = Date)) +
  geom_point(aes(x= NonMatch_Rotation, y= Accuracy, group = Date, color = Date), size= 3) +
  geom_line(aes(x= NonMatch_Rotation, y= Accuracy), size= 1) + 
  facet_grid( Difficulty_Level ~ Sample_Rotation ) +ylim(0, 1) + 
  theme_bw()
NrTrials_Diff_Sample_NonMatch =  ggplot(data=Tabl_Diff_Sample_NonMatch, aes(x= NonMatch_Rotation, y= NrTrial, group = Date, color = Date)) +
  geom_point(aes(x= NonMatch_Rotation, y= NrTrial, group = Date, color = Date), size= 3) +
  geom_line(aes(x= NonMatch_Rotation, y= NrTrial), size= 1) +
  facet_grid( Difficulty_Level ~ Sample_Rotation )


percRT_Diff_Sample_NonMatch =  ggplot(data=Tabl_Diff_Sample_NonMatch, aes(x= NonMatch_Rotation, y= percResponseTime, group = Date, color = Date)) +
  geom_point(aes(x= NonMatch_Rotation, y= percResponseTime, group = Date, color = Date), size= 3) +
  geom_line(aes(x= NonMatch_Rotation, y= percResponseTime), size= 1) +
  facet_grid( Difficulty_Level ~Sample_Rotation ) 
############################################################################
## RT - different sample, position
## RT - Histogram
#############################################################

Hist_ResponseTime_Difficulty=  ggplot(data=table) +
  geom_histogram(data = subset(table,table$completed == 1 & table$success == 0),aes(M2S_ResponseTime ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 &table$success == 1),aes(M2S_ResponseTime), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ Rot_Diff ) + xlab("perceptual response time (s)")
ggtitle(paste0(table$Monkey[1],"     perceptual response Time" ))+ theme_bw()

Hist_ResponseTime_Count=  ggplot(data=table) +
  geom_histogram(data = subset(table,table$completed == 1 & table$success == 0),aes(y = ..count.., M2S_ResponseTime ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 &table$success == 1),aes(y = ..count..,M2S_ResponseTime), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ . ) + xlab("perceptual response time (s)")+
ggtitle(paste0(table$Monkey[1],"     perceptual response Time" ))+ theme_bw()

Hist_ResponseTime_Density=  ggplot(data=table) +
  geom_histogram(data = subset(table,table$completed == 1 & table$success == 0),aes(y = (..density..)/sum(..density..), M2S_ResponseTime ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 &table$success == 1),aes(y = (..density..)/sum(..density..),M2S_ResponseTime), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ . ) + xlab("perceptual response time (s)")+
ggtitle(paste0(table$Monkey[1],"     perceptual response Time" ))+ theme_bw()

Perform_RT =  ggplot(data=Tabl_Perfor_Date) +
  geom_point(aes(x=Date, y= `ResponseTime (error)`),  color="red", size= 4)+
  geom_point(aes(x=Date, y= `ResponseTime (correct)`),  color="green", size= 4)+
  geom_point(aes(x=Date, y= percResponseTime),  color="black", size= 4)+
  ggtitle("average time from target appears to touch - red: errors, green: correct RT, black: all ")+theme_bw()+ 
  ylab('average RT in s') + xlab('Session')+ theme(axis.text.x = element_text(angle = 90))+ theme_bw()

############################################################################
## Types of Errors
## 
#############################################################

#################################
### Histogram for the Motion comparing error vs correct trials
#################################
setwd( paste0("/Users/kristinkaduk/Dropbox/promotion/Projects/Wagering_Monkey/results/", monkey, "/Graph"))
pdf(file = paste0(monkey,"Performance_M2S_since_", starting_folder, "to",ending_folder, ".pdf"), width = 10, height = 15)
par(mfrow=c(1,3), xpd=T, mar=c(5, 4, 4, 2) + 1)
Page1 = grid.arrange( Perform_PerDate,Perform_DiffLevel )  # Perform_DiffLevel
Page2 = grid.arrange( Perform_PerErrorType )  # Perform_DiffLevel

Page3 = grid.arrange( Perform_DiffLevel, NrTrial_DiffLevel, PercResponse_DiffLevel, PositionPreference_DiffLevel)  
Page4 = grid.arrange( Perform_Sample ,PercRT_Sample, NrTrial_Sample)  # Perform_DiffLevel
Page5 = grid.arrange( Perform_Diff_Sample_NonMatch)  
Page6 = grid.arrange( Perform_Diff_Sample_NonMatch, NrTrials_Diff_Sample_NonMatch,
                     percRT_Diff_Sample_NonMatch) 
Page7 = grid.arrange(Perform_RT,Hist_ResponseTime_Count )
Page8 = grid.arrange(Hist_ResponseTime_Difficulty)
Page9 = grid.arrange(Hist_ResponseTime_Count, Hist_ResponseTime_Density)
dev.off() 

#############################################################
#############################################################


### Motion during the complete TRIAL
Hist_FreqMot_FixAcq_to_TarHold = ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y =(..density..)/sum(..density..) ,FreqMot_FixAcq_to_TarHol ), fill = "red",  position = "identity",alpha = 0.4, lwd = 0.3)+
  geom_histogram(data = subset(table, table$completed == 1 &table$success == 1),aes(y =(..density..)/sum(..density..) ,FreqMot_FixAcq_to_TarHol), fill = "green",  position = "identity",alpha = 0.4, lwd = 0.3)+
  ggtitle(paste0(table$Monkey[1],"    Percentage of Motion in each trial - FixAcq to TarHol" ))+
  xlab(paste0("  % of Motion in each trial - Period: FixAcq to start of TarHol"))+
  scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
  #expand_limits(x = c(0,1), y= c(0,0.5))+ 
  theme_bw()+
  facet_grid( DifficultyLevel ~ Date ) 

############################################
### Motion from Target_Hold until Sound ###
###########################################
Hist_FreqMot_TarHol_to_Sound =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y =(..density..)/sum(..density..) ,x = FreqMot_TarHol_to_Sound ), fill = "red",  position = "identity",alpha = 0.4, lwd = 0.3)+
  geom_histogram(data = subset(table, table$completed == 1 &table$success == 1),aes(y =(..density..)/sum(..density..) ,x =FreqMot_TarHol_to_Sound), fill = "green",  position = "identity",alpha = 0.4, lwd = 0.3)+
  ggtitle(paste0(table$Monkey[1],"    Motion tar_hold to Feedback-Sound" ))+
  xlab("% of Motion in each trial - Period: TarHol to Sound")+
  scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
  #expand_limits(x = c(0,1), y= c(0,0.5))+ 
  theme_bw()+ 
  facet_grid( DifficultyLevel ~ Date ) 
################################################################
### Motion from Sound to the timepoint before Reward delivery ###
###############################################################
Hist_FreqMot_Sound_to_RewardDelivery =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y =(..density..)/sum(..density..) ,FreqMot_Sound_to_RewardDelivery ),fill = "red",  position = "identity",alpha = 0.4, lwd = 0.3)+
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(y =(..density..)/sum(..density..) ,FreqMot_Sound_to_RewardDelivery), fill = "green",  position = "identity",alpha = 0.4, lwd = 0.3)+
  ggtitle(paste0(table$Monkey[1],"  % of Motion in each trial - Period: from Sound to the timepoint before RewardDelivery " ))+
  xlab("% of Motion in each trial ")+ 
  scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
 # expand_limits(x = c(0,1), y= c(0,0.5))+ 
  theme_bw()+
  facet_grid( DifficultyLevel ~ Date ) 

################################################################
### Motion during TargetHold to End of Reward delivery ###
###############################################################
Hist_TarHol_BeforeRewardDelivery =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y =(..density..)/sum(..density..) ,FreqMot_TarHol_BeforeRewardDelivery ),fill = "red",  position = "identity",alpha = 0.7, lwd = 0.3)+
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(y =(..density..)/sum(..density..) ,FreqMot_TarHol_BeforeRewardDelivery), fill = "green",  position = "identity",alpha = 0.7, lwd = 0.3)+
  ggtitle(paste0(table$Monkey[1],"     % of Motion in each trial - Period: TarHol to before RewardDelivery" ))+
  xlab("% of Motion in each trial ")+ 
  scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
  expand_limits(x = c(0,1), y= c(0,1))+ 
  theme_bw()+
  facet_grid( . ~ Date) 
################################################################
### Motion during Reward delivery ###
###############################################################
Hist_RewardDelivery =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y =(..density..)/sum(..density..) ,Freq_RewardDelivery ),fill = "red",  position = "identity",alpha = 0.7, lwd = 0.3)+
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(y =(..density..)/sum(..density..) ,Freq_RewardDelivery), fill = "green",  position = "identity",alpha = 0.7, lwd = 0.3)+
  ggtitle(paste0(table$Monkey[1],"     % of Motion in each trial - Period: reward delivery" ))+
  xlab("Motion in one trial ")+ 
  ylab("Trials")+ 
  scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
  expand_limits(x = c(0,1), y= c(0,1))+ 
  theme_bw() +
  facet_grid( DifficultyLevel ~ Date ) 
  

################################################################
### Motion during Intertrial-Interval ###
###############################################################
Hist_FreqMot_ITI =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y =(..density..)/sum(..density..) ,FreqMot_ITI ),fill = "red",  position = "identity",alpha = 0.4, lwd = 0.3)+
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(y =(..density..)/sum(..density..) ,FreqMot_ITI), fill = "green",  position = "identity",alpha = 0.4, lwd = 0.3)+
  ggtitle(paste0(table$Monkey[1],"     % of Motion in each trial - Period: ITI-Interval" ))+
  xlab("% of Motion in each trial - Period: FreqMot_ITI")+
  scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
  #expand_limits(x = c(0,1), y= c(0,0.5))+ 
  theme_bw()+ 
  facet_grid( DifficultyLevel ~ Date ) 

setwd( paste0("/Users/kristinkaduk/Dropbox/promotion/Projects/Wagering_Monkey/results/", monkey))
pdf(file = paste0(monkey,"Graphs_Motion_since_", starting_folder, "to",ending_folder, ".pdf"), width = 10, height = 15)
par(mfrow=c(1,3), xpd=T, mar=c(5, 4, 4, 2) + 1)

Page1= grid.arrange(Hist_FreqMot_FixAcq_to_TarHold)
Page2= grid.arrange(Hist_FreqMot_TarHol_to_Sound)
Page3= grid.arrange(Hist_FreqMot_Sound_to_RewardDelivery)
Page4= grid.arrange(Hist_RewardDelivery)
Page5= grid.arrange(Hist_TarHol_EndRewardDelivery )
Page6= grid.arrange(Hist_FreqMot_ITI )
dev.off() 

##############################################################################
###############################################################################
i_Sess = 1#11
i_Diff = 5#11

  Data = c();
  Data = table[table$Date == unique(table$Date)[i_Sess], ]
  Data = table[table$DifficultyLevel == sort(unique(table$DifficultyLevel))[i_Diff], ]
  
  Hist_TarHol_BeforeRewardDelivery =  ggplot(data=Data) +
    geom_histogram(data = subset(Data, Data$completed == 1 & Data$success == 0),aes(y =(..density..)/sum(..density..) ,FreqMot_TarHol_BeforeRewardDelivery ),fill = "red",  position = "identity",alpha = 0.7, lwd = 0.3)+
    geom_histogram(data = subset(Data, Data$completed == 1 & Data$success == 1),aes(y =(..density..)/sum(..density..) ,FreqMot_TarHol_BeforeRewardDelivery), fill = "green",  position = "identity",alpha = 0.7, lwd = 0.3)+
    ggtitle(paste0(Data$Monkey[1],"  ", Data$Date[1]," TarHol until before RewardDelivery" ))+
    xlab("Motion in one trial ")+ 
    ylab("Trials")+ 
    scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
    expand_limits(x = c(0,1), y= c(0,1))+ 
    theme_bw()+
    theme(text = element_text(size = 25)) +
    facet_grid( DifficultyLevel ~ . )     
  
  Hist_TarHol_BeforeRewardDelivery =  ggplot(data=table) +
    geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y =(..density..)/sum(..density..) ,FreqMot_TarHol_BeforeRewardDelivery ),fill = "red",  position = "identity",alpha = 0.7, lwd = 0.3)+
    geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(y =(..density..)/sum(..density..) ,FreqMot_TarHol_BeforeRewardDelivery), fill = "green",  position = "identity",alpha = 0.7, lwd = 0.3)+
    ggtitle(paste0(table$Monkey[1],"  "," TarHol until before RewardDelivery" ))+
    xlab("Motion in one trial ")+ 
    ylab("Trials")+ 
    scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
    expand_limits(x = c(0,1), y= c(0,0))+ 
    theme_bw()+
    theme(text = element_text(size = 25)) +
    facet_grid( DifficultyLevel ~ Date)     
  
  
  Hist_RewardDelivery=  ggplot(data=Data) +
    geom_histogram(data = subset(Data, Data$completed == 1 & Data$success == 0),aes(y =(..density..)/sum(..density..) ,Freq_RewardDelivery ),fill = "red",  position = "identity",alpha = 0.7, lwd = 0.3)+
    geom_histogram(data = subset(Data, Data$completed == 1 & Data$success == 1),aes(y =(..density..)/sum(..density..) ,Freq_RewardDelivery), fill = "green",  position = "identity",alpha = 0.7, lwd = 0.3)+
    ggtitle(paste0(Data$Monkey[1],"  Session: ", Data$Date[1] ))+
    xlab("Motion in one trial ")+ 
    ylab("Trials")+ 
    scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
    expand_limits(x = c(0,1), y= c(0,1))+ 
    theme_bw()+
    theme(text = element_text(size = 25)) 
  
  
  
  Hist_ITI=  ggplot(data=Data) +
    geom_histogram(data = subset(Data, Data$completed == 1 & Data$success == 0),aes(y =(..density..)/sum(..density..) ,FreqMot_ITI ),fill = "red",  position = "identity",alpha = 0.7, lwd = 0.3)+
    geom_histogram(data = subset(Data, Data$completed == 1 & Data$success == 1),aes(y =(..density..)/sum(..density..) ,FreqMot_ITI), fill = "green",  position = "identity",alpha = 0.7, lwd = 0.3)+
    ggtitle(paste0(Data$Monkey[1],"  Session: ", Data$Date[1] ))+
    xlab("Motion in one trial ")+ 
    ylab("Trials")+ 
    scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
    expand_limits(x = c(0,1), y= c(0,1))+ 
    theme_bw()+
    theme(text = element_text(size = 25)) 
  
  
  
    Hist_FreqMot_Sound_to_RewardDelivery_1 =  ggplot(data=Data) +
      geom_histogram(data = subset(Data, Data$completed == 1 & Data$success == 0),aes(y =(..density..)/sum(..density..) ,FreqMot_Sound_to_RewardDelivery ),fill = "red",  position = "identity",alpha = 0.7, lwd = 0.3)+
      geom_histogram(data = subset(Data, Data$completed == 1 & Data$success == 1),aes(y =(..density..)/sum(..density..) ,FreqMot_Sound_to_RewardDelivery), fill = "green",  position = "identity",alpha = 0.7, lwd = 0.3)+
      ggtitle(paste0(Data$Monkey[1],"  Session: ", Data$Date[1] ))+
      xlab("Motion in one trial ")+ 
      ylab("Trials")+ 
      scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
      expand_limits(x = c(0,1), y= c(0,0.5))+ 
      theme_bw()+
      theme(text = element_text(size = 25)) 
    
    
    
    
    Hist_FreqMot_TarHol_to_Sound_1 =  ggplot(data=Data) +
      geom_histogram(data = subset(Data, Data$completed == 1 & Data$success == 0),aes(y =(..density..)/sum(..density..) ,x = FreqMot_TarHol_to_Sound ), fill = "red",  position = "identity",alpha = 0.7, lwd = 0.3)+
      geom_histogram(data = subset(Data, Data$completed == 1 &Data$success == 1),aes(y =(..density..)/sum(..density..) ,x =FreqMot_TarHol_to_Sound), fill = "green",  position = "identity",alpha = 0.7, lwd = 0.3)+
      ggtitle(paste0(Data$Monkey[1],"    Motion tar_hold to Feedback-Sound" ))+
      xlab("Motion in one trial ")+ 
      ylab("Trials in %")+ 
      scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
      expand_limits(x = c(0,1), y= c(0,0.5))+ 
      theme_bw()+ 
      theme(text = element_text(size = 25)) 
    
    +
      facet_grid( DifficultyLevel ~ Date )     
    
    

setwd( paste0("/Users/kristinkaduk/Dropbox/promotion/Projects/Wagering_Monkey/results/", monkey, '/Graph_TCM1/'))
pdf(file = paste0(monkey,"DifficutlyLevel",".ai"), width = 10, height = 15)
par(mfrow=c(1,3), xpd=T, mar=c(5, 4, 4, 2) + 1)
Page1= grid.arrange( Perform_DiffLevel, Perform_DiffLevel)
Page1= grid.arrange(   Hist_ITI, Hist_ITI, Hist_ITI, Hist_ITI, Hist_ITI, Hist_ITI)

dev.off() 



## 1. How often (counts) the monkey moves XX in one session? 
Hist_FreqMot_FixAcq_to_ITI =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(FreqMot_ITI) , fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(FreqMot_ITI), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ . , margins = TRUE) + xlab(" Percentage of Motion in each trial")+ 
  ggtitle(paste0(table$Monkey[1],"    Percentage of Motion in each trial - Period:FixAcq_to_ITI" ))


Hist_FreqMot_FixAcq_to_ITI_den =  ggplot(data=Data, aes(x = Data$FreqMot_FixAcq_to_ITI , fill = success, group= success ) ) +
  geom_histogram(aes(y =(..density..)/sum(..density..)) , alpha = 0.4, position = "identity", lwd = 0.3)+
  scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())

Hist_FreqMot_FixAcq_to_ITI_den2 =  ggplot(data=table, aes(x = table$FreqMot_FixAcq_to_ITI ) ) +
  geom_histogram(aes(y =(..density..)/sum(..density..),fill =  table$success, group=  table$success ) , alpha = 0.4, position = "identity", lwd = 0.3)+
  scale_y_continuous(labels= percent_format())+  scale_x_continuous(labels= percent_format())+
  facet_grid( Date ~ success )


Hist_FreqMot_FixAcq_to_ITI =  ggplot(data=table, aes(x = FreqMot_FixAcq_to_ITI)) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y = ..count../sum(..count..)*100) , fill = "red", alpha = 0.4, position = "identity")+
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(y = ..count../sum(..count..)*100), fill = "green", alpha = 0.4, position = "identity")+
  facet_grid( Date ~ . , margins = TRUE) + xlab(" Percentage of Motion in each trial")+ 
  ggtitle(paste0(table$Monkey[1],"    Percentage of Motion in each trial - Period:FixAcq_to_ITI" )) +
  scale_y_continuous(labels= percent_format())



##  Time before FeedbackSound
Hist_FreqMot_TarHol_to_Sound =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y =(..density..)/sum(..density..), FreqMot_TarHol_to_Sound ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(y =(..density..)/sum(..density..), FreqMot_TarHol_to_Sound), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ . ) +
  ggtitle(paste0(table$Monkey[1],"     Motion tar_hold to Feedback-Sound" ))

Hist_FreqMot_TarHol_to_Sound_Runs =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(FreqMot_TarHol_to_Sound ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(FreqMot_TarHol_to_Sound), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ Run ) +
  ggtitle(paste0(table$Monkey[1],"     Motion tar_hold to Feedback-Sound" ))

Hist_FreqMot_TarHol_to_Sound_Difficulty =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(FreqMot_TarHol_to_Sound ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 1),aes(FreqMot_TarHol_to_Sound), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ Rot_Diff) +
  ggtitle(paste0(table$Monkey[1],"     Motion tar_hold to Feedback-Sound" ))

# Correct: from Sound to RewardDelivery
# Error
Hist_FreqMot_Sound_to_RewardDelivery =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(y =(..density..)/sum(..density..),FreqMot_ITI ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 &table$success == 1),aes(y =(..density..)/sum(..density..),FreqMot_Sound_to_RewardDelivery), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ . , margins = TRUE) +
  ggtitle(paste0(table$Monkey[1],"     Motion after the Feedback-Sound Incorrect: ITI-Interval & for Correct: until ITI starts" ))

Hist_FreqMot_Sound_to_RewardDelivery_Runs =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(FreqMot_ITI ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 &table$success == 1),aes(FreqMot_Sound_to_RewardDelivery), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ Run ) +
  ggtitle(paste0(table$Monkey[1],"     Motion after the Feedback-Sound Incorrect: ITI-Interval & for Correct: until ITI starts" ))

Hist_FreqMot_Sound_to_RewardDelivery_Difficulty =  ggplot(data=table) +
  geom_histogram(data = subset(table, table$completed == 1 & table$success == 0),aes(FreqMot_ITI ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 &table$success == 1),aes(FreqMot_Sound_to_RewardDelivery), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ Rot_Diff ) +
  ggtitle(paste0(table$Monkey[1],"     Difficulty Levels (difficult to easy)-Motion after the Feedback-Sound Incorrect: ITI-Interval & for Correct: until ITI starts" ))
## ITI
Hist_FreqMot_ITI =  ggplot(data=table) +
  geom_histogram(data = subset(table,table$completed == 1 & table$success == 0),aes(FreqMot_ITI ), fill = "red", alpha = 0.4)+
  geom_histogram(data = subset(table, table$completed == 1 &table$success == 1),aes(FreqMot_ITI), fill = "green", alpha = 0.4)+
  facet_grid( Date ~ . , margins = TRUE) +
  ggtitle(paste0(table$Monkey[1],"     Motion in the ITI-Interval" ))


# save Graph as pdf
setwd( paste0("/Users/kristinkaduk/Dropbox/promotion/Projects/Wagering_Monkey/results/", monkey, "/Graph"))
pdf(file = paste0(monkey,"Performance_M2S_since", starting_folder, "_",ending_folder, ".pdf"), width = 10, height = 15)
par(mfrow=c(1,3), xpd=T, mar=c(5, 4, 4, 2) + 1)
Page1 = grid.arrange( Perform_PerDate, Perform_DiffLevel,Perform_Sample )  
Page2 = grid.arrange(Perform_RT)
Page3 = grid.arrange(Hist_ResponseTime)
Page4 = grid.arrange(Hist_ResponseTime_Difficulty)
Page3 = grid.arrange(Hist_FreqMot_FixAcq_to_ITI)
Page4 = grid.arrange(Hist_FreqMot_TarHol_to_Sound,Hist_FreqMot_Sound_to_RewardDelivery )
Page5 = grid.arrange(Hist_FreqMot_Sound_to_RewardDelivery_Difficulty)
Page6 = grid.arrange(Hist_FreqMot_Sound_to_RewardDelivery_Runs)
Page7 = grid.arrange(Hist_FreqMot_TarHol_to_Sound_Runs)
Page8 = grid.arrange(Hist_FreqMot_TarHol_to_Sound_Difficulty)
Page9 = grid.arrange(Hist_FreqMot_ITI)


dev.off() 


############### Fisher exact test #################
#INput: either a two-dimensional contingency table in matrix form, or a factor object.

m1 <- lmer( zDataset[,i_variables] ~ Interaction + (1|Monkey) + (1|NumRun:Session), data= zDataset, REML = FALSE ) 
challenge.df = matrix(c(1,4,7,4), nrow = 2)
fisher.test(challenge.df)
