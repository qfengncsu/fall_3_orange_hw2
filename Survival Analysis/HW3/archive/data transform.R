library(data.table)
library(dplyr)

setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Survival Analysis\\Data\\")

katrina <- fread('katrina.csv')

temp = katrina

temp = temp[-442,]
temp = temp[-631,]
temp = temp[-645,]

temp = temp %>% mutate(ID = row.names(temp), start=rep(0), stop=rep(0), twelve=rep(0), running=rep(0), survive2=rep(0))

newData = temp
for (i in 1:length(temp$backup)){
  
  j=0
  temp$start[i] = 1
  temp$running[i] = temp[i,9]
  
  while (j < (temp$hour[i])){
    
    flag = F
    while (!flag){
      
      j=j+1
      if (j == 49){
        flag = T
      }
      else if (temp[i,min(9+j,9+temp$hour[i]-1)] != temp$running[i]){
        flag = T
      }
    }
    
    if (j == 49){
      temp$stop[i] = 48
    }
    else{
      temp$stop[i] = j
    }
    
    if ((temp$stop[i] - temp$start[i]) >= 11){
      
      temp$twelve[i] = 1
    }
    
    if (j < temp$hour[i]){
      temp$survive2[i] = 1
    }
    else{
      temp$survive2[i] = temp$survive[i]
      temp$stop[i] = temp$hour[i]
    }
    
    newData = newData %>% bind_rows(temp[i,])
    
    temp$running[i] = temp[i,9+j]
    temp$start[i] = j+1
    temp$stop[i] = 0
  }
}

newData = newData[6:length(newData$backup),]
