#For Live Session Unit 5

library(tidyverse)
library(dplyr)
library(plotly)

#Read in bball data
bball = read.csv('C:/Users/aleppla/GitRepos/MDS-6306-Doing-Data-Science/Unit 2/PlayersBBall.csv')

#Convert height to total inches
summary(bball$height) #Inspect for missing or NA values
bball1 = bball %>% filter(height!="") %>% separate(height, into = c("ft","inch")) #filter out missing values and separate ft and in 
bball1$height_in=as.numeric(bball1$ft)*12 + as.numeric(bball1$inch) #convert ft to inches and output height in inches


#Histogram Panels
bball1 %>% ggplot(aes(x=height_in,fill=position)) + geom_histogram() + ggtitle("NBA Player Heights by Position") + 
  xlab("Height (in)") + ylab("Number of Players") + facet_wrap(~position) + coord_cartesian(xlim=c(65,90))


fifa=read.csv('C:/Users/aleppla/GitRepos/MDS-6306-Doing-Data-Science/Unit 3/FIFA Players.csv')

#Inspect for missing values
summary(fifa$Height)
summary(fifa$Weight)

#Filter our missing values, covert Height to total inches, reformat Weight to remove 'lbs'
fifa1 = fifa %>% filter(Height!="" | Weight!="") %>% separate(Height, into = c("ft","inch"))
fifa1$height_in=as.numeric(fifa1$ft)*12 + as.numeric(fifa1$inch)
fifa1$Weight = as.numeric( str_extract_all(str_c(fifa1$Weight),"\\d+") )


#EDA

fifa1 %>% ggplot(aes(height_in,Weight)) + geom_point() + geom_smooth(method='lm') + 
  ggtitle("Weight vs. Height for FIFA Players") + xlab("Height (in)") + ylab("Weight (lb)")

p1 = fifa1 %>% ggplot(aes(height_in,Weight,color=Position)) + geom_smooth(method='lm',se=F)  + 
  ggtitle("Weight vs. Height by Position") + xlab("Height (in)") + ylab("Weight (lb)")

ggplotly(p1)

fifa1 %>% ggplot(aes(height_in,Weight,color=Position)) + geom_point() + geom_smooth(method='lm',color="black")  + 
  facet_wrap(~Position) + ggtitle("Weight vs. Height by Position") + xlab("Height (in)") + ylab("Weight (lb)")

fifa1 %>% filter(Position=='LB' | Position=='LM') %>% ggplot(aes(height_in,Weight,color=Position)) + geom_point() + 
  geom_smooth(method='lm') + ggtitle("FIFA - Left Backs (LB) vs. Left Midfielders (LM)")
                 

#Baby Names

##Q1a
df=read.delim('C:/Users/aleppla/GitRepos/MDS-6306-Doing-Data-Science/Unit 5/yob2016.txt',header=F,sep=';')
names(df)=c("Name","Gender","Count")

##Q1b
summary(df)
str(df)

##Q1c
df$Name[grep("yyy$",df$Name)] #Find & display the duplicate misspelled name

##Q1d
y2016=df[-grep("yyy$",df$Name),] #Delete the row with the duplicate misspelled name

###Check that it was deleted
grep("yyy$",y2016$Name) 
str(y2016)


##Q2a
y2015=read.csv('C:/Users/aleppla/GitRepos/MDS-6306-Doing-Data-Science/Unit 5/yob2015.txt',header=F)
names(y2015)=c("Name","Gender","Count")

##Q2b
tail(y2015,n=10)
#All these cool Z names are male!

##Q2c
final1 = merge(y2016, y2015, by = "Name")
str(final1)

final = merge(y2016, y2015, by = c("Name","Gender"))
str(final)

final1[final1$Name=="Aalijah",]
final[final$Name=="Aalijah",]

##Q3a
final1$Total=final1$Count.x + final1$Count.y
sum(final1$Total) #Merged by Name Only

final$Total=final$Count.x + final$Count.y
sum(final$Total) #Merged by Name and Gender

#Q3b
final_max=final[order(final$Total,decreasing=T),]
head(final_max,n=10)
head(final_max,n=10)$Name

#Q3c
final_F = final %>% filter(Gender=='F') 
final_F=final_F[order(final_F$Total,decreasing=T),]
head(final_F,n=10)$Name

#Q3d
write_csv(data.frame(head(final_F,n=10)$Name),'C:/Users/aleppla/GitRepos/MDS-6306-Doing-Data-Science/Unit 5/Top10GirlNames.csv',col_names=F)

#Q4 - Data Vis
final %>% filter(Count.y>14000) %>% ggplot(aes(x=Count.y,y=Count.x,color=Gender,label=Name)) + geom_point() + 
  ggtitle("Most Popular Names of 2015 Decline in 2016") + xlab('Count in 2015') + ylab('Count in 2016') + 
  geom_text(nudge_x=410,nudge_y=0) + geom_abline(slope=1,intercept=0)

final$diff = final$Count.x - final$Count.y
final$perc = final$diff/final$Count.y*100

final %>% filter(perc>5 & Count.y>6000) %>% ggplot(aes(x=Name,y=perc,fill=Gender)) + geom_col() +
  ggtitle("Most Popular Names of 2015 Trending Up in 2016") + ylab('% Increase in 2016 vs. 2015')

final %>% filter(perc > 300 & Count.y<10) %>% ggplot(aes(x=Name,y=perc,fill=Gender)) + geom_col() +
  ggtitle("Obscure Names of 2015 Trending Up in 2016") + ylab('% Increase in 2016 vs. 2015')