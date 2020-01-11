#For Live Session Unit 2


##Use the PlayerBBall.csv dataset to visually represent (summarize) the number of players in each position. 

bball = read.csv('C:/Users/allep/OneDrive/Documents/MDS-6306-Doing-Data-Science/Unit 2/PlayersBBall.csv')

summary(bball)

###library(ggplot2, magrittr)

bball %>% ggplot(aes(x=position)) + geom_bar() + ggtitle("NBA Players in each Position") + 
  ylab("Number of Players") + xlab("Position")


##Use the dataset to visually investigate the distribution of the weight of centers (C) 
##is greater than the distribution of the weight of forwards (F).  

###Attempt just using ggplot, can't get "dodge" to work, use facet_wrap to get side-by-side instead

ggplot() + geom_histogram(data=bball[bball$position=="F",], aes(x=weight,fill="Forward")) + 
  geom_histogram(data=bball[bball$position=="C",], aes(x=weight,fill="Center")) + facet_wrap(~position)

###Subsetting and recombining data frames for C and F.  Can now use "dodge" to overlay side-by-side

C = bball[bball$position=="C",]
Fw = bball[bball$position=="F",]
C_Fw=rbind(C,Fw)

a = C_Fw %>% ggplot(aes(x=weight, fill=position)) + geom_histogram(position="dodge") + ggtitle("Weight Distributions of NBA Centers vs. Forwards")
a
ggplotly(a)


##Use the dataset to visually investigate if the distribution of the height of centers (C) is greater than the distribution of the height of forwards (F).

C_Fw %>% ggplot(aes(x=height, fill=position)) + geom_bar(position="dodge") + ggtitle("Height Distributions of NBA Centers vs. Forwards")

#Need to reorder the height factor levels 6-10, 6-11 to go after 6-9
##https://www.r-bloggers.com/reorder-factor-levels/
bball$height1=factor(bball$height, levels(bball$height)[c(1,4:9,2:3,10:11,14:21,12:13,22:29)])
levels(bball$height1)

C = bball[bball$position=="C",]
Fw = bball[bball$position=="F",]
C_Fw=rbind(C,Fw)
C_Fw %>% ggplot(aes(x=height1, fill=position)) + geom_bar(position="dodge") + ggtitle("Height Distributions of NBA Centers vs. Forwards")


##Use the dataset to visually investigate if the distribution of height is different 
##between any of the positions.  

bball[bball$position!="",] %>% ggplot(aes(x=height1, fill=position)) + 
  geom_bar(position="dodge")

bball[bball$position!="",] %>% ggplot(aes(x=height1, y=..prop..,group=1,fill=position)) + 
  geom_bar() +   facet_wrap(~position) + coord_cartesian(ylim = c(0, 0.3)) + 
  ggtitle("NBA Height Distributions by Position") + xlab("Height (5ft 3in to 7ft 7in)") + ylab("Proportion") 


##Use the dataset to investigate how the player's height is related to the player's weight. 
##How does height change as the weight changes?  

bball %>% ggplot(aes(x=height1, y=weight)) + geom_point() + ggtitle("Weight vs. Height in the NBA") +
  xlab("Height (5ft 3in to 7ft 7in)") + ylab("Weight (lb)")


##Is their any difference in the relationship between height and weight between positions?  
##Are height and weight related differently for different positions.

bball %>% ggplot(aes(x=height1, y=weight, color=position)) + geom_point() + ggtitle("Weight vs. Height by Position in the NBA") +
  xlab("Height (5ft 3in to 7ft 7in)") + ylab("Weight (lb)")


##A historian would like to investigate the claim that the heights of players have increased over the years.  Analyze this claim graphically / visually. 

bball %>% ggplot(aes(x=height1, y=year_start)) + geom_point() + ggtitle("Height vs. Starting Year in the NBA") +
  xlab("Height (5ft 3in to 7ft 7in)") + ylab("Starting Year") + coord_flip()

bball %>% ggplot(aes(x=height1, y=year_end)) + geom_point() + ggtitle("Weight vs. Ending Year in the NBA") +
  xlab("Height (5ft 3in to 7ft 7in)") + ylab("Ending Year") + coord_flip()


##Create a 3D plot of height vs. weight vs. year and color code the points by position.  

p <- plot_ly(bball, x = ~height1, y = ~weight, z = ~year_start, color = ~position) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Height (ft-in)'),
                      yaxis = list(title = 'Weight (lb)'),
                      zaxis = list(title = 'Starting Year')))
p


##Go to this website and use one of the 50 best plots to visualize some aspect of the data and provide at least one insight.  You will present your work in breakout! http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

#Years in the league
bball$years=bball$year_end-bball$year_start

bball %>% ggplot(aes(weight,years)) + geom_count()
bball[bball$years==0,] %>% ggplot(aes(height1,years)) + geom_count()


##Separate dataset:  The EducationIncome.csv dataset has incomes of randomly selected Americans and their level of education
###Visually test the claim that the distribution of incomes increase (mean or median) as the education level rises. 

Educ.Inc = read.csv('C:/Users/aleppla/Documents/Andy - Personal/MS DataSci/MSDS 6306 - Doing Data Science/Unit 2/Education_Income.csv')
summary(Educ.Inc)

#Reorder Educ so >16 is after 16
Educ.Inc$Educ1 = factor(Educ.Inc$Educ, levels(Educ.Inc$Educ)[c(1,3:5,2)])
levels(Educ.Inc$Educ1)

Educ.Inc %>% ggplot(aes(x=Income2005,fill=Educ1)) + geom_histogram(position="dodge") + coord_cartesian(xlim=c(0,250000)) +
  ggtitle("Income Distributions by Education Level") + xlab("2005 Income ($)") + ylab("Count")

scale_x_log10()  # convert to log scale

#Log transform for highly right-skewed data
Educ.Inc$Log.Inc=log(Educ.Inc$Income2005)

Educ.Inc %>% ggplot(aes(x=Log.Inc,fill=Educ1)) + stat_bin(position="dodge",bins=15) + coord_cartesian(xlim=c(7,13)) +
  ggtitle("Log-Income Distributions by Education Level") +  xlab("Log of 2005 Income") + ylab("Count")  
