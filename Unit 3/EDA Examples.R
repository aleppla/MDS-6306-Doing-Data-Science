library(car,carData,magrittr,plotly)

#categorical with count labels
totals = count(mpg,class) 
mpg %>% ggplot(aes(class, fill=class)) + geom_bar() + 
  geom_text(aes(class,totals$n+2,label=totals$n),data=totals)
#n+2 puts the numbers above the car line

#Try making stacked bar plots with freq using cut()



#Histogram grid with stacked rows 
mpg %>% ggplot(aes(cty, fill=class)) + geom_histogram() + 
  facet_grid(rows=vars(class))

#Distributions - Center and Spread
mpg %>% 
  group_by(class) %>%
  summarize(mean=mean(cty),sd=sd(cty),median=median(cty),range=max(cty)-min(cty),IQR=IQR(cty),count=n())

