#Mutate Examples

#Create a data frame "mpgDiff" with added formula column "diffHC"
mpgDiff = mpg %>% mutate(diffHC=hwy-cty)
mpgDiff

#Does NOT add formula column to data frame "mpg", just prints it
mpg %>% mutate(diffHC=hwy-cty)

#Pass formula column to ggplot
mpg %>% 
  mutate(diffHC=hwy-cty) %>%
  ggplot(aes(x=displ,y=diffHC)) + geom_point() + geom_smooth(method="lm")

##Color by class
mpg %>% 
  mutate(diffHC=hwy-cty) %>%
  ggplot(aes(x=displ,y=diffHC,color=class)) + geom_point() 

##Color by model
mpg %>% 
  mutate(diffHC=hwy-cty) %>%
  ggplot(aes(x=displ,y=diffHC,color=model)) + geom_point() 

