#Filtering

library(dplyr,ggplot2)

#Same outputs (paired)
mpg[mpg$class == "compact",]
mpg %>% filter(class=="compact")

mpg[mpg$class == "compact",] %>% ggplot(aes(x = cty)) + 
  geom_histogram() + coord_cartesian(xlim = c(0, 50))
mpg %>% filter(class=="compact") %>% ggplot(aes(x = cty)) + 
  geom_histogram() + coord_cartesian(xlim = c(0, 50))

#Filter by numeric  
mpg %>% filter(year < 2000)

mpg %>% filter(cty>=20)

#Numeric and categorical
mpg %>% filter(class =="suv" & cty>17)

#SUVs with city mpg > 13 OR hwy mpg > 10
mpg %>% filter(class =="suv" & (cty>13 | hwy>10))

#Two factor levels
mpg %>% filter(class =="suv" | class =="compact") %>% ggplot(aes(x=hwy, color=class))+
  geom_histogram(position="dodge")
