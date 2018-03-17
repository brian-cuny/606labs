#Exploartory analysis

load('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\606\\Labs\\nc.RData')


#1. The cases are the 1000 observations, the babies born in North Caronlina in 2004.

summary(nc)

#2. The average baby for both groups appears to be roughly the same however there are many more lower outliers for the nonsmokers compared to the smokers.

boxplot(nc$weight ~ nc$habit)

by(nc$weight, nc$habit, mean)

#3. independence: yes, more than 30 in each group: yes

by(nc$weight, nc$habit, length)

#4. H0: udiff = 0, Ha: udiff != 0


#5. 

inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0, 
          alternative = "twosided", method = "theoretical")

#On your own

#1. It can be stated with 95% confidence that the average pregancy length of the population is between 38.1528 and 38.5165 weeks.

inference(y = nc$weeks, est = "mean", type = "ci", null = 0, 
          alternative = "twosided", method = "theoretical")

#2. It can be stated with 95% confidence that the average pregancy length of the population is between 38.182 and 38.4873 weeks.

inference(y = nc$weeks, est = "mean", type = "ci", null = 0, 
          alternative = "twosided", method = "theoretical", conflevel=0.9)

#3. With a p-value of 0.1686, we fail to reject the null hypothesis. There is no evidence to suggest that there is a difference in weight gain between mature mothers and young mothers.

inference(y = nc$gained, x=nc$mature, est = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")


#4. 

library(tidyverse)

nc %>%
  group_by(mature) %>%
  summarise(min = min(mage), max=max(mage))

#The cut off is between 34 and 35. I printed out the min and max ages for each group.

#5. Is there a relationship between a baby being premature and the number of visits per week to the hospital during the pregnancy (visits, premie)

# H0: udiff = 0 HA: udiff != 0

inference(y = nc$visits/ nc$weeks, x=nc$premie, est = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")

#With a p-value of .6068, we fail to reject the null hypothesis. There is no evidence to suggest that mother's of premature babies visit the hospital on a less frequent basis than mother's who have full term children.










