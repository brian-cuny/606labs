load('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\606\\Labs\\mlb11.RData')
library(tidyverse)
library(magrittr)

#Exercises

#1. A scatterplot would be ideal for this relationship. The data appears to have a small positive correlation. Yes, I would be comfortable using a linear model.

ggplot(mlb11, aes(at_bats, runs)) +
  geom_point()

#2. There appears to be a roughly linear, positive relationship between at bats and runs scored. That is, as the number of at bats increases so does the number of runs scored.

plot_ss(mlb11$at_bats, mlb11$runs, showSquares=TRUE)

#3. The smallest sum of squares I got was 126766.8 the result of the line y=0.5084-2111.5071

lm(runs ~ homeruns, data=mlb11) %>%
  summary()

#4. runs = homeruns*1.8345 + 415.2389. There is a positive relationship between runs scored and homeruns hit. As the number of homeruns hit increases, so does the number of runs scored. The slope of 1.8 indicates that, on average, each homerun adds nearly 2 to the total number of runs scored.


plot(mlb11$runs ~ mlb11$at_bats)
abline(lm(runs ~ at_bats, data = mlb11))

#5. y=-2789.2429+.6305(5578) = 727.6861 The residual for this prediction is 0. The prediction is based on the least-squares line and thus sits right on it. We would need to know the actual number of runs scored to see how this differs from the predicted value.
# The closest actual point, 5579 at bats predicts 713 runs. Those the 5578 is an overestimate.

#6. There is no apparently pattern in the residuals plot. This indicates that the relationship can be modeled as linear.

#7. Yes, the nearly normal residuals condition appears to be roughly met.

#8. Yes, the variability appears to be roughly constant.


#On Your Own

#1. I plotted runs against wins. Yes, from a glance there appears to be a positive, linear relationship.

plot(mlb11$runs ~ mlb11$wins)
abline(lm(runs ~ wins, data = mlb11))

#2. The R2 for runs ~ wins is 0.3381 while the R2 for runs ~ atbats is 0.3505. The variable at_bats does a better job of predicting total runs than wins does do the fact that it has a higher R2.

lm(runs ~ wins, data = mlb11) %>%
  summary()

#3. Batting Average has the strongest R2 with 0.6438

lm(runs ~ hits, data = mlb11) %>%
  summary()
#R2 = 0.6292

lm(runs ~ homeruns, data = mlb11) %>%
  summary()
#R2 = 0.6132

lm(runs ~ bat_avg, data = mlb11) %>%
  summary()
#R2 = 0.6438

lm(runs ~ strikeouts, data = mlb11) %>%
  summary()
#R2 = 0.1397

lm(runs ~ stolen_bases, data = mlb11) %>%
  summary()
#R2 = -0.0327

plot(mlb11$runs ~ mlb11$bat_avg)
abline(lm(runs ~ bat_avg, data = mlb11))

#4. 

lm(runs ~ new_onbase, data = mlb11) %>%
  summary()
#R2 = .8437

lm(runs ~ new_slug, data = mlb11) %>%
  summary()
#R2 = .8932

m2 <- lm(runs ~ new_obs, data = mlb11) %T>%
  summary()
#R2 = .9326

plot(mlb11$runs ~ mlb11$new_obs)
abline(lm(runs ~ new_obs, data = mlb11))

#The best predictor across all 10 statistics is obs or on-base plus slugging. This result makes sense. While the old statistics measured singular aspects of a baseball team's offense (more at bats mean more batters on getting on base and thus scoring runs, more homeruns means more runs)
#the new statistics do a better job of summarizing this data. In order to score a run, you must get on base and slugging measures how successful a team is at moving those players once they get on base. Getting players on base and successfully advancing them are the two essential skills
#needed to score more runs.


#5. 

plot(m2$residuals ~ mlb11$new_obs)
abline(h = 0, lty = 3)

# There is no pattern to the residuals. 

qqnorm(m2$residuals)
qqline(m2$residuals) 

# The residuals are roughly normal in distribution.

# The original plot shows a near constant level of variability. The conditions for inference have been met.





