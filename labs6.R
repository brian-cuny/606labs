library(tidyverse)
#Exercises

#1
#This is a sample statistic. The information was derviced from the sample of people researchers asked. 

#2
#We must assume that the sample was representative of the world. It is likely that religious views vary greatly by region and country. It is also likely that it would difficult to gain representative samples
# in somes countries (do to war, remoteness or simple cost). In fact, only 59 of the roughly 200 recognized nations had survey's conducted. However, these countries represent a disproportionately
#large percent of the entire earth population. Calling this study Global seems fair.

load('C:/Users/Brian/Desktop/GradClasses/Spring18/606/606labs/atheism.RData')

#3
#Each row corresponds to one observation, that is, one person's response to the question. Each row of atheism corresponds to one responded indicated that they are a 'convinced atheist'

#4
us12 <- subset(atheism, nationality == "United States" & year == "2012")
us12 %>%
  group_by(response) %>%
  summarize(num = n()) %>%
  mutate(denom = num / sum(num))
#It matches the percentage in Table 6 within reasonable rounding error.

#5
# 1. Data is collected independently. This is a reasonable assumption
# 2. Less than 10% of the population. This is clearly true.
# 3. Expecting at least 10 positive and negative responses. This is clearly true.

#6. 
inference(us12$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")

.0499-0.0364

#The margin of error for the estimate of the proportion is 0.0135.

#7.
ar12 <- atheism %>%
  filter(nationality == 'Argentina' & year == '2012')
ar12 %>%
  group_by(response) %>%
  summarize(num = n()) %>%
  mutate(denom = num / sum(num))
inference(ar12$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")

#All three conditions for inference are met.
# 0.0547, 0.0866


ma12 <- atheism %>%
  filter(nationality == 'Macedonia' & year == '2012')

ma12 %>%
  group_by(response) %>%
  summarize(num = n()) %>%
  mutate(denom = num / sum(num))

#All three conditions for inference are met.
.00993*1209

inference(ma12$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")

#8
#As the probability of success tends towards 0.5, the margin of error increases. 

#9
par(mfrow = c(2, 2))

p <- 0.1
n <- 1040
p_hats <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}

hist(p_hats, main = "p = 0.1, n = 1040", xlim = c(0, 0.18))

mean(p_hats)
sd(p_hats)
max(p_hats) - min(p_hats)
#The sampling distribution is nearly normal. The center is ~0.099 with a standard deviation of ~0.0092. The spread is 0.059

p <- 0.1
n <- 400
p_hats <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}

hist(p_hats, main = "p = 0.1, n = 400", xlim = c(0, 0.18))

p <- 0.02
n <- 1040
p_hats <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}

hist(p_hats, main = "p = 0.02, n = 1040", xlim = c(0, 0.18))

p <- 0.02
n <- 400
p_hats <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}

hist(p_hats, main = "p = 0.02, n = 400", xlim = c(0, 0.18))

#The value of p affects the center of the sampling distribution, that is, the value of p-hat
#The value of n affects the spread of the sampling distribution. The larger n is, the smaller the spread.


#11

#Yes. All the conditions for inference are met and visual inspection of the data shows a nearly normal distribution.

#On Your Own

#1

sp <- atheism %>%
  filter(nationality == 'Spain')

sp05 <- sp %>%
  filter(year == '2005')

inference(sp05$response, est = "proportion", type = "ci", method = "theoretical", 
        success = "atheist")

sp12 <- sp %>%
  filter(year == '2012')

inference(sp12$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")

#H0 = The levels of atheism in Spain between 2005 and 2012 remains the same. Ha = The levels of atheism in Spain have changed.
# Sample are independent, less than 10% of the population, np and n(1-p) are both > 10

#Since the confidence intervals overlaps, there is no evidence to suggest a chance in atheism levels.

#b
usa <- atheism %>%
  filter(nationality == 'Spain')

usa05 <- usa %>%
  filter(year == '2005')

inference(usa05$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")

usa12 <- usa %>%
  filter(year == '2012')

inference(usa12$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")

#Since the confidence intervals overlap, there is no evidence of a change.

#2
#Considering there are 39 countries we would expect 39*.05=1.95 about 1 or 2 false positives.
39*.05

p <- 0.5
# 1.96*sqrt(.5*(1-.5)/n) < .01

n=9604
#Assume 0.5 as it provides the worst ME.






