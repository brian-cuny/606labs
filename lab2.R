download.file("http://www.openintro.org/stat/data/kobe.RData", destfile = "kobe.RData")
load("kobe.RData")

#Exercise 1
#A streak of length 1 means that Kobe made one basket before missing a shot. A streak of length 0 means that kobe made 0 baskets before missing a shot. Generally, a k-length streak means that Kobe made k baskets before missing a shot.

#2
kobe_streak <- calc_streak(kobe$basket)
barplot(table(kobe_streak))
table(kobe_streak)
summary(kobe_streak)

#Kobe's streaks are skewed right. His most common streak is 0 then 1 (although those may not necessarily be considered a streak by conventional definition). In that event he has an equal number of 2 and 3 streaks. His longest streak of baskets was 4 in a row. This event occurred one time.

#3
outcomes <- c("heads", "tails")
sim_unfair_coin <- sample(outcomes, size = 100, replace = TRUE, prob = c(0.2, 0.8))
length(which(sim_unfair_coin == 'heads'))

#4
outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))

#On Your Own 1
independent_streak <- calc_streak(sim_basket)
table(independent_streak)
summary(independent_streak)
#The most common streak is 0 and 1, just as with Kobe. The longest streak is 4, just as with Kobe.

library(tidyverse)
library(reshape2)
repeated_simulations <- replicate(100, calc_streak(sample(outcomes,size=133, replace = TRUE, prob=c(0.45, 0.55))))
data_simulations <- data.frame(mean=sapply(repeated_simulations, mean), sd=sapply(repeated_simulations, sd), max=sapply(repeated_simulations, max))
ggplot(melt(data_simulations), aes(x=value)) + geom_histogram(bins=8) + facet_wrap(~variable, scales='free_x') + labs(title='Mean, Standard Deviation and Max Streaks of 100 Simulations') + xlab('Value') + ylab('Count')


#2
#I would except the simulation to be similar but unlikely to be identical. By the law of large numbers as the number of trials increases the probabilities all trend to their theoretical probabilities. As the simulation size of 133 is large I would expect the distributions to all be fairly close to their theoretical values. Random small variations are also to be expected.

#3
library(ggplot2)
library(reshape)
table(kobe_streak)
table(independent_streak)


tabled_info <- data.frame(sapply(repeated_simulations, table) %>%
  lapply(`length<-`, max(lengths(tabled_info))))
tabled_info[is.na(tabled_info)] <- 0
tabled_info <- sapply(tabled_info, as.numeric) %>%
  apply(1, mean)


#Adapted form Stackoverflow
tabled_info <- sapply(repeated_simulations, table)
tabled_info <- data.frame(lapply(tabled_info, `length<-`, max(lengths(tabled_info))))
tabled_info[is.na(tabled_info)] <- 0
tabled_info <- sapply(tabled_info, as.numeric) %>%
  apply(1, mean)

to_plot <- data.frame(0:7, tabled_info, table(kobe_streak)[seq(tabled_info)], table(independent_streak)[seq(tabled_info)]) %>%
  subset(select=c(1,2,4,6))
to_plot[is.na(to_plot)] <- 0
names(to_plot) <- c('Streak', 'Repated', 'Kobe', 'Simulated')
ggplot(melt(to_plot, id.var = 'Streak'), aes(x=Streak, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + labs(title='Streak Comparison between Kobe Bryant and Simulations') + scale_x_continuous(labels=as.character(to_plot$Streak), breaks=to_plot$Streak)


summary(kobe_streak)
summary(independent_streak)

#Comparing the streak lengths between Kobe and the simulated shooter shows significant similarities. Both have a large number of 0 and 1 streaks and a max streak of 4. The differences between the number of streaks appears within chance.
#The summary statistics support this interpretation. There is no compelling evidence that Kobe's shooting pattern matches the hot hand model.