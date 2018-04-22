load('evals.RData')

library(tidyverse)

#1. This is an observational study. The ratings given by students for each teacher do not follow experimental design. As such, it is impossible to answer this question with 100% certain as there will be other variables that cannot be controlled for. I would rephrase the question: Is there evidence to suggest that beauty plays a role in teacher evaulations?

#2. The data is heavily skewed. This tells me that most students are satisfied with the quality of the courses and professors that they take. Nearly 100% of teachers are "above average". I'm not sure what I expected from the results. It does suggest that the ratings may not be helpful though. Perhaps a new system is needed for this reason alone.

summary(evals$score)

boxplot(evals$score)

ggplot(evals) +
  geom_bar(aes(score))


#3. I selected cls_did_eval (students who did the evaluation) and cls_students (students in class). There is a strong positive correlation between the number of students in the class and the number of students who responded to the survey. About 58% of students respond to the surveys.

ggplot(evals, aes(cls_students, cls_did_eval)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  coord_equal(ratio=1)

lm(cls_did_eval ~ cls_students, data=evals)

cor(evals$cls_did_eval, evals$cls_students)

#4. The initial plot had many points plotted on top of one another. The jittered pointed do a better job of showing clusters of points.

ggplot(evals, aes(bty_avg, score)) +
  geom_jitter()

#5. The lm equation is score=0.06664*beauty + 3.88034 The slope indicates that for each increase in beauty average by 1 point, a teacher can expected an increase of 0.06664 points on their evaluations average.
#. The p-value on the slope is suitably small. This provides strong evidence of a relationship between the two variables. However, the sall R2 value of 0.03293 indicates that beauty only explains a small portion of the variable in scores.

m_bty <- lm(score ~ bty_avg, data=evals)
m_bty

summary(m_bty)

ggplot(evals, aes(bty_avg, score)) +
  geom_jitter() + 
  geom_smooth(se=FALSE, method='lm')

#6. The residual plot shows no pattern and consistent variability. The histogram of the residuals shows a heavy skew.


ggplot(evals) +
  geom_point(aes(bty_avg, m_bty$residuals)) +
  geom_hline(yintercept=0, color='red')

ggplot() +
  geom_histogram(aes(m_bty$residuals), bins=10)

#7. The conditions for regression are met and the p-value may be trusted.

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

#Linearity of residuals
ggplot(evals) +
  geom_point(aes(bty_avg, m_bty_gen$residuals)) +
  geom_hline(yintercept=0, color='red')


#Independence of residuals

ggplot() +
  geom_point(aes(1:463, m_bty_gen$residuals))


#Normal distribution of residuals

ggplot() +
  geom_histogram(aes(m_bty_gen$residuals), bins=20)

#Equal variance of residuals 

ggplot() +
  geom_point(aes(m_bty_gen$fitted.values, abs(m_bty_gen$residuals)))

#8. bty_avg is still a significant factor in score. The addition of gender has changed the size of its effect.

multiLines(m_bty_gen)

#9. The line corresponding to males is the blue line. Men tend to have a higher rating than Female of equal beauty. The size of this effect is calculated to be 0.17239 points.

#10. The regression spreads the data over 2 columns, putting a 1 in the "tenure track" column or "tenured" column when appropriate. If both are 0, then the teacher is in the "teaching".

m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)

#11. If I had to guess I would select "cls_profs". The number of professors in the class shouldn't effect a student's ranking because they are evaluating each professor separately.

m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)

#12. With a p-value of 0.77806, cls_prof is the least significant.

#13. If the coefficient is 0 that means the professor is is a minority while if it is a 1 that means the professor is not a minority. Teachers that are not minorities generally have slightly higher scores than those who are minorities.

#14. I dropped cls_profs and reran the regression. The coefficients and significance of the other variables changed, although some more than others.

m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)

#15. Removing cls_profs is the only category that can be removed from the regression. I attempted to remove each other category but all of them lead to a fall in the adjusted R2.

m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + 
               cls_students + cls_level + cls_credits + bty_avg + pic_outfit  +
               pic_color, data = evals)
summary(m_full)

#16. The below graphs show that all conditions for inference have been met.

#Linearity of residuals
ggplot(evals) +
  geom_point(aes(bty_avg, m_full$residuals)) +
  geom_hline(yintercept=0, color='red')


#Independence of residuals

ggplot() +
  geom_point(aes(1:463, m_full$residuals))


#Normal distribution of residuals

ggplot() +
  geom_histogram(aes(m_full$residuals), bins=20)

#Equal variance of residuals 

ggplot() +
  geom_point(aes(m_full$fitted.values, abs(m_full$residuals)))

#17. Yes. A professor that teaches more courses will be represented more times in the study and have undue weight on the overall answers. 

#18. According to our model a high scoring professor would be, in no particular order: neither tenured or on tenure track, not a minority, male, speaks english, is young, teaching upper level courses worth one credit and is physically attractive.

#19. It seems reasonable to generalize this study to other schools in the United States. I would be wary about straying too far away to places with different cultural norms.