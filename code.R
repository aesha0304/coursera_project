library(ggplot2)
library(dplyr)
library(tidyr)

data("Boston", package = "MASS")

ggplot(data = Boston, aes(y = medv)) +
  geom_boxplot() +
  labs(title = "Boxplot of Median Value of Owner-Occupied Homes")

ggplot(data = Boston, aes(x = factor(chas), fill = factor(chas))) +
  geom_bar() +
  labs(title = "Bar Plot of Charles River Variable") +
  scale_fill_discrete(name = "Charles River", labels = c("Not bounded", "Bounded"))

Boston %>%
  mutate(age_group = cut(age, breaks = c(0, 35, 70, max(age)), labels = c("35 years and younger", "Between 35 and 70 years", "70 years and older"))) %>%
  ggplot(aes(x = age_group, y = medv)) +
  geom_boxplot() +
  labs(title = "Boxplot of Median Value of Homes vs Age Group", x = "Age Group", y = "Median Value of Homes")

ggplot(data = Boston, aes(x = nox, y = indus)) +
  geom_point() +
  labs(title = "Scatter Plot of Nitric Oxide Concentrations vs Proportion of Non-Retail Business Acres",
       x = "Nitric Oxide Concentrations",
       y = "Proportion of Non-Retail Business Acres per Town")

ggplot(data = Boston, aes(x = ptratio)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Pupil to Teacher Ratio", x = "Pupil to Teacher Ratio", y = "Frequency")

t_test_result <- t.test(medv ~ chas, data = Boston)
t_test_result

anova_result <- aov(medv ~ cut(age, breaks = c(0, 1940, max(Boston$age))), data = Boston)
summary(anova_result)

correlation <- cor.test(Boston$nox, Boston$indus, method = "pearson")
correlation

regression_model <- lm(medv ~ dis, data = Boston)
summary(regression_model)
