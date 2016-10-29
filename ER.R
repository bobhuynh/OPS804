library(reshape2) # For resahping the data
library(dplyr) # For data preperation
library(e1071) # Skewness and Kurtosis
library(car) # For the Levenes test
library(ggplot2) # For nice plotting
setwd("C:\\GitHub\\kh28\\Class2\\Cars")
ER <- read.csv(file = "ERWaiting.csv")
head(ER) #output first six recordds

#melt the data: convert from wide format to long format
ER <- melt(ER, id= "day")

#rename column names for better readability
colnames(ER) <- c("day","type","time")

#factor the data
ER$day <- as.factor(ER$day)

#Generate Descriptive Stats

ER.desc <- ER %>%
  group_by(type) %>%
  summarise(
    count = n(),
    sum = sum(time, na.rm = TRUE),
    min = min(time, na.rm = TRUE),
    max = max(time, na.rm = TRUE),
    mean = mean(time, na.rm = TRUE),
    median = median(time, na.rm = TRUE),
    range = max - min,
    q1 = as.numeric(quantile(time, na.rm = TRUE)[2]),
    q3 = as.numeric(quantile(time, na.rm = TRUE)[4]),
    iqr = q3 - q1,
    sd = sd(time, na.rm = TRUE),
    var = var(time, na.rm = TRUE),
    kurt = kurtosis(time, na.rm = TRUE),
    skew = skewness(time, na.rm = TRUE),
    shapiro = shapiro.test(time)$statistic[1]
  )



## ANOVA - Fit the data
fit <- lm(data = ER, time~type)
ER.aov <- aov(fit)
ER.aov.summary <- summary(ER.aov)
print(ER.aov)
print(ER.aov.summary)


## Perform the Tukey-Kramer procedure
ER.tukey <- TukeyHSD(ER.aov)
## Print results
print(ER.tukey)

# Create plot and save it into a variable
qqp <- ggplot(ER) +
  stat_qq(aes(sample = time, colour = factor(type))) +
  guides(col = guide_legend(title = "Hospital Type"))
print(qqp)

## Homogenity of variance
ER.levene <- leveneTest(fit)
## Print results
print(ER.levene)

