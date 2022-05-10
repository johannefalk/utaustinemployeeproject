rm(list=ls()) # reset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set working directory to source location
library(ggplot2)
library(reshape2)
library(data.table)
library(stringr)
library(gridExtra)
library(car)

# Read data
data = na.omit(fread("utaustin_locations_jobs.csv")) # Read and remove NAs
data = data[(race != "") & (race != "NATIVE HAWAIIAN/PACIFIC ISLANDER")]
data = data[word(data$address,-2) == "TX"] # Only keep texas observations
data$job = as.factor(trimws(data$job))
data$dept = as.factor(data$dept)
data$job = as.factor(data$job)
data$hire_date = as.Date(as.character(data$hire_date), format = "%Y%m%d")
data$race = as.factor(data$race)
data$gender = as.factor(data$gender)
data$type = as.factor(data$type)
data$zip = as.factor(word(data$address,-1))

# Most frequent jobs
top10job = names(sort(table(data$job), decreasing = T)[1:10])

# New dataframe only included top 10 jobs
data2 = data[job %in% top10job]
data2$job = droplevels(data2$job)

# Plots of salary distribution by race and gender
par(mfrow = c(1,1))
p1 = ggplot(data2)+
  geom_boxplot(aes(x = job, y = salary, col = gender))+
  theme(axis.text.x = element_text(angle = 15))
p2 = ggplot(data2)+
  geom_boxplot(aes(x = job, y = salary, col = race))+
  theme(axis.text.x = element_text(angle = 15))
grid.arrange(p1, p2, nrow = 2)


# Boxplot by job only
ggplot(data2)+
  geom_boxplot(aes(x = job, y = salary, col = job))+
  theme(axis.text.x = element_text(angle = 15))

# Not important
data2$raceGender = paste0(data2$race," - ",data2$gender)
ggplot(data2)+
  geom_boxplot(aes(x = job, y = salary, col = raceGender))+
  theme(axis.text.x = element_text(angle = 15)) +
  theme(legend.position="bottom")

# Boxplot in decending order by job
library(tidyr)
library(tidyverse)
library(scales)
data2 %>% 
  ggplot(aes(x= fct_reorder(job,salary, .desc=TRUE), y=salary, fill=job)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 15, h = 0.9))+
  scale_y_continuous(labels = comma) +
  xlab("Job") +
  ylab("Yearly salary $")


# Anova
# We chocse this GLM Anova model
drop1(fit1 <- glm(salary ~  job*race +job*gender + race*gender, data = data2, family = Gamma), test = "Chisq")
drop1(fit2 <- update(fit1, ~. -job:race), test = "Chisq")
drop1(fit3 <- update(fit2, ~. -race:gender), test = "Chisq")
par(mfrow = c(2,2))
plot(fit3)
Anova(fit3)

# Not this model
fitSimple = glm(salary ~  job + race + gender, data = data2, family = Gamma)
Anova(fitSimple)
Anova = aov(salary ~ job + race + gender, data = data2)

# Post hoc
test = pairwise.t.test(log(data2$salary), data2$race,p.adjust = "bonferroni")$p.value < 0.05
test.melt = melt(test)

test.melt$value2 = ifelse(test.melt$value == TRUE, "Different salary", ifelse(test.melt$value == FALSE, "Same salary", NA))


# Plot p-values for job
ggplot(test.melt, aes(x = Var2, y = Var1, fill = value2)) +
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 30,, hjust=1)) +
  ylab("") +
  xlab("") +
  scale_fill_manual(values = c('dark red','light green'), na.value = 'white')+
  theme(legend.title = element_blank())+
  coord_equal() +
  ggtitle("Salary difference in race")+
  theme(plot.title = element_text(hjust = 0.5))
  


# Plotting p-values of t-test by gender/job
menWomen = data.table(job = sort(unique(data2$job)), y = 1,
                      pvalue = c("Same salary","Same salary",
                                 "Different salary","Different salary",
                                 "Different salary","Same salary",
                                 "Different salary","Different salary",
                                 "Different salary","Same salary"))

ggplot(menWomen, aes(job, y)) +
  geom_raster(aes(fill = pvalue)) +
  coord_equal() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, hjust=1)) +
  ylab("") +
  ggtitle("Salary discrimination by gender")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y= element_blank()) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c('dark red','light green'))

