# YUVA Intern - Virtual R Data Analyst Internship
# UCI Adult Income Dataset
# Set your working directory to the project root before running.

library(ggplot2)
library(dplyr)

adult_raw <- read.csv("Data/adult.data", header=FALSE,
                      na.strings="?", strip.white=TRUE)
colnames(adult_raw) <- c("age","workclass","fnlwgt","education",
 "education_num","marital_status","occupation","relationship","race",
 "sex","capital_gain","capital_loss","hours_per_week",
 "native_country","income")

adult_clean <- na.omit(adult_raw)
factor_cols <- c("workclass","education","marital_status","occupation",
                 "relationship","race","sex","native_country","income")
adult_clean[factor_cols] <- lapply(adult_clean[factor_cols], factor)

# Keep original data for interpretation; create normalized copy separately.
normalize01 <- function(x) (x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
adult_normalized <- adult_clean
adult_normalized$age <- normalize01(adult_clean$age)
adult_normalized$education_num <- normalize01(adult_clean$education_num)
adult_normalized$hours_per_week <- normalize01(adult_clean$hours_per_week)

# Duplicate check
sum(duplicated(adult_clean))

# Descriptive statistics
summary(adult_clean[,c("age","fnlwgt","education_num",
                       "capital_gain","capital_loss","hours_per_week")])

# Income distribution
prop.table(table(adult_clean$income))*100

# Week 2 visualizations
p1 <- ggplot(adult_clean,aes(education,fill=income))+
  geom_bar(position="fill")+
  labs(title="Income Distribution by Education Level",
       x="Education",y="Proportion")+
  theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave("plots/week2_education_income.png",p1,width=10,height=6)

p2 <- ggplot(adult_clean,aes(workclass,fill=income))+
  geom_bar(position="fill")+
  labs(title="Income Distribution by Workclass",
       x="Workclass",y="Proportion")+
  theme_minimal()+theme(axis.text.x=element_text(angle=30,hjust=1))
ggsave("plots/week2_workclass_income.png",p2,width=9,height=6)

p3 <- ggplot(adult_clean,aes(age,fill=income))+
  geom_histogram(bins=30,alpha=.7,position="identity")+
  labs(title="Age Distribution by Income Group",x="Age",y="Count")+
  theme_minimal()
ggsave("plots/week2_age_histogram.png",p3,width=9,height=6)

p4 <- ggplot(adult_clean,aes(education_num,hours_per_week,color=income))+
  geom_point(alpha=.25)+
  geom_smooth(method="lm",se=FALSE)+
  labs(title="Education Number vs Hours Worked",x="Education Number",
       y="Hours per Week")+theme_minimal()
ggsave("plots/week2_scatter.png",p4,width=9,height=6)
