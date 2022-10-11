library("foreign")
library("tidyverse")
library("ggplot2")
library("ROSE")

# Dataset retrieved from: https://www.openml.org/search?type=data&status=active&id=31

#setwd("~/Humber/Fall 2022/Big Data 2") # Location of the file

credit_df <- read.arff("dataset_31_credit-g.arff") #reading an arff file as a dataframe



class_plot <- ggplot(credit_df, aes(x=class, fill=class)) + geom_bar()
class_plot



purpose_df <- credit_df %>%
  group_by(purpose) %>%
  mutate(purpose_occur=n())

purpose_plot <- ggplot(purpose_df, aes(x=reorder(purpose, -purpose_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class)
purpose_plot 



job_df <- credit_df %>%
  group_by(job) %>%
  mutate(job_occur=n())

job_plot <- ggplot(job_df, aes(x=reorder(job, -job_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class)
job_plot 



history_df <- credit_df %>%
  group_by(credit_history) %>%
  mutate(history_occur=n())

history_plot <- ggplot(history_df, aes(x=reorder(credit_history, -history_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class)
history_plot 



savings_df <- credit_df %>%
  group_by(savings_status) %>%
  mutate(savings_occur=n())

savings_plot <- ggplot(savings_df, aes(x=reorder(savings_status, -savings_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class)
savings_plot 



amount_plot <- ggplot(credit_df, aes(x=credit_amount, fill=class)) + geom_density(alpha=0.25)
amount_plot



age_plot <- ggplot(credit_df, aes(x=age, fill=class)) + geom_density(alpha=0.25)
age_plot


credit_df_balanced <- ovun.sample(class~., data=credit_df, method="under", seed=42)$data
table(credit_df_balanced$class)



purpose_df_balanced <- credit_df_balanced %>%
  group_by(purpose) %>%
  mutate(purpose_occur=n())

purpose_plot_bal <- ggplot(purpose_df_balanced, aes(x=reorder(purpose, -purpose_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class)
purpose_plot_bal



job_df_balanced <- credit_df_balanced %>%
  group_by(job) %>%
  mutate(job_occur=n())

job_plot_bal <- ggplot(job_df_balanced, aes(x=reorder(job, -job_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class)
job_plot_bal



history_df_balanced <- credit_df_balanced %>%
  group_by(credit_history) %>%
  mutate(history_occur=n())

history_plot_balanced <- ggplot(history_df_balanced, aes(x=reorder(credit_history, -history_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class)
history_plot_balanced



savings_df_balanced <- credit_df_balanced %>%
  group_by(savings_status) %>%
  mutate(savings_occur=n())

savings_plot_balanced <- ggplot(savings_df_balanced, aes(x=reorder(savings_status, -savings_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class)
savings_plot_balanced  



amount_plot_balanced <- ggplot(credit_df_balanced, aes(x=credit_amount, fill=class)) + geom_density(alpha=0.25)
amount_plot_balanced



age_plot_balanced <- ggplot(credit_df_balanced, aes(x=age, fill=class)) + geom_density(alpha=0.25)
age_plot_balanced
