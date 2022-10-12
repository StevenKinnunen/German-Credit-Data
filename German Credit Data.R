library("foreign") # This library allows us to use the read.arff() function
library("tidyverse")
library("ggplot2")
library("ROSE") # This library allows us to use ovun.sample() to balance the class variable
# by either oversampling from the minority class ('bad') or undersampling from the majority
# class ('good')

# Dataset retrieved from: https://www.openml.org/search?type=data&status=active&id=31

#setwd("~/Humber/Fall 2022/Big Data 2") # Location of the file

credit_df <- read.arff("dataset_31_credit-g.arff") #reading an arff file as a dataframe


# This plot allows us to look at the count of 'good' and 'bad' values in class column
class_plot <- ggplot(credit_df, aes(x=class, fill=class)) + geom_bar()
class_plot 


# Creates a new dataframe that will allow us to reorder based on count of purpose column
purpose_df <- credit_df %>%
  group_by(purpose) %>%
  mutate(purpose_occur=n()) 

# Creates a plot based on the purpose dataframe, orders by descending values for count
purpose_plot <- ggplot(purpose_df, aes(x=reorder(purpose, -purpose_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class) # facet wrap produces multiple plots (2) based on class values
purpose_plot 


# Creates a new dataframe that will allow us to reorder based on count of job column
job_df <- credit_df %>%
  group_by(job) %>%
  mutate(job_occur=n())

# Creates a plot based on the job dataframe, orders by descending values for count
job_plot <- ggplot(job_df, aes(x=reorder(job, -job_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class) # facet wrap produces multiple plots (2) based on class values
job_plot 


# Creates a new dataframe that will allow us to reorder based on count of credit_history column
history_df <- credit_df %>%
  group_by(credit_history) %>%
  mutate(history_occur=n())

# Creates a plot based on the history dataframe, orders by descending values for count
history_plot <- ggplot(history_df, aes(x=reorder(credit_history, -history_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class) # facet wrap produces multiple plots (2) based on class values
history_plot 


# Creates a new dataframe that will allow us to reorder based on count of savings_status column
savings_df <- credit_df %>%
  group_by(savings_status) %>%
  mutate(savings_occur=n())

# Creates a plot based on the savings dataframe, orders by descending values for count
savings_plot <- ggplot(savings_df, aes(x=reorder(savings_status, -savings_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class) # facet wrap produces multiple plots (2) based on class values
savings_plot 


# Creates a density plot based on original credit dataframe to show proportion of class values by credit_amount
# low alpha value allows us to view overlapping densities
amount_plot <- ggplot(credit_df, aes(x=credit_amount, fill=class)) + geom_density(alpha=0.25)
amount_plot


# Creates a density plot based on original credit dataframe to show proportion of class values by age
# low alpha value allows us to view overlapping densities
age_plot <- ggplot(credit_df, aes(x=age, fill=class)) + geom_density(alpha=0.25)
age_plot


# We create a new dataframe that undersamples from the 'good' records for the class target variable
credit_df_balanced <- ovun.sample(class~., data=credit_df, method="under", seed=42)$data
table(credit_df_balanced$class) #table checks the number of each class value in balanced dataframe


# Creates a new dataframe that will allow us to reorder based on count of purpose column
purpose_df_balanced <- credit_df_balanced %>%
  group_by(purpose) %>%
  mutate(purpose_occur=n())

# Creates a plot based on the purpose dataframe, orders by descending values for count
purpose_plot_bal <- ggplot(purpose_df_balanced, aes(x=reorder(purpose, -purpose_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class) # facet wrap produces multiple plots (2) based on class values
purpose_plot_bal


# Creates a new dataframe that will allow us to reorder based on count of job column
job_df_balanced <- credit_df_balanced %>%
  group_by(job) %>%
  mutate(job_occur=n())

# Creates a plot based on the job dataframe, orders by descending values for count
job_plot_bal <- ggplot(job_df_balanced, aes(x=reorder(job, -job_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class) # facet wrap produces multiple plots (2) based on class values
job_plot_bal


# Creates a new dataframe that will allow us to reorder based on count of credit_history column
history_df_balanced <- credit_df_balanced %>%
  group_by(credit_history) %>%
  mutate(history_occur=n())

# Creates a plot based on the history dataframe, orders by descending values for count
history_plot_balanced <- ggplot(history_df_balanced, aes(x=reorder(credit_history, -history_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class) # facet wrap produces multiple plots (2) based on class values
history_plot_balanced


# Creates a new dataframe that will allow us to reorder based on count of savings_status column
savings_df_balanced <- credit_df_balanced %>%
  group_by(savings_status) %>%
  mutate(savings_occur=n())

# Creates a plot based on the savings dataframe, orders by descending values for count
savings_plot_balanced <- ggplot(savings_df_balanced, aes(x=reorder(savings_status, -savings_occur), fill=class)) + 
  geom_bar(stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~class) # facet wrap produces multiple plots (2) based on class values
savings_plot_balanced  


# Creates a density plot based on original credit dataframe to show proportion of class values by credit_amount
# low alpha value allows us to view overlapping densities
amount_plot_balanced <- ggplot(credit_df_balanced, aes(x=credit_amount, fill=class)) + geom_density(alpha=0.25)
amount_plot_balanced


# Creates a density plot based on original credit dataframe to show proportion of class values by age
# low alpha value allows us to view overlapping densities
age_plot_balanced <- ggplot(credit_df_balanced, aes(x=age, fill=class)) + geom_density(alpha=0.25)
age_plot_balanced
