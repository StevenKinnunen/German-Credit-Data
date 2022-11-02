library("foreign") # This library allows us to use the read.arff() function
library("tidyverse")
library("ggplot2")
library("ROSE") # This library allows us to use ovun.sample() to balance the class variable
# by either oversampling from the minority class ('bad') or undersampling from the majority
# class ('good')

# Dataset retrieved from: https://www.openml.org/search?type=data&status=active&id=31

setwd("~/Humber/Fall 2022/Big Data 2") # Location of the file

credit_df <- read.arff("dataset_31_credit-g.arff") #reading an arff file as a dataframe

class_palette <- c(good = "#000066", bad = "#660000") # setting consistent color palette for class values,
# good are blue, whereas bad are red


# This pie chart allows us to look at the count of 'good' and 'bad' values in the class variable.
class_plot <- ggplot(credit_df, aes(x="", y=class, fill=class)) + geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + scale_fill_manual(name="Class", values = class_palette) + # Sets the colors to class_palette and fill by manually
  ggtitle("Customers' Credit Classification") + # Sets a title for the pie.
  labs(y=NULL, x=NULL) + # Removes the y and x axis labels for the pie.
  theme(plot.title = element_text(hjust=0.5), axis.text=element_blank(), axis.ticks=element_blank(), 
        panel.grid=element_blank(), legend.position="bottom") # Removes excess labels and centres the legend.
class_plot 


# We create a new dataframe that oversamples from the 'bad' records for the class target variable
credit_df_balanced <- ovun.sample(class~., data=credit_df, method="over", seed=42)$data
table(credit_df_balanced$class) #table checks the number of each class value in balanced dataframe


# Creates a new dataframe that will allow us to reorder based on count of purpose column
purpose_df_balanced <- credit_df_balanced %>%
  group_by(purpose) %>%
  mutate(purpose_occur=n())

# Creates a plot based on the purpose dataframe, orders by descending values for count
purpose_plot_bal <- ggplot(purpose_df_balanced, aes(x=reorder(purpose, -purpose_occur), fill=class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers by Purpose of Loan and Credit Type") + labs(y="Count", x="Purpose of Loan") +
  theme(plot.title = element_text(hjust=0.5))
purpose_plot_bal


# Creates a new dataframe that will allow us to reorder based on count of job column
job_df_balanced <- credit_df_balanced %>%
  group_by(job) %>%
  mutate(job_occur=n())

# Creates a plot based on the job dataframe, orders by descending values for count
job_plot_bal <- ggplot(job_df_balanced, aes(x=reorder(job, -job_occur), fill=class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers by Occupation and Credit Type") + labs(y="Count", x="Occupation of Applicant") +
  theme(plot.title = element_text(hjust=0.5))
job_plot_bal


# Creates a new dataframe that will allow us to reorder based on count of credit_history column
history_df_balanced <- credit_df_balanced %>%
  group_by(credit_history) %>%
  mutate(history_occur=n())

# Creates a plot based on the history dataframe, orders by descending values for count
history_plot_balanced <- ggplot(history_df_balanced, aes(x=reorder(credit_history, -history_occur), fill=class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers by Credit History and Credit Type") + labs(y="Count", x="Credit History") +
  theme(plot.title = element_text(hjust=0.5))
history_plot_balanced


# Creates a new dataframe that will allow us to reorder based on count of savings_status column
savings_df_balanced <- credit_df_balanced %>%
  group_by(savings_status) %>%
  mutate(savings_occur=n())

# Creates a plot based on the savings dataframe, orders by descending values for count
savings_plot_balanced <- ggplot(savings_df_balanced, aes(x=reorder(savings_status, -savings_occur), fill=class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers by Savings and Credit Type") + labs(y="Count", x="Savings") +
  theme(plot.title = element_text(hjust=0.5))
savings_plot_balanced  


# Creates a density plot based on original credit dataframe to show proportion of class values by credit_amount
# low alpha value allows us to view overlapping densities
amount_plot_balanced <- ggplot(credit_df_balanced, aes(x=credit_amount, fill=class)) + geom_density(alpha=0.50) +
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Density of Customers by Loan Amount and Credit Type") + labs(y="Proportion", x="Loan Amount") +
  theme(plot.title = element_text(hjust=0.5))
amount_plot_balanced


# Creates a density plot based on original credit dataframe to show proportion of class values by age
# low alpha value allows us to view overlapping densities
age_plot_balanced <- ggplot(credit_df_balanced, aes(x=age, fill=class)) + geom_density(alpha=0.50) + 
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Density of Customers by Age and Credit Type") + labs(y="Proportion", x="Age") +
  theme(plot.title = element_text(hjust=0.5))
age_plot_balanced

# Creates a bar plot based on property type
property_plot = ggplot(data = credit_df_balanced, aes(x= property_magnitude, fill = class)) + geom_bar(position = "dodge", stat = "count") +
  scale_fill_manual(name="Credit Type", values = class_palette) +  ggtitle("Count of Customers by Asset Type and Credit Type") + labs(y="Count", x= "Asset Type") +
  theme(plot.title = element_text(hjust=0.5))
property_plot

#Creates a bar plot based on personal staus (sex and martial status) 
personvcredit_plot = ggplot(data = credit_df, aes(x= personal_status, fill = class)) + geom_bar(position = "dodge", stat = "count") +
  scale_fill_manual(name="Credit Type", values = class_palette)
personvcredit_plot

#Short-term vs Long-term
short_term_loan <- filter(credit_df_balanced, duration <= 18)
long_term_loan <- filter(credit_df_balanced, duration > 18)

short_df <- short_term_loan %>%
  group_by(class) %>%
  mutate(short_diff=n())

short_plot <- ggplot(short_df, aes(x=reorder(class, -short_diff), fill = class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) + #
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers that have a short-term loan grouped by credit type") + labs(y="Count", x="Good vs. Bad") +
  theme(plot.title = element_text(hjust=0.5))
short_plot

long_df <- long_term_loan %>%
  group_by(class) %>%
  mutate(long_diff=n())

long_plot <- ggplot(long_df, aes(x=reorder(class, -long_diff), fill = class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) + #
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers that have a long-term loan grouped by credit type") + labs(y="Count", x="Good vs. Bad") +
  theme(plot.title = element_text(hjust=0.5))
long_plot
