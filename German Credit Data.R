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


# This plot allows us to look at the count of 'good' and 'bad' values in class column
class_plot <- ggplot(credit_df, aes(x=class, fill=class)) + geom_bar() +
  scale_fill_manual(values = class_palette) + # Sets the colors to class_palette and fill by manually
  ggtitle("Count of Customers with Good and Bad Credit") + # Sets a title for the plot
  labs(y="Count", x="Credit Type") + # Sets the y and x axis labels respectively for the plot
  theme(plot.title = element_text(hjust=0.5), legend.position = "none") # Centre the title and
# suppresses the legend since it is redundant in this plot
class_plot 


# Creates a new dataframe that will allow us to reorder based on count of purpose column
purpose_df <- credit_df %>%
  group_by(purpose) %>% # Selecting the purpose column to group by
  mutate(purpose_occur=n()) # adds column purpose_occur, which counts how often the specific value of purpose 
# occurs and adds the total count to each row with grouped purpose value

# Creates a plot based on the purpose dataframe, orders by descending values for count
purpose_plot <- ggplot(purpose_df, aes(x=reorder(purpose, -purpose_occur), fill=class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) + #
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers by Purpose of Loan and Credit Type") + labs(y="Count", x="Purpose of Loan") +
  theme(plot.title = element_text(hjust=0.5))
purpose_plot 


# Creates a new dataframe that will allow us to reorder based on count of job column
job_df <- credit_df %>%
  group_by(job) %>%
  mutate(job_occur=n())

# Creates a plot based on the job dataframe, orders by descending values for count
job_plot <- ggplot(job_df, aes(x=reorder(job, -job_occur), fill=class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers by Occupation and Credit Type") + labs(y="Count", x="Occupation of Applicant") +
  theme(plot.title = element_text(hjust=0.5))
job_plot 


# Creates a new dataframe that will allow us to reorder based on count of credit_history column
history_df <- credit_df %>%
  group_by(credit_history) %>%
  mutate(history_occur=n())

# Creates a plot based on the history dataframe, orders by descending values for count
history_plot <- ggplot(history_df, aes(x=reorder(credit_history, -history_occur), fill=class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers by Credit History and Credit Type") + labs(y="Count", x="Credit History") +
  theme(plot.title = element_text(hjust=0.5))
history_plot 


# Creates a new dataframe that will allow us to reorder based on count of savings_status column
savings_df <- credit_df %>%
  group_by(savings_status) %>%
  mutate(savings_occur=n())

# Creates a plot based on the savings dataframe, orders by descending values for count
savings_plot <- ggplot(savings_df, aes(x=reorder(savings_status, -savings_occur), fill=class)) + 
  geom_bar(position="dodge", stat="count") + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Count of Customers by Savings and Credit Type") + labs(y="Count", x="Savings") +
  theme(plot.title = element_text(hjust=0.5))
savings_plot 


# Creates a density plot based on original credit dataframe to show proportion of class values by credit_amount
# low alpha value allows us to view overlapping densities
amount_plot <- ggplot(credit_df, aes(x=credit_amount, fill=class)) + geom_density(alpha=0.50) +
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Density of Customers by Loan Amount and Credit Type") + labs(y="Proportion", x="Loan Amount") +
  theme(plot.title = element_text(hjust=0.5))
amount_plot


# Creates a density plot based on original credit dataframe to show proportion of class values by age
# low alpha value allows us to view overlapping densities
age_plot <- ggplot(credit_df, aes(x=age, fill=class)) + geom_density(alpha=0.50) +
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Density of Customers by Age and Credit Type") + labs(y="Proportion", x="Age") +
  theme(plot.title = element_text(hjust=0.5))
age_plot


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

# Creates a bar plot based on propertry magnitude (are these meant to be assets?) and credit - could look further into this maybe
property_plot = ggplot(data = credit_df, aes(x= property_magnitude, colour = class)) + geom_bar(position = "dodge", stat = "count")
property_plot

#Creates a bar plot based on personal staus (sex and martial status) 
personvcredit_plot = ggplot(data = credit_df, aes(x= personal_status, colour = class)) + geom_bar(position = "dodge", stat = "count")
personvcredit_plot


# Creates a function for an asymmetrical violin plot integrating a dichotomous variable.
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

# Create a split violin plot for savings and credit, distinguishing good from bad classes of credit.
credit_by_savings_plot <- ggplot(savings_df_balanced, aes(x=reorder(savings_status, -savings_occur), 
                                                          y=credit_amount, fill=class)) +
  geom_split_violin() + theme_minimal() + 
  labs(x="Savings Status", y="Credit",title="Credit Density by Savings and Class")
credit_by_savings_plot
                                                      

#Boxplot housing type, credit amount_SM
p2 <- ggplot(credit_df, aes(x=housing, y=credit_amount, fill=class)) + 
  geom_boxplot() +
  facet_wrap(~housing, scale="free")
p2

#Boxplot job type and credit amount_SM
p1 <- ggplot(credit_df, aes(x=job, y=credit_amount, fill=class)) + 
  geom_boxplot() +
  facet_wrap(~class) + coord_flip()
p1

#Density Residence since_SM
residence_since_plot_balanced <- ggplot(credit_df_balanced, aes(x=residence_since, fill=class)) + geom_density(alpha=0.50) + 
  scale_fill_manual(name="Credit Type", values = class_palette) +
  ggtitle("Density of Customers by Residence Since") + labs(y="Proportion", x="Residence_Since") +
  theme(plot.title = element_text(hjust=0.5))
residence_since_plot_balanced
