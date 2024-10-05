
library(dplyr)
library(ggplot2)

#import data 
sd254_cards <- read.csv("D:\\Master's Data Analytics\\MPS_Quarter 1\\ALY6000_Intro to Analytics\\sd254_cards.csv", header=TRUE, stringsAsFactors=FALSE)
sd254_cards

#Part 1
#dataset structure 
struc <- str(sd254_cards)

# Remove the dollar sign ($) from 'Credit.Limit' variable
sd254_cards$Credit.Limit <- as.numeric(gsub("\\$", "", sd254_cards$Credit.Limit))
# Print the updated 'Credit.Limit' values
print(sd254_cards$Credit.Limit)

#summary statistics 
summary(sd254_cards$Card.Number)
summary(sd254_cards$Expires)
summary( sd254_cards$CVV)
summary(sd254_cards$Cards.Issued)
summary(sd254_cards$Credit.Limit)
summary(sd254_cards$Acct.Open.Date)
summary(sd254_cards$sd254_cards$Year.PIN.last.Changed)

# Summarize the data in a table
summary_table <- summary(sd254_cards)
print(summary_table)

# Graphs to visualize the data
# Example bar chart to give frequency count of cards brand 
bar_chart <- ggplot(sd254_cards, aes(x = Card.Brand)) +
  geom_bar(fill = "blue") +
  labs(title = "Card Brand Distribution", x = "Card Brand", y = "Frequency Count")
print(bar_chart)
# Example pie chart for displaying if card has sensor chips
pie_chart <- ggplot(sd254_cards, aes(x = "", fill = Has.Chip)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Chip Cards", fill = "Has Chip")
print(pie_chart)

#check unsual values 
# Clean the data by removing out-of-range values for Credit.Limit
data_clean <- sd254_cards %>% filter(Credit.Limit >= 0 & Credit.Limit <= 40000)
data_clean
#(with out-of-range values) ORIGINAL Data
# 1. Summarize the data in a table 
summary_table <- sd254_cards %>%
  summarise(
    Average_Cards_Issued = mean(Cards.Issued),
    Average_Credit_Limit = mean(Credit.Limit)
  )
print("Summary Table (with out-of-range values):")
print(summary_table)
# Histogram: Credit Limit Distribution
credit_limit_chart <- ggplot(sd254_cards, aes(x = Credit.Limit)) +
  geom_histogram(binwidth = 5000, fill = "steelblue") +
  labs(title = "Credit Limit Distribution (with out-of-range values)", x = "Credit Limit", y = "Count")
print(credit_limit_chart)

# (without out-of-range values) FILTERED DATA CREDIT.LIMIT OF $40000
# Summarize the data in a table 
summary_table_clean <- data_clean %>%
  summarise(
    Average_Cards_Issued = mean(Cards.Issued),
    Average_Credit_Limit = mean(Credit.Limit)
  )
print("Summary Table (without out-of-range values):")
print(summary_table_clean)
# Histogram: Credit Limit Distribution
credit_limit_chart_clean <- ggplot(data_clean, aes(x = Credit.Limit)) +
  geom_histogram(binwidth = 5000, fill = "steelblue") +
  labs(title = "Credit Limit Distribution(without out-of-range values)", x = "Credit Limit", y = "Count")
print(credit_limit_chart_clean)




#Part 2
#New variable 1
# Calculate difference in credit limit utilization
sd254_cards$Credit.Utilization <- sd254_cards$Credit.Limit / sd254_cards$Cards.Issued
head(sd254_cards,5)
#New Variable 2
# Calculate difference in years since the PIN was last changed
current_year <- 2023  # Assuming the current year is 2023
sd254_cards$Years.Since.PIN.Change <- current_year - sd254_cards$Year.PIN.last.Changed

# Compute mean and median for the new variables
mean_credit_utilization <- mean(sd254_cards$Credit.Utilization)
median_credit_utilization <- median(sd254_cards$Credit.Utilization)
cat("Mean Credit Utilization:", mean_credit_utilization, "\n")
cat("Median Credit Utilization:", median_credit_utilization, "\n")


mean_years_since_pin_change <- mean(sd254_cards$Years.Since.PIN.Change)
median_years_since_pin_change <- median(sd254_cards$Years.Since.PIN.Change)
cat("Mean Years Since PIN Change:", mean_years_since_pin_change, "\n")
cat("Median Years Since PIN Change:", median_years_since_pin_change, "\n")


