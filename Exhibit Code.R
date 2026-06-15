install.packages("readxl")
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)



#Create Exhibit 1 (Market Share)

MarketShare <- read_excel("~/Downloads/VisaMarketShare.xlsx", sheet = 2, skip = 3)



MarketShare_clean <- MarketShare %>%
  rename(Year = `Market share of payment card brands - Visa, Mastercard, American Express, or in-market local card schemes - in the United States from 2016 to 2024`) %>%
  select(Year, 2, 3, 4, 5, 
         6, 7, 8, 9, 10) %>%
  filter(!is.na(Year)) 


MarketShare_long <- MarketShare_clean %>%
  mutate(across(-Year, ~as.numeric(na_if(., "-")))) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = -Year, 
               names_to = "Company", 
               values_to = "MarketShare") %>% mutate(
                 Company = 
                   recode(Company,
                     '...2' = "Visa",
                     '...3' = "Mastercard",
                     '...4' = "American Express",
                     '...5' = "Discover",
                     '...6' = "Star",
                     '...7' = "Pulse",
                     '...8' = "Diners Club",
                     '...9' ="Local card schemes",
                     '...10' ="Others"
                   )
               )

ggplot(MarketShare_long, aes(x = factor(Year), y = MarketShare, fill = Company)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Exhibit #1",
    subtitle = "Market Share of Payment Card Schemes in the U.S (2016 - 2024)",
    x = "Year",
    y = "Market Share (%)",
    fill = "Scheme/Company"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#Exhibit 2 (Profit Margin)

Profit_Share <- read_excel("~/Visa.xlsx")
colnames(Profit_Share)[1] <- "Metric"

Profit_Share_Long <- Profit_Share %>%
  filter(Metric == "Profit Margin") %>%
  pivot_longer(cols = -Metric, names_to = "Year", values_to = "Value")

Profit_Share_Long$Year <- as.numeric(Profit_Share_Long$Year)

# Create the line graph
ggplot(Profit_Share_Long, aes(x = Year, y = Value)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Exhibit #2",
    subtitle = "Visa Profit Margin (2010-2014)",
       x = "Year",
       y = "Profit Margin") +
  theme_minimal()

