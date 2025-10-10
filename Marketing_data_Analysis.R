library(dplyr)
library(ggplot2)

set.seed(42)
n <- 500

marketing_campaign <- data.frame(
  Customer_ID = 1:n,
  Age = sample(c(18:65, NA), n, replace = TRUE),  # Some missing ages
  Gender = sample(c("Male", "Female", NA), n, replace = TRUE),  # Some missing genders
  Annual_Income = sample(c(20000:150000, NA), n, replace = TRUE),
  Spending_Score = sample(c(1:100, NA), n, replace = TRUE),
  Ad_Clicks = sample(0:50, n, replace = TRUE),
  Purchase_Amount = round(runif(n, 50, 2000), 2),
  Region = sample(c("North", "South", "East", "West", NA), n, replace = TRUE)
)
# Randomly duplicate 20 rows to simulate messy data
duplicate_rows <- marketing_campaign[sample(1:n, 20), ]
marketing_campaign <- rbind(marketing_campaign, duplicate_rows)
# Remove duplicate rows
marketing_campaign <- distinct(marketing_campaign)

# Remove rows with missing (NA) values
marketing_campaign <- na.omit(marketing_campaign)

# Remove extreme Purchase_Amount values (top 1%)
marketing_campaign <- filter(marketing_campaign, Purchase_Amount < quantile(Purchase_Amount, 0.99))

# Rename columns to make them simpler
marketing_campaign <- rename(marketing_campaign,
                             ID = Customer_ID,
                             Income = Annual_Income,
                             Score = Spending_Score,
                             Clicks = Ad_Clicks,
                             Purchase = Purchase_Amount)

# View cleaned dataset
summary(marketing_campaign)
# Standardize column names if needed (safe, idempotent)
if ("Customer_ID" %in% names(marketing_campaign)) marketing_campaign <- marketing_campaign %>% rename(ID = Customer_ID)
if ("Annual_Income" %in% names(marketing_campaign)) marketing_campaign <- marketing_campaign %>% rename(Income = Annual_Income)
if ("Spending_Score" %in% names(marketing_campaign)) marketing_campaign <- marketing_campaign %>% rename(Score = Spending_Score)
if ("Ad_Clicks" %in% names(marketing_campaign)) marketing_campaign <- marketing_campaign %>% rename(Clicks = Ad_Clicks)
if ("Purchase_Amount" %in% names(marketing_campaign)) marketing_campaign <- marketing_campaign %>% rename(Purchase = Purchase_Amount)

# Remove exact duplicate rows and rows with any NA (use this cleaned df for plots)
mc_clean <- marketing_campaign %>%
  distinct() %>%
  na.omit()
# Quick preview
head(mc_clean)

# Structure and size
str(mc_clean)
dim(mc_clean)

# Summary statistics
summary(mc_clean)
# Age histogram
ggplot(mc_clean, aes(x = Age)) +
  geom_histogram(binwidth = 3, alpha = 0.8, color = "black") +
  labs(title = "Age distribution", x = "Age", y = "Count")

# Income histogram
ggplot(mc_clean, aes(x = Income)) +
  geom_histogram(binwidth = 10000, alpha = 0.8, color = "black") +
  labs(title = "Annual Income distribution", x = "Income", y = "Count")

# Purchase histogram
ggplot(mc_clean, aes(x = Purchase)) +
  geom_histogram(binwidth = 200, alpha = 0.8, color = "black") +
  labs(title = "Purchase Amount distribution", x = "Purchase", y = "Count")
# Gender counts
ggplot(mc_clean, aes(x = Gender)) +
  geom_bar() +
  labs(title = "Gender counts", x = "Gender", y = "Count")

# Region counts
ggplot(mc_clean, aes(x = Region)) +
  geom_bar() +
  labs(title = "Region counts", x = "Region", y = "Count")
# Income vs Purchase with linear fit
ggplot(mc_clean, aes(x = Income, y = Purchase)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Income vs Purchase (with linear fit)", x = "Income", y = "Purchase")

# Age vs Purchase
ggplot(mc_clean, aes(x = Age, y = Purchase)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Age vs Purchase (loess)", x = "Age", y = "Purchase")

# Purchase by Gender (boxplot)
ggplot(mc_clean, aes(x = Gender, y = Purchase)) +
  geom_boxplot() +
  labs(title = "Purchase by Gender", x = "Gender", y = "Purchase")

# Purchase by Region (boxplot)
ggplot(mc_clean, aes(x = Region, y = Purchase)) +
  geom_boxplot() +
  labs(title = "Purchase by Region", x = "Region", y = "Purchase")
# Income vs Purchase; color by Gender, size by Score, facet by Region
ggplot(mc_clean, aes(x = Income, y = Purchase, color = Gender, size = Score)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Region) +
  labs(title = "Income vs Purchase by Gender & Region (point size = Score)", x = "Income", y = "Purchase")
# Income distribution by Gender
ggplot(mc_clean, aes(x = Income)) +
  geom_histogram(binwidth = 10000, alpha = 0.6) +
  facet_wrap(~ Gender) +
  labs(title = "Income distribution by Gender", x = "Income", y = "Count")

# Purchase density by Region
ggplot(mc_clean, aes(x = Purchase, fill = Region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ Region) +
  labs(title = "Purchase density by Region", x = "Purchase")