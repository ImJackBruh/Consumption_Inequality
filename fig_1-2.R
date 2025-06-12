# Calculate Var. and Gini coef. etc.
# Author: BoYanHuang
# Created Jun 7, 2025
# Last modify Jun 12, 2025 by BoYanHuang

library(tidyverse)
library(DescTools)
library(modi)

# read the cross sectional all file
df_all <- read.csv("HH_inc_exp//HH_all.csv") %>% as_tibble()
df_all <- df_all %>% filter(year != 2012)

# !!NOTE: some NA are zero and some NA are missing val.
# we turn all 0 to NA for earnings
df_all <- df_all %>% mutate(inc_earnings = ifelse(inc_earnings <= 100, NA, inc_earnings))
df_all <- df_all %>% mutate(across(starts_with('inc_earnings'), ~ ifelse(. <= 100, NA, .)))

# Fig 1 for wage
df_Fig_1_wage <- df_all %>% group_by(year) %>% 
    summarise(Var_wage = weighted.var(log(inc_earnings, exp(1)), weight, na.rm = T),
              Gini_wage = Gini(inc_earnings, weight, na.rm = T),
              P50_P10 = weighted.quantile(inc_earnings, weight, prob = 0.5) / weighted.quantile(inc_earnings, weight, prob = 0.1),
              P90_P50 = weighted.quantile(inc_earnings, weight, prob = 0.9) / weighted.quantile(inc_earnings, weight, prob = 0.5))
# plot
ggplot(df_Fig_1_wage, aes(x = year, y = Var_wage)) +
    geom_line(colour = "blue", linewidth = 1) +
    labs(title = "Variance of Log Equiv. HH Earnings", x = "Year", y = "Variance of Log") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_1//Fig_1a_Var_Earnings.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_1_wage, aes(x = year, y = Gini_wage)) +
    geom_line(colour = "blue", linewidth = 1) +
    labs(title = "Gini Coefficient of Equiz. HH Earnings", x = "Year", y = "Gini Coefficient") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_1//Fig_1b_Gini_Earnings.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_1_wage, aes(x = year, y = P50_P10)) +
    geom_line(colour = "blue", linewidth = 1) +
    labs(title = "P50-P10 Ratio of Equiv. HH Earnings", x = "Year", y = "Ratio") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_1//Fig_1c_P50_P10_Earnings.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_1_wage, aes(x = year, y = P90_P50)) +
    geom_line(colour = "blue", linewidth = 1) +
    labs(title = "P90-P50 Ratio of Equiv. HH Earnings", x = "Year", y = "Ratio") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_1//Fig_1d_P90_P50_Earnings.png", dpi = 600, width = 4, height = 4)

# Fig 2 for Percentiles of the household earnings distribution 1981
df_Fig_2_wage <- df_all %>% group_by(year) %>% 
    summarise(P5 = weighted.quantile(inc_earnings, weight, prob = 0.05),
              P10 = weighted.quantile(inc_earnings, weight, prob = 0.1),
              P25 = weighted.quantile(inc_earnings, weight, prob = 0.25),
              P50 = weighted.quantile(inc_earnings, weight, prob = 0.50),
              P75 = weighted.quantile(inc_earnings, weight, prob = 0.75),
              P90 = weighted.quantile(inc_earnings, weight, prob = 0.9),
              P95 = weighted.quantile(inc_earnings, weight, prob = 0.95))
df_Fig_2_wage <- df_Fig_2_wage %>%
    mutate(n_P5 = P5 - as.numeric(df_Fig_2_wage[1, "P5"]),
           n_P10 = P10 - as.numeric(df_Fig_2_wage[1, "P10"]),
           n_P25 = P25 - as.numeric(df_Fig_2_wage[1, "P25"]),
           n_P50 = P50 - as.numeric(df_Fig_2_wage[1, "P50"]),
           n_P75 = P75 - as.numeric(df_Fig_2_wage[1, "P75"]),
           n_P90 = P90 - as.numeric(df_Fig_2_wage[1, "P90"]),
           n_P95 = P95 - as.numeric(df_Fig_2_wage[1, "P95"]))
df_Fig_2_wage <- df_Fig_2_wage %>%
    mutate(log_P5 = log(P5, exp(1)),
           log_P10 = log(P10, exp(1)),
           log_P25 = log(P25, exp(1)),
           log_P50 = log(P50, exp(1)),
           log_P75 = log(P75, exp(1)),
           log_P90 = log(P90, exp(1)),
           log_P95 = log(P95, exp(1)))
df_Fig_2_wage <- df_Fig_2_wage %>%
    mutate(n_log_P5 = log_P5 - as.numeric(df_Fig_2_wage[1, "log_P5"]),
           n_log_P10 = log_P10 - as.numeric(df_Fig_2_wage[1, "log_P10"]),
           n_log_P25 = log_P25 - as.numeric(df_Fig_2_wage[1, "log_P25"]),
           n_log_P50 = log_P50 - as.numeric(df_Fig_2_wage[1, "log_P50"]),
           n_log_P75 = log_P75 - as.numeric(df_Fig_2_wage[1, "log_P75"]),
           n_log_P90 = log_P90 - as.numeric(df_Fig_2_wage[1, "log_P90"]),
           n_log_P95 = log_P95 - as.numeric(df_Fig_2_wage[1, "log_P95"]))
# plot
colors <- c("P5" = "blue", "P10" = "green", "P25" = "red", "P50" = "cyan", 
            "P75" = "purple", "P90" = "yellow", "P95" = "black")
ggplot(df_Fig_2_wage) +
    geom_line(aes(x = year, y = n_log_P5, colour = "P5"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P10, colour = "P10"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P25, colour = "P25"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P50, colour = "P50"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P75, colour = "P75"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P90, colour = "P90"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P95, colour = "P95"), linewidth = 1) +
    scale_x_continuous(name = "Year", limits = c(1981, 2022)) +
    scale_y_continuous(name = "Log (normalized to 0 in 1981)") +
    labs(title = "Equivalized Household Earnings", color = "Legend") +
    scale_color_manual(
        guide = guide_legend(reverse = TRUE),
        name = "Percentiles",
        values = colors,
        breaks = c("P5", "P10", "P25", "P50", "P75", "P90", "P95"),
        labels = c("P5", "P10", "P25", "P50", "P75", "P90", "P95")
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_2//Fig_2_percentiles_1981.png", dpi = 600, width = 9, height = 6)

# Fig 2 for Percentiles of the household earnings distribution 1995
df_Fig_2_wage <- df_Fig_2_wage %>%
    mutate(n_log_P5 = log_P5 - as.numeric(df_Fig_2_wage[15, "log_P5"]),
           n_log_P10 = log_P10 - as.numeric(df_Fig_2_wage[15, "log_P10"]),
           n_log_P25 = log_P25 - as.numeric(df_Fig_2_wage[15, "log_P25"]),
           n_log_P50 = log_P50 - as.numeric(df_Fig_2_wage[15, "log_P50"]),
           n_log_P75 = log_P75 - as.numeric(df_Fig_2_wage[15, "log_P75"]),
           n_log_P90 = log_P90 - as.numeric(df_Fig_2_wage[15, "log_P90"]),
           n_log_P95 = log_P95 - as.numeric(df_Fig_2_wage[15, "log_P95"]))
# plot
ggplot(df_Fig_2_wage) +
    geom_line(aes(x = year, y = n_log_P5, colour = "P5"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P10, colour = "P10"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P25, colour = "P25"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P50, colour = "P50"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P75, colour = "P75"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P90, colour = "P90"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P95, colour = "P95"), linewidth = 1) +
    scale_x_continuous(name = "Year",limits = c(1981, 2022)) +
    scale_y_continuous(name = "Log (normalized to 0 in 1995)") +
    labs(title = "Equivalized Household Earnings", color = "Legend") +
    scale_color_manual(
        guide = guide_legend(reverse = TRUE),
        name = "Percentiles",
        values = colors,
        breaks = c("P5", "P10", "P25", "P50", "P75", "P90", "P95"),
        labels = c("P5", "P10", "P25", "P50", "P75", "P90", "P95")
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_2//Fig_2_percentiles_1995.png", dpi = 600, width = 9, height = 6)

# Fig 3 for Percentiles of the household earnings distribution after 1995
# plot
ggplot(df_Fig_2_wage %>% filter(year >= 1995)) +
    geom_line(aes(x = year, y = n_log_P5, colour = "P5"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P10, colour = "P10"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P25, colour = "P25"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P50, colour = "P50"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P75, colour = "P75"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P90, colour = "P90"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P95, colour = "P95"), linewidth = 1) +
    scale_x_continuous(name = "Year",limits = c(1995, 2022)) +
    scale_y_continuous(name = "Log (normalized to 0 in 1995)") +
    labs(title = "Equivalized Household Earnings", color = "Legend") +
    scale_color_manual(
        guide = guide_legend(reverse = TRUE),
        name = "Percentiles",
        values = colors,
        breaks = c("P5", "P10", "P25", "P50", "P75", "P90", "P95"),
        labels = c("P5", "P10", "P25", "P50", "P75", "P90", "P95")
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_2//Fig_2_percentiles_a1995.png", dpi = 600, width = 9, height = 6)
