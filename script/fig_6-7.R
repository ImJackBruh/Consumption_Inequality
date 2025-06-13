# Calculate Var. and Gini coef. etc.
# Author: BoYanHuang
# Created Jun 12, 2025
# Last modify Jun 13, 2025 by BoYanHuang

library(tidyverse)
library(DescTools)
library(modi)

# read the cross sectional all file
df_all <- read.csv("HH_inc_exp//HH_all.csv") %>% as_tibble()
df_all <- df_all %>% filter(year != 2012)

# !!NOTE: some NA are zero and some NA are missing val.
# fill 0 for NA inc_gtransfer, inc_total, exp_tax
df_all <- df_all %>% mutate(across(c(inc_gtransfer, inc_total, exp_tax), ~ ifelse(is.na(.), 0, .)))
df_all <- df_all %>% mutate(Pre_Govt = inc_total - inc_gtransfer,
                            Pre_Tax = inc_total,
                            Disposable = inc_total - exp_tax)
# turn 0 to NA inc_gtransfer, inc_total, exp_tax, Pre_Govt, Pre_Tax, Disposable
df_all <- df_all %>% mutate(across(c(inc_gtransfer, inc_total, exp_tax, Pre_Govt, Pre_Tax, Disposable), 
                                   ~ ifelse(. <= 0, NA, .)))
# NA exp
df_all <- df_all %>% mutate(across(starts_with('exp_'), ~ ifelse(. <= 100, NA, .)))

# Fig6 Equiv. Disp. Inc. & Equiv. Consumption
df_Fig_6 <- df_all %>% group_by(year) %>% 
    summarise(Var_disp_inc = weighted.var(log(Disposable, exp(1)), weight, na.rm = T),
              Gini_disp_inc = Gini(Disposable, weight, na.rm = T),
              P50_P10_disp_inc = weighted.quantile(Disposable, weight, prob = 0.5) / weighted.quantile(Disposable, weight, prob = 0.1),
              P90_P50_disp_inc = weighted.quantile(Disposable, weight, prob = 0.9) / weighted.quantile(Disposable, weight, prob = 0.5),
              Var_consump = weighted.var(log(exp_consumption, exp(1)), weight, na.rm = T),
              Gini_consump = Gini(exp_consumption, weight, na.rm = T),
              P50_P10_consump = weighted.quantile(exp_consumption, weight, prob = 0.5) / weighted.quantile(exp_consumption, weight, prob = 0.1),
              P90_P50_consump = weighted.quantile(exp_consumption, weight, prob = 0.9) / weighted.quantile(exp_consumption, weight, prob = 0.5))
# plot
colors <- c("disp_inc" = "blue", "consump" = "red")
ggplot(df_Fig_6) +
    geom_line(aes(x = year, y = Var_disp_inc, color = "disp_inc"), linewidth = 1) +
    geom_line(aes(x = year, y = Var_consump, color = "consump"), linewidth = 1) +
    labs(title = "Variance of Log", x = "Year", y = NULL) +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("disp_inc", "consump"),
        labels = c("Equiv. Disp. Inc.", "Equiv. Consumption")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.24, 0.14))
ggsave("Fig_6//Fig_6a.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_6) +
    geom_line(aes(x = year, y = Gini_disp_inc, color = "disp_inc"), linewidth = 1) +
    geom_line(aes(x = year, y = Gini_consump, color = "consump"), linewidth = 1) +
    labs(title = "Gini Coefficient", x = "Year", y = NULL) +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("disp_inc", "consump"),
        labels = c("Equiv. Disp. Inc.", "Equiv. Consumption")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.24, 0.14))
ggsave("Fig_6//Fig_6b.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_6) +
    geom_line(aes(x = year, y = P50_P10_disp_inc, color = "disp_inc"), linewidth = 1) +
    geom_line(aes(x = year, y = P50_P10_consump, color = "consump"), linewidth = 1) +
    labs(title = "P50-P10 Ratio", x = "Year", y = NULL) +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("disp_inc", "consump"),
        labels = c("Equiv. Disp. Inc.", "Equiv. Consumption")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.24, 0.14))
ggsave("Fig_6//Fig_6c.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_6) +
    geom_line(aes(x = year, y = P90_P50_disp_inc, color = "disp_inc"), linewidth = 1) +
    geom_line(aes(x = year, y = P90_P50_consump, color = "consump"), linewidth = 1) +
    labs(title = "P90-P50 Ratio", x = "Year", y = NULL) +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("disp_inc", "consump"),
        labels = c("Equiv. Disp. Inc.", "Equiv. Consumption")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.24, 0.14))
ggsave("Fig_6//Fig_6d.png", dpi = 600, width = 4, height = 4)

# Fig7 Percentiles of the household consumption distribution 1981
df_Fig_7 <- df_all %>% group_by(year) %>% 
    summarise(P5 = weighted.quantile(exp_consumption, weight, prob = 0.05),
              P10 = weighted.quantile(exp_consumption, weight, prob = 0.1),
              P25 = weighted.quantile(exp_consumption, weight, prob = 0.25),
              P50 = weighted.quantile(exp_consumption, weight, prob = 0.50),
              P75 = weighted.quantile(exp_consumption, weight, prob = 0.75),
              P90 = weighted.quantile(exp_consumption, weight, prob = 0.9),
              P95 = weighted.quantile(exp_consumption, weight, prob = 0.95))
df_Fig_7 <- df_Fig_7 %>%
    mutate(n_P5 = P5 - as.numeric(df_Fig_7[1, "P5"]),
           n_P10 = P10 - as.numeric(df_Fig_7[1, "P10"]),
           n_P25 = P25 - as.numeric(df_Fig_7[1, "P25"]),
           n_P50 = P50 - as.numeric(df_Fig_7[1, "P50"]),
           n_P75 = P75 - as.numeric(df_Fig_7[1, "P75"]),
           n_P90 = P90 - as.numeric(df_Fig_7[1, "P90"]),
           n_P95 = P95 - as.numeric(df_Fig_7[1, "P95"]))
df_Fig_7 <- df_Fig_7 %>%
    mutate(log_P5 = log(P5, exp(1)),
           log_P10 = log(P10, exp(1)),
           log_P25 = log(P25, exp(1)),
           log_P50 = log(P50, exp(1)),
           log_P75 = log(P75, exp(1)),
           log_P90 = log(P90, exp(1)),
           log_P95 = log(P95, exp(1)))
df_Fig_7 <- df_Fig_7 %>%
    mutate(n_log_P5 = log_P5 - as.numeric(df_Fig_7[1, "log_P5"]),
           n_log_P10 = log_P10 - as.numeric(df_Fig_7[1, "log_P10"]),
           n_log_P25 = log_P25 - as.numeric(df_Fig_7[1, "log_P25"]),
           n_log_P50 = log_P50 - as.numeric(df_Fig_7[1, "log_P50"]),
           n_log_P75 = log_P75 - as.numeric(df_Fig_7[1, "log_P75"]),
           n_log_P90 = log_P90 - as.numeric(df_Fig_7[1, "log_P90"]),
           n_log_P95 = log_P95 - as.numeric(df_Fig_7[1, "log_P95"]))
# plot
colors <- c("P5" = "blue", "P10" = "green", "P25" = "red", "P50" = "cyan", 
            "P75" = "purple", "P90" = "yellow", "P95" = "black")
ggplot(df_Fig_7) +
    geom_line(aes(x = year, y = n_log_P5, colour = "P5"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P10, colour = "P10"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P25, colour = "P25"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P50, colour = "P50"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P75, colour = "P75"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P90, colour = "P90"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P95, colour = "P95"), linewidth = 1) +
    scale_x_continuous(name = "Year", limits = c(1981, 2022)) +
    scale_y_continuous(name = "Log (normalized to 0 in 1981)") +
    labs(title = "Equivalized Household Consumption", color = "Legend") +
    scale_color_manual(
        guide = guide_legend(reverse = TRUE),
        name = "Percentiles",
        values = colors,
        breaks = c("P5", "P10", "P25", "P50", "P75", "P90", "P95"),
        labels = c("P5", "P10", "P25", "P50", "P75", "P90", "P95")
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_7//Fig_7_percentiles_1981.png", dpi = 600, width = 9, height = 6)

# Fig7 Percentiles of the household consumption distribution 1995
df_Fig_7 <- df_Fig_7 %>%
    mutate(n_log_P5 = log_P5 - as.numeric(df_Fig_7[15, "log_P5"]),
           n_log_P10 = log_P10 - as.numeric(df_Fig_7[15, "log_P10"]),
           n_log_P25 = log_P25 - as.numeric(df_Fig_7[15, "log_P25"]),
           n_log_P50 = log_P50 - as.numeric(df_Fig_7[15, "log_P50"]),
           n_log_P75 = log_P75 - as.numeric(df_Fig_7[15, "log_P75"]),
           n_log_P90 = log_P90 - as.numeric(df_Fig_7[15, "log_P90"]),
           n_log_P95 = log_P95 - as.numeric(df_Fig_7[15, "log_P95"]))
# plot
ggplot(df_Fig_7) +
    geom_line(aes(x = year, y = n_log_P5, colour = "P5"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P10, colour = "P10"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P25, colour = "P25"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P50, colour = "P50"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P75, colour = "P75"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P90, colour = "P90"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P95, colour = "P95"), linewidth = 1) +
    scale_x_continuous(name = "Year",limits = c(1981, 2022)) +
    scale_y_continuous(name = "Log (normalized to 0 in 1995)") +
    labs(title = "Equivalized Household Consumption", color = "Legend") +
    scale_color_manual(
        guide = guide_legend(reverse = TRUE),
        name = "Percentiles",
        values = colors,
        breaks = c("P5", "P10", "P25", "P50", "P75", "P90", "P95"),
        labels = c("P5", "P10", "P25", "P50", "P75", "P90", "P95")
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_7//Fig_7_percentiles_1995.png", dpi = 600, width = 9, height = 6)

# Fig 3 for Percentiles of the household earnings distribution after 1995
# plot
ggplot(df_Fig_7 %>% filter(year >= 1995)) +
    geom_line(aes(x = year, y = n_log_P5, colour = "P5"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P10, colour = "P10"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P25, colour = "P25"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P50, colour = "P50"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P75, colour = "P75"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P90, colour = "P90"), linewidth = 1) +
    geom_line(aes(x = year, y = n_log_P95, colour = "P95"), linewidth = 1) +
    scale_x_continuous(name = "Year",limits = c(1995, 2022)) +
    scale_y_continuous(name = "Log (normalized to 0 in 1995)") +
    labs(title = "Equivalized Household Consumption", color = "Legend") +
    scale_color_manual(
        guide = guide_legend(reverse = TRUE),
        name = "Percentiles",
        values = colors,
        breaks = c("P5", "P10", "P25", "P50", "P75", "P90", "P95"),
        labels = c("P5", "P10", "P25", "P50", "P75", "P90", "P95")
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_7//Fig_7_percentiles_a1995.png", dpi = 600, width = 9, height = 6)

# Fig8 food & housing consumption
# top: food var, foog gini
# bottom: housing var, housing gini
df_Fig_8 <- df_all %>% group_by(year) %>% 
    summarise(Var_food = weighted.var(log(exp_food, exp(1)), weight, na.rm = T),
              Gini_food = Gini(exp_food, weight, na.rm = T),
              Var_housing = weighted.var(log(exp_housing, exp(1)), weight, na.rm = T),
              Gini_housing = Gini(exp_housing, weight, na.rm = T))
# plot
ggplot(df_Fig_8, aes(x = year, y = Var_food)) +
    geom_line(colour = "blue", linewidth = 1) +
    labs(title = "Equiv. HH Food Consumption", x = "Year", y = "Variance of Log") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_8//Fig_8a_Var_Food.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_8, aes(x = year, y = Gini_food)) +
    geom_line(colour = "blue", linewidth = 1) +
    labs(title = "Equiv. HH Food Consumption", x = "Year", y = "Gini Coefficient") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_8//Fig_8b_Gini_Food.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_8, aes(x = year, y = Var_housing)) +
    geom_line(colour = "blue", linewidth = 1) +
    labs(title = "Equiv. HH Housing Services", x = "Year", y = "Variance of Log") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_8//Fig_8c_Var_Housing.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_8, aes(x = year, y = Gini_housing)) +
    geom_line(colour = "blue", linewidth = 1) +
    labs(title = "Equiv. HH Housing Services", x = "Year", y = "Gini Coefficient") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("Fig_8//Fig_8d_Gini_Housing.png", dpi = 600, width = 4, height = 4)
