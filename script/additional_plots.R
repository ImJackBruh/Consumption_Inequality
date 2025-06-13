# draw some additional plots for the paper
# Author: BoYanHuang
# Created Jun 13, 2025
# Last modify Jun 13, 2025 by BoYanHuang

library(tidyverse)
library(ggplot2)

# read the file of cpi
cpi <- read.csv("cpi_70_112.csv") %>% as_tibble()

cpi$year = cpi$year + 1911

# plot cpi
ggplot(cpi, aes(x = year, y = cpi_total)) +
    geom_line(color = "blue") +
    labs(title = "Consumer Price Index (CPI) in Taiwan", x = "Year", y = "CPI") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/cpi.png", dpi = 600, width = 4, height = 4)