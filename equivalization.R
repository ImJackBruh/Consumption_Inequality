# Extract column needed and equivalize them
# Author: BoYanHuang
# Created Jun 7, 2025
# Last modify Jun 12, 2025 by BoYanHuang

library(tidyverse)
library(haven)

# read the file of cpi 
cpi <- read.csv("cpi_70_112.csv") %>% as_tibble()

years <- c(70:112)

HH_all <- tibble()

for(y in years){
    inc_y <- read_dta(paste0("inc//inc", y, ".dta")) %>% as_tibble()

    # this part is for calculating OECD equivalence scale
    inc_y <- inc_y %>% mutate(across(starts_with('b4_'), .names = 'is_adult_{col}', ~ as.integer(. > 17)))
    inc_y <- inc_y %>% mutate(across(starts_with('b4_'), .names = 'is_child_{col}', ~ as.integer(. <= 17)))
    inc_y <- inc_y %>% rowwise() %>% mutate(adult_count = sum(pick(starts_with('is_adult_')), na.rm=TRUE)) %>% ungroup()
    inc_y <- inc_y %>% rowwise() %>% mutate(child_count = sum(pick(starts_with('is_child_')), na.rm=TRUE)) %>% ungroup()
    inc_y$eqi_w <- 1 + (inc_y$adult_count - 1) * 0.7 + inc_y$child_count * 0.5

    # demographic
    # marriage year >= 77
    if(y < 77){
        inc_y$marriage = NA
    }else{
        inc_y <- inc_y %>% mutate(marriage = case_when(b16_1 == 0 ~ NA,
                                                    b16_1 %in% c(91,94,95,96,97) ~ 0,
                                                    TRUE ~ 1))
    }
    # spouse_id year >= 77
    if(y < 77){
        inc_y$spouse_id = NA
    }else{
        inc_y <- inc_y %>% mutate(spouse_id = case_when(b16_1 >= 2 & b16_1 <= 8 ~ b16_1,
                                                    TRUE ~ NA))
    }
    inc_y <- inc_y %>% ungroup()

    # fill 0 for NA in itm19# and itm24#
    inc_y <- inc_y %>% mutate(across(starts_with('itm19'), ~ replace_na(., 0)),
                              across(starts_with('itm24'), ~ replace_na(., 0)))

    # income
    # inc_earnings itm190+itm240, inc_investment itm330, inc_transfer itm410, inc_total itm400
    if(y == 84){
        inc_y <- inc_y %>% mutate(inc_earnings = (itm190 + itm240) / eqi_w,
                                  inc_earnings_main = itm191 + itm241,
                                  inc_investment = itm330 / eqi_w,
                                  inc_gtransfer = itm430 / eqi_w,
                                  inc_ptransfer = itm410 / eqi_w - inc_gtransfer,
                                  inc_total = itm400 / eqi_w)
    }else{
        inc_y <- inc_y %>% mutate(inc_earnings = (itm190 + itm240) / eqi_w,
                                  inc_earnings_main = itm191 + itm241,
                                  inc_investment = itm330 / eqi_w,
                                  inc_gtransfer = itm430 / eqi_w,
                                  inc_ptransfer = itm410 / eqi_w - inc_gtransfer,
                                  inc_total = itm400 / eqi_w)
    }
    if(y == 84 | y == 83){
        inc_y <- inc_y %>% rowwise() %>% 
            mutate(inc_earnings_max = max(itm191 + itm241, itm192 + itm242, itm193 + itm243, itm194 + itm244, itm195 + itm245, itm196 + itm246, itm197 + itm247))
    }else{
        inc_y <- inc_y %>% rowwise() %>% 
            mutate(inc_earnings_max = max(itm191 + itm241, itm192 + itm242, itm193 + itm243, itm194 + itm244, itm195 + itm245, itm196 + itm246, itm197 + itm247, itm198 + itm248))
    }
    # spouse_earnings
    # spouse_earnings year >= 77
    if(y < 77){
        inc_y$inc_earnings_spouse = NA
    }else if(y == 84 | y == 83){
        inc_y <- inc_y %>% mutate(inc_earnings_spouse = case_when(spouse_id == 2 ~ itm192 + itm242,
                                                            spouse_id == 3 ~ itm193 + itm243,
                                                            spouse_id == 4 ~ itm194 + itm244,
                                                            spouse_id == 5 ~ itm195 + itm245,
                                                            spouse_id == 6 ~ itm196 + itm246,
                                                            spouse_id == 7 ~ itm197 + itm247,
                                                            TRUE ~ NA))
    }else{
        inc_y <- inc_y %>% mutate(inc_earnings_spouse = case_when(spouse_id == 2 ~ itm192 + itm242,
                                                            spouse_id == 3 ~ itm193 + itm243,
                                                            spouse_id == 4 ~ itm194 + itm244,
                                                            spouse_id == 5 ~ itm195 + itm245,
                                                            spouse_id == 6 ~ itm196 + itm246,
                                                            spouse_id == 7 ~ itm197 + itm247,
                                                            spouse_id == 8 ~ itm198 + itm248,
                                                            TRUE ~ NA))
    }
    # expenditure/consumption
    inc_y <- inc_y %>% mutate(exp_interest = itm540 / eqi_w,
                            exp_tax = (itm590 + itm610 + itm620) / eqi_w,
                            exp_transfer = itm560 / eqi_w - exp_tax)
    # exp_food <99 itm710+itm731 >=99 itm1010, exp_consumption <99 itm800 >=99 itm1000, exp_housing <99 itm760 >=99 itm1040
    if(y == 98){
        inc_y <- inc_y %>% mutate(exp_consumption = itm800 / eqi_w,
                                exp_food = (itm710+itm731) / eqi_w,
                                exp_housing = itm1003 / eqi_w)
    }else if(y < 99){
        inc_y <- inc_y %>% mutate(exp_consumption = itm800 / eqi_w,
                                exp_food = (itm710+itm731) / eqi_w,
                                exp_housing = itm760 / eqi_w)
    }else{
        inc_y <- inc_y %>% mutate(exp_consumption = itm1000 / eqi_w,
                                exp_food = itm1010 / eqi_w,
                                exp_housing = itm1040 / eqi_w)
    }

    # weight y < 90 a21, y >= 90 a21
    if(y < 90){
        inc_y$weight <- inc_y$a21
    }else{
        inc_y$weight <- inc_y$a20
    }

    inc_y$year <- 1911 + y
    inc_y <- inc_y %>% select(year, id, weight, eqi_w, marriage, spouse_id, inc_earnings, inc_earnings_max, inc_earnings_main, inc_earnings_spouse, inc_investment, inc_gtransfer, inc_ptransfer, inc_total, exp_interest, exp_tax, exp_transfer, exp_consumption, exp_food, exp_housing)

    cpi_y <- as.numeric(cpi[y - 70 + 1, 2])
    inc_y <- inc_y %>% mutate(across(starts_with('inc_'), ~ . / cpi_y * 100))
    inc_y <- inc_y %>% mutate(across(starts_with('exp_'), ~ . / cpi_y * 100))

    HH_all <- rbind(HH_all, inc_y)
    write.csv(inc_y, file = paste0("HH_inc_exp//HH_", y, ".csv"), row.names = F)
    write.csv(HH_all, file = paste0("HH_inc_exp//HH_", "all", ".csv"), row.names = F)
    print(paste("year", y, "done!"))
}
