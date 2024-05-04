###############################################################################.
#
# WRDS TAQ 5-Min Frequency
# 
#
###############################################################################.
rm(list=ls());gc()

library(tidyverse)


# =============================================================================.
# DATA -------------
# =============================================================================.

data_path <- 
  "/Users/jason/Documents/OneDrive - Universitaet St.Gallen/Financial-Volatility/TAQ_5min"

taq_yrs <- c(2021, 2022, 2023, 2024)

# Function to tidy data
tidy5min <- function(x){
  fname <- paste0("data", x, ".csv")
  dtemp <- read_csv(file.path(data_path, fname))
  
  dtemp <- dtemp %>%
    mutate(date = as.Date(as.character(DATE), "%Y%m%d")) %>%
    mutate(dt = as.POSIXct(paste(date, rtime_m))) %>% # nice datetime format
    select(dt, "ticker" = SYM_ROOT, "price" = iprice) # only keep date, ticker, price
  
  return(dtemp)
}

# Convenient For-loop
#data_list <- lapply(taq_yrs, tidy5min)
#hfd <- bind_rows(data_list) %>% arrange(ticker, dt)

#summary(hfd)
#save(hfd, file = file.path(data_path, "taq_5min.RData"))

# =============================================================================.
# REALIZED VOLATILITIES -------------
# =============================================================================.

drv <- hfd %>%
  mutate(ttime = format(dt, "%H:%M:%S")) %>%
  filter(between(ttime, "09:30:00", "16:00:00")) %>%
  mutate(date = as.Date(dt), log_p = log(price)) %>%
  arrange(ticker, dt) %>%
  group_by(ticker, date) %>%
  mutate(ret = log_p - lag(log_p, n = 1L)) %>%
  summarise(rv = sqrt(sum(ret^2, na.rm = TRUE)), 
            open = price[1], 
            close = price[n()]) %>%
  ungroup() %>%
  group_by(ticker) %>%
  mutate(log_ret = log(open) - lag(log(open), n = 1L)) %>%
  ungroup()

#save(drv, file = file.path(data_path, "daily_data.RData"))
load(file.path(data_path, "daily_data.RData"))
# =============================================================================.
# PLOTS -------------
# =============================================================================.
  
# Squared Returns
ggplot(drv, aes(x = date, y = dret^2)) +
  geom_line() +
  facet_wrap(~ ticker)
