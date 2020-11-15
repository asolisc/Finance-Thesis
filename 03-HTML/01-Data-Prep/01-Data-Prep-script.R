# --- Data Wrangling ---
library(tidyverse)
library(janitor)
library(arrow)
library(lubridate)
library(hms)
library(readxl)

# --- Modeling Time Series ---
library(timetk)
library(anomalize) # for outlier detection


# --- Paths Management ---
library(here)


# --- Source ggplot2 themes ---
source(here("05-Resources", "ggplot2_themes.R"))

# Read the data
IPC <- arrow::read_parquet(file = here("01-Data", "parquet", "raw_MEXICO_IPC.parquet"))

IPC %>% glimpse()



# Create vector with new column names
column_names <- c("ticker","raw_date","raw_time","type",
                  "open","high","low","last","volume",
                  "average_price","vwap","no_trades",
                  "correction_qualifiers","open_bid",
                  "high_bid","low_bid","close_bid",
                  "no_bids","open_ask","high_ask","low_ask",
                  "close_ask","no_ask")

# Rename the columns
IPC <- IPC %>% set_names(column_names)

# We can count how many `NA`s are present in our data. We do this per column:

map_df(IPC, ~sum(is.na(.))) %>% glimpse()

# We see that there are 10 columns (variables) that have all values as `NA`. We assign these variables to the `columns_to_remove` object and remove them from the data.

# Get which columns have all values as NAs.
columns_to_remove <- map_df(IPC, ~ sum(is.na(.))) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "no_missing") %>% 
  filter(no_missing > 0) %>%
  pull(variable)

# Print the selected columns.
columns_to_remove

# We name the *clean* dataset as `IPC_ip` (*IPC intraday prices*) and again see the column names and each data type.
# Get a new dataset without the selected columns
IPC_ip <- IPC %>% select(-all_of(columns_to_remove))

# Remove the older df
rm(IPC)

# Take a look at the data
IPC_ip %>% glimpse()

# 2. Feature Engineering & Data Wrangling

IPC_tbl <- IPC_ip %>% 
  
  # Create time variables: tidy_date, tidy_time, tidy_dttm
  mutate(tidy_date = lubridate::ymd(raw_date),
         tidy_time = hms::as_hms(raw_time)
         ) %>%
  
  # Remove some columns
  select(-c(raw_date, raw_time, ticker, type, open, high, low, no_bids, no_ask, average_price)) %>% 
  
  # Get newly created variable to the beginning of tibble
  relocate(starts_with("tidy"), .before = 1) %>% 
  
  # Rename "last" variable
  rename(last_price = last)

# Remove the older df
rm(IPC_ip)

## 2.1 Narrowing down the time window for prices

IPC_tbl %>% 
  distinct(tidy_date) %>% nrow()


IPC_tbl %>% 
  select(tidy_time) %>% 
  unique() %>% 
  arrange(tidy_time)

  
IPC_tbl %>% 
  select(tidy_time) %>% 
  unique() %>% 
  arrange(desc(tidy_time))

IPC_tbl %>% 
  mutate(tidy_hour = as.factor(hour(tidy_time))) %>% 
  count(tidy_hour, sort = T, name = "trades_per_hour") %>% 
  # mutate(tidy_hour = fct_reorder(tidy_hour, .x = trades_per_hour, .desc = T)) %>%
  ggplot(aes(tidy_hour, trades_per_hour)) +
  geom_col(fill = amazing_colors[1]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(
    title = "No. Prices per Hour in the Sample",
    x = "Hour",
    y = NULL
  )

IPC_tbl %>% 
  mutate(tidy_hour = as.factor(hour(tidy_time)),
         tidy_minute = minute(tidy_time)) %>%
  filter(tidy_hour == 8) %>% 
  count(tidy_minute, sort = T, name = "trades_per_minute") %>% 
  ggplot(aes(tidy_minute, trades_per_minute)) +
  geom_col(fill = amazing_colors[8]) +
  scale_x_continuous(breaks = seq(0, 60, 5)) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(
    title = "Number of Trades per Minute from 8:00AM-8:59AM",
    x = "Minute",
    y = NULL
  )

IPC_tbl %>% 
  mutate(tidy_hour = as.factor(hour(tidy_time)),
         tidy_minute = minute(tidy_time)) %>%
  filter(tidy_hour == 15) %>% 
  count(tidy_minute, sort = T, name = "trades_per_minute") %>% 
  ggplot(aes(tidy_minute, trades_per_minute)) +
  geom_col(fill = amazing_colors[8]) +
  scale_x_continuous(breaks = seq(0, 60, 5)) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(
    title = "Number of Trades per Minute from 3:00PM-3:59PM",
    x = "Minute",
    y = NULL
  )


### Time Series Index

IPC_tbl <- IPC_tbl %>% 
  
  # Add time series index: tidy_dttm
  mutate(tidy_dttm = ymd_hms(str_c(tidy_date, tidy_time))) %>% 
  
  relocate(tidy_dttm, .before = 1)

IPC_tbl %>% select(tidy_dttm)

left_edge <- IPC_tbl %>% 
  
  # Arrange in descending tidy_dttm order
  arrange(tidy_dttm) %>% 
  
  # Get the vector
  pull(tidy_dttm) %>% 
  
  # Get the first element of the vector
  first()

right_edge <- IPC_tbl %>% 
  
  # Arrange in descending tidy_dttm order
  arrange(tidy_dttm) %>% 
  
  # Get the vector
  pull(tidy_dttm) %>% 
  
  # Get the last element of the vector
  last()

paste("The first date-time in our data is:", left_edge) 
paste("The last date-time in our data is:", right_edge) 

first_dttm <- left_edge %>% 
  as_date() %>% 
  paste("08:30:00") %>% 
  as_datetime()

last_dttm <- right_edge %>% 
  as_date() %>% 
  paste("15:00:00") %>% 
  as_datetime()

# Make index by using vectorized function
ts_index <- tibble(
  
  index = timetk::tk_make_timeseries(start_date = first_dttm,
                                     end_date = last_dttm,
                                     by = "60 sec")
) %>% 
  
  # Create date and time columns
  mutate(index_date = as_date(index),
         
         index_time = hms(seconds = second(index),
                          minutes = minute(index),
                          hours = hour(index)
         )
  ) %>% 
  
  # Filter for times between 08:30 and 15:00
  filter(index_time >= as_hms("08:30:00"),
         index_time <= as_hms("15:00:00")) %>% 
  
  # Get rid of absent dates  
  filter(index_date %in% IPC_tbl$tidy_date)

# Print first 10 values
ts_index

# Print last 10 values
ts_index %>% 
  arrange(desc(index))

### Joining the Data

joined_tbl <- ts_index %>% 
  left_join(
    IPC_tbl,
    by = c("index" = "tidy_dttm")) %>% 
  
  # Remove useless columns
  select(-c(tidy_date, tidy_time))
  
joined_tbl

## 2.2 Price Imputation


joined_tbl$last_price[1:6] <- joined_tbl$last_price[7]

joined_tbl


full_IPC <- joined_tbl %>% 
  fill(last_price)
  
full_IPC %>% map_df(.f = ~ sum(is.na(.)))

full_IPC

full_IPC %>% 
  count(index_date, name = "date_count")


full_IPC %>% 
  count(index_date, name = "date_count") %>% 
  distinct(date_count)


## 2.3 Time Series Signature

IPC_signature <- full_IPC %>% 
  
  # Create time series signature
  mutate(
    index_num   = as.numeric(index),
    diff        = index_num - lag(index_num),
    year        = year(index),
    half        = semester(index, with_year = FALSE),
    quarter     = quarter(index),
    month       = month(index),
    month_label = month(index, label = TRUE),
    day         = day(index),
    hour        = hour(index),
    minute      = minute(index),
    wday        = wday(index, week_start = 1),
    wday_label  = wday(index, label = TRUE),
    qday        = qday(index),
    yday        = yday(index),
    # mweek       =
    # week        = lubridate::week(),
    # mday7       = day %% 7,
    date_change = if_else(index_date == lag(index_date), FALSE, TRUE)
  )

IPC_signature %>% 
  distinct(wday_label)

# First 10 unique values
IPC_signature %>% 
  distinct(index_time)

# Last 10 unique values
IPC_signature %>% 
  arrange(-index_time) %>% 
  distinct(index_time)

returns_tbl <- IPC_signature %>%

    mutate(log_ret = log(last_price) - lag(log(last_price))) %>% 
  
  relocate(log_ret, .after = 4)

# Remove the older df
rm(full_IPC)
rm(ts_index)
rm(joined_tbl)
rm(IPC_tbl)

## 2.5 Setting Overnight Returns to Zero

nor_tbl <- returns_tbl %>% 
   
  mutate(log_ret = ifelse(test = date_change == FALSE,
                          yes = log_ret, 
                          no = 0))
# Remove old df
# rm(IPC_tfr_tbl)

  
nor_tbl %>% 
  filter(is.na(log_ret)) %>% 
  select(index_date, log_ret)

nor_tbl$log_ret[1] <- 0

nor_tbl

nor_tbl %>% 
  filter(date_change == 1) %>% 
  distinct(log_ret, date_change) 

nor_tbl %>% 
  filter(date_change == TRUE) %>% 
  select(index_date, log_ret, date_change)

nor_tbl %>% distinct(index_date) %>% nrow()

returns_plot <- nor_tbl %>% 
  ggplot(aes(index, log_ret)) +
  geom_point(aes(alpha = .005,
                 colour = log_ret >= 0), 
             show.legend = FALSE) +
  scale_color_manual(values = amazing_colors[c(2,8)]) +
  labs(
    title = "Visualizing 1-minute log-returns for the S&P/BMV IPC",
    x = NULL,
    y = NULL
  ) +
  scale_x_datetime(date_breaks = "2 years", 
                   date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.05, 0.05, by = 0.01),
                     labels = scales::percent_format(accuracy = 0.1)) +
  theme(
    plot.background = element_rect(fill = "#F4F3EE")
  )

returns_plot


# ggsave(
#   filename = "IPC_returns.png",
#   units = "in",
#   width = 8 * 1.61803398875,
#   height = 8,
#   path = here("02-Plots"),
#   dpi = 500,
#   device = "png"
# )

## 2.6 Removing Outliers

threshold <- 0.0012

IPC_fr <- nor_tbl %>% 
  
  # we keep values below the threshold
  mutate(log_ret = ifelse(test = abs(log_ret) < threshold, 
                          yes = log_ret, 
                          no = 0)
         )

# Remove old df
# rm(IPC_oar_tbl)

returns_plot2 <- IPC_fr %>% 
  ggplot(aes(index, log_ret)) +
  geom_point(aes(alpha = .005,
                 colour = log_ret >= 0), 
             show.legend = FALSE) +
  scale_color_manual(values = amazing_colors[c(2,8)]) +
  labs(
    title = "Visualizing 1-minute log-returns for the S&P/BMV IPC",
    x = NULL,
    y = NULL
  ) +
  scale_x_datetime(date_breaks = "2 years", 
                   date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme(
    plot.background = element_rect(fill = "#F4F3EE")
  )

returns_plot2

# Write data to parquet
# IPC_fr %>% 
  # write_parquet(sink = here("01-Data", "parquet", "IPC_fr2.parquet"))

# IPC and Volatility Indices -- Daily Data

indices_data <- 
  
  # Read Excel file
  read_excel(path = here("01-Data", "mexbol-vimex.xlsx"),
             
             # Specify values for NA
             na = c("#NA", "#N/A N/A"),
             
             # Specify column type
             col_types = c("date", rep("numeric", 3))
             ) %>%
  
  # Set names for columns
  set_names(c("date", "mexbol_ipc", "vimex", "ipc_vix")) %>% 
  
  # Parse date column
  mutate(date = ymd(date))

indices_data



# Build IPC plot
plot_ipc <- indices_data %>% 
  filter(!is.na(vimex)) %>% 
  ggplot(aes(date, mexbol_ipc)) +
  geom_line(colour = amazing_colors[1]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  scale_x_date(breaks = "2 years", labels = scales::date_format("%Y")) +
  labs(
    title = "IPC Index & VIMEX Volatility Index",
    subtitle = "(2005 - 2019)",
    x = NULL,
    y = "S&P/BMV IPC"
  )


# Build VIMEX plot
plot_vimex <- indices_data %>% 
  filter(!is.na(vimex)) %>% 
  ggplot(aes(date, vimex)) +
  geom_line(colour = amazing_colors[2]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  scale_x_date(breaks = "2 years", labels = scales::date_format("%Y")) +
  labs(
    x = NULL,
    y = "VIMEX"
  )

# Print plot
cowplot::plot_grid(plot_ipc, 
                   plot_vimex, 
                   ncol = 1,
                   rel_heights = c(1, 1))

# Build IPC plot with new title
plot_ipc2 <- indices_data %>% 
  filter(!is.na(ipc_vix)) %>% 
  ggplot(aes(date, mexbol_ipc)) +
  geom_line(colour = amazing_colors[1]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  scale_x_date(breaks = "1 year", labels = scales::date_format("%Y")) +
  labs(
    title = "IPC Index & IPC VIX Volatility Index",
    subtitle = "(2015 - 2019)",
    x = NULL,
    y = "S&P/BMV IPC"
  )


# Build IPC VIX plot
plot_ipcvix <- indices_data %>% 
  filter(!is.na(ipc_vix)) %>% 
  ggplot(aes(date, ipc_vix)) +
  geom_line(colour = amazing_colors[2]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  scale_x_date(breaks = "1 year", labels = scales::date_format("%Y")) +
  labs(
    x = NULL,
    y = "IPC VIX"
  )

# Print plot
cowplot::plot_grid(plot_ipc2, 
                   plot_ipcvix, 
                   ncol = 1,
                   rel_heights = c(1, 1))

# Build IPC plot
plot_ipc <- indices_data %>% 
  filter(!is.na(vimex)) %>% 
  ggplot(aes(date, mexbol_ipc)) +
  geom_line(colour = amazing_colors[1]) +
  geom_smooth(colour = amazing_colors[3]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  scale_x_date(breaks = "2 years", labels = scales::date_format("%Y")) +
  labs(
    title = "IPC Index & VIMEX Volatility Index",
    subtitle = "(2005 - 2019)",
    x = NULL,
    y = "S&P/BMV IPC"
  )


# Build VIMEX plot
plot_vimex <- indices_data %>% 
  filter(!is.na(vimex)) %>% 
  ggplot(aes(date, vimex)) +
  geom_line(colour = amazing_colors[2]) +
  geom_smooth(colour = amazing_colors[4]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  scale_x_date(breaks = "2 years", labels = scales::date_format("%Y")) +
  labs(
    x = NULL,
    y = "VIMEX"
  )

# Print plot
cowplot::plot_grid(plot_ipc, 
                   plot_vimex, 
                   ncol = 1,
                   rel_heights = c(1, 1))


# Build IPC plot with new title
plot_ipc2 <- indices_data %>% 
  filter(!is.na(ipc_vix)) %>% 
  ggplot(aes(date, mexbol_ipc)) +
  geom_line(colour = amazing_colors[1]) +
  geom_smooth(colour = amazing_colors[3]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  scale_x_date(breaks = "1 year", labels = scales::date_format("%Y")) +
  labs(
    title = "IPC Index & IPC VIX Volatility Index",
    subtitle = "(2015 - 2019)",
    x = NULL,
    y = "S&P/BMV IPC"
  )


# Build IPC VIX plot
plot_ipcvix <- indices_data %>% 
  filter(!is.na(ipc_vix)) %>% 
  ggplot(aes(date, ipc_vix)) +
  geom_line(colour = amazing_colors[2]) +
  geom_smooth(colour = amazing_colors[4]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  scale_x_date(breaks = "1 year", labels = scales::date_format("%Y")) +
  labs(
    x = NULL,
    y = "IPC VIX"
  )

# Print plot
cowplot::plot_grid(plot_ipc2, 
                   plot_ipcvix, 
                   ncol = 1,
                   rel_heights = c(1, 1))
