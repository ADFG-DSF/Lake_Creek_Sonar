Sonar_counts <- function(data, min_threshold = 50, threshold_date = NULL, new_threshold = 75) {
  
  # Determine the date range dynamically from the data
  date_range <- range(data$Date, na.rm = TRUE)
  start_date <- date_range[1]
  end_date <- date_range[2]
  
  # Add a column for filtering based on thresholds and threshold_date
  filtered_data <- data %>%
    mutate(
      Length_Filter = if (!is.null(threshold_date)) {
        ifelse(Date >= as.Date(threshold_date), new_threshold, min_threshold)
      } else {
        min_threshold
      }
    ) %>%
    filter(Length >= Length_Filter) %>% # Apply the dynamic length filter
    mutate(
      Strata = case_when(
        Range >= 0 & Range <= 8 ~ "Stratum1",
        Range > 8 & Range <= 20 ~ "Stratum2",
        Range > 20 & Range <= 32 ~ "Stratum3",
        TRUE ~ NA_character_
      ),
      Time = as.POSIXct(Time, format = "%H:%M:%S"), # Convert time to POSIX format
      hour = hour(ymd_hms(Time)), # Extract hour
      Expanded = (60 / Duration) * File # Expand hourly counts
    )
  
  # Calculate hourly expanded counts for UP
  hourly_up <- filtered_data %>%
    filter(Dir == "Up", Date >= start_date & Date <= end_date) %>%
    group_by(Date, hour) %>%
    summarize(Expanded_Up = sum(Expanded), .groups = "drop") %>%
    complete(Date = seq(as.Date(start_date), as.Date(end_date), by = "day"), 
             hour = 0:23, fill = list(Expanded_Up = 0))
  
  # Calculate hourly expanded counts for DOWN
  hourly_down <- filtered_data %>%
    filter(Dir == "Down", Date >= start_date & Date <= end_date) %>%
    group_by(Date, hour) %>%
    summarize(Expanded_Down = sum(Expanded), .groups = "drop") %>%
    complete(Date = seq(as.Date(start_date), as.Date(end_date), by = "day"), 
             hour = 0:23, fill = list(Expanded_Down = 0))
  
  # Merge UP and DOWN counts, subtract to get net counts
  hourly <- full_join(hourly_up, hourly_down, by = c("Date", "hour")) %>%
    mutate(
      Expanded_Up = coalesce(Expanded_Up, 0),
      Expanded_Down = coalesce(Expanded_Down, 0),
      Net_Expanded = Expanded_Up - Expanded_Down
    )
  
  # Summarize daily expanded counts
  daily <- hourly %>%
    group_by(Date) %>%
    summarize(
      daily_up = sum(Expanded_Up),
      daily_down = sum(Expanded_Down),
      daily_net_up = sum(Net_Expanded),
      sum_diff_sq_up = sum((Expanded_Up - lag(Expanded_Up))^2, na.rm = TRUE),
      sum_diff_sq_down = sum((Expanded_Down - lag(Expanded_Down))^2, na.rm = TRUE),
      sum_phi = n(),
      f = 20 / 60,
      var_up = (24^2) * (1 - f) * (sum_diff_sq_up / (2 * sum_phi * sum_phi)),
      var_down = (24^2) * (1 - f) * (sum_diff_sq_down / (2 * sum_phi * sum_phi)),
      var = var_up + var_down,
      lower_ci = daily_net_up - qnorm(0.95) * sqrt(var),
      upper_ci = daily_net_up + qnorm(0.95) * sqrt(var),
      .groups = "drop"
    ) %>%
    complete(Date = seq(as.Date(start_date), as.Date(end_date), by = "day"), 
             fill = list(daily_net_up = 0, var = 0, lower_ci = 0, upper_ci = 0)) %>%
    mutate(length_threshold = if (!is.null(threshold_date)) {
      ifelse(Date >= as.Date(threshold_date), new_threshold, min_threshold)
    } else {
      min_threshold
    })
  
  # Calculate cumulative counts
  daily <- daily %>%
    arrange(Date) %>%
    mutate(cumulative_expanded = cumsum(daily_net_up))
  
  # Calculate total counts
  totals <- daily %>%
    summarize(
      Total_expanded = sum(daily_net_up),
      Total_var = sum(var, na.rm = TRUE),
      lower_ci = Total_expanded - qnorm(0.95) * sqrt(Total_var),
      upper_ci = Total_expanded + qnorm(0.95) * sqrt(Total_var)
    ) %>%
    mutate(min_threshold = min_threshold,
           threshold_date = threshold_date,
           new_threshold = new_threshold)
  
  list(Daily = daily, Totals = totals)
}