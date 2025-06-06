# LIBRARIES-------------
library(tidyverse)
library(janitor)

# OPTIONS---------------
# run options(op) at end of script to restore defaults
op <- options(digits.secs = 3) # accommodate 3 decimal places in seconds of time stamp


# LOAD DATA------------
# Raw data from greenhouse gas analyzer
gga <- readr::read_csv(file.choose(), skip = 1) %>% #load .txt file from the los gatos
  janitor::clean_names() %>% # standardize names
  rename_with(~gsub("x_", "", .x), contains("x_")) %>% # remove x_ from names
  # deal with time
  mutate(date_time = as.POSIXct(time, format = "%m/%d/%Y %H:%M:%OS"), # OS to accommodate decimals in seconds
         time_seconds = as.numeric(date_time)) # this converts date_time object to seconds since Jan 1, 1970
head(gga) # check data 


# File with peak names and injection times
tp <- readxl::read_xlsx(file.choose()) %>% # choose .xlsx file with time of peak injections (injection name, date, hour, minute, and second)
  mutate(date = as.Date(date), # convert to date format
         date_time = as.POSIXct(paste0(date, " ", hour, ":", min, ":", second)), # create date_time object
         start = as.numeric(date_time), # start of peak in seconds since Jan. 1, 1970 
         stop = start + 60) %>% # assume peak is through within 60 seconds
  select(name, start, stop)
tp # preview data





# FORMAT DATA--------------
# Put time series for each unique combination of injection x variable
# into list element
dat <- list() # dump each data subset into list element
for (i in 1:nrow(tp)) { # for each injection
  injection_time.i <- tp[i, c("name", "start", "stop")] # extract start and stop time
  dat[[i]] <- gga %>% # start will full time series
    filter(time_seconds >= injection_time.i$start, # after or equal to start time
           time_seconds <= injection_time.i$stop) %>% # before or equal to end time
    select(time_seconds, ch4_ppm, ch4_d_ppm, co2_ppm, co2_d_ppm) %>% # simplify df
    mutate(across(contains("co2"), ~.x + abs(min(.x)))) %>% # co2 baseline is negative. set min value to 0
    pivot_longer(-time_seconds, names_to = "variable") %>% # pivot to long for facet wrap below
    mutate(name = tp %>% slice(i) %>% pull(name)) %>% # add peak name
    group_split(variable, name) # split data from each injection by variable (e.g. ch4, co2)
}

dat <- flatten(dat) # flatten list (e.g. eliminate nesting)






# INSPECT DATA------------
# look at each data subset to make sure time interval is correct

# map applies function to each list element
map(dat, ~{ 
  # create ggplot image and store in object (plot.i)
  plot.i <- .x %>% ggplot(aes(time_seconds, value)) +
    geom_point() +
    ggtitle(.x$name) + # injection name for title
    facet_wrap(~variable, scales = "free") # separate plot for each analyte
  return(plot.i) # dump plot into list element
}) %>%
  # the next map function displays each plot. you must press "enter"
  # to advance to next plot. Adjust start and stop time until happy
  # with plots
  map(~{ 
    .x %>% print # print the plot
    readline(prompt="Press [enter] to proceed") # require user input before printing next plot
  })
  


# INTEGRATE AREA UNDER CURVE------------  
# This can be done several ways. 

# 1. STATS::INTEGRATE
# This first approach uses stats::integrate. This function provides error estimate
# 
area_integrate1 <- 
  # map applies function to each list element
  map_df(dat, ~{ # map_df returns data frame
    # apply stats::integrate to each injection x variable combination. Temporarily
    # store output (a list) as `integrate_object`,
    integrate_object <- integrate( # integrate requires a function to integrate over
      approxfun(.x$time_seconds, .x$value), # this creates a function that fits the data
      min(.x$time_seconds), # integrate from
      max(.x$time_seconds),
      subdivisions = 1000
      ) # try 1000 or even 5000 # integrate to
    
    # integrate_object is a list. extract the pieces we want
    area <- integrate_object[[1]] # calculated area under the curve
    error <- integrate_object[[2]] # area error
    name <- unique(.x$name) # injection name
    variable = unique(.x$variable) # ch4, co2,....
    
    # return the following tibble
    return(tibble(area_integrate = area, 
                  error = error, 
                  name = name, 
                  variable = variable))
  }) 


# 2. DESCTOOLS::AUC
# area under curve can also be calculated with DescTools::AUC


# 2.1 the area_auc list created immediately below has separate list elements
# for a ggplot image and dataframe from each injection x variable combination
area_auc <- 
  map(dat, ~{
    # extrapolate between points
    .dat <- approx(.x$time_seconds, .x$value) %>% # interpolates between observations
      as.data.frame() # dump into new df
    
    # calculate area under curve
    area_auc <- DescTools::AUC(.dat$x, .dat$y) # calculate area under curve
    
    # plot interpolation for inspection
    ylab.i <- unique(.x$variable)
    title.i <- unique(.x$name)
    plot.i <- ggplot(.x, aes(time_seconds, value)) +
      geom_point(color = "black") +
      geom_point(data = .dat, aes(x,y), color = "red") +
      ylab(ylab.i) +
      ggtitle(title.i)
    
    
    # return ggplot image and dataframe of results
    return(list( # put all object into a list
      tibble( # create tibble with results
        area_auc = area_auc,
        name = unique(.x$name),
        variable = unique(.x$variable)),
      plot.i)) # include plot
    
  }) %>%
  flatten # this simplifies list structure

# 2.2 put ggplot images into separate list. 
# create TRUE/FALSE index for whether list element is a ggplot object
area_auc_plots_index <- map_lgl(area_auc, 
                            # returns T for element containing only ggplot image
                            # for elements containing the DF, returns T or F for each column 
                            ~{.x %>% class %in% "ggplot" %>% 
                                # `any` converts list elements containing multiple T/F to a single T or F
                                any})

# subset list for ggplot images
area_auc_plots <- area_auc[area_auc_plots_index]

# 2.3 put dataframes containing results into a single df
# `!` converts T to F and vice versa. becomes index for list elements containing df
area_auc_results <- area_auc[!area_auc_plots_index] %>% 
  bind_rows

# 3. MERGE RESULTS FROM STATS::INTEGRATE AND DESCTOOLS::AUC
area <- full_join(area_integrate, area_auc_results)

# how do they compare?
ggplot(area, aes(area_integrate, area_auc)) + geom_point() # basically perfect


# 4. INSPECT ANY SUSPECT VALUES
# function below shows each time series plot. Review any that gave weird results.
map(area_auc_plots, ~{print(.x) 
  readline("Press Enter to continue")})

# 5. WRITE RESULTS TO DISK
write.csv(area, file = "output/areas_peaks_standards.csv") #creates a file with the name of the sample and the areas of the peaks

# restore default options
options(op)

# ----------------------
# CALIBRATION AND PLOTTING
# ----------------------

# Load calibration data (file with columns that include CH4 and CO2 concentrations)
calibration <- readr::read_csv("gga_discrete_analysis_calibration_metadata.csv") %>%
  clean_names() %>%
  rename_with(~ "ch4_ppm", .cols = matches("ch4")) %>%
  rename_with(~ "co2_ppm", .cols = matches("co2")) %>%
  distinct(name, .keep_all = TRUE)


# Load standard peak areas and incubation peak areas
standard_area <- readr::read_csv("areas_peaks_standards.csv") %>%
  clean_names()

incubation_area <- readr::read_csv("AAJ_peaks_incubations.csv") %>%
  clean_names()

# Join calibration concentrations to area data for standard injections
std_data <- standard_area %>%
  filter(name %in% calibration$name,
         variable %in% c("ch4_ppm", "co2_ppm")) %>%
  left_join(calibration, by = "name") %>%
  mutate(concentration = case_when(
    variable == "ch4_ppm" ~ ch4_ppm,
    variable == "co2_ppm" ~ co2_ppm,
    TRUE ~ NA_real_
  ))

# Fit linear model for each gas
standard_models <- std_data %>%
  group_by(variable) %>%
  summarise(model = list(lm(concentration ~ area_auc, data = .)), .groups = "drop")

# Merge methane calibration data with peak areas
methane_cal <- standard_area %>%
  filter(variable == "ch4_ppm") %>%
  left_join(calibration %>% select(name, ch4_ppm = matches("ch4")), by = "name")

# Plot the calibration curve
ggplot(methane_cal, aes(x = area_auc, y = ch4_ppm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Methane Calibration Curve",
    x = "Area Under Curve (AUC)",
    y = "Methane Concentration (ppm)"
  ) +
  theme_bw()

# Apply the models to incubation sample peaks
incubation_data <- incubation_area %>%
  filter(variable %in% c("ch4_ppm", "co2_ppm", "ch4_d_ppm", "co2_d_ppm")) %>%
  mutate(
    variable = recode(variable,
                      "ch4_d_ppm" = "ch4_ppm",
                      "co2_d_ppm" = "co2_ppm")
  ) %>%
  left_join(standard_models, by = "variable") %>%
  mutate(
    concentration = purrr::map2_dbl(model, area_auc,
                                    ~predict(.x, newdata = tibble(area_auc = .y))),
    bottle_id = stringr::str_extract(name, "[A-Z]$"),       # Extract last letter
    day = as.integer(stringr::str_extract(name, "(?<=SB-)[0-9]+"))  # Extract numeric day
  )

# Plot CH4 and CO2 concentration vs incubation day per bottle
incubation_plot <- incubation_data %>%
  ggplot(aes(x = day, y = concentration, color = variable)) +
  geom_line() +
  geom_point() +
  facet_wrap(~bottle_id, scales = "free_y") +
  labs(x = "Incubation Day", y = "Concentration (ppm)", title = "CH₄ and CO₂ over Incubation Time") +
  theme_bw()

# Plot CH4 rates vs incubation day per bottle (Rates are missing)
