# ...

ott <- AvrMai |> filter(str_detect(station_name, "OTTAWA"))

### HOURLY

otthour <- weather_dl(station_ids = "49568", start = "2019-03-31", end = "2025-05-31", interval = "hour")
otthour2 <- otthour

fill_weather_gaps <- function(weather_vector) {
  # Create a copy to modify
  filled_vector <- weather_vector
  
  # Identify indices with NA values
  na_indices <- which(is.na(weather_vector))
  
  # Define the patterns we're looking for
  patterns <- c("cloud", "clear", "shower")
  
  for (i in na_indices) {
    # Find the nearest non-NA values before and after
    prev_val <- NA
    prev_dist <- Inf
    for (j in (i-1):1) {
      if (!is.na(weather_vector[j])) {
        prev_val <- weather_vector[j]
        prev_dist <- i - j
        break
      }
    }
    
    next_val <- NA
    next_dist <- Inf
    for (j in (i+1):length(weather_vector)) {
      if (!is.na(weather_vector[j])) {
        next_val <- weather_vector[j]
        next_dist <- j - i
        break
      }
    }
    
    # Check if either neighbor contains our patterns
    prev_has_pattern <- if (!is.na(prev_val)) any(sapply(patterns, grepl, x = tolower(prev_val))) else FALSE
    next_has_pattern <- if (!is.na(next_val)) any(sapply(patterns, grepl, x = tolower(next_val))) else FALSE
    
    if (prev_has_pattern && next_has_pattern) {
      # Both have patterns - use the closer one, or next if equal distance
      if (prev_dist < next_dist) {
        filled_vector[i] <- prev_val
      } else if (next_dist < prev_dist) {
        filled_vector[i] <- next_val
      } else {
        # Equal distance - use the next value
        filled_vector[i] <- next_val
      }
    } else if (prev_has_pattern) {
      filled_vector[i] <- prev_val
    } else if (next_has_pattern) {
      filled_vector[i] <- next_val
    } else {
      # No matching patterns found - default to "Cloudy"
      filled_vector[i] <- "Cloudy"
    }
  }
  
  return(filled_vector)
}

# Apply the function to your weather column
otthour2$filled_weather <- fill_weather_gaps(otthour2$weather)

otthour_light <- otthour2 %>%
  select(
    station_name, station_id, climate_id, TC_id, date, time, year, month, day, hour, filled_weather, weather
  ) %>%
  filter(month %in% c("04", "05")) %>%
  mutate(suncloud = if_else(str_detect(filled_weather, "Clear"), 0, 1))

otthour_calc <- otthour_light %>%
  group_by(date, year, month, day) %>%
  reframe(
    sumClouds = sum(suncloud)
  )

######### GGCAL FUNCTION CUSTOM

ggcal <- function(dates, fills, days, calc, unitm) {
  # get ordered vector of month names
  months <- format(seq(as.Date("2016-05-01"), as.Date("2016-10-01"), by = "1 month"), "%B")
  
  # get lower and upper bound to fill in missing values
  mindate <- as.Date(format(min(dates), "%Y-%m-01"))
  maxdate <- (seq(as.Date(format(max(dates), "%Y-%m-01")), length.out = 2, by = "1 month")-1)[2]
  # set up tibble with all the dates.
  filler <- tibble(date = seq(mindate, maxdate, by="1 day"))
  
  t1 <- tibble(date = dates, fill_var = fills, day = days) %>%
    #right_join(filler, by="date") %>% # fill in missing dates with NA [SC] # This was giving me an extra NA month - gotcha!
    mutate(dow = as.numeric(format(date, "%w"))) %>%
    mutate(month = format(date, "%B")) %>%
    mutate(woy = as.numeric(format(date, "%U"))) %>%
    mutate(year = as.numeric(format(date, "%Y"))) %>%
    #mutate(month = factor(month, levels=months, ordered=TRUE)) %>%
    arrange(year, month) %>%
    mutate(monlabel=month)
  
  if (length(unique(t1$year))>1) { # multi-year data set
    t1$monlabel <- paste(t1$month, t1$year)
  }
  
  t2 <- t1 %>%
    mutate(monlabel = factor(monlabel, ordered = TRUE)) %>%
    mutate(monlabel = fct_inorder(monlabel)) %>%
    mutate(monthweek = woy-min(woy),
           y=max(monthweek)-monthweek+1)
  
  #tsumr <- t2 %>% group_by(year, month) %>% reframe(summ = sum(fill_var, na.rm = T)) 
  
  #print(t2)
  #print(tsumr)
  
  summary_data <- t2 %>%
    group_by(year, month) %>%  # Your facet variables
    summarize(sum_value = round(sum(fill_var, na.rm = TRUE), digits = 0),
              avg_value = round(mean(fill_var, na.rm = TRUE), digits = 0),
              x_pos = 3,
              y_pos = 3) %>%
    mutate(
      y_pos = if_else(month == "avril", 7.5, 3.5),
      display_value = if(calc == "sum") sum_value else avg_value,
      monthly_label = if(calc == "sum") "total" else "moyenne"
    )
  
  print(summary_data)
  
  weekdays <- c("D", "L", "M", "M", "J", "V", "S")
  ggplot(t2, aes(x = dow, y = y)) +
    geom_tile(aes(dow, y, fill = fill_var), color = "black", linewidth = .3) +
    #geom_text(data = t2 %>% filter(days == "01"), aes(dow, y), label = 1, size = 2.5) +
    #geom_text(aes(dow, y, label = sum(fill_var, na.rm = T)), size = 2.5) +
    facet_grid(month~year, scales = "free_y", switch = "both") +
    geom_label(fill = "black", color = "white", data = summary_data, aes(x = x_pos, y = y_pos, label = paste(display_value, unitm)), inherit.aes = T, size = 3.2, fontface = "bold", label.r = unit(0.5, "lines")) +
    geom_label(fill = "black", color = "white", data = summary_data %>% filter(year == "2025", month == "avril"), aes(x = x_pos + 4.3, y = y_pos - 2.5, label = monthly_label), inherit.aes = T, angle = 270, size = 3.5, hjust = .5, fontface = "bold", label.r = unit(0.5, "lines")) +
    scale_x_continuous(position="top",
                       breaks=seq(0,6), labels=weekdays) +
    theme_minimal() +
    theme(panel.background = element_rect(fill=NA, color=NA),
          strip.background = element_rect(fill=NA, color=NA),
          panel.grid = element_blank(),
          strip.text.x = element_text(size = 16, face = "bold", margin = margin(t = 0, b = 5, unit = "pt")),
          legend.title = element_blank(),
          legend.text.position = "right",
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12, angle = -270),
          axis.text.y = element_blank(),
          legend.position = "right",
          strip.placement = "outside",
          plot.title = element_text(hjust = 0.5),
          panel.spacing.y = unit(-.63, "cm", data = NULL),
          panel.spacing.x = unit(9, "pt"),
          legend.margin = margin(l = -.2, r = 0, unit = "cm"),
          strip.text.y = element_text(margin = margin(r = 0, l = 22, unit = "pt"), size = 13, face = "bold")
    ) +
    coord_cartesian(clip = "off")
}

######### TEMP

ottTemps <- ggcal(
  fills = ott$max_temp, 
  dates = ott$date,
  days = ott$day,
  calc = "mean",
  unitm = "°C"
) +
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = (11.3 + 19.6) / 2, 
    na.value = "black",
    guide = guide_colorbar(barwidth = .5, barheight = 6, theme = theme(legend.ticks = element_blank()))) +
  theme(strip.text.x = element_blank(),
        plot.margin = margin(l = 0, r = 0, t = 0, b = 0, unit = "pt")) +
  labs(y = "Température max (°C)") +
  scale_y_continuous(position = "right")

#### VIZ CLOUDS

cloudpal <- colorRampPalette(c("yellow", "lightyellow", "lightgray", "#787878"))
cloudcolr <- cloudpal(6)

ottClouds <- ggcal(
  fills = otthour_calc$sumClouds, 
  dates = otthour_calc$date,
  days = otthour_calc$day,
  calc = "sum",
  unitm = "h"
) +
  scale_fill_gradient(
    low = "yellow",
    high = "gray",
    na.value = "black",
    guide = guide_colorbar(barwidth = .5, barheight = 6, theme = theme(legend.ticks = element_blank()))) +
  theme(axis.text.x = element_blank(), strip.text.x = element_blank(),
        plot.margin = margin(l = 0, r = 0, t = 0, b = 0, unit = "pt")) +
  labs(y = "Nuageux (heures)") +
  scale_y_continuous(position = "right")


###### RAIN

#RainSum <- ott %>% group_by(year, month) %>% reframe(sum = sum(total_precip, na.rm = T))

ottRain <- ggcal(
  fills = ott$total_precip, 
  dates = ott$date,
  days = ott$day,
  calc = "sum",
  unitm = "mm"
) +
  scale_fill_gradientn(
    colours = c("#FFFFFF", colorRampPalette(c("turquoise3", "turquoise4"))(100)),
    values = rescale(c(0, seq(1, 20, length.out = 100))),
    limits = c(0, NA),
    na.value = "black",
    guide = guide_colorbar(barwidth = .5, barheight = 6, theme = theme(legend.ticks = element_blank()))) +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(l = 0, r = 0, t = 0, b = 0, unit = "pt")) +
  labs(y = "Précipitations (mm)") +
  scale_y_continuous(position = "right")

THEOTT <- (ottTemps / ottClouds / ottRain) + 
  plot_annotation(
    title = "Températures, temps nuageux et pluie en avril et mai à Ottawa",
    caption = "Les cases noires sont des données indisponibles | Fait par @stevecarufel.bsky.social avec #Rstats et la {tidyverse} | Source : Environnement et Changement climatique Canada via {weathercan}"
  ) &
  theme(
    text = element_text(family = "Rubik"),
    title = element_text(family = "Rubik"),
    plot.caption = element_text(hjust = .5, margin = margin(t = 3, b = 7, unit = "pt")), 
    plot.margin = margin(l = 0, r = 0, t = 0, b = 0, unit = "pt"),
    axis.title.y = element_text(margin = margin(l = 20, r = 10, unit = "pt")),
    plot.title = element_text(hjust = .5, size = 18, face = "bold", margin = margin(t = 10, b = 5, unit = "pt")),
    #strip.text.y = element_text(margin = margin(l = 12, unit = "pt")),
    legend.box.margin = margin(r = 20, unit = "pt"),
    plot.background = element_rect(fill = "#FEFCE9", color = NA)
  )

ggsave("OTT.png", plot = THEOTT, width = (1912 / 144), height = (970 / 144), dpi = 200, bg = "#FEFCE9")