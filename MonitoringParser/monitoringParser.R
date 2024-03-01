library(here)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(ggstream)
library(paqr)

# Load data

dataLocation <- here("MonitoringParser", "ticheVinoADane.txt")

data <- jsonlite::fromJSON(dataLocation)

# We will now select data which is a dataframe under feed_chart

dataSmall <- data$feed_chart$data

# Create a tibble to join with our future data consisting of 3 columns:  
# category_type, color, and label

category_type <- c(1:8)
color <- c("#13CDF0", "#C829A8", "red", "#FEBD35", "#6CC829", "red", "#9998EA", "#199798")
label <- c("Online", "Tisk", "NA", "Televize", "Rozhlas", "NA", "Socialni media", "Podcasty")

categoryData <- tibble(category_type, color, label)

# We now have two columns one being date and one being data which is a list
# We will now unnest the data column

dataUnnested <- dataSmall %>% 
  unnest(data) %>% 
  left_join(categoryData, by = "category_type") %>%
  mutate(
    date = lubridate::ymd(date), 
    category_type = as.factor(category_type), 
    label = as.factor(label) %>% fct_rev()
  )

# Stacked bar chart
# Using ggplot2 we will created a stacked bar chart, grouped by category_type

dataUnnested %>%
  ggplot(aes(x = date, y = count, fill = label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(dataUnnested$color, dataUnnested$label)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Feed Chart", x = "Date", y = "Count", fill = "Category Type")

# Area chart
# We will now plot the same data but as a area chart

dataUnnested %>%
  ggplot(aes(x = date, y = count, fill = label)) +
  geom_area() +
  scale_fill_manual(values = setNames(dataUnnested$color, dataUnnested$label)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Feed Chart", x = "Date", y = "Count", fill = "Category Type")

# Stream chart
# We will now smooth the area chart to create a stream chart

dataUnnested %>%
  filter(count > 0) %>% # Has issues with 0 values
  ggplot(aes(x = date, y = count, fill = label)) +
  ggstream::geom_stream(type = 'ridge', bw = 0.3, n_grid = 5000) +
  scale_fill_manual(values = setNames(dataUnnested$color, dataUnnested$label)) +
  paqr::theme_paq() +
  theme(
    # axis.text.x = element_text(angle = 90, hjust = 1), 
    legend.position = 'bottom'
    ) +
  labs(title = "Feed Chart", x = "Date", y = "Count", fill = "Category Type")


citation('ggstream')
citation('paqr')






