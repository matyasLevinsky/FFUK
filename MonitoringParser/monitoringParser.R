library(here)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(ggstream)
library(paqr)
library(treemapify)
library(ggtext) 
library(patchwork)

# Load data

dataLocation <- here("MonitoringParser", "ticheVinoADane.txt")
dataLocation2 <- here("MonitoringParser", "vinoAndDanAndPAQ.txt")

data <- jsonlite::fromJSON(dataLocation)
data2 <- jsonlite::fromJSON(dataLocation2)

# We will now select data which is a dataframe under feed_chart

dataSmall <- data$feed_chart$data
dataSmall2 <- data2$feed_chart$data %>% 
  filter(date != "2024-03-01") # Keep series for same time period

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
  bind_rows(NoMention = ., PaqMentioned = unnest(data = dataSmall2, cols = data), .id = "id") %>% 
  left_join(categoryData, by = "category_type") %>%
  mutate(
    date = lubridate::ymd(date), 
    category_type = as.factor(category_type), 
    label = as.factor(label) %>% fct_rev()
  )

# Stacked bar chart
# Using ggplot2 we will created a stacked bar chart, grouped by category_type

dataUnnested %>%
  filter(id == "NoMention") %>%
  ggplot(aes(x = date, y = count, fill = label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(dataUnnested$color, dataUnnested$label)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Feed Chart", x = "Date", y = "Count", fill = "Category Type")

# Area chart
# We will now plot the same data but as a area chart

dataUnnested %>%
  filter(id == "NoMention") %>%
  ggplot(aes(x = date, y = count, fill = label)) +
  geom_area() +
  scale_fill_manual(values = setNames(dataUnnested$color, dataUnnested$label)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Feed Chart", x = "Date", y = "Count", fill = "Category Type")

# Stream chart
# We will now smooth the area chart to create a stream chart

StreamPlot <- dataUnnested %>%
  filter(count > 0 & id == "NoMention") %>% # Has issues with 0 values
  ggplot(aes(x = date, y = count, fill = label)) +
  ggstream::geom_stream(type = 'ridge', geom = "contour", color = "black", size = 1, bw = 0.3, n_grid = 5000) +
  ggstream::geom_stream(type = 'ridge', bw = 0.3, n_grid = 5000) +
  scale_fill_manual(values = setNames(dataUnnested$color, dataUnnested$label)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = c(50, 100, 150, 200, 250, 300)) +
  paqr::theme_paq() +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = "Datum", y = "Pošet denních zmínek", fill = "**Druh** media\nse zmínkou")

# Annotate main plot 

annotation <- c("Pre-peak", 
                "1st Peak, we published XXX in XXX", 
                "2nd Peak, we published XXX in XXX", 
                "3rd Peak, XXX happened")
date <- lubridate::ymd(c("2023-01-01", "2023-01-01", "2023-09-14", "2023-12-01"))
count <- c(50, 270, 155, 115)
annotationsTib <- tibble(annotation, date, count)

annotatedStreamPlot <- StreamPlot + 
  geom_textbox(
    data = annotationsTib, 
    aes(date, count, label = annotation), 
    color = "#ECB925",
    size = 4,
    fill = "#063E78", 
    maxwidth = unit(9, "lines"),
    hjust = .5
    ) + 
  coord_cartesian(clip = "off")

# Create a treemap showing the category of all the data

TreePlot <- dataUnnested %>%
  filter(id == "NoMention") %>%
  group_by(label) %>%
  summarise(count = sum(count)) %>%
  ggplot(aes(area = count, fill = label, label = paste0(" ", label, " \n n = ", count, " "))) + 
  treemapify::geom_treemap(size = 5, color = "white", layout = "squarified") +
  treemapify::geom_treemap_text(colour = "white", place = "centre", layout = "squarified", size = 12) +
  scale_fill_manual(values = setNames(dataUnnested$color, dataUnnested$label)) +
  paqr::theme_paq() +
  theme(legend.position = 'none')

# We will now create a proportional streamplot that shows the proportion of mentions of PAQ

PaqIndex <- c("PaqMentioned", "NoMention")
PaqColors <- c("#ECB925", "white")
PaqScale <- tibble(PaqIndex, PaqColors)

PropPlot <- dataUnnested %>%
  group_by(id, date) %>% 
  summarise(count = sum(count)) %>%
  ggplot(aes(x = date, y = count, fill = id, alpha = if_else(id == "PaqMentioned", 1, 0))) + # Alpha is buggy
  ggstream::geom_stream(type = 'proportion', bw = 0.3, n_grid = 1000, sorting = 'onset', 
                        geom = "contour", color = "black", size = 1) +
  ggstream::geom_stream(type = 'proportion', bw = 0.3, n_grid = 1000, sorting = 'onset') +
  scale_fill_manual(values = setNames(PaqScale$PaqColors, PaqScale$PaqIndex)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("100 %", "50 %", "0 %")) +
  scale_alpha(guide = 'none') +
  paqr::theme_paq() +
  theme(
    axis.ticks.x = element_blank(), 
    axis.text.x = element_blank(),
    axis.title.x = element_blank()) +
    # legend.title = element_markdown(family = "Inter Regular")) +
  labs(x = "Datum", y = "Podíl zmínek o PAQ", fill = "Zmínka o <span style = 'color:#063E78;'><b>PAQ</b></span>?")

# Building the composite plot form our three existing plots

compositePlot <- annotatedStreamPlot + 
  PropPlot +
  plot_layout(widths = 1, heights = c(4, 1), guides = 'collect', axes = 'collect') &
  theme(
    legend.position = 'bottom',
    legend.title = element_markdown(family = "Inter Regular"), 
    legend.key = element_rect(colour = "black", linewidth = .5L)
  ) 

compositePlot

?theme
  
# Save as a high quality pdf 

ggsave(plot = compositePlot, filename = here("MonitoringParser.pdf"), width = 21, height = 29.7, units = "cm")











