library(here)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(ggstream)
library(paqr)
library(treemapify)
library(ggtext) 
library(patchwork)

### Simple txt. version, where we take network packets one by one ----------------------------------
# Load txt exports 

dataLocation <- here("MonitoringParser", "ticheVinoADane.txt")
dataLocation2 <- here("MonitoringParser", "vinoAndDanAndPAQ.txt")
data <- jsonlite::fromJSON(dataLocation)
data2 <- jsonlite::fromJSON(dataLocation2)

# We will now select data which is a dataframe under feed_chart
dataSmall <- data$feed_chart$data
dataSmall2 <- data2$feed_chart$data %>% 
  filter(date != "2024-03-01") # Keep series for same time period

### More advanced verersion we will be loading directly form a HAR file and be harvesting ----------
# way more data

harPath <- here("MonitoringParser", "app.mediaboard.com.har")

# Load the HAR file

har <- jsonlite::fromJSON(harPath)
onlyResponse <- har$log$entries$response

onlyJson <- onlyResponse %>% 
  filter(content$mimeType == "application/json")

onlyFeed <- onlyJson %>% 
  filter(content$size > 1000 & is.na(content$encoding)) %>% 
  unnest(content) %>% 
  select(text)

parsedText <- lapply(onlyFeed$text, jsonlite::fromJSON) 

combinedStories <- map_dfr(parsedText, ~ .x$feed_stories) %>% 
  unnest(cols = c(news_source, article_type), names_sep = "_", names_repair = "universal") %>% 
  unnest(cols = c(news_source_publisher, news_source_category), names_sep = "_", names_repair = "universal") %>% 
  unnest(cols = c(news_source_category_category_type), names_sep = "_", names_repair = "universal") %>% 
  janitor::clean_names()

reducedStories <- combinedStories %>% 
  select(news_source_name, news_source_publisher_name, news_source_category_name,  
         news_source_category_category_type_id_15, news_source_category_category_type_text, 
         news_source_url, news_source_monthly_sessions:humanized_source_name, article_type_text, 
         title, url, published, authors) %>% 
  mutate(
    published = lubridate::ymd_hms(published),
    article_type_text = as.factor(article_type_text),
    news_source_category_category_type_text = as.factor(news_source_category_category_type_text),
    news_source_category_name = as.factor(news_source_category_name),
    news_source_publisher_name = if_else(is.na(news_source_publisher_name), "Unknown", news_source_publisher_name),
    news_source_publisher_name = as.factor(news_source_publisher_name),
    news_source_name = as.factor(news_source_name), 
    authors = map_chr(authors, function(author) { # It just works, If_else wont work
      if(is.data.frame(author) && "name" %in% names(author)) {
        paste(author$name, collapse = ", ")
      } else {
        NA_character_}})
    )

# Advamced data are ready for plotting, yay!


### Data Visualization -----------------------------------------------------------------------------
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
  ggstream::geom_stream(type = 'proportion', bw = 0.3, n_grid = 1000, sorting = 'inside_out', 
                        geom = "contour", color = "black", size = 1) +
  ggstream::geom_stream(type = 'proportion', bw = 0.3, n_grid = 1000, sorting = 'inside_out') +
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
    legend.title = element_markdown(family = "Inter"), 
    legend.key = element_rect(colour = "black", linewidth = .5L)
  ) 

compositePlot
  
# Save as a high quality pdf 

# ggsave(plot = compositePlot, filename = here("MonitoringParser.pdf"), width = 21, height = 29.7, units = "cm")

# install.packages("extrafont")
library(extrafont)
# loadfonts(device = "win")

# Working with data2 -------------------------------------------------------------------------------

# palette genearation
library(colorspace)  
library(scales)

# number of palettes needed
n <- length(unique(reducedStories$news_source_category_category_type_text))
hues <- c(300, 50, 250, 100, 200, 150)

# now calculate the colors for each data point
addedColor <- reducedStories %>%
  group_by(news_source_category_category_type_text, news_source_publisher_name, humanized_source_name) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  filter(news_source_category_category_type_text != "Sociální média")

?sequential_hcl
?gradient_n_pal()

addedColor %>%
  ggplot(
    aes(area = count,  
      fill = news_source_category_category_type_text, 
      subgroup = news_source_category_category_type_text, 
      subgroup2 = news_source_publisher_name, 
      label = paste0(humanized_source_name, ", n:\u00A0", count))) + 
  treemapify::geom_treemap() + # Reduce range of alpha here
  treemapify::geom_treemap(colour = "white", 
    aes(fill = "black", alpha = rescale(as.numeric(news_source_publisher_name), to = c(.999, 1)))) +
  treemapify::geom_treemap_text(colour = "white", place = "centre", size = 14,  min.size = 8, reflow = TRUE) +
  treemapify::geom_treemap_subgroup_border(colour = "white",size = 8) +
  treemapify::geom_treemap_subgroup2_border(colour = "white",size = 4) + 
  treemapify::geom_treemap_subgroup_text(place = "center", alpha = .5, color = "#063E78", grow = TRUE) + 
  paqr::theme_paq() +
  theme(legend.position = "none")

as.numeric(addedColor$news_source_publisher_name)

?geom_treemap_subgroup_text()


alphaTib <- as.numeric(addedColor$news_source_publisher_name)

rescale(as.numeric(addedColor$news_source_publisher_name), to = c(.8, 1))

