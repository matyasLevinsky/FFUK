library(here)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(ggstream)
library(paqr)
library(treemapify)
library(ggtext) 
library(patchwork)

### More advanced version we will be loading directly form a HAR file and be harvesting ----------
# way more data

harPathsSelected <- here("MonitoringParser", "VinoDanPAQProkopNERV.har")
nameGenerator <- paste0("VinoDanPart", 1:5, ".har")
harPathsUnselected <- here("MonitoringParser", nameGenerator)

# Load the HAR file as a function

extractParsedHar <- function(harPath) {
  har <- jsonlite::fromJSON(harPath)
  
  onlyFeed <- har$log$entries$response %>% 
    filter(content$mimeType == "application/json") %>% 
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
      news_source_publisher_name = if_else(is.na(news_source_publisher_name), "Unknown", news_source_publisher_name),
      across(c(article_type_text, news_source_category_category_type_text, news_source_category_name, news_source_publisher_name, news_source_name), as.factor),
      authors = map_chr(authors, ~ if(is.data.frame(.x) && "name" %in% names(.x)) paste(.x$name, collapse = ", ") else NA)
    )
    
    return(reducedStories)
}

dataMention <- extractParsedHar(harPathsSelected)
dataUnmentioned <- map(harPathsUnselected, extractParsedHar)

dataUnmentionedTib <- bind_rows(dataUnmentioned) %>% 
  distinct()

# compared <- dataUnmentionedTib %>% 
#   mutate(published = lubridate::as_date(published)) %>% 
#   group_by(published, news_source_category_category_type_text) %>% 
#   summarise(count = n()) %>% 
#   pivot_wider(names_from = "news_source_category_category_type_text", values_from = "count")
# 
# comparison <- read.csv(here("MonitoringParser", "comparison.csv")) %>% 
#   filter(Online > 0 | Tisk > 0 | Televize > 0 | Rozhlas >  0 | Podcasty > 0 | Sociální.média > 0)

combinedDf <- bind_rows(PAQMentioned = dataMention, PAQNotMentioned = dataUnmentionedTib, .id = "Mentions") %>% 
  arrange(Mentions) %>% 
  group_by(pick(-Mentions)) %>% 
  filter(row_number() == 1) %>% # Getting rid of duplicates
  ungroup() %>% 
  mutate(date = as_date(published)) %>% 
  filter(date >= "2022-07-01" & date <= "2023-12-31")

### Data Visualization -----------------------------------------------------------------------------
# Stream chart
# We will now smooth the area chart to create a stream chart

StreamPlot <- combinedDf %>%
  group_by(Mentions, date) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = date, y = count, fill = Mentions)) +
  ggstream::geom_stream(type = 'ridge', geom = "contour", color = "black", linewidth = 1, bw = 0.2, n_grid = 2000, sorting = 'inside_out') +
  ggstream::geom_stream(type = 'ridge', bw = 0.2, n_grid = 2000, sorting = 'inside_out') 

StreamPlot

# We will now create a proportional streamplot that shows the proportion of mentions of PAQ

PropPlot <- combinedDf %>%
  group_by(Mentions, date) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = date, y = count, fill = Mentions)) +
  ggstream::geom_stream(type = 'proportion', bw = 0.35, n_grid = 2000, sorting = 'inside_out') +
  ggstream::geom_stream(type = 'proportion', bw = 0.35, n_grid = 2000, geom = "contour", 
                        color = "black", linewidth = .5, sorting = 'inside_out')

PropPlot

# Building the composite plot form our three existing plots

compositePlot <- StreamPlot + 
  PropPlot +
  plot_layout(widths = 1, heights = c(6, 1), guides = 'collect', axes = 'collect') &
  theme(legend.position = 'bottom',
    legend.title = element_markdown(family = "Inter"), 
    legend.text = element_markdown(family = "Inter"), 
    axis.title = element_blank()) &
  labs(fill = "Zmínka o <span style = 'color:#063E78;'><b>PAQ</b></span>?") &
  scale_fill_manual(
    values = c("#063E78", "#ECB925"),
    labels = c("<span style = 'color:#063E78;'><b>PAQ</b></span> nebyl zmníněn", "<span style = 'color:#063E78;'><b>PAQ</b></span> zmníněn")
    )

compositePlot

?scale_fill_discrete
  
# Working with data2 -------------------------------------------------------------------------------
# palette generation
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


