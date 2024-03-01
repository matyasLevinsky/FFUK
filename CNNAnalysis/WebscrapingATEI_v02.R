library(rvest)
library(tidyverse)
library(jsonlite)
library(janitor)

#############

# Search term was 'site:cnn.com "mass shooting" live-news'
# Time was limited to past Year
# Articles where selected by Googles ranking with removed duplicates until I had 15.

# Load data
urlList <- c("https://edition.cnn.com/europe/live-news/prague-shooting-charles-university-12-21-23/index.html", "https://edition.cnn.com/us/live-news/lewiston-maine-shootings-active-shooter-10-25-23/index.html", "https://edition.cnn.com/us/live-news/farmington-new-mexico-shooting-05-16-23/index.html", "https://edition.cnn.com/us/live-news/nashville-shooting-covenant-school-03-27-23/index.html", "https://edition.cnn.com/us/live-news/unlv-shooting-12-06-23/index.html", "https://edition.cnn.com/us/live-news/kansas-city-chiefs-super-bowl-shooting-02-15-24/index.html", "https://edition.cnn.com/us/live-news/atlanta-midtown-shooting/index.html", "https://edition.cnn.com/us/live-news/louisville-kentucky-shooting-04-11-23/index.html?tab=all", "https://edition.cnn.com/us/live-news/oxford-shooting-jennifer-crumbley-trial-verdict/index.html", "https://edition.cnn.com/us/live-news/kansas-city-chiefs-parade-shooting-02-14-24/index.html", "https://edition.cnn.com/us/live-news/louisville-kentucky-shooting-04-10-23/index.html", "https://edition.cnn.com/us/live-news/lewiston-maine-shootings-active-shooter-10-25-23/index.html", "https://edition.cnn.com/us/live-news/allen-texas-mall-shooting-news-05-07-23/index.html")

readUrlList <- function(urlList, silent = FALSE) {
  tmp <- vector(mode = "list", length = length(urlList))
  if(silent == F) pb <- txtProgressBar(min = 0, max = length(urlList), style = 3)
  
  for (i in 1:length(urlList)) {
    tmp[[i]] <- read_html(urlList[[i]])
    if(silent == F) setTxtProgressBar(pb, i)
  }
  return(tmp)
}

htmlList <- readUrlList(urlList, silent = T)

parseUrlList <- function(htmlList) {
  tmp <- vector(mode = "list", length = length(htmlList))
  pb <- txtProgressBar(min = 0, max = length(htmlList), style = 3)
  
  for (i in 1:length(htmlList)) {
    tmp[[i]] <- htmlList[[i]] %>% 
      html_nodes('script[data-rh="true"][id="liveBlog-schema"][type="application/ld+json"]') %>%
      html_text() 
    tmp[[i]] <- fromJSON(sprintf("[%s]", tmp[[i]]))
    setTxtProgressBar(pb, i)
  }
  tmp <- bind_rows(tmp)
  return(tmp)
}

# parsedDb <- parseUrlList(htmlList)
# saveRDS(parsedDb, file = "parsedDB_WebscrapingCNN")

parsedDb <- readRDS("parsedDB_WebscrapingCNN")

options(max.print = 25)
head(parsedDb, 1)

blogPostsExpanded <- parsedDb %>%
  unnest(cols = c(liveBlogUpdate), names_sep = "_blog") %>% 
  rename_with(~ str_replace_all(.x, "[@\\$]", "")) %>% 
  select(mainEntityOfPage:datePublished, author:liveBlogUpdate_blogheadline, liveBlogUpdate_blogauthor:liveBlogUpdate_blogarticleBody)

##### TEI Compatible corpus --------------------------------------------------------------------------------------------

blogTEI <- blogPostsExpanded %>% 
  mutate(
    MetadataColumn = paste0("eventName: ", about$name, "headline:", liveBlogUpdate_blogheadline), 
    TextColumn = liveBlogUpdate_blogarticleBody
  )

generateTeiCorpus <- function(MetadataColumn, TextColumn, MetadataSting) {
  pb <- txtProgressBar(min = 0, max = length(blogTEI), style = 3)
  main <- paste0("<teiCorpus xmlns=\"http://www.tei-c.org/ns/1.0\"><teiHeader>", MetadataSting, "</teiHeader>")
  
  for (i in 1:length(TextColumn)){
    tmp <- str_c("<TEI><teiHeader>", MetadataColumn[[i]], "</teiHeader><text>", TextColumn[[i]], "</text></TEI>")
    main <- str_c(main, tmp)
    setTxtProgressBar(pb, i)
  }
  paste0(main, "</teiCorpus>")
}

description <- paste0("A TEI corpus containg ", length(blogTEI), " live blogpost from CNN about mass shootings.")

prague <- generateTeiCorpus(MetadataColumn = blogTEI$MetadataColumn, TextColumn = blogTEI$TextColumn, MetadataSting = description)

prague

writeLines(prague, "TEICorpusMassShootingsCNN.xml")

#### -------------------------------------------------------------------------------------------------------------------
library(tidytext)
library(textdata)

# Analysis

reducedTb <- blogPostsExpanded %>% 
  unnest(cols = c(about)) %>% 
  select(name, liveBlogUpdate_blogheadline, liveBlogUpdate_blogarticleBody) %>% 
  rename("headline" = liveBlogUpdate_blogheadline, "text" = liveBlogUpdate_blogarticleBody) %>% 
  mutate(
    text = tolower(text),
    name = gsub("April 10, 2023: |April 11, 2023 - |Live updates: |March 27, 2023 |May 3, 2023 - |October 25, 2023 - ", "", name), 
    name = str_wrap(name, width = 40)
    ) %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

# NrcSentiments <- get_sentiments("nrc")  

articlesSentiment <- reducedTb %>%
  inner_join(NrcSentiments, by = "word", relationship = "many-to-many") %>%
  count(name, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  mutate(
    total = rowSums(select(., anger:trust)),
    other = rowSums(select(., c(disgust, joy, surprise))),
    reducedTb %>% group_by(name) %>% summarise(articles = n_distinct(headline)) %>% select("articles"), # Direct add articles
    across(anger:other, ~ .x / total, .names = "{.col}Prop")
  )

articlesSentimentLong <- articlesSentiment %>% 
  select(name, angerProp, anticipationProp, fearProp, negativeProp:sadnessProp, trustProp, otherProp) %>% 
  pivot_longer(cols = angerProp:otherProp,  names_to = "emotion", values_to = "prevalence") %>% 
  mutate(emotion = gsub("Prop", "", emotion))

unique(articlesSentimentLong$emotion)
  
### Plotting -----------------------------------------------------------------------------------------------------------

library(ggplot2)
library(scales)

ggplot(articlesSentimentLong) +
  aes(x = name, y = prevalence, fill = emotion) +
  geom_col() +
  geom_text(
    aes(label = percent(prevalence, accuracy = 0.1)),
    nudge_y = if_else(articlesSentimentLong$prevalence > .15, -.05, .05),  
    size = 3, 
    color =  "black") + 
  coord_flip() +
  theme_minimal() + 
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none") +
  facet_grid(~ emotion)

### Latent Dirichlet allocation --------------------------------------------------------------------------

library(topicmodels)

removeWords <- c("cnn", "p.m")
toRemoveDf <- tibble(document = NA, word = removeWords, n = 1)

# Dataformatting
# Need one-term-per-document-per-row

DtmData <- reducedTb %>% 
  group_by(name, headline) %>% 
  count(word, sort = T) %>% 
  ungroup() %>% 
  mutate(document = paste0(name, "_", headline)) %>% 
  select(document, word, n) %>% 
  anti_join(toRemoveDf, by = "word")

head(DtmData, 5)

# Modeling

castData <- cast_tdm(data = DtmData, term = document, document = word, value = n) # reverse necessary

castData

model1 <- LDA(castData, k = 12, control = list(seed = 1234)) # reverses, see above
  
model1

model1Topics <- tidytext::tidy(model1, matrix = "beta")

model1Topics

# Preliminary graphing

shootingTopTerms <- model1Topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

shootingTopTerms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Conclusion: The articles appear to be very simmilar /// have to use 12 topics

model1Gamma <- tidytext::tidy(model1, matrix = "gamma")

model1Gamma

separatedGamma <- model1Gamma %>%
  separate(document, c("Event", "Headline"), sep = "_", convert = TRUE)

separatedGamma

separatedGamma %>%
  mutate(title = reorder(Event, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ Event) +
  labs(x = "topic", y = expression(gamma))
