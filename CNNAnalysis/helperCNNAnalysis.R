# URL-list and helper functions, mostly for Loops
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


?setTxtProgressBar

parseUrlList <- function(htmlList, silent = FALSE) {
  tmp <- vector(mode = "list", length = length(htmlList))
  if(silent == F) pb <- txtProgressBar(min = 0, max = length(htmlList), style = 3)
  
  for (i in 1:length(htmlList)) {
    tmp[[i]] <- htmlList[[i]] %>% 
      html_nodes('script[data-rh="true"][id="liveBlog-schema"][type="application/ld+json"]') %>%
      html_text() 
    tmp[[i]] <- fromJSON(sprintf("[%s]", tmp[[i]]))
    if(silent == F) setTxtProgressBar(pb, i)
  }
  tmp <- bind_rows(tmp)
  return(tmp)
}

generateTeiCorpus <- function(MetadataColumn, TextColumn, MetadataSting, silent = FALSE) {
  if(silent == F) pb <- txtProgressBar(min = 0, max = length(blogTEI), style = 3)
  main <- paste0("<teiCorpus xmlns=\"http://www.tei-c.org/ns/1.0\"><teiHeader>", MetadataSting, "</teiHeader>")
  
  for (i in 1:length(TextColumn)){
    tmp <- str_c("<TEI><teiHeader>", MetadataColumn[[i]], "</teiHeader><text>", TextColumn[[i]], "</text></TEI>")
    main <- str_c(main, tmp)
    if(silent == F) setTxtProgressBar(pb, i)
  }
  paste0(main, "</teiCorpus>")
}
