
## Load libraries

``` r
library(knitr)
library(RefManageR)
library(tidyverse)
library(here)
library(bib2df)
library(igraph)
library(ggraph)
theme_set(theme_test())
```

## About bib

  - The `.bib` file is generated from my library in papers.app
  - The entire collection is exported (`File > Export > BibTeX Library`)
    and the `BibTeX Record` is set to `standard`
  - The resulting .bib file is saved as `papers.bib` at the root level
    of the `mySources` project
  - The file is formated using `BibDesk.app`
      - Open file
      - Convert html crap
      - Save
  - This will give the file consistent spacing/naming/etc.
  - The `papers.bib` file was last generated on 05/17/2018

## Downloading

You can download the latest version into your projects using the
following
code:

``` r
fileUrl <- "https://raw.githubusercontent.com/jvcasillas/mySources/master/papers.bib"
download.file(fileUrl, destfile = "papers.bib", method = "curl")
```

## Load bib

``` r
bib <- bib2df("papers.bib")
```

## Citation Types

``` r
counts <- xtabs(~CATEGORY, data = bib) %>% as.tibble

counts %>% 
  mutate(., CATEGORY = fct_reorder(CATEGORY, n)) %>% 
  ggplot(., aes(x = CATEGORY, y = n, label = n)) + 
    geom_bar(stat = 'identity', color = 'black', 
             fill = 'lightblue', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Citation Type") + 
    coord_flip() + 
    theme_test()
```

<img src="https://i.imgur.com/hOlsKah.png" width="960" />

## Journals

``` r
bib %>% 
  group_by(., JOURNAL) %>% 
  summarize(., counts = n()) %>% 
  na.omit() %>% 
  mutate(., JOURNAL = fct_reorder(JOURNAL, counts)) %>% 
  arrange(., desc(counts)) %>% 
  slice(., 1:25) %>% 
  ggplot(., aes(x = JOURNAL, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'lightblue', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Journal") + 
    scale_x_discrete(expand = expand_scale(0.04))  + 
    coord_flip() + 
    theme_test()
```

<img src="https://i.imgur.com/zky96tL.png" width="960" />

## Authors

``` r
top_authors <- unlist(bib$AUTHOR) %>% 
  as.tibble(.) %>%
  group_by(., value) %>% 
  summarize(., counts = n()) %>% 
  arrange(., desc(counts)) %>% 
  slice(., 1:100) 

top_authors %>% 
  slice(., 1:50) %>% 
  mutate(., value = fct_reorder(value, counts)) %>% 
  ggplot(., aes(x = value, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'lightblue', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Author") + 
    scale_x_discrete(expand = expand_scale(0.04)) + 
    coord_flip() + 
    theme_test()
```

<img src="https://i.imgur.com/lGo6e6N.png" width="960" />

## Co-authors

### Co-authorship over time

``` r
bib %>% 
  mutate(., n_authors = lengths(AUTHOR)) %>% 
  filter(., YEAR >= 1950) %>% 
  ggplot(., aes(x = YEAR, y = n_authors)) + 
    geom_jitter(height = 0.2, alpha = 0.5, pch = 20) + 
    geom_smooth(method = "glm", method.args = list(family = "poisson")) + 
    labs(x = "Publication Year", y = "Coauthors per Publication") + 
    theme_test()
```

<img src="https://i.imgur.com/0ZHMXzh.png" width="960" />

### Co-authors network

``` r
# Function to get pairs of co authors
get_pairs <- function(x) {
  if (length(x) >= 2) {
    combn(x, m = 2) 
  } else { 
      NA_character_ }
}

# get all coauthor pairs and 
# convert to igraph object
cograph <- bib$AUTHOR %>% 
  map(., .f = get_pairs) %>% 
  do.call("cbind", .) %>% 
  t(.) %>% 
  data.frame(.) %>% 
  na.omit() %>% 
  mutate(., N = 1L) %>% 
  filter(., X1 %in% top_authors$value & X2 %in% top_authors$value) %>% 
  group_by(., X1, X2) %>% 
  summarize(., sum = sum(N)) %>% 
  graph_from_data_frame(., directed = FALSE)

cograph %>% 
  ggraph(., "igraph", algorithm = "nicely") + 
  geom_edge_link(aes(edge_width = log(sum)), colour = "gray") + 
  geom_node_text(aes(label = name), fontface = 1, size = 2) + 
  theme_void()
```

<img src="https://i.imgur.com/h4GvSnd.png" width="960" />

### Betweenness centrality (?)

``` r
betweenness(cograph) %>% 
  data.frame(betweenness = .) %>% 
  mutate(., authors = rownames(.)) %>% 
  arrange(., desc(betweenness)) %>% 
  slice(., 1:30) %>% 
  mutate(., authors = fct_reorder(authors, betweenness)) %>% 
  ggplot(., aes(x = authors, y = betweenness)) + 
    geom_bar(stat = "identity") + 
    labs(y = "Network Betweenness", x = "Author Name") + 
    coord_flip()
```

<img src="https://i.imgur.com/WafWcLi.png" width="960" />

## Publication Years

``` r
bib %>% 
  ggplot(., aes(x = YEAR)) + 
    geom_histogram(binwidth = 1, color = 'black') 
```

<img src="https://i.imgur.com/5S6JVxK.png" width="960" />

## Missing fields

``` r
missingness <- function(x) {
  prop <- sum(is.na(x) == TRUE) / length(x)
  return(prop)
} 

bib %>% 
  filter(., CATEGORY == "ARTICLE") %>% 
  select(., YEAR, VOLUME, TITLE, PAGES, NUMBER, 
            MONTH, JOURNAL, AUTHOR, ANNOTE) %>% 
  map(., .f = missingness) %>% 
  unlist(.) %>% 
  enframe(.) %>% 
  mutate(., name = fct_reorder(name, value)) %>% 
  ggplot(., aes(x = name, y = value)) +
    geom_bar(stat = "identity") + 
    ylab("Proportion Missing") + 
    ylim(c(0,1)) +
    xlab("") + 
    coord_flip()
```

<img src="https://i.imgur.com/tF2DoRh.png" width="960" />

``` r
unlink("cache", recursive = TRUE)
unlink("figure", recursive = TRUE)
```
