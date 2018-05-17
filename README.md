
## Load libraries

``` r
library(knitr)
library(RefManageR)
library(tidyverse)
library(here)
#library(bib2df)
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

bib <- ReadBib("papers.bib", check = FALSE)
```

## Load bib

``` r
bib <- suppressWarnings(ReadBib(here("papers.bib"), check = 'warn'))

data <- bib %>% 
  as.tibble(.) %>% 
  mutate(., year = as.numeric(year))
```

## Citation Types

``` r
counts <- xtabs(~bibtype, data = bib) %>% as.tibble

counts %>% 
  mutate(., bibtype = fct_reorder(bibtype, n)) %>% 
  ggplot(., aes(x = bibtype, y = n, label = n)) + 
    geom_bar(stat = 'identity', color = 'black', 
             fill = 'lightblue', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Citation Type") + 
    coord_flip() + 
    theme_test()
```

<img src="https://i.imgur.com/K9YvYdZ.png" width="960" />

## Journals

``` r
data %>% 
  group_by(., journal) %>% 
  summarize(., counts = n()) %>% 
  na.omit() %>% 
  mutate(., journal = fct_reorder(journal, counts)) %>% 
  arrange(., desc(counts)) %>% 
  slice(., 1:50) %>% 
  ggplot(., aes(x = journal, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'lightblue', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Journal") + 
    scale_x_discrete(expand = expand_scale(0.04))  + 
    coord_flip() + 
    theme_test()
```

<img src="https://i.imgur.com/AGBwG4o.png" width="960" />

## Authors

``` r
# Initialize list
authors <- list()

# For each element in list, get last name of author and store in 
# 'authors' list
for (i in 1:length(bib)) {
  authors[[i]] <- bib[i]$author$family %>% unlist(.)
}

# Convert to tibble and plot
authors %>% 
  unlist(.) %>% 
  as.tibble(.) %>% 
  group_by(., value) %>% 
  summarize(., counts = n()) %>% 
  arrange(., desc(counts)) %>% 
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

<img src="https://i.imgur.com/krPI057.png" width="960" />

## Co-authors

``` r
data$nauthors <- lengths(data$author)
ggplot(dat[!is.na(dat$YEAR) & dat$YEAR > 1900, ], aes(x = YEAR, y = nauthors)) + 
  geom_point() + 
  geom_smooth(method = "gam") + 
  xlab("Publication Year") + 
  ylab("Coauthors per Publication")
```

## Publication Years

``` r
data %>% 
  ggplot(., aes(x = year)) + 
    geom_histogram(binwidth = 1, color = 'black') 
```

<img src="https://i.imgur.com/Y3JntcQ.png" width="960" />

``` r
unlink("cache", recursive = TRUE)
unlink("figure", recursive = TRUE)
```
