
## Load libraries

``` r
library(knitr)
library(RefManageR)
library(tidyverse)
library(here)
library(bib2df)
library(igraph)
library(ggraph)
theme_set(theme_minimal(base_family = 'Times', base_size = 16))
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
xtabs(~CATEGORY, data = bib) %>% 
  as.tibble(.) %>% 
  mutate(., CATEGORY = fct_reorder(CATEGORY, n)) %>% 
  ggplot(., aes(x = CATEGORY, y = n, label = n)) + 
    geom_bar(stat = 'identity', color = 'black', 
             fill = 'darkgrey', width = 0.3) + 
    geom_point(pch = 21, size = 12, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Citation Type") + 
    coord_flip() 
```

<img src="https://i.imgur.com/9d4wkXa.png" width="960" />

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
             fill = 'grey50', width = 0.5) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Journal") + 
    scale_x_discrete(expand = expand_scale(0.04))  + 
    coord_flip() 
```

<img src="https://i.imgur.com/wZdb8ws.png" width="960" />

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
             fill = 'darkgrey', width = 0.3) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Author") + 
    scale_x_discrete(expand = expand_scale(0.04)) + 
    coord_flip()
```

<img src="https://i.imgur.com/ql6tjL7.png" width="960" />

## Co-authors

### Co-authorship over time

``` r
bib %>% 
  mutate(., n_authors = lengths(AUTHOR)) %>% 
  filter(., YEAR >= 1950) %>% 
  ggplot(., aes(x = YEAR, y = n_authors)) + 
    geom_jitter(height = 0.2, alpha = 0.5, pch = 20) + 
    geom_smooth(method = "glm", method.args = list(family = "poisson")) + 
    labs(x = "Publication Year", y = "Coauthors per Publication")
```

<img src="https://i.imgur.com/Vxq51rT.png" width="960" />

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
  geom_node_text(aes(label = name), fontface = 1, size = 3.5) + 
  theme_void()
```

<img src="https://i.imgur.com/G0t0erj.png" width="960" />

### Betweenness centrality

``` r
betweenness(cograph) %>% 
  data.frame(betweenness = .) %>% 
  mutate(., authors = rownames(.)) %>% 
  arrange(., desc(betweenness)) %>% 
  slice(., 1:30) %>% 
  mutate(., authors = fct_reorder(authors, betweenness)) %>% 
  ggplot(., aes(x = authors, y = betweenness, label = round(betweenness))) + 
    geom_bar(stat = "identity") + 
    geom_point(pch = 21, size = 11, fill = 'grey70') + 
    geom_text(color = 'white') + 
    labs(y = "Network Betweenness", x = "Author Name") + 
    scale_x_discrete(expand = expand_scale(0.04)) + 
    coord_flip()
```

<img src="https://i.imgur.com/2Kw0D5z.png" width="960" />

## Publication Years

``` r
bib %>% 
  ggplot(., aes(x = YEAR)) + 
    geom_histogram(binwidth = 1, color = 'black', fill = 'grey60') + 
    scale_x_continuous(breaks = seq(1900, 2020, 10))
```

<img src="https://i.imgur.com/sTa0Mkh.png" width="960" />

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
  ggplot(., aes(x = name, y = value, label = round(value, 2))) +
    geom_bar(stat = "identity", color = 'black', fill = 'grey30', width = 0.5) + 
    geom_point(pch = 21, fill = 'grey60', size = 15) + 
    geom_text(color = 'white') + 
    ylim(c(0, 1)) + 
    scale_x_discrete(expand = expand_scale(0.1)) + 
    labs(y = "Proportion Missing", x = "Field") + 
    coord_flip()
```

<img src="https://i.imgur.com/00ppWJv.png" width="960" />

## List of incomplete articles

``` r
# Function to get name of missing field where
# for each bib entry
get_cols <- function(bib) {
  cols <- simplify2array(apply(bib, 1, function(x) paste(names(bib)[is.na(x)], collapse = ", ")))
  return(cols)
}

# Create table of bibcite key and missing field(s)
bib %>% 
  filter(., CATEGORY == "ARTICLE") %>% 
  select(., BIBTEXKEY, CATEGORY, YEAR, TITLE, PAGES, JOURNAL, AUTHOR) %>% 
  filter_all(., any_vars(is.na(.))) %>% 
  mutate(., n = row_number(), what = get_cols(.)) %>% 
  select(., n, BIBTEXKEY, what) %>% 
  kable(., format = 'html') %>% 
  kableExtra::kable_styling(bootstrap_options = 'basic', position = 'center', 
                            font_size = 10)
```

<table class="table" style="font-size: 10px; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:right;">

n

</th>

<th style="text-align:left;">

BIBTEXKEY

</th>

<th style="text-align:left;">

what

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Colina:2008vb

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Zahn:2008um

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

Hualde:1989tt

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

4

</td>

<td style="text-align:left;">

Sundara:2008ys

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:left;">

Bosque:2011vt

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:left;">

Colina:2010uz

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

7

</td>

<td style="text-align:left;">

Schertz:2013dfa

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

8

</td>

<td style="text-align:left;">

Dunbar:1993ws

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:left;">

Hickok:2003ui

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

10

</td>

<td style="text-align:left;">

MacKay:1987vk

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

11

</td>

<td style="text-align:left;">

Lakens:2017ts

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

12

</td>

<td style="text-align:left;">

Anonymous:-7kf7cVY

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

13

</td>

<td style="text-align:left;">

Michnowicz:2007uz

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

14

</td>

<td style="text-align:left;">

ringen2012voicing

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

15

</td>

<td style="text-align:left;">

Vogel:2006tv

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

16

</td>

<td style="text-align:left;">

Akustyk

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

17

</td>

<td style="text-align:left;">

Casillas:2009vn

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

18

</td>

<td style="text-align:left;">

Correa:2008p87

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

19

</td>

<td style="text-align:left;">

Healy:2011wf

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

20

</td>

<td style="text-align:left;">

Teetor:2011wm

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:left;">

Caramazza:1973gk

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

22

</td>

<td style="text-align:left;">

Peng:2015tu

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

23

</td>

<td style="text-align:left;">

Anonymous:TQAAGnIB

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

24

</td>

<td style="text-align:left;">

Pakin:2009wp

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

25

</td>

<td style="text-align:left;">

vanLeussen:2011ur

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

26

</td>

<td style="text-align:left;">

myers2005vowel

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

27

</td>

<td style="text-align:left;">

Colina:2010tja

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

28

</td>

<td style="text-align:left;">

Bakovic:2007un

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

29

</td>

<td style="text-align:left;">

Colina:2008ve

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

30

</td>

<td style="text-align:left;">

Olarrea:2011tj

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

31

</td>

<td style="text-align:left;">

Hanssen:2012un

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

32

</td>

<td style="text-align:left;">

Drager:2010te

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

33

</td>

<td style="text-align:left;">

anderson2008static

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

34

</td>

<td style="text-align:left;">

Colina:2010uj

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

35

</td>

<td style="text-align:left;">

Bullock:2008us

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

36

</td>

<td style="text-align:left;">

Colina:2010vu

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

37

</td>

<td style="text-align:left;">

Alonso:1945vn

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

38

</td>

<td style="text-align:left;">

Harris:1984wx

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

39

</td>

<td style="text-align:left;">

Anonymous:8c7\_DpUJ

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

40

</td>

<td style="text-align:left;">

Pallier:2000ta

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

41

</td>

<td style="text-align:left;">

McKee:1983vo

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

42

</td>

<td style="text-align:left;">

Jaeger:ed

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

43

</td>

<td style="text-align:left;">

Llisterri:2011tw

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

44

</td>

<td style="text-align:left;">

leyden2006prosody

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

45

</td>

<td style="text-align:left;">

myers2003f0

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

46

</td>

<td style="text-align:left;">

Shively:2008wa

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

47

</td>

<td style="text-align:left;">

Pallier:2012uy

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

48

</td>

<td style="text-align:left;">

RCoreTeam2012

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

49

</td>

<td style="text-align:left;">

Anonymous:2014cg

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

Ricci:2005vf

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

51

</td>

<td style="text-align:left;">

Dunbar:1998us

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

52

</td>

<td style="text-align:left;">

Anonymous:DZnMd13t

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

53

</td>

<td style="text-align:left;">

Harris:1991wm

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

54

</td>

<td style="text-align:left;">

Anonymous:kaB9ANNg

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

55

</td>

<td style="text-align:left;">

Achugar:2008wb

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

56

</td>

<td style="text-align:left;">

Lacabex:ty

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

57

</td>

<td style="text-align:left;">

Colina:2006uf

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

58

</td>

<td style="text-align:left;">

Gray:2007tx

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

59

</td>

<td style="text-align:left;">

katz2012compression

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

60

</td>

<td style="text-align:left;">

Kohler:2002tu

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

61

</td>

<td style="text-align:left;">

Colina:2010tx

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

62

</td>

<td style="text-align:left;">

Colina:2010tjb

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

63

</td>

<td style="text-align:left;">

Face:2009vi

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

64

</td>

<td style="text-align:left;">

Lynch:2001vq

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

65

</td>

<td style="text-align:left;">

van2002relationship

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

66

</td>

<td style="text-align:left;">

PascalvanLieshout:2003ws

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

67

</td>

<td style="text-align:left;">

Peterson1952

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

68

</td>

<td style="text-align:left;">

Morrison:2005tt

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

69

</td>

<td style="text-align:left;">

Kinginger:2009tc

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

70

</td>

<td style="text-align:left;">

Faraway:2011to

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

71

</td>

<td style="text-align:left;">

Anonymous:MVThQNOE

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

72

</td>

<td style="text-align:left;">

Torreira:2011jo

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

73

</td>

<td style="text-align:left;">

Moyer:2008tz

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

74

</td>

<td style="text-align:left;">

Botello:2010p108

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

Tremblay:2006vb

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

76

</td>

<td style="text-align:left;">

McKee:1983vb

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

77

</td>

<td style="text-align:left;">

Figueredo:2013vl

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

78

</td>

<td style="text-align:left;">

Barrios:IFybV93w

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

79

</td>

<td style="text-align:left;">

Lotto:2004wt

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

80

</td>

<td style="text-align:left;">

Murphy:2003tv

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

81

</td>

<td style="text-align:left;">

Kawahara:2011uo

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

82

</td>

<td style="text-align:left;">

Colina:2008tp

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

83

</td>

<td style="text-align:left;">

Sternefeld:2009ua

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

84

</td>

<td style="text-align:left;">

Pease:2011wo

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

85

</td>

<td style="text-align:left;">

ordonez2000clausal

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

86

</td>

<td style="text-align:left;">

Hutchins:2013ct

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

87

</td>

<td style="text-align:left;">

Harris:1982vd

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

88

</td>

<td style="text-align:left;">

Colina:2010ws

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

89

</td>

<td style="text-align:left;">

Casillas:2010we

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

90

</td>

<td style="text-align:left;">

Torreira:2005us

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

91

</td>

<td style="text-align:left;">

Soskuthy:2017ui

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

92

</td>

<td style="text-align:left;">

Boersma:2003wg

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

93

</td>

<td style="text-align:left;">

Williams:2014tc

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

94

</td>

<td style="text-align:left;">

Morrison:2008wy

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

95

</td>

<td style="text-align:left;">

Crowhurst:1992ve

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

96

</td>

<td style="text-align:left;">

Maclagan:2007vp

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

97

</td>

<td style="text-align:left;">

Anonymous:yGUo8Lyr

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

98

</td>

<td style="text-align:left;">

parrell2010b

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

99

</td>

<td style="text-align:left;">

Gillespie:2011ug

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

100

</td>

<td style="text-align:left;">

Siskind:2009vr

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

101

</td>

<td style="text-align:left;">

Hay:2006wp

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

102

</td>

<td style="text-align:left;">

Anonymous:TQdisolA

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

103

</td>

<td style="text-align:left;">

Silva:2014ts

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

104

</td>

<td style="text-align:left;">

Leisch:2008wa

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

105

</td>

<td style="text-align:left;">

Cross:2001ur

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

106

</td>

<td style="text-align:left;">

Ernestus:2011vb

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

107

</td>

<td style="text-align:left;">

Sanders:2012wz

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

108

</td>

<td style="text-align:left;">

hualde2008lexical

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

109

</td>

<td style="text-align:left;">

bataller2010making

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

110

</td>

<td style="text-align:left;">

Correa:vo

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

111

</td>

<td style="text-align:left;">

Anonymous:mgnEjvuA

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

112

</td>

<td style="text-align:left;">

Colina:2010tj

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

113

</td>

<td style="text-align:left;">

Lee:2011cg

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

114

</td>

<td style="text-align:left;">

Meijer:1994tm

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

115

</td>

<td style="text-align:left;">

beckman2011rate

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

116

</td>

<td style="text-align:left;">

Winter:2011uz

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

117

</td>

<td style="text-align:left;">

Simonet:2010wg

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

118

</td>

<td style="text-align:left;">

Manning:2007vi

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

119

</td>

<td style="text-align:left;">

Baker:2010ui

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

120

</td>

<td style="text-align:left;">

mehotcheva2010after

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

121

</td>

<td style="text-align:left;">

Jaeger:2004vz

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

122

</td>

<td style="text-align:left;">

Fukui:2004vq

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

123

</td>

<td style="text-align:left;">

Magloire:2000ud

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

124

</td>

<td style="text-align:left;">

Merker:2000uv

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

125

</td>

<td style="text-align:left;">

Harris:1992tr

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

126

</td>

<td style="text-align:left;">

Prieto:2007tw

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

127

</td>

<td style="text-align:left;">

OrtegaLlebaria:2001wl

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

128

</td>

<td style="text-align:left;">

Colina:2008wl

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

129

</td>

<td style="text-align:left;">

Hattori:2010vv

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

130

</td>

<td style="text-align:left;">

Locklin:2008vn

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

131

</td>

<td style="text-align:left;">

Schmidt:2006tp

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

132

</td>

<td style="text-align:left;">

sundara2005acoustic

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

133

</td>

<td style="text-align:left;">

Colina:2008vv

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

134

</td>

<td style="text-align:left;">

Giannakopoulou:2013ef

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

135

</td>

<td style="text-align:left;">

Gorman:2009vz

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

136

</td>

<td style="text-align:left;">

Nadeu:2012ht

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

137

</td>

<td style="text-align:left;">

Hualde:1989ve

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

138

</td>

<td style="text-align:left;">

Lipski:1990p201

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

139

</td>

<td style="text-align:left;">

McMurray:2010ep

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

140

</td>

<td style="text-align:left;">

Amengual

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

141

</td>

<td style="text-align:left;">

mueller2011learners

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

142

</td>

<td style="text-align:left;">

Llisterri:2012ut

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

143

</td>

<td style="text-align:left;">

Laks:2012vx

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

144

</td>

<td style="text-align:left;">

Wiley:2008wb

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

145

</td>

<td style="text-align:left;">

Oetiker:2011vt

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

146

</td>

<td style="text-align:left;">

bajuniemi2013teaching

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

147

</td>

<td style="text-align:left;">

Colina:2008uw

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

148

</td>

<td style="text-align:left;">

Hirschorn:2011tm

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

149

</td>

<td style="text-align:left;">

Moore:2011vd

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

150

</td>

<td style="text-align:left;">

Escudero:2007wn

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

151

</td>

<td style="text-align:left;">

Colina:2010uf

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

152

</td>

<td style="text-align:left;">

Park:2013gr

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

153

</td>

<td style="text-align:left;">

Hualde:1997tq

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

154

</td>

<td style="text-align:left;">

Ernestus:2010vt

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

155

</td>

<td style="text-align:left;">

Colina:2008ud

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

156

</td>

<td style="text-align:left;">

praat

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

157

</td>

<td style="text-align:left;">

Anonymous:2005vf

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

158

</td>

<td style="text-align:left;">

escudero2000developmental

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

159

</td>

<td style="text-align:left;">

Burgaleta:2013cr

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

160

</td>

<td style="text-align:left;">

Simonet:2014uy

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

161

</td>

<td style="text-align:left;">

Gulian:2007vc

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

162

</td>

<td style="text-align:left;">

diaz2012individual

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

163

</td>

<td style="text-align:left;">

Bereznak:1995vn

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

164

</td>

<td style="text-align:left;">

Recasens:2010vc

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

165

</td>

<td style="text-align:left;">

Kartushina:2014cg

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

166

</td>

<td style="text-align:left;">

simonet2011intonational

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

167

</td>

<td style="text-align:left;">

Morrison:2007ur

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

168

</td>

<td style="text-align:left;">

Piske:2002to

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

169

</td>

<td style="text-align:left;">

Lawrence:2012ud

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

170

</td>

<td style="text-align:left;">

Anonymous:CAHnH-l3

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

171

</td>

<td style="text-align:left;">

LoureiroRodriguez:2008ua

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

172

</td>

<td style="text-align:left;">

Guion:1998wq

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

173

</td>

<td style="text-align:left;">

Woolard:1997uj

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

174

</td>

<td style="text-align:left;">

Anonymous:mCkzXHwO

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

175

</td>

<td style="text-align:left;">

Montaruli:2011ue

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

176

</td>

<td style="text-align:left;">

Ernestus:2011ky

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

177

</td>

<td style="text-align:left;">

Morrison:2006wn

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

178

</td>

<td style="text-align:left;">

Ramachandra:2009tx

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

179

</td>

<td style="text-align:left;">

Hualde:1989vr

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

180

</td>

<td style="text-align:left;">

Wood:2009ux

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

181

</td>

<td style="text-align:left;">

Munro1996

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

182

</td>

<td style="text-align:left;">

Cooper:2017ve

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

183

</td>

<td style="text-align:left;">

Colina:2006ug

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

184

</td>

<td style="text-align:left;">

Anonymous:QobCWDq7

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

185

</td>

<td style="text-align:left;">

gordon2001memory

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

186

</td>

<td style="text-align:left;">

Colina:2008vz

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

187

</td>

<td style="text-align:left;">

Colina:2010tp

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

188

</td>

<td style="text-align:left;">

Cumming:2011ua

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

189

</td>

<td style="text-align:left;">

Flege1999b

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

190

</td>

<td style="text-align:left;">

Recasens:2009vc

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

191

</td>

<td style="text-align:left;">

Ernestus:2012vr

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

192

</td>

<td style="text-align:left;">

Colina:2010ud

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

193

</td>

<td style="text-align:left;">

Nadeu:2011vx

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

194

</td>

<td style="text-align:left;">

Beaudrie:2007ti

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

195

</td>

<td style="text-align:left;">

Colina:2007vp

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

196

</td>

<td style="text-align:left;">

simonet2011vx

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

197

</td>

<td style="text-align:left;">

TorresReyna:2011wf

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

198

</td>

<td style="text-align:left;">

simonet2011

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

199

</td>

<td style="text-align:left;">

Harris:1980ua

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

200

</td>

<td style="text-align:left;">

Mackain1981

</td>

<td style="text-align:left;">

PAGES, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

201

</td>

<td style="text-align:left;">

Colina:1997uu

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

202

</td>

<td style="text-align:left;">

Bickerton:1996vf

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

203

</td>

<td style="text-align:left;">

MartinezGil:1997vt

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

204

</td>

<td style="text-align:left;">

Piske2001

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

205

</td>

<td style="text-align:left;">

chladkova2011context

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

206

</td>

<td style="text-align:left;">

escudero2009cross

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

207

</td>

<td style="text-align:left;">

Tatman:2013um

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

</tbody>

</table>

``` r
unlink("cache", recursive = TRUE)
unlink("figure", recursive = TRUE)
```
