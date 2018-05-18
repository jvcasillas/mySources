
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
  select(., BIBTEXKEY, YEAR, VOLUME, TITLE, PAGES, NUMBER, 
            MONTH, JOURNAL, AUTHOR) %>%   
  filter_all(., any_vars(is.na(.))) %>% 
  mutate(., n = row_number(), what = get_cols(.)) %>% 
  select(., n, BIBTEXKEY, TITLE, what) %>% 
  kable(., format = 'html') %>% 
  kableExtra::kable_styling(bootstrap_options = 'striped', position = 'center', 
                            full_width = TRUE, font_size = 10)
```

<table class="table table-striped" style="font-size: 10px; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:right;">

n

</th>

<th style="text-align:left;">

BIBTEXKEY

</th>

<th style="text-align:left;">

TITLE

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

Sebastian-Galles:2009qf

</td>

<td style="text-align:left;">

Developmental shift in the discrimination of vowel contrasts in
bilingual infants: is the distributional account all there is to it?

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

broersma2010perception

</td>

<td style="text-align:left;">

Perception of final fricative voicing: Native and nonnative listeners’
use of vowel duration

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

Cutler:2009mz

</td>

<td style="text-align:left;">

Greater sensitivity to prosodic goodness in non-native than in native
listeners

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

4

</td>

<td style="text-align:left;">

barron2006learning

</td>

<td style="text-align:left;">

Learning to say “you” in German: The acquisition of sociolinguistic
competence in a study abroad context

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:left;">

Sumner2011

</td>

<td style="text-align:left;">

The role of variation in the perception of accented speech.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:left;">

Diehl:1989ej

</td>

<td style="text-align:left;">

On the Objects of Speech Perception

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

7

</td>

<td style="text-align:left;">

abrahamsson2012age

</td>

<td style="text-align:left;">

Age of onset and nativelike L2 ultimate attainment of morphosyntactic
and phonetic intuition

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

8

</td>

<td style="text-align:left;">

Au:2008bk

</td>

<td style="text-align:left;">

Salvaging a childhood language

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:left;">

mcgurk1976

</td>

<td style="text-align:left;">

Hearing Lips and Seeing Voices

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

10

</td>

<td style="text-align:left;">

Sundara:2006iq

</td>

<td style="text-align:left;">

Language-experience facilitates discrimination of /d-{}/ in monolingual
and bilingual acquisition of English

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

11

</td>

<td style="text-align:left;">

Colina:2008vb

</td>

<td style="text-align:left;">

Clases naturales Ej.

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

12

</td>

<td style="text-align:left;">

Riggs:1949ui

</td>

<td style="text-align:left;">

Alternate Phonemic Analyses of Comanche

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

13

</td>

<td style="text-align:left;">

Zahn:2008um

</td>

<td style="text-align:left;">

Learning to Sweave in APA style

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

14

</td>

<td style="text-align:left;">

Campbell-Kibler2007

</td>

<td style="text-align:left;">

Accent, (Ing), and the Social Logic of Listener Perceptions

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

15

</td>

<td style="text-align:left;">

Best:2003wq

</td>

<td style="text-align:left;">

Cross-language perception of nonnative vowels: Phonological and phonetic
effects of listeners{} native languages

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

16

</td>

<td style="text-align:left;">

Munro:1993tf

</td>

<td style="text-align:left;">

Productions of English vowels by native speakers of Arabic: acoustic
measurements and accentedness ratings.

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

17

</td>

<td style="text-align:left;">

Nishi:2008rt

</td>

<td style="text-align:left;">

Acoustic and perceptual similarity of Japanese and American English
vowels

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

18

</td>

<td style="text-align:left;">

Hualde:1989tt

</td>

<td style="text-align:left;">

Procesos conson{'a}nticos y estructuras geom{'e}tricas en espa{~n}ol

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

19

</td>

<td style="text-align:left;">

Sundara:2008ys

</td>

<td style="text-align:left;">

Development of coronal stop perception: Bilingual infants keep pace with
their monolingual peers

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

20

</td>

<td style="text-align:left;">

Bosque:2011vt

</td>

<td style="text-align:left;">

La negaci{'o}n y el principio de las categor{'}as vac{'}as

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:left;">

Colina:2010uz

</td>

<td style="text-align:left;">

Repaso sistema fonol{'o}gico

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

22

</td>

<td style="text-align:left;">

Watkins:2003cz

</td>

<td style="text-align:left;">

Seeing and hearing speech excites the motor system involved in speech
production

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

23

</td>

<td style="text-align:left;">

freed2004context

</td>

<td style="text-align:left;">

Context of learning and second language fluency in French: Comparing
regular classroom, study abroad, and intensive domestic immersion
programs

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

24

</td>

<td style="text-align:left;">

Schertz:2013dfa

</td>

<td style="text-align:left;">

Exaggeration of featural contrasts in clarifications of misheard speech
in English

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

Rauber:2005uo

</td>

<td style="text-align:left;">

The interrelation between the perception and production of English
vowels by native speakers of Brazilian Portuguese

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

26

</td>

<td style="text-align:left;">

Canonge:1957uo

</td>

<td style="text-align:left;">

Voiceless Vowels in Comanche

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

27

</td>

<td style="text-align:left;">

isabelli2006study

</td>

<td style="text-align:left;">

Study abroad social networks, motivation and attitudes: Implications for
second language acquisition

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

28

</td>

<td style="text-align:left;">

Best:2001di

</td>

<td style="text-align:left;">

Discrimination of non-native consonant contrasts varying in perceptual
assimilation to the listener{}s native phonological system

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

29

</td>

<td style="text-align:left;">

Dunbar:1993ws

</td>

<td style="text-align:left;">

Coevolution of neocortex size, group size and language in humans

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

30

</td>

<td style="text-align:left;">

Piske:2002fy

</td>

<td style="text-align:left;">

The production of English vowels by fluent early and late
Italian-English bilinguals

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

31

</td>

<td style="text-align:left;">

Nooteboom:1980cd

</td>

<td style="text-align:left;">

Production and perception of vowel length in spoken sentences

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

32

</td>

<td style="text-align:left;">

Hickok:2003ui

</td>

<td style="text-align:left;">

Auditory-motor interaction revealed by fMRI: speech, music, and working
memory in area Spt

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

33

</td>

<td style="text-align:left;">

Rivera-Mills:2009ys

</td>

<td style="text-align:left;">

Latinos or Hispanics? Changing demographics, implications, and continued
diversity

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

34

</td>

<td style="text-align:left;">

segalowitz2004context

</td>

<td style="text-align:left;">

Context, contact, and cognition in oral fluency acquisition: Learning
Spanish in at home and study abroad contexts

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

35

</td>

<td style="text-align:left;">

MacKay:1987vk

</td>

<td style="text-align:left;">

Asynlmetries in the Relationsh ip Between Speech Perception and
Production

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

36

</td>

<td style="text-align:left;">

Okane:1950p123

</td>

<td style="text-align:left;">

On the Names of the Refr{'a}n

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

37

</td>

<td style="text-align:left;">

ruiz2009reorienting

</td>

<td style="text-align:left;">

Reorienting Language-as-Resource

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

38

</td>

<td style="text-align:left;">

Flege1997

</td>

<td style="text-align:left;">

Effects of experience on non-native speakers’ production and perception
of English vowels

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

39

</td>

<td style="text-align:left;">

Samuel2011

</td>

<td style="text-align:left;">

Speech perception.

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

40

</td>

<td style="text-align:left;">

Larsson:2008yq

</td>

<td style="text-align:left;">

Lexical plasticity in early bilinguals does not alter phoneme
categories: I. neurodynamical modeling

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

41

</td>

<td style="text-align:left;">

Lakens:2017ts

</td>

<td style="text-align:left;">

Equivalence Testing for Psychological Research: A Tutorial

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

42

</td>

<td style="text-align:left;">

Anonymous:-7kf7cVY

</td>

<td style="text-align:left;">

L1-Spanish Speakers’ Acquisition of the English/i/{}/I/Contrast:
Duration-based Perception is Not the Initial Developmental Stage

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

43

</td>

<td style="text-align:left;">

Pallier:2001vx

</td>

<td style="text-align:left;">

The influence of native-language phonology on lexical access:
exemplar-Based versus abstract lexical entries.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

44

</td>

<td style="text-align:left;">

Face:2003fk

</td>

<td style="text-align:left;">

Intonation in spanish declaratives : differences between lab speech and
spontaneous speech.

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

45

</td>

<td style="text-align:left;">

Michnowicz:2007uz

</td>

<td style="text-align:left;">

El habla de Yucat{'a}m: Final \[m\] in a Dialect in Contact

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

46

</td>

<td style="text-align:left;">

ringen2012voicing

</td>

<td style="text-align:left;">

The voicing contrast in Fenno-Swedish stops

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

47

</td>

<td style="text-align:left;">

escudero2011perceptual

</td>

<td style="text-align:left;">

Perceptual assimilation of Dutch vowels by Peruvian Spanish listeners

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

48

</td>

<td style="text-align:left;">

Vogel:2006tv

</td>

<td style="text-align:left;">

xyling {} LATEX macros for linguistic graphics using the xypic module

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

49

</td>

<td style="text-align:left;">

Montrul:2010p426

</td>

<td style="text-align:left;">

How similar are adult second language learners and Spanish heritage
speakers? Spanish clitics and word order

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

broersma2008flexible

</td>

<td style="text-align:left;">

Flexible cue use in nonnative phonetic categorization

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

51

</td>

<td style="text-align:left;">

pierrehumbert1989categories

</td>

<td style="text-align:left;">

Categories of tonal alignment in English

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

52

</td>

<td style="text-align:left;">

Ganong1980

</td>

<td style="text-align:left;">

Phonetic categorization in auditory word perception.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

53

</td>

<td style="text-align:left;">

Akustyk

</td>

<td style="text-align:left;">

AKUSTYK for Praat

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

54

</td>

<td style="text-align:left;">

Casillas:2009vn

</td>

<td style="text-align:left;">

EL USO DE LOS REFRANES EN 

</td>

<td style="text-align:left;">

VOLUME, PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

55

</td>

<td style="text-align:left;">

Ingvalson:2011dx

</td>

<td style="text-align:left;">

Years of Exposure to English Predicts Perception and Production of /r/
and /l/ by Native Speakers of Japanese

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

56

</td>

<td style="text-align:left;">

Correa:2008p87

</td>

<td style="text-align:left;">

Metalinguistic knowledge and the acquisition of the Spanish subjunctive
by learners at three proficiency levels

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

57

</td>

<td style="text-align:left;">

Worlf:2010vb

</td>

<td style="text-align:left;">

El ensordecimiento del yeismo porte{~n}o, un cambio fonol{'o}gico en
marcha

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

58

</td>

<td style="text-align:left;">

Bradlow1996

</td>

<td style="text-align:left;">

A Perceptual Comparison of the /i/-/e/ and /u/-/o/ Contrasts in English
and in Spanish: Universal and Language-Specific Aspects

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

59

</td>

<td style="text-align:left;">

Brady:1978tf

</td>

<td style="text-align:left;">

A range effect in the perception of voicing

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

60

</td>

<td style="text-align:left;">

antoniou2013focusing

</td>

<td style="text-align:left;">

Focusing the lens of language experience: Perception of Ma’di stops by
Greek and English bilinguals and monolinguals

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

61

</td>

<td style="text-align:left;">

Healy:2011wf

</td>

<td style="text-align:left;">

Choosing Your Workflow Applications

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

62

</td>

<td style="text-align:left;">

Newman1997

</td>

<td style="text-align:left;">

Lexical neighborhood effects in phonetic processing.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

63

</td>

<td style="text-align:left;">

Teetor:2011wm

</td>

<td style="text-align:left;">

R~Cookbook

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

64

</td>

<td style="text-align:left;">

so2010cross

</td>

<td style="text-align:left;">

Cross-language perception of non-native tonal contrasts: Effects of
native phonological and phonetic influences

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

65

</td>

<td style="text-align:left;">

Escudero:2014gd

</td>

<td style="text-align:left;">

Magnitude of phonetic distinction predicts success at early word
learning in native and non-native accents.

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

66

</td>

<td style="text-align:left;">

Caramazza:1973gk

</td>

<td style="text-align:left;">

The acquisition of a new phonological contrast: The case of stop
consonants in French-English bilinguals

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

67

</td>

<td style="text-align:left;">

king1995did

</td>

<td style="text-align:left;">

Who did what and when? Using word-and clause-level ERPs to monitor
working memory usage in reading

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

68

</td>

<td style="text-align:left;">

Peng:2015tu

</td>

<td style="text-align:left;">

R Programming for Data Science

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

69

</td>

<td style="text-align:left;">

escudero2012native

</td>

<td style="text-align:left;">

Native dialect influences second-language vowel perception: Peruvian
versus Iberian Spanish learners of Dutch

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

70

</td>

<td style="text-align:left;">

Harris:1998p437

</td>

<td style="text-align:left;">

Spanish Imperatives: Syntax Meets Morphology

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

71

</td>

<td style="text-align:left;">

Anonymous:TQAAGnIB

</td>

<td style="text-align:left;">

Exploratory Data Analysis with R

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

72

</td>

<td style="text-align:left;">

Strange:2011gi

</td>

<td style="text-align:left;">

Automatic selective perception (ASP) of first and second language
speech: A working model

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

73

</td>

<td style="text-align:left;">

Gaies1991

</td>

<td style="text-align:left;">

The Matched-Guise Technique for Measuring Attitudes and Their
Implications for Language Education: A Critical Assessment

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

74

</td>

<td style="text-align:left;">

Pakin:2009wp

</td>

<td style="text-align:left;">

The Comprehensive LaTeX Symbol List

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

johnson2011infant

</td>

<td style="text-align:left;">

Infant ability to tell voices apart rests on language experience

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

76

</td>

<td style="text-align:left;">

bills2005comunidades

</td>

<td style="text-align:left;">

Las comunidades ling{"u}{'}sticas y el mantenimiento del espa{~n}ol en
Estados Unidos

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

77

</td>

<td style="text-align:left;">

Johnson:1993im

</td>

<td style="text-align:left;">

The Hyperspace Effect: Phonetic Targets Are Hyperarticulated

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

78

</td>

<td style="text-align:left;">

sunderman2009study

</td>

<td style="text-align:left;">

When study-abroad experience fails to deliver: The internal resources
threshold effect

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

79

</td>

<td style="text-align:left;">

vanLeussen:2011ur

</td>

<td style="text-align:left;">

Acoustic properties of Dutch steady-state vowels: Contextual effects and
a comparison with previous studies

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

80

</td>

<td style="text-align:left;">

Pallier2003

</td>

<td style="text-align:left;">

Brain imaging of language plasticity in adopted adults: can a second
language replace the first?

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

81

</td>

<td style="text-align:left;">

myers2005vowel

</td>

<td style="text-align:left;">

Vowel duration and neutralization of vowel length contrasts in
Kinyarwanda

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

82

</td>

<td style="text-align:left;">

gilichinskaya2010perceptual

</td>

<td style="text-align:left;">

Perceptual assimilation of American English vowels by inexperienced
Russian listeners

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

83

</td>

<td style="text-align:left;">

Best1993

</td>

<td style="text-align:left;">

Learning to Perceive the Sound Pattern of English

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

84

</td>

<td style="text-align:left;">

SchillingEstes:2002tc

</td>

<td style="text-align:left;">

Investigating Stylistic Variation

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

85

</td>

<td style="text-align:left;">

Harris:2001p434

</td>

<td style="text-align:left;">

Wisdom of the People: Potential and Pitfalls in Efforts by the Comanches
to Recreate Traditional Ways of Building Consensus

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

86

</td>

<td style="text-align:left;">

Colina:2010tja

</td>

<td style="text-align:left;">

Conceptos estructuralistas - terminlolog{'}a

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

87

</td>

<td style="text-align:left;">

Simonet:2014bo

</td>

<td style="text-align:left;">

Phonetic consequences of dynamic cross-linguistic interference in
proficient bilinguals

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

88

</td>

<td style="text-align:left;">

Mazzaro:2005wc

</td>

<td style="text-align:left;">

Speaking Spanish with Style:(s) Deletion in Argentine Spanish and
Labov{}s Decision Tree

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

89

</td>

<td style="text-align:left;">

Anonymous:iwkObd-4

</td>

<td style="text-align:left;">

The role of prosodic structure in the L2 acquisition of Spanish stop
lenition

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

90

</td>

<td style="text-align:left;">

Bakovic:2007un

</td>

<td style="text-align:left;">

Hiatus resolution and incomplete identity

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

91

</td>

<td style="text-align:left;">

Colina:2008ve

</td>

<td style="text-align:left;">

Rasgos distintivos lecture

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

92

</td>

<td style="text-align:left;">

Olarrea:2011tj

</td>

<td style="text-align:left;">

problema1

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

93

</td>

<td style="text-align:left;">

Hanssen:2012un

</td>

<td style="text-align:left;">

Installing fonts in LaTEX: a user{}s experience

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

94

</td>

<td style="text-align:left;">

Drager:2010te

</td>

<td style="text-align:left;">

A Novice{}s Guide to Understanding Mixed Effects Models

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

95

</td>

<td style="text-align:left;">

willis2005initial

</td>

<td style="text-align:left;">

An initial examination of Southwest Spanish vowels

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

96

</td>

<td style="text-align:left;">

JS1989

</td>

<td style="text-align:left;">

Critical period effects in second language learning: the influence of
maturational state on the acquisition of English as a second language.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

97

</td>

<td style="text-align:left;">

6653863720111001

</td>

<td style="text-align:left;">

Dynamical account of how /b, d, g/ differ from /p, t, k/ in Spanish:
Evidence from labials.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

98

</td>

<td style="text-align:left;">

Oh:2010hx

</td>

<td style="text-align:left;">

Early childhood language memory in the speech perception of
international adoptees

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

99

</td>

<td style="text-align:left;">

anderson2008static

</td>

<td style="text-align:left;">

Static palatography for language fieldwork

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

100

</td>

<td style="text-align:left;">

Colina:2010uj

</td>

<td style="text-align:left;">

Trancripci{'o}n de laterales - Ej

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

101

</td>

<td style="text-align:left;">

KrebsLazendic:2013ei

</td>

<td style="text-align:left;">

First language suprasegmentally-conditioned syllable length distinctions
influence perception and production of second language vowel contrasts

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

102

</td>

<td style="text-align:left;">

HancinBhatt:wi

</td>

<td style="text-align:left;">

Optimal L2 syllables: Interaction of transfer and developmental effects

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

103

</td>

<td style="text-align:left;">

Flege1999a

</td>

<td style="text-align:left;">

Native Italian speakers’ perception and production of English vowels.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

104

</td>

<td style="text-align:left;">

Bullock:2008us

</td>

<td style="text-align:left;">

Kreyol incursions into Dominican Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

105

</td>

<td style="text-align:left;">

Colina:2010vu

</td>

<td style="text-align:left;">

Neutralizaci{'o}n de constrastres - Lecture

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

106

</td>

<td style="text-align:left;">

Alonso:1945vn

</td>

<td style="text-align:left;">

Una ley fonol{'o}gica del espa{~n}ol

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

107

</td>

<td style="text-align:left;">

Harris:1984wx

</td>

<td style="text-align:left;">

La espirantizaci{'o}n en castellano y la representaci{'o}n fonol{'o}gica
autosegmental

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

108

</td>

<td style="text-align:left;">

Anonymous:8c7\_DpUJ

</td>

<td style="text-align:left;">

knitr: A General-Purpose Tool for Dynamic Report Generation in R

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

109

</td>

<td style="text-align:left;">

Chaiken1994

</td>

<td style="text-align:left;">

Heuristic processing can bias systematic processing: Effects of source
credibility, argument ambiguity, and task importance on attitude
judgment.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

110

</td>

<td style="text-align:left;">

Corral:2010p1789

</td>

<td style="text-align:left;">

La ‘ch’ fricativa en Granada: un sonido del habla masculina

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

111

</td>

<td style="text-align:left;">

Schwartz:1996iw

</td>

<td style="text-align:left;">

L2 cognitive states and the Full Transfer/Full Access model

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

112

</td>

<td style="text-align:left;">

Pallier:2000ta

</td>

<td style="text-align:left;">

Premiers pas en Emacs

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

113

</td>

<td style="text-align:left;">

McKee:1983vo

</td>

<td style="text-align:left;">

The Effects of Intensive Language Instruction on Student Performance in
Beginning College French. (Report no. FL013910)

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

114

</td>

<td style="text-align:left;">

Hovland1952

</td>

<td style="text-align:left;">

Source credibility and effective communication

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

115

</td>

<td style="text-align:left;">

Torreira:2014vw

</td>

<td style="text-align:left;">

Quasi-neutralization of stress contrasts in Spanish

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

116

</td>

<td style="text-align:left;">

Flege:2007tt

</td>

<td style="text-align:left;">

Language contact in bilingualism: phonetic system interactions

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

117

</td>

<td style="text-align:left;">

Jaeger:ed

</td>

<td style="text-align:left;">

Mixed effect models for genetic and areal dependencies in linguistic
typology

</td>

<td style="text-align:left;">

PAGES, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

118

</td>

<td style="text-align:left;">

Llisterri:2011tw

</td>

<td style="text-align:left;">

Fon{'e}tica y fonolog{'}a del espa{~n}ol

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

119

</td>

<td style="text-align:left;">

santa1999like

</td>

<td style="text-align:left;">

Like an animal I was treated: Anti-immigrant metaphor in US public
discourse

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

120

</td>

<td style="text-align:left;">

leyden2006prosody

</td>

<td style="text-align:left;">

On the prosody of Orkney and Shetland dialects

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

121

</td>

<td style="text-align:left;">

Elvin:2014br

</td>

<td style="text-align:left;">

Spanish is better than English for discriminating Portuguese vowels:
acoustic similarity versus vowel inventory size.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

122

</td>

<td style="text-align:left;">

myers2003f0

</td>

<td style="text-align:left;">

F0 timing in Kinyarwanda

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

123

</td>

<td style="text-align:left;">

Muhlhausler:1992p128

</td>

<td style="text-align:left;">

Preserving Languages or Language Ecologies? A Top-down Approach to
Language Survival

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

124

</td>

<td style="text-align:left;">

chen1970vowel

</td>

<td style="text-align:left;">

Vowel length variation as a function of the voicing of the consonant
environment

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

125

</td>

<td style="text-align:left;">

dupoux2008persistent

</td>

<td style="text-align:left;">

Persistent stress `deafness': The case of French learners of Spanish
</td> <td style="text-align:left;"> MONTH </td> </tr> <tr> <td
style="text-align:right;"> 126 </td> <td style="text-align:left;">
Rungruang:2014un </td> <td style="text-align:left;"> The Relationship
between the Perception and Production of English Coda Clusters by EFL
Thai Learners </td> <td style="text-align:left;"> MONTH </td> </tr> <tr>
<td style="text-align:right;"> 127 </td> <td style="text-align:left;">
knouse2012acquisition </td> <td style="text-align:left;"> The
Acquisition of Dialectal Phonemes in a Study Abroad Context: The Case of
the Castilian Theta </td> <td style="text-align:left;"> MONTH </td>
</tr> <tr> <td style="text-align:right;"> 128 </td> <td
style="text-align:left;"> Shively:2008wa </td> <td
style="text-align:left;"> Development of Spanish requests and apologies
during study abroad </td> <td style="text-align:left;"> VOLUME, PAGES,
NUMBER, MONTH </td> </tr> <tr> <td style="text-align:right;"> 129 </td>
<td style="text-align:left;"> zampini1998relationship </td> <td
style="text-align:left;"> The Relationship between the Production and
Perception of L2 Spanish Stops </td> <td style="text-align:left;"> MONTH
</td> </tr> <tr> <td style="text-align:right;"> 130 </td> <td
style="text-align:left;"> Lappin:2000tc </td> <td
style="text-align:left;"> The structure of unscientific revolutions
</td> <td style="text-align:left;"> MONTH </td> </tr> <tr> <td
style="text-align:right;"> 131 </td> <td style="text-align:left;">
sebastian2012neuroanatomical </td> <td style="text-align:left;">
Neuroanatomical markers of individual differences in native and
non-native vowel perception </td> <td style="text-align:left;"> MONTH
</td> </tr> <tr> <td style="text-align:right;"> 132 </td> <td
style="text-align:left;"> Sternthal1978 </td> <td
style="text-align:left;"> The Persuasive Effect of Source Credibility:
Tests of Cognitive Response </td> <td style="text-align:left;"> MONTH
</td> </tr> <tr> <td style="text-align:right;"> 133 </td> <td
style="text-align:left;"> LevAri:2014ii </td> <td
style="text-align:left;"> Comprehending non-native speakers: theory and
evidence for adjustment in manner of processing. </td> <td
style="text-align:left;"> NUMBER </td> </tr> <tr> <td
style="text-align:right;"> 134 </td> <td style="text-align:left;">
face2003effects </td> <td style="text-align:left;"> Effects of syntactic
constituency on the intonational marking of Spanish contrastive focus
</td> <td style="text-align:left;"> VOLUME, NUMBER, MONTH </td> </tr>
<tr> <td style="text-align:right;"> 135 </td> <td
style="text-align:left;"> Pisoni1974 </td> <td style="text-align:left;">
Reaction times to comparisons within and across phonetic categories
</td> <td style="text-align:left;"> MONTH </td> </tr> <tr> <td
style="text-align:right;"> 136 </td> <td style="text-align:left;">
Pallier:2012uy </td> <td style="text-align:left;"> TEX et BibTEX a` la
puissance Emacs

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

137

</td>

<td style="text-align:left;">

ingvalson2012can

</td>

<td style="text-align:left;">

Can native Japanese listeners learn to differentiate/r–l/on the basis of
F3 onset frequency?

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

138

</td>

<td style="text-align:left;">

RCoreTeam2012

</td>

<td style="text-align:left;">

R: A Language and Environment for Statistical Computing

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

139

</td>

<td style="text-align:left;">

Bosch2003

</td>

<td style="text-align:left;">

Simultaneous bilingualism and the perception of a language-specific
vowel contrast in the first year of life

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

140

</td>

<td style="text-align:left;">

Hillenbrand1995

</td>

<td style="text-align:left;">

Acoustic characteristics of American English vowels

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

141

</td>

<td style="text-align:left;">

escudero2011cross

</td>

<td style="text-align:left;">

Cross-language acoustic similarity predicts perceptual assimilation of
Canadian English and Canadian French vowels

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

142

</td>

<td style="text-align:left;">

Samuel2009

</td>

<td style="text-align:left;">

Perceptual learning for speech

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

143

</td>

<td style="text-align:left;">

Anonymous:2014cg

</td>

<td style="text-align:left;">

Is the low-cost EyeTribe eye tracker any good for research?

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

144

</td>

<td style="text-align:left;">

Ricci:2005vf

</td>

<td style="text-align:left;">

R FUNCTIONS FOR REGRESSION ANALYSIS

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

145

</td>

<td style="text-align:left;">

Dunbar:1998us

</td>

<td style="text-align:left;">

Theory of mind and the evolution of language

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

146

</td>

<td style="text-align:left;">

Warren1970

</td>

<td style="text-align:left;">

Perceptual Restoration of Missing Speech Sounds

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

147

</td>

<td style="text-align:left;">

Anonymous:DZnMd13t

</td>

<td style="text-align:left;">

The R Inferno

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

148

</td>

<td style="text-align:left;">

johnson2009voice

</td>

<td style="text-align:left;">

Do voice recordings reveal whether a person is intoxicated? A case study

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

149

</td>

<td style="text-align:left;">

broersma2008phantom

</td>

<td style="text-align:left;">

Phantom word activation in L2

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

150

</td>

<td style="text-align:left;">

Blasingame:2014gd

</td>

<td style="text-align:left;">

Switched-dominance bilingual speech production: Continuous usage versus
early exposure

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

151

</td>

<td style="text-align:left;">

winter\_2013lr

</td>

<td style="text-align:left;">

Linear models and linear mixed effects models in R with linguistic
applications

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

152

</td>

<td style="text-align:left;">

Flemming:2003vr

</td>

<td style="text-align:left;">

Speech perception in phonology

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

153

</td>

<td style="text-align:left;">

DAusillo:2009du

</td>

<td style="text-align:left;">

The Motor Somatotopy of Speech Perception

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

154

</td>

<td style="text-align:left;">

iverson2010cross

</td>

<td style="text-align:left;">

Cross-language effects for non-speech analogs: A comparison of English/
w/-/ v/ perception by native speakers of Hindi and English.

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

155

</td>

<td style="text-align:left;">

Harris:1991wm

</td>

<td style="text-align:left;">

The exponence of gender in Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

156

</td>

<td style="text-align:left;">

Eckman:2007to

</td>

<td style="text-align:left;">

Perception and Production in the Acquisition of L2 Phonemic Contrasts

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

157

</td>

<td style="text-align:left;">

face2002local

</td>

<td style="text-align:left;">

Local intonational marking of Spanish contrastive focus

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

158

</td>

<td style="text-align:left;">

suarez2002paradox

</td>

<td style="text-align:left;">

The paradox of linguistic hegemony and the maintenance of Spanish as a
heritage language in the United States

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

159

</td>

<td style="text-align:left;">

Anonymous:kaB9ANNg

</td>

<td style="text-align:left;">

Native and Non-Native Speech Perception

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

160

</td>

<td style="text-align:left;">

Rivers:1991p102

</td>

<td style="text-align:left;">

Sancho y la duquesa: Una nota socioliteraria

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

161

</td>

<td style="text-align:left;">

Achugar:2008wb

</td>

<td style="text-align:left;">

Counter-hegemonic language practices and ideologies Creating a new space
and value for Spanish in Southwest Texas

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

162

</td>

<td style="text-align:left;">

Williams1974

</td>

<td style="text-align:left;">

The Identification of Linguistic Attitudes

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

163

</td>

<td style="text-align:left;">

Rizzolatti:2004by

</td>

<td style="text-align:left;">

The mirror-neuron system

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

164

</td>

<td style="text-align:left;">

Fowler:2008fk

</td>

<td style="text-align:left;">

Cross language phonetic influences on the speech of French–English
bilinguals

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

165

</td>

<td style="text-align:left;">

Lacabex:ty

</td>

<td style="text-align:left;">

Relationship between perception and production in non-native speech

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

166

</td>

<td style="text-align:left;">

Colina:2006uf

</td>

<td style="text-align:left;">

Output-to-output Correspondence and the Emergence of the Unmarked in
Spanish Plural Formation

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

167

</td>

<td style="text-align:left;">

Gray:2007tx

</td>

<td style="text-align:left;">

Textmate Power Editing for the Mac (Pragmatic, 2007)

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

168

</td>

<td style="text-align:left;">

2006PNAS..103.7865P

</td>

<td style="text-align:left;">

Motor cortex maps articulatory features of speech sounds

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

169

</td>

<td style="text-align:left;">

Diehl2004

</td>

<td style="text-align:left;">

Speech perception

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

170

</td>

<td style="text-align:left;">

ocampo2003notion

</td>

<td style="text-align:left;">

On the notion of focus in spoken Spanish: An empirical approach

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

171

</td>

<td style="text-align:left;">

wright2007heritage

</td>

<td style="text-align:left;">

Heritage language programs in the era of English-only and No Child Left
Behind

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

172

</td>

<td style="text-align:left;">

Ionin:2008df

</td>

<td style="text-align:left;">

Sources of linguistic knowledge in the second language acquisition of
English articles

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

173

</td>

<td style="text-align:left;">

johnson2004acoustic

</td>

<td style="text-align:left;">

Acoustic and auditory phonetics

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

174

</td>

<td style="text-align:left;">

katz2012compression

</td>

<td style="text-align:left;">

Compression effects in English

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

175

</td>

<td style="text-align:left;">

Kohler:2002tu

</td>

<td style="text-align:left;">

Hearing sounds, understanding actions: action representation in mirror
neurons

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

176

</td>

<td style="text-align:left;">

Samuel2003

</td>

<td style="text-align:left;">

Memory and Language Lexical activation ( and other factors ) can mediate
compensation for coarticulation

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

177

</td>

<td style="text-align:left;">

Colina:2010tx

</td>

<td style="text-align:left;">

Inventario de los fonemas y alofonos del espanol estandar

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

178

</td>

<td style="text-align:left;">

Colina:2010tjb

</td>

<td style="text-align:left;">

SILABIFICACI{'O}N Y LA TEOR{'I}A DE LA OPTIMIDAD

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

179

</td>

<td style="text-align:left;">

diaz2004context

</td>

<td style="text-align:left;">

Context of learning in the acquisition of Spanish second language
phonology

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

180

</td>

<td style="text-align:left;">

Face:2009vi

</td>

<td style="text-align:left;">

Acquisition of the Spanish voiced spirants by second language learners

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

181

</td>

<td style="text-align:left;">

Agheyisi1970a

</td>

<td style="text-align:left;">

Language Attitude Studies: A Brief Survey of Methodological Approaches

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

182

</td>

<td style="text-align:left;">

tsukada2011perception

</td>

<td style="text-align:left;">

The perception of Arabic and Japanese short and long vowels by native
speakers of Arabic, Japanese, and Persian

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

183

</td>

<td style="text-align:left;">

Holt2008

</td>

<td style="text-align:left;">

Speech Perception Within an Auditory Cognitive Science Framework.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

184

</td>

<td style="text-align:left;">

Lynch:2001vq

</td>

<td style="text-align:left;">

Social factors and language proficiency in postsecondary Spanish
immersion: Issues and implications

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

185

</td>

<td style="text-align:left;">

Tsalikis1991

</td>

<td style="text-align:left;">

The Role of Accent on the Credibility and Effectiveness of the
Salesperson

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

186

</td>

<td style="text-align:left;">

van2002relationship

</td>

<td style="text-align:left;">

The relationship between vowel and consonant duration in Orkney and
Shetland dialects

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

187

</td>

<td style="text-align:left;">

PascalvanLieshout:2003ws

</td>

<td style="text-align:left;">

Short Tutorial

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

188

</td>

<td style="text-align:left;">

Peterson1952

</td>

<td style="text-align:left;">

Control Methods Used in a Study of the Vowels

</td>

<td style="text-align:left;">

MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

189

</td>

<td style="text-align:left;">

Morrison:2005tt

</td>

<td style="text-align:left;">

Towards a Quantitative Speech Learning Model (QSLM)

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

190

</td>

<td style="text-align:left;">

Osborn:1949vl

</td>

<td style="text-align:left;">

Formulae for Comanche stem and word formation

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

191

</td>

<td style="text-align:left;">

Kinginger:2009tc

</td>

<td style="text-align:left;">

Language learning and study abroad: A critical reading of research

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

192

</td>

<td style="text-align:left;">

Kusumoto:2012tg

</td>

<td style="text-align:left;">

Between perception and production: Is the ability to hear L1-L2 sound
differences related to the ability to pronounce the same sounds
accurately

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

193

</td>

<td style="text-align:left;">

Fowler:1986vb

</td>

<td style="text-align:left;">

An event approach to the study of speech perception from a
direct-realist perspective

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

194

</td>

<td style="text-align:left;">

Faraway:2011to

</td>

<td style="text-align:left;">

Using R

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

195

</td>

<td style="text-align:left;">

Boersma:1999wf

</td>

<td style="text-align:left;">

On the need for a separate perception grammar

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

196

</td>

<td style="text-align:left;">

Oh:vh

</td>

<td style="text-align:left;">

Holding on to childhood language memory

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

197

</td>

<td style="text-align:left;">

Tuinman:2011kx

</td>

<td style="text-align:left;">

Perception of intrusive /r/ in English by native, cross-language and
cross-dialect listeners

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

198

</td>

<td style="text-align:left;">

Anonymous:MVThQNOE

</td>

<td style="text-align:left;">

The Acquisition of Resyllabification in Spanish by English Speakers

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

199

</td>

<td style="text-align:left;">

Neary:tk1980

</td>

<td style="text-align:left;">

On the physical interpretation of vowel quality: cinefluorographic and
acoustic evidence

</td>

<td style="text-align:left;">

VOLUME, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

200

</td>

<td style="text-align:left;">

Pallier:2003gi

</td>

<td style="text-align:left;">

Brain imaging of language plasticity in adopted adults: can a second
language replace the first?

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

201

</td>

<td style="text-align:left;">

Torreira:2011jo

</td>

<td style="text-align:left;">

Realization of voiceless stops and vowels in conversational French and
Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

202

</td>

<td style="text-align:left;">

Lantolf:1994ds

</td>

<td style="text-align:left;">

Sociocultural Theory and Second Language Learning

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

203

</td>

<td style="text-align:left;">

SebastianGalles:2005vo

</td>

<td style="text-align:left;">

The influence of initial exposure on lexical representation: comparing
early and simultaneous bilinguals

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

204

</td>

<td style="text-align:left;">

grodner2005consequences

</td>

<td style="text-align:left;">

Consequences of the serial nature of linguistic input for sentenial
complexity

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

205

</td>

<td style="text-align:left;">

Meister:2007jw

</td>

<td style="text-align:left;">

The essential role of premotor cortex in speech perception

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

206

</td>

<td style="text-align:left;">

Moyer:2008tz

</td>

<td style="text-align:left;">

Research as Practice: Linking Theory, Method, and Data

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

207

</td>

<td style="text-align:left;">

Casagrande:1948vo

</td>

<td style="text-align:left;">

Comanche baby language

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

208

</td>

<td style="text-align:left;">

Eisend2006

</td>

<td style="text-align:left;">

Source Credibility Dimensions in Marketing Communication {} A
Generalized Solution

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

209

</td>

<td style="text-align:left;">

ANA:1999vc

</td>

<td style="text-align:left;">

textquoteleft}Like an animal I was treated{}: anti-immigrant metaphor in
US public discourse

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

210

</td>

<td style="text-align:left;">

Botello:2010p108

</td>

<td style="text-align:left;">

El refr{'a}n como texto oral y escrito

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

211

</td>

<td style="text-align:left;">

Tremblay:2006vb

</td>

<td style="text-align:left;">

Cross-Linguistic Influence in Third Language Acquisition: The Role of L2
Proficiency and L2 Exposure

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

212

</td>

<td style="text-align:left;">

Spolsky:2011qy

</td>

<td style="text-align:left;">

Does the United States need a language policy?

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

213

</td>

<td style="text-align:left;">

McKee:1983vb

</td>

<td style="text-align:left;">

The Effects of Intensive Language Instruction on Student Performance in
Beginning College French.

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

214

</td>

<td style="text-align:left;">

Figueredo:2013vl

</td>

<td style="text-align:left;">

Psych 507a

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

215

</td>

<td style="text-align:left;">

Flege1995

</td>

<td style="text-align:left;">

Factors affecting strength of perceived foreign accent in a second
language.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

216

</td>

<td style="text-align:left;">

Barrios:IFybV93w

</td>

<td style="text-align:left;">

Native Language Constraints on L2 Perception

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

217

</td>

<td style="text-align:left;">

hidalgo1995language

</td>

<td style="text-align:left;">

Language and ethnicity in the “taboo” region: The U.S.-Mexican border.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

218

</td>

<td style="text-align:left;">

Lotto:2004wt

</td>

<td style="text-align:left;">

Mapping the task for the second language learner: The case of Japanese
acquisition of /r/ and /l/

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

219

</td>

<td style="text-align:left;">

Jongman:2000gr

</td>

<td style="text-align:left;">

Acoustic characteristics of English fricatives

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

220

</td>

<td style="text-align:left;">

endress2010word

</td>

<td style="text-align:left;">

Word segmentation with universal prosodic cues

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

221

</td>

<td style="text-align:left;">

Liberman1967

</td>

<td style="text-align:left;">

Perception of the speech code.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

222

</td>

<td style="text-align:left;">

simonet2010dark

</td>

<td style="text-align:left;">

Dark and clear laterals in Catalan and Spanish: Interaction of phonetic
categories in early bilinguals

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

223

</td>

<td style="text-align:left;">

Murphy:2003tv

</td>

<td style="text-align:left;">

Second Language Transfer During Third Language Acquisition

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

224

</td>

<td style="text-align:left;">

Morrison2008

</td>

<td style="text-align:left;">

Perception of Synthetic Vowels by Monolingual Canadian-English,
Mexican-Spanish, and Peninsular-Spanish Listeners

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

225

</td>

<td style="text-align:left;">

McCroskey1999

</td>

<td style="text-align:left;">

Goodwill: A reexamination of the construct and its measurement

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

226

</td>

<td style="text-align:left;">

AkahaneYamada:1996kn

</td>

<td style="text-align:left;">

Does training in speech perception modify speech production?

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

227

</td>

<td style="text-align:left;">

Liberman:1954ft

</td>

<td style="text-align:left;">

The role of consonant-vowel transitions in the perception of the stop
and nasal consonants.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

228

</td>

<td style="text-align:left;">

Strange:2009lq

</td>

<td style="text-align:left;">

Cross-language categorization of French and German vowels by na{"}ve
American listeners

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

229

</td>

<td style="text-align:left;">

olarrea2012

</td>

<td style="text-align:left;">

Word Order and Information Structure

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

230

</td>

<td style="text-align:left;">

Kawahara:2011uo

</td>

<td style="text-align:left;">

Praat Scripting for dummies

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

231

</td>

<td style="text-align:left;">

Kondaurova2008

</td>

<td style="text-align:left;">

The relationship between native allophonic experience with vowel
duration and perception of the English tense/lax vowel contrast by
Spanish and Russian listeners

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

232

</td>

<td style="text-align:left;">

anderson2005generation

</td>

<td style="text-align:left;">

Generation and Spanish language use in the Lower Rio Grande Valley of
Texas

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

233

</td>

<td style="text-align:left;">

1989Sci…243..489L

</td>

<td style="text-align:left;">

A Specialization for Speech-Perception

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

234

</td>

<td style="text-align:left;">

Colina:2008tp

</td>

<td style="text-align:left;">

Acentuaci{'o}n

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

235

</td>

<td style="text-align:left;">

silva2004spanish

</td>

<td style="text-align:left;">

Spanish in the Southwest

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

236

</td>

<td style="text-align:left;">

Sternefeld:2009ua

</td>

<td style="text-align:left;">

linguex.sty Documentation

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

237

</td>

<td style="text-align:left;">

ryan1992acquisition

</td>

<td style="text-align:left;">

Acquisition of lexical meaning in a study abroad environment: Ser and
estar and the Granada experience

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

238

</td>

<td style="text-align:left;">

Ruiz:1984rt

</td>

<td style="text-align:left;">

Orientations in language planning

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

239

</td>

<td style="text-align:left;">

Ainsworth:1972ui

</td>

<td style="text-align:left;">

Duration as a cue in the recognition of synthetic vowels

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

240

</td>

<td style="text-align:left;">

Pease:2011wo

</td>

<td style="text-align:left;">

Tree Macros

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

241

</td>

<td style="text-align:left;">

Hosoda:2012jb

</td>

<td style="text-align:left;">

The effect of Hispanic accents on employment decisions

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

242

</td>

<td style="text-align:left;">

ordonez2000clausal

</td>

<td style="text-align:left;">

The clausal architecture of Spanish: a comparative study

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

243

</td>

<td style="text-align:left;">

Boersma:2001uj

</td>

<td style="text-align:left;">

Phonology-semantics interaction in OT, and its acquisition

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

244

</td>

<td style="text-align:left;">

Hutchins:2013ct

</td>

<td style="text-align:left;">

The Linked Dual Representation model of vocal perception and production

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

245

</td>

<td style="text-align:left;">

Correa:2004p85

</td>

<td style="text-align:left;">

Adult grammar awareness as an advantage for the acquisition of
additional languages

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

246

</td>

<td style="text-align:left;">

Harris:1982vd

</td>

<td style="text-align:left;">

Syllable structure and stress in Spanish: a nonlinear analysis

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

247

</td>

<td style="text-align:left;">

Colina:2010ws

</td>

<td style="text-align:left;">

Transcripci{'o}n de nasales - EJ

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

248

</td>

<td style="text-align:left;">

Sisinni:2014vb

</td>

<td style="text-align:left;">

The perception of American English vowels by Salento Italian adult
listeners: Longitudinal development in the classroom context

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

249

</td>

<td style="text-align:left;">

Casillas:2010we

</td>

<td style="text-align:left;">

La vibrante m{'u}ltiple intervoc{'a}lica. Los ejercicios de canto como
ayuda a su pronunciaci{'o}n en espa{~n}ol

</td>

<td style="text-align:left;">

VOLUME, PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

250

</td>

<td style="text-align:left;">

Torreira:2005us

</td>

<td style="text-align:left;">

Aspirated Stops in Andalusian Spanish

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

251

</td>

<td style="text-align:left;">

Soskuthy:2017ui

</td>

<td style="text-align:left;">

Generalised additive mixed models for dynamic analysis in linguistics: a
practical introduction

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

252

</td>

<td style="text-align:left;">

Boersma:2003wg

</td>

<td style="text-align:left;">

Learning abstract phonological from auditory phonetic categories: An
integrated model for the acquisition of language-specific sound
categories

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

253

</td>

<td style="text-align:left;">

Boersma:2008tk

</td>

<td style="text-align:left;">

Learning to perceive a smaller L2 vowel inventory: an Optimality Theory
account

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

254

</td>

<td style="text-align:left;">

Williams:2014tc

</td>

<td style="text-align:left;">

Native and Non-Native Speech Perception

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

255

</td>

<td style="text-align:left;">

Morrison:2008wy

</td>

<td style="text-align:left;">

Manuscript: DE070483 Revised Date: 28 January 2008L1-Spanish speakers{}
acquisition of the English

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

256

</td>

<td style="text-align:left;">

escudero2011enhanced

</td>

<td style="text-align:left;">

Enhanced bimodal distributions facilitate the learning of second
language vowels

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

257

</td>

<td style="text-align:left;">

Crowhurst:1992ve

</td>

<td style="text-align:left;">

Diminutives and augmentatives in Mexican Spanish: a prosodic analysis

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

258

</td>

<td style="text-align:left;">

colome2010language

</td>

<td style="text-align:left;">

Language effects in addition: How you say it counts

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

259

</td>

<td style="text-align:left;">

Fowler:1993vq

</td>

<td style="text-align:left;">

Coordination and coarticulation in speech production.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

260

</td>

<td style="text-align:left;">

Anonymous:tLFHwYTL

</td>

<td style="text-align:left;">

The split-apply-combine strategy for data analysis

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

261

</td>

<td style="text-align:left;">

Maclagan:2007vp

</td>

<td style="text-align:left;">

Getting fed up with our feet: Contrast maintenance and the New Zealand
English {}short{} front vowel shift

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

262

</td>

<td style="text-align:left;">

marques2011study

</td>

<td style="text-align:left;">

Study Abroad, Previous Language Experience, and Spanish L2 Development

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

263

</td>

<td style="text-align:left;">

Ramon-Casas2009

</td>

<td style="text-align:left;">

Vowel categorization during word recognition in bilingual toddlers.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

264

</td>

<td style="text-align:left;">

Tsurutani:2012jb

</td>

<td style="text-align:left;">

Evaluation of speakers with foreign-accented speech in Japan: the effect
of accent produced by English native speakers

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

265

</td>

<td style="text-align:left;">

Anonymous:yGUo8Lyr

</td>

<td style="text-align:left;">

A Byte of Python

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

266

</td>

<td style="text-align:left;">

Lord:2000vb

</td>

<td style="text-align:left;">

The Combined Effects of Immersion and Instruction on Second Language
Pronunciation

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

267

</td>

<td style="text-align:left;">

Bennett:2011uh

</td>

<td style="text-align:left;">

Neural correlates of interspecies perspective taking in the post-mortem
atlantic salmon: an argument for proper multiple comparisons correction

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

268

</td>

<td style="text-align:left;">

parrell2010b

</td>

<td style="text-align:left;">

How /b, d, g/ differ from /p, t, k/ in Spanish: A dynamic account

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

269

</td>

<td style="text-align:left;">

Kuhl:1986tk

</td>

<td style="text-align:left;">

Theoretical Contributions of Tests on Animals to the Special-Mechanisms
Debate in Speech

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

270

</td>

<td style="text-align:left;">

Gillespie:2011ug

</td>

<td style="text-align:left;">

Categorical Variables in Regression Analyses

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

271

</td>

<td style="text-align:left;">

Morrison:2008vn

</td>

<td style="text-align:left;">

L1-Spanish speakers’ acquisition of the english /i/-/I/ contrast:
duration-based perception is not the initial developmental stage

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

272

</td>

<td style="text-align:left;">

Siskind:2009vr

</td>

<td style="text-align:left;">

Qtree, a LATEX tree-drawing package

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

273

</td>

<td style="text-align:left;">

escudero2006phonological

</td>

<td style="text-align:left;">

The phonological and phonetic development of new vowel contrasts in
Spanish learners of English

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

274

</td>

<td style="text-align:left;">

Bradlow:1995wn

</td>

<td style="text-align:left;">

A comparative acoustic study of english and spanish vowels

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

275

</td>

<td style="text-align:left;">

Hay:2006wp

</td>

<td style="text-align:left;">

Factors influencing speech perception in the context of a
merger-in-progress

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

276

</td>

<td style="text-align:left;">

SCHEELE:2010p420

</td>

<td style="text-align:left;">

The home language environment of monolingual and bilingual children and
their language proficiency

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

277

</td>

<td style="text-align:left;">

Ladefoged:1972wl

</td>

<td style="text-align:left;">

An auditory-motor theory of speech production

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

278

</td>

<td style="text-align:left;">

Anonymous:TQdisolA

</td>

<td style="text-align:left;">

Forest: a pgf/TikZ-based package for drawing linguistic trees

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

279

</td>

<td style="text-align:left;">

Silva:2014ts

</td>

<td style="text-align:left;">

Fon{'e}tica e fonologia do portugu{^e}s

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

280

</td>

<td style="text-align:left;">

Milinski:1997tj

</td>

<td style="text-align:left;">

How to avoid seven deadly sins in the study of behavior

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

281

</td>

<td style="text-align:left;">

sebastian2012bilingual

</td>

<td style="text-align:left;">

A bilingual advantage in visual language discrimination in infancy

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

282

</td>

<td style="text-align:left;">

Leisch:2008wa

</td>

<td style="text-align:left;">

Sweave users manual

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

283

</td>

<td style="text-align:left;">

Kuhl2008

</td>

<td style="text-align:left;">

Neural Substrates of Language Acquisition

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

284

</td>

<td style="text-align:left;">

Cross:2001ur

</td>

<td style="text-align:left;">

Music, mind and evolution

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

285

</td>

<td style="text-align:left;">

Ernestus:2011vb

</td>

<td style="text-align:left;">

An introduction to reduced pronunciation variants

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

286

</td>

<td style="text-align:left;">

del2011conflict

</td>

<td style="text-align:left;">

Conflict and cognitive control during sentence comprehension:
Recruitment of a frontal network during the processing of Spanish
object-first sentences

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

287

</td>

<td style="text-align:left;">

Llisterri:1995wb

</td>

<td style="text-align:left;">

Relationships between speech production and speech perception in a
second language

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

288

</td>

<td style="text-align:left;">

RalloFabra:2012dc

</td>

<td style="text-align:left;">

Native Catalan learners’ perception and production of English vowels

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

289

</td>

<td style="text-align:left;">

costa2008bilingualism

</td>

<td style="text-align:left;">

Bilingualism aids conflict resolution: Evidence from the ANT task

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

290

</td>

<td style="text-align:left;">

Sanders:2012wz

</td>

<td style="text-align:left;">

Documentation for the

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

291

</td>

<td style="text-align:left;">

hualde2008lexical

</td>

<td style="text-align:left;">

Lexical tone and stress in Goizueta Basque

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

292

</td>

<td style="text-align:left;">

bataller2010making

</td>

<td style="text-align:left;">

Making a request for a service in Spanish: Pragmatic development in the
study abroad setting

</td>

<td style="text-align:left;">

MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

293

</td>

<td style="text-align:left;">

McMurray2008

</td>

<td style="text-align:left;">

Gradient sensitivity to within-category variation in words and
syllables.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

294

</td>

<td style="text-align:left;">

Correa:vo

</td>

<td style="text-align:left;">

Heritage Language Learner Programs and Life after the Classroom{}A Not
So Critical Approach

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

295

</td>

<td style="text-align:left;">

Anonymous:mgnEjvuA

</td>

<td style="text-align:left;">

Developing Data Products in R

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

296

</td>

<td style="text-align:left;">

segalowitz2004comparison

</td>

<td style="text-align:left;">

A Comparison of Spanish Second Language Acquisition in Two Different
Learning Contexts: Study Abroad and the Domestic Classroom.

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

297

</td>

<td style="text-align:left;">

Colina:2010tj

</td>

<td style="text-align:left;">

Tabla de consonantes

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

298

</td>

<td style="text-align:left;">

Menezes:2013bi

</td>

<td style="text-align:left;">

Second Language Acquisition: Reconciling Theories

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

299

</td>

<td style="text-align:left;">

Lee:2011cg

</td>

<td style="text-align:left;">

An OT account of the precedence relationship between perception and
production in the acquisition of English stress

</td>

<td style="text-align:left;">

PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

300

</td>

<td style="text-align:left;">

Meijer:1994tm

</td>

<td style="text-align:left;">

The apacite package

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

301

</td>

<td style="text-align:left;">

DeShieldsJr2011

</td>

<td style="text-align:left;">

The varying influence of spokesperson’s accent in communication
effectiveness: A comparative study in two different regions of Mexico

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

302

</td>

<td style="text-align:left;">

Eckert:2007ul

</td>

<td style="text-align:left;">

Age as a sociolinguistic variable. The Handbook of Sociolinguistics

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

303

</td>

<td style="text-align:left;">

chang2011production

</td>

<td style="text-align:left;">

Production of phonetic and phonological contrast by heritage speakers of
Mandarin

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

304

</td>

<td style="text-align:left;">

beckman2011rate

</td>

<td style="text-align:left;">

Rate effects on Swedish VOT: Evidence for phonological overspecification

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

305

</td>

<td style="text-align:left;">

Winter:2011uz

</td>

<td style="text-align:left;">

Pseudoreplication in phonetic research

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

306

</td>

<td style="text-align:left;">

Ito:2009gf

</td>

<td style="text-align:left;">

Perception of allophonic cues to English word boundaries by Japanese
second language learners of English

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

307

</td>

<td style="text-align:left;">

Simonet:2010wg

</td>

<td style="text-align:left;">

Introduction to R

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

308

</td>

<td style="text-align:left;">

Herrick:2011bp

</td>

<td style="text-align:left;">

On Comanche’s Central Mid Vowel

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

309

</td>

<td style="text-align:left;">

Knightly:2003ky

</td>

<td style="text-align:left;">

Production benefits of childhood overhearing

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

310

</td>

<td style="text-align:left;">

Manning:2007vi

</td>

<td style="text-align:left;">

Logistic regression (with R)

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

311

</td>

<td style="text-align:left;">

Patzer1983

</td>

<td style="text-align:left;">

Source credibility as a function of communicator physical attractiveness

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

312

</td>

<td style="text-align:left;">

Hernandez:2008uq

</td>

<td style="text-align:left;">

Grammatical category-specific deficits in bilingual aphasia

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

313

</td>

<td style="text-align:left;">

Wanrooij:2013cd

</td>

<td style="text-align:left;">

What do listeners learn from exposure to a vowel distribution? An
analysis of listening strategies in distributional learning

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

314

</td>

<td style="text-align:left;">

Baker:2010ui

</td>

<td style="text-align:left;">

The ot-tableau package

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

315

</td>

<td style="text-align:left;">

Flege1991

</td>

<td style="text-align:left;">

The interlingual identification of Spanish and English vowels:
orthographic evidence.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

316

</td>

<td style="text-align:left;">

mehotcheva2010after

</td>

<td style="text-align:left;">

After the fiesta is over: foreign language attrition of Spanish in Dutch
and German Erasmus Students

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

317

</td>

<td style="text-align:left;">

repp1984categorical

</td>

<td style="text-align:left;">

Categorical perception: Issues, methods, findings

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

318

</td>

<td style="text-align:left;">

Jaeger:2004vz

</td>

<td style="text-align:left;">

Praat scripting tutorial Basics

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

319

</td>

<td style="text-align:left;">

Fukui:2004vq

</td>

<td style="text-align:left;">

TIPA Manual

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

320

</td>

<td style="text-align:left;">

oh2010early

</td>

<td style="text-align:left;">

Early childhood language memory in the speech perception of
international adoptees\*

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

321

</td>

<td style="text-align:left;">

Magloire:2000ud

</td>

<td style="text-align:left;">

A cross-language comparison of speaking rate effects on the production
of voice onset time in English and Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

322

</td>

<td style="text-align:left;">

lipski2012language

</td>

<td style="text-align:left;">

Language experience modulates weighting of acoustic cues for vowel
perception: An event-related potential study

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

323

</td>

<td style="text-align:left;">

long1996role

</td>

<td style="text-align:left;">

The role of the linguistic environment in second language acquisition

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

324

</td>

<td style="text-align:left;">

hernandez2010

</td>

<td style="text-align:left;">

The impact of bilingualism on the executive control and orienting
networks of attention

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

325

</td>

<td style="text-align:left;">

just1996brain

</td>

<td style="text-align:left;">

Brain activation modulated by sentence comprehension

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

326

</td>

<td style="text-align:left;">

Merker:2000uv

</td>

<td style="text-align:left;">

Synchronous chorusing and human origins

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

327

</td>

<td style="text-align:left;">

strange2011cross

</td>

<td style="text-align:left;">

Cross-language perceptual similarity predicts categorial discrimination
of American vowels by na{"}ve Japanese listeners

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

328

</td>

<td style="text-align:left;">

Polka:1994ig

</td>

<td style="text-align:left;">

Developmental changes in perception of nonnative vowel contrasts

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

329

</td>

<td style="text-align:left;">

Harris:1992tr

</td>

<td style="text-align:left;">

With respect to metrical constituents in Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

330

</td>

<td style="text-align:left;">

Serrano:2010wo

</td>

<td style="text-align:left;">

Del pret{'e}rito indefinido al pret{'e}rito perfecto: Un caso de cambio
y gramaticalizaci{'o}n en el espa{~n}ol de Canarias y Madrid

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

331

</td>

<td style="text-align:left;">

Moore:1958p124

</td>

<td style="text-align:left;">

The Idealism of Sancho Panza

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

332

</td>

<td style="text-align:left;">

Prieto:2007tw

</td>

<td style="text-align:left;">

The segmental anchoring hypothesis revisited: Syllable structure and
speech rate effects on peak timing in Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

333

</td>

<td style="text-align:left;">

OrtegaLlebaria:2001wl

</td>

<td style="text-align:left;">

Auditory-visual L2 speech perception: Effects of visual cues and
acoustic-phonetic context for Spanish learners of English

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

334

</td>

<td style="text-align:left;">

Yeni-Komshian2000

</td>

<td style="text-align:left;">

Pronunciation Proficiency in the First and Second Languages of
Korean-English Bilinguals.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

335

</td>

<td style="text-align:left;">

Colina:2008wl

</td>

<td style="text-align:left;">

La s{'}laba handout

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

336

</td>

<td style="text-align:left;">

Hattori:2010vv

</td>

<td style="text-align:left;">

Examination of the Relationship between L2 Perception and Production: An
Investigation of English /r/-/l/ Perception and Production by Adult
Japanese Speakers

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

337

</td>

<td style="text-align:left;">

escudero2010spanish

</td>

<td style="text-align:left;">

Spanish listeners perception of American and Southern British English
vowels

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

338

</td>

<td style="text-align:left;">

Ohanian1990

</td>

<td style="text-align:left;">

Construction and Validation of a Scale to Measure Celebrity Endorsers’
Perceived Expertise, Trustworthiness, and Attractiveness

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

339

</td>

<td style="text-align:left;">

Locklin:2008vn

</td>

<td style="text-align:left;">

R notes for experimental psychology

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

340

</td>

<td style="text-align:left;">

diaz2008brain

</td>

<td style="text-align:left;">

Brain potentials to native phoneme discrimination reveal the origin of
individual differences in learning the sounds of a second language

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

341

</td>

<td style="text-align:left;">

YeniKomshian:2000ve

</td>

<td style="text-align:left;">

Pronunciation proficiency in the first and second languages of
Korean-English bilinguals

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

342

</td>

<td style="text-align:left;">

Schmidt:2006tp

</td>

<td style="text-align:left;">

Font selection in LATEX: The most frequently asked questions

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

343

</td>

<td style="text-align:left;">

sundara2005acoustic

</td>

<td style="text-align:left;">

Acoustic-phonetics of coronal stops: A cross-language study of Canadian
English and Canadian French

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

344

</td>

<td style="text-align:left;">

morrison2009l1

</td>

<td style="text-align:left;">

L1-Spanish speakers’ acquisition of the English /i/-/I/ contrast ii:
perception of vowel inherent spectral change

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

345

</td>

<td style="text-align:left;">

Kuhl:2006gn

</td>

<td style="text-align:left;">

Infants show a facilitation effect for native language phonetic
perception between 6 and 12 months

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

346

</td>

<td style="text-align:left;">

cubillos2008impact

</td>

<td style="text-align:left;">

The impact of short-term study abroad programs on L2 listening
comprehension skills

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

347

</td>

<td style="text-align:left;">

Moyna:2005fr

</td>

<td style="text-align:left;">

A historical perspective on Spanish in the California Borderlands

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

348

</td>

<td style="text-align:left;">

poarch2012cross

</td>

<td style="text-align:left;">

Cross-language activation in children’s speech production: Evidence from
second language learners, bilinguals, and trilinguals

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

349

</td>

<td style="text-align:left;">

howard2001effects

</td>

<td style="text-align:left;">

The effects of study abroad on the L2 learners structural skills:
Evidence from advanced learners of French

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

350

</td>

<td style="text-align:left;">

Colina:2008vv

</td>

<td style="text-align:left;">

Acentuaci{'o}n - handout08

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

351

</td>

<td style="text-align:left;">

Hidalgo1986a

</td>

<td style="text-align:left;">

Language Contact, Language Loyalty, and Language Prejudice on the
Mexican Border

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

352

</td>

<td style="text-align:left;">

baker2008child

</td>

<td style="text-align:left;">

Child—Adult Differences in Second-Language Phonological Learning: The
Role of Cross-Language Similarity

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

353

</td>

<td style="text-align:left;">

bylund2012does

</td>

<td style="text-align:left;">

Does First Language Maintenance Hamper Nativelikeness in a Second
Language?

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

354

</td>

<td style="text-align:left;">

Hayes-Harb:2008qy

</td>

<td style="text-align:left;">

Development of the ability to lexically encode novel second language
phonemic contrasts

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

355

</td>

<td style="text-align:left;">

Giannakopoulou:2013ef

</td>

<td style="text-align:left;">

Enhanced plasticity in spoken language acquisition for child learners:
Evidence from phonetic training studies in child and adult learners of
English

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

356

</td>

<td style="text-align:left;">

Gorman:2009vz

</td>

<td style="text-align:left;">

On VARBRUL {} Or, The Spirit of {}74

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

357

</td>

<td style="text-align:left;">

Freed:2004bq

</td>

<td style="text-align:left;">

Learning Context and Its Effects on Second Language Acquisition

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

358

</td>

<td style="text-align:left;">

lisker1967

</td>

<td style="text-align:left;">

Some effects of context on Voice Onset Time in English stops

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

359

</td>

<td style="text-align:left;">

Nadeu:2012ht

</td>

<td style="text-align:left;">

Lenition and Phonemic Overlap in Rome Italian

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

360

</td>

<td style="text-align:left;">

liu:EL427

</td>

<td style="text-align:left;">

Categorical perception of intonation contrasts: Effects of listeners’
language background

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

361

</td>

<td style="text-align:left;">

HancinBhatt:2000bz

</td>

<td style="text-align:left;">

Optimality in second language phonology: codas in Thai ESL

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

362

</td>

<td style="text-align:left;">

Hualde:1989ve

</td>

<td style="text-align:left;">

Silabeo y estructura morf{'e}mica en espa{~n}ol

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

363

</td>

<td style="text-align:left;">

Lipski:1990p201

</td>

<td style="text-align:left;">

Aspects of Ecuadorian vowel reduction

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

364

</td>

<td style="text-align:left;">

McMurray:2010ep

</td>

<td style="text-align:left;">

Unmasking the acoustic effects of vowel-to-vowel coarticulation: A
statistical modeling approach

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

365

</td>

<td style="text-align:left;">

iverson2011cross

</td>

<td style="text-align:left;">

Cross-language specialization in phonetic processing: English and Hindi
perception of/w/-/v/speech and nonspeech

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

366

</td>

<td style="text-align:left;">

Amengual

</td>

<td style="text-align:left;">

Bilingual language profile: an easy-to-use instrument to assess
bilingualism

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

367

</td>

<td style="text-align:left;">

mueller2011learners

</td>

<td style="text-align:left;">

Learners’ identity negotiations and beliefs about pronunciation in study
abroad contexts

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

368

</td>

<td style="text-align:left;">

Llisterri:2012ut

</td>

<td style="text-align:left;">

Las tecnolog{'}as del habla

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

369

</td>

<td style="text-align:left;">

Sheldon:1985kd

</td>

<td style="text-align:left;">

THE RELATIONSHIP BETWEEN PRODUCTION AND PERCEPTION OF THE/r/-/I/CONTRAST
IN KOREAN ADULTS LEARNING ENGLISH: A REPLY TO BORDEN, {}

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

370

</td>

<td style="text-align:left;">

schneider2011reaction

</td>

<td style="text-align:left;">

Reaction time and decision difficulty in the perception of intonation

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

371

</td>

<td style="text-align:left;">

Laks:2012vx

</td>

<td style="text-align:left;">

Generative phonology: its origins, its principles, and its successors

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

372

</td>

<td style="text-align:left;">

recasens2012cross

</td>

<td style="text-align:left;">

A cross-language acoustic study of initial and final allophones of /l/

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

373

</td>

<td style="text-align:left;">

Fowler:2005ih

</td>

<td style="text-align:left;">

The relation of speech perception and speech production

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

374

</td>

<td style="text-align:left;">

Wiley:2008wb

</td>

<td style="text-align:left;">

Emacs and R Integration via ESS: Installation How-To

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

375

</td>

<td style="text-align:left;">

Oyama1976

</td>

<td style="text-align:left;">

A sensitive period for the acquisition of a nonnative phonological
system

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

376

</td>

<td style="text-align:left;">

Tyler:2009dq

</td>

<td style="text-align:left;">

Cross-language differences in cue use for speech segmentation

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

377

</td>

<td style="text-align:left;">

Oetiker:2011vt

</td>

<td style="text-align:left;">

The Not So Short Introduction to LATEX2\(\varepsilon\)

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

378

</td>

<td style="text-align:left;">

Brouwer:2012cm

</td>

<td style="text-align:left;">

Linguistic contributions to speech-on-speech masking for native and
non-native listeners: Language familiarity and semantic content

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

379

</td>

<td style="text-align:left;">

Carifio:2007hg

</td>

<td style="text-align:left;">

Ten Common Misunderstandings, Misconceptions, Persistent Myths and Urban
Legends about Likert Scales and Likert Response Formats and their
Antidotes

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

380

</td>

<td style="text-align:left;">

bundgaard2012second

</td>

<td style="text-align:left;">

Second language learners vocabulary expansion is associated with
improved second language vowel intelligibility

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

381

</td>

<td style="text-align:left;">

Lafford:2004dz

</td>

<td style="text-align:left;">

The Effect of the Context of Learning on the Use of Communication
Strategies by Learners of Spanish as a Second Language

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

382

</td>

<td style="text-align:left;">

bajuniemi2013teaching

</td>

<td style="text-align:left;">

Teaching Intervention on the Pronunciation of Spanish Intervocalic /d/

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

383

</td>

<td style="text-align:left;">

Colina:2008uw

</td>

<td style="text-align:left;">

Cuadro de rasgos

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

384

</td>

<td style="text-align:left;">

Hirschorn:2011tm

</td>

<td style="text-align:left;">

Using the exam document class

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

385

</td>

<td style="text-align:left;">

Moore:2011vd

</td>

<td style="text-align:left;">

How to Insert BibTEX Entries into a CV, Syllabus, . . .

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

386

</td>

<td style="text-align:left;">

best2010perception

</td>

<td style="text-align:left;">

Perception of initial obstruent voicing is influenced by gestural
organization

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

387

</td>

<td style="text-align:left;">

Escudero:2007wn

</td>

<td style="text-align:left;">

Multilingual sound perception and word recognition

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

388

</td>

<td style="text-align:left;">

Colina:2010uf

</td>

<td style="text-align:left;">

Rasgos distintivos-summary fall 2010

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

389

</td>

<td style="text-align:left;">

Amador:2008p104

</td>

<td style="text-align:left;">

Los refranes del Quijote

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

390

</td>

<td style="text-align:left;">

oh2011one

</td>

<td style="text-align:left;">

A one-year longitudinal study of English and Japanese vowel production
by Japanese adults and children in an English-speaking setting

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

391

</td>

<td style="text-align:left;">

Park:2013gr

</td>

<td style="text-align:left;">

Detecting foreign accent in monosyllables: The role of L1 phonotactics

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

392

</td>

<td style="text-align:left;">

MedinaRivera:2007vm

</td>

<td style="text-align:left;">

Discourse genre, type of situation and topic of conversation in relation
to phonological variables in Puerto Rican Spanish

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

393

</td>

<td style="text-align:left;">

Hualde:1997tq

</td>

<td style="text-align:left;">

Spanish /i/ and related sounds: An exercise in phonemic analysis

</td>

<td style="text-align:left;">

PAGES, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

394

</td>

<td style="text-align:left;">

WERKER1984

</td>

<td style="text-align:left;">

Cross-language speech perception: evidence for perceptual reorganization
during the first year of life

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

395

</td>

<td style="text-align:left;">

Anonymous:UHdLv225

</td>

<td style="text-align:left;">

The legal framework for reproducible scientific research: Licensing and
copyright

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

396

</td>

<td style="text-align:left;">

Rogers2006

</td>

<td style="text-align:left;">

Effects of bilingualism , noise , and reverberation on speech perception
by listeners with normal hearing

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

397

</td>

<td style="text-align:left;">

Ernestus:2010vt

</td>

<td style="text-align:left;">

PartB2\_ForeignCasualSpeech

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

398

</td>

<td style="text-align:left;">

BardoviHarlig:2011wp

</td>

<td style="text-align:left;">

Proficiency, length of stay, and intensity of interaction and the
acquisition of conventional expressions in L2 pragmatics

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

399

</td>

<td style="text-align:left;">

Colina:2008ud

</td>

<td style="text-align:left;">

Transcripci{'o}n EJ - Hualde et al

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

400

</td>

<td style="text-align:left;">

Williams:2014el

</td>

<td style="text-align:left;">

Influences of listeners’ native and other dialects on cross-language
vowel perception.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

401

</td>

<td style="text-align:left;">

Sebastian-Galls2006

</td>

<td style="text-align:left;">

First- and Second-language Phonological Representations in the Mental
Lexicon

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

402

</td>

<td style="text-align:left;">

Nakagawa2013

</td>

<td style="text-align:left;">

A general and simple method for obtaining R2 from generalized linear
mixed-effects models

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

403

</td>

<td style="text-align:left;">

praat

</td>

<td style="text-align:left;">

Praat: doing phonetics by computer

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

404

</td>

<td style="text-align:left;">

BlasArroyo:1995wq

</td>

<td style="text-align:left;">

De nuevo el espa{~n}ol y el catal{'a}n, juntos y en contraste: estudio
de actitudes ling{"u}{'}sticas

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

405

</td>

<td style="text-align:left;">

Samuel2001

</td>

<td style="text-align:left;">

Knowing a word affects the fundamental perception of the sounds within
it.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

406

</td>

<td style="text-align:left;">

Anonymous:2005vf

</td>

<td style="text-align:left;">

Beamer by Example

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

407

</td>

<td style="text-align:left;">

Fridland:2012df

</td>

<td style="text-align:left;">

Exploring the relationship between production and perception in the mid
front vowels of U.S. English

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

408

</td>

<td style="text-align:left;">

RalloFabra:2005ub

</td>

<td style="text-align:left;">

Predicting ease of acquisition of L2 speech sounds. a perceived
dissimilarity test

</td>

<td style="text-align:left;">

VOLUME, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

409

</td>

<td style="text-align:left;">

escudero2000developmental

</td>

<td style="text-align:left;">

Developmental patterns in the adult L2 acquisition of new contrasts: the
acoustic cue weighting in the perception of Scottish tense/lax vowels by
Spanish speakers

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

410

</td>

<td style="text-align:left;">

Lisker:1964wl

</td>

<td style="text-align:left;">

A Cross-language Study of Voicing in Initial Stops: Acoustical
Measurements

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

411

</td>

<td style="text-align:left;">

Guion:2003ty

</td>

<td style="text-align:left;">

The vowel systems of Quichua-Spanish bilinguals

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

412

</td>

<td style="text-align:left;">

grosjean2012attempt

</td>

<td style="text-align:left;">

An attempt to isolate, and then differentiate, transfer and interference

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

413

</td>

<td style="text-align:left;">

Elman:1977dx

</td>

<td style="text-align:left;">

Perceptual switching in bilinguals

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

414

</td>

<td style="text-align:left;">

Beach:2001id

</td>

<td style="text-align:left;">

Bilingualism and the relationship between perception and production:
Greek/English bilinguals and Thai bilabial stops

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

415

</td>

<td style="text-align:left;">

LAMBERT1960

</td>

<td style="text-align:left;">

Evaluational reactions to spoken languages.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

416

</td>

<td style="text-align:left;">

Burgaleta:2013cr

</td>

<td style="text-align:left;">

Brain structure is related to speech perception abilities in bilinguals

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

417

</td>

<td style="text-align:left;">

morgan2012explicit

</td>

<td style="text-align:left;">

Explicit and implicit second language training differentially affect the
achievement of native-like brain activation patterns

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

418

</td>

<td style="text-align:left;">

Smalley:1953tq

</td>

<td style="text-align:left;">

Phonemic Rhythm in Comanche

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

419

</td>

<td style="text-align:left;">

CT1988

</td>

<td style="text-align:left;">

Examination of perceptual reorganization for nonnative speech contrasts:
Zulu click discrimination by English-speaking adults and infants.

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

420

</td>

<td style="text-align:left;">

Abramson1973

</td>

<td style="text-align:left;">

Voice-Timing Perception in Spanish Word-Initial Stops

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

421

</td>

<td style="text-align:left;">

Casagrande:1989wz

</td>

<td style="text-align:left;">

Comanche Linguistic Acculturation

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

422

</td>

<td style="text-align:left;">

hualde2002intonation

</td>

<td style="text-align:left;">

Intonation in Spanish and the other Ibero-Romance languages: overview
and status quaestionis

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

423

</td>

<td style="text-align:left;">

Simonet:2014uy

</td>

<td style="text-align:left;">

textbf{The phonetics and phonology of bilingualism}

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

424

</td>

<td style="text-align:left;">

Gulian:2007vc

</td>

<td style="text-align:left;">

Supervision hampers distributional learning of vowel contrasts

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

425

</td>

<td style="text-align:left;">

au2008salvaging

</td>

<td style="text-align:left;">

Salvaging a childhood language

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

426

</td>

<td style="text-align:left;">

Saldivar:1980p121

</td>

<td style="text-align:left;">

Don Quijote’s Metaphors and the Grammar of Proper Language

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

427

</td>

<td style="text-align:left;">

Llanes:2012jh

</td>

<td style="text-align:left;">

Age Effects in a Study Abroad Context: Children and Adults Studying
Abroad and at Home

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

428

</td>

<td style="text-align:left;">

escudero2010effect

</td>

<td style="text-align:left;">

The effect of L1 orthography on non-native vowel perception

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

429

</td>

<td style="text-align:left;">

mccarthy2007

</td>

<td style="text-align:left;">

What is Optimality Theory?

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

430

</td>

<td style="text-align:left;">

diaz2012individual

</td>

<td style="text-align:left;">

Individual differences in late bilinguals’ L2 phonological processes:
From acoustic-phonetic analysis to lexical access

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

431

</td>

<td style="text-align:left;">

Ladd:1997vn

</td>

<td style="text-align:left;">

The perception of intonational emphasis: continuous or categorical?

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

432

</td>

<td style="text-align:left;">

Bereznak:1995vn

</td>

<td style="text-align:left;">

Review of: a grammar of Comanche, by Jean Ormsbee Charney

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

433

</td>

<td style="text-align:left;">

Darcy2012

</td>

<td style="text-align:left;">

Direct mapping of acoustics to phonology: On the lexical encoding of
front rounded vowels in L1 English–L2 French acquisition

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

434

</td>

<td style="text-align:left;">

swain1995three

</td>

<td style="text-align:left;">

Three functions of output in second language learning

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

435

</td>

<td style="text-align:left;">

Recasens:2010vc

</td>

<td style="text-align:left;">

Lingual kinematics and coarticulation for alveolopalatal and velar
consonants in Catalan

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

436

</td>

<td style="text-align:left;">

Kartushina:2014cg

</td>

<td style="text-align:left;">

On the effects of L2 perception and of individual differences in L1
production on L2 pronunciation

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

437

</td>

<td style="text-align:left;">

simonet2011intonational

</td>

<td style="text-align:left;">

Intonational convergence in language contact: Utterance-final F0
contours in Catalan{}Spanish early bilinguals

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

438

</td>

<td style="text-align:left;">

Morrison:2007ur

</td>

<td style="text-align:left;">

A cross-dialect comparison of Peninsula-and Peruvian-Spanish vowels

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

439

</td>

<td style="text-align:left;">

Hyltenstam:2009gn

</td>

<td style="text-align:left;">

Dominant-language replacement: The case of international adoptees

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

440

</td>

<td style="text-align:left;">

JongKong:2012cc

</td>

<td style="text-align:left;">

Voiced stop prenasalization in two dialects of Greek

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

441

</td>

<td style="text-align:left;">

Elvin:2014ua

</td>

<td style="text-align:left;">

Perception of Brazilian Portuguese Vowels by Australian English and
Spanish Listeners

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

442

</td>

<td style="text-align:left;">

Hyltenstam:2009ly

</td>

<td style="text-align:left;">

Dominant-language replacement: The case of international adoptees

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

443

</td>

<td style="text-align:left;">

de2012singing

</td>

<td style="text-align:left;">

Singing a different tune in your native language: first language
attrition of prosody

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

444

</td>

<td style="text-align:left;">

Piske:2002to

</td>

<td style="text-align:left;">

The production of English vowels by fluent early and late
Italian-English bilinguals

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

445

</td>

<td style="text-align:left;">

Lawrence:2012ud

</td>

<td style="text-align:left;">

Package {}ez{}

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

446

</td>

<td style="text-align:left;">

sebastian2011bilingual

</td>

<td style="text-align:left;">

Bilingual Language Acquisition: Where Does the Difference Lie?

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

447

</td>

<td style="text-align:left;">

Anonymous:CAHnH-l3

</td>

<td style="text-align:left;">

Regression Models for Data Science in R

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

448

</td>

<td style="text-align:left;">

albareda2011acquisition

</td>

<td style="text-align:left;">

The acquisition of phonetic categories in bilingual infants: new data
from an anticipatory eye movement paradigm

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

449

</td>

<td style="text-align:left;">

LoureiroRodriguez:2008ua

</td>

<td style="text-align:left;">

Conflicting values at a conflicting age

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

450

</td>

<td style="text-align:left;">

Guion:1998wq

</td>

<td style="text-align:left;">

The role of perception in the sound change of velar palatalization

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

451

</td>

<td style="text-align:left;">

Chomsky:1959tf

</td>

<td style="text-align:left;">

Review of B.F. Skinner, Verbal Behavior

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

452

</td>

<td style="text-align:left;">

Cebrian:2006el

</td>

<td style="text-align:left;">

Experience and the use of non-native duration in L2 vowel categorization

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

453

</td>

<td style="text-align:left;">

tuinman2011perception

</td>

<td style="text-align:left;">

Perception of intrusive/r/in English by native, cross-language and
cross-dialect listeners

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

454

</td>

<td style="text-align:left;">

Woolard:1997uj

</td>

<td style="text-align:left;">

Between friends: Gender, peer group structure, and bilingualism in urban
Catalonia

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

455

</td>

<td style="text-align:left;">

Anonymous:mCkzXHwO

</td>

<td style="text-align:left;">

knitr Graphics Manual

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

456

</td>

<td style="text-align:left;">

Bosch:2011cp

</td>

<td style="text-align:left;">

Variability in vowel production by bilingual speakers: can input
properties hinder the early stabilization of contrastive categories?

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

457

</td>

<td style="text-align:left;">

lee2012effects

</td>

<td style="text-align:left;">

Effects of speaker variability and noise on Mandarin fricative
identification by native and non-native listeners

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

458

</td>

<td style="text-align:left;">

Montaruli:2011ue

</td>

<td style="text-align:left;">

Identity, language, and ethnic relations in the Bilingual Autonomous
Communities of Spain

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

459

</td>

<td style="text-align:left;">

Andrews:1949p119

</td>

<td style="text-align:left;">

Aspectos Sociol{'o}gicos del Lenguaje Popular

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

460

</td>

<td style="text-align:left;">

Ernestus:2011ky

</td>

<td style="text-align:left;">

Vowel elision in casual French: The case of vowel/e/in the word
c’{'e}tait

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

461

</td>

<td style="text-align:left;">

Morrison:2006wn

</td>

<td style="text-align:left;">

L1 {\&amp;} L2 Production and Perception of English and Spanish Vowels A
Statistical Modelling Approach

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

462

</td>

<td style="text-align:left;">

Oh:2003is

</td>

<td style="text-align:left;">

Holding on to childhood language memory

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

463

</td>

<td style="text-align:left;">

Ramachandra:2009tx

</td>

<td style="text-align:left;">

The Role of Mirror Neurons in Processing Vocal Emotions: Evidence from
Psychophysiological Data

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

464

</td>

<td style="text-align:left;">

sebastian2012first

</td>

<td style="text-align:left;">

First and Second Language Speech Perception: Graded Learning

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

465

</td>

<td style="text-align:left;">

Hualde:1989vr

</td>

<td style="text-align:left;">

Autosegmental and metrical spreading in the vowel-harmony systems of
northwestern Spain

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

466

</td>

<td style="text-align:left;">

Wang:1999wc

</td>

<td style="text-align:left;">

The perception of English tense-lax vowel pairs by native Mandarin
speakers: the effect of training on attention to temporal and spectral
cues

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

467

</td>

<td style="text-align:left;">

Wood:2009ux

</td>

<td style="text-align:left;">

A Primer for Linguists6 October 2009

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

468

</td>

<td style="text-align:left;">

TARNOVSKA:2005p95

</td>

<td style="text-align:left;">

Sobre los refranes de El Quijote: The proverbs in The Quixote

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

469

</td>

<td style="text-align:left;">

Sebastian-Galles:2009pd

</td>

<td style="text-align:left;">

Lexical plasticity in early bilinguals does not alter phoneme
categories: II. Experimental evidence

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

470

</td>

<td style="text-align:left;">

Munro1996

</td>

<td style="text-align:left;">

The effects of age of second language learning on the production of
English vowels

</td>

<td style="text-align:left;">

PAGES, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

471

</td>

<td style="text-align:left;">

cutler2008consonant

</td>

<td style="text-align:left;">

Consonant identification in noise by native and non-native listeners:
Effects of local context

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

472

</td>

<td style="text-align:left;">

Peirce:2008gm

</td>

<td style="text-align:left;">

Generating stimuli for neuroscience using PsychoPy

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

473

</td>

<td style="text-align:left;">

Lombardi:2003vt

</td>

<td style="text-align:left;">

Second language data and constraints on manner: explaining substitutions
for the English interdentals

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

474

</td>

<td style="text-align:left;">

Cooper:2017ve

</td>

<td style="text-align:left;">

A Guide to Reproducible Code in Ecology and Evolution

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

475

</td>

<td style="text-align:left;">

gordon2002cross

</td>

<td style="text-align:left;">

A cross-linguistic acoustic study of voiceless fricatives

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

476

</td>

<td style="text-align:left;">

Clopper2005

</td>

<td style="text-align:left;">

Acoustic characteristics of the vowel systems of six regional varieties
of American English

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

477

</td>

<td style="text-align:left;">

Colina:2006ug

</td>

<td style="text-align:left;">

No double plurals in dominican spanish: an optimality-theoretic account

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

478

</td>

<td style="text-align:left;">

antoniou2012two

</td>

<td style="text-align:left;">

Two ways to listen: do L2-dominant bilinguals perceive stop voicing
according to language mode?

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

479

</td>

<td style="text-align:left;">

Anonymous:QobCWDq7

</td>

<td style="text-align:left;">

INTRODUCTION TO EMPIRICAL BAYES

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

480

</td>

<td style="text-align:left;">

gordon2001memory

</td>

<td style="text-align:left;">

Memory interference during language processing

</td>

<td style="text-align:left;">

PAGES, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

481

</td>

<td style="text-align:left;">

Colina:2008vz

</td>

<td style="text-align:left;">

Estructura sil{'a}bica

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

482

</td>

<td style="text-align:left;">

Pallier1997a

</td>

<td style="text-align:left;">

A limit on behavioral plasticity in speech perception

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

483

</td>

<td style="text-align:left;">

Colina:2010tp

</td>

<td style="text-align:left;">

Silabificaci{'o}n y diptongos - Ej

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

484

</td>

<td style="text-align:left;">

Cumming:2011ua

</td>

<td style="text-align:left;">

Understanding The New Statistics: Effect Sizes, Confidence Intervals,
and Meta-Analysis

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

485

</td>

<td style="text-align:left;">

Flege1999b

</td>

<td style="text-align:left;">

Age Constraints on Second-Language Acquisition

</td>

<td style="text-align:left;">

PAGES, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

486

</td>

<td style="text-align:left;">

forrest1988statistical

</td>

<td style="text-align:left;">

Statistical analysis of word-initial voiceless obstruents: Preliminary
data

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

487

</td>

<td style="text-align:left;">

PallierGalles1999

</td>

<td style="text-align:left;">

Phonological representations and repetition priming

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

488

</td>

<td style="text-align:left;">

escudero2004bridging

</td>

<td style="text-align:left;">

Bridging the gap between L2 speech perception research and phonological
theory

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

489

</td>

<td style="text-align:left;">

Navarra2005

</td>

<td style="text-align:left;">

The Perception of Second Language Sounds in Early Bilinguals: New
Evidence from an Implicit Measure

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

490

</td>

<td style="text-align:left;">

Kinginger:2008fg

</td>

<td style="text-align:left;">

Language Learning in Study Abroad: Case Studies of Americans in France

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

491

</td>

<td style="text-align:left;">

ressel2012effect

</td>

<td style="text-align:left;">

An Effect of Bilingualism on the Auditory Cortex

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

492

</td>

<td style="text-align:left;">

brenders2011word

</td>

<td style="text-align:left;">

Word recognition in child second language learners: Evidence from
cognates and false friends

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

493

</td>

<td style="text-align:left;">

Hernandez:2010kr

</td>

<td style="text-align:left;">

The Relationship Among Motivation, Interaction, and the Development of
Second Language Oral Proficiency in a Study-Abroad Context

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

494

</td>

<td style="text-align:left;">

Parrell:2012ira

</td>

<td style="text-align:left;">

The role of gestural phasing in Western Andalusian Spanish aspiration

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

495

</td>

<td style="text-align:left;">

Bohn:1993tc

</td>

<td style="text-align:left;">

Perceptual switching in Spanish/English bilinguals: Evidence for
universal factors in stop voicing judgments

</td>

<td style="text-align:left;">

VOLUME, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

496

</td>

<td style="text-align:left;">

Recasens:2009vc

</td>

<td style="text-align:left;">

An articulatory investigation of lingual coarticulatory resistance and
aggressiveness for consonants and vowels in Catalan

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

497

</td>

<td style="text-align:left;">

Ernestus:2012vr

</td>

<td style="text-align:left;">

European Research Council

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

498

</td>

<td style="text-align:left;">

Colina:2010ud

</td>

<td style="text-align:left;">

Transcripci{'o}n EJ - Hualde et al.

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

499

</td>

<td style="text-align:left;">

Nadeu:2011vx

</td>

<td style="text-align:left;">

Consonant lenition and phonological recategorization

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

zimmerman1958note

</td>

<td style="text-align:left;">

Note on vowel duration seen cross-linguistically

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

501

</td>

<td style="text-align:left;">

Peperkamp:2011wl

</td>

<td style="text-align:left;">

The relation between perception and production in L2 phonological
processing

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

502

</td>

<td style="text-align:left;">

Simonet:2012jx

</td>

<td style="text-align:left;">

Dialectal Differences in Spanish Voiced Obstruent Allophony: Costa Rican
versus Iberian Spanish

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

503

</td>

<td style="text-align:left;">

Beaudrie:2007ti

</td>

<td style="text-align:left;">

Articles

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

504

</td>

<td style="text-align:left;">

Tucker1969

</td>

<td style="text-align:left;">

White and Negro Listeners’ Reactions to Various American-English
Dialects

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

505

</td>

<td style="text-align:left;">

Colina:2007vp

</td>

<td style="text-align:left;">

Optimality-Theoretic Studies in Spanish Phonology

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

506

</td>

<td style="text-align:left;">

Whitehead1968

</td>

<td style="text-align:left;">

Factors of source credibility

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

507

</td>

<td style="text-align:left;">

felix2013refusing

</td>

<td style="text-align:left;">

Refusing in L2 Spanish: The effects of the context of learning during a
short-term study abroad program

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

508

</td>

<td style="text-align:left;">

simonet2011vx

</td>

<td style="text-align:left;">

Consonant lenition and phonological recategorization

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

509

</td>

<td style="text-align:left;">

Hay2010

</td>

<td style="text-align:left;">

Stuffed toys and speech perception

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

510

</td>

<td style="text-align:left;">

TorresReyna:2011wf

</td>

<td style="text-align:left;">

Data Preparation/Descriptive Statistics

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

511

</td>

<td style="text-align:left;">

simonet2011

</td>

<td style="text-align:left;">

Technology in Phonetic Science: Setting Up a Basic Phonetics Laboratory.

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

512

</td>

<td style="text-align:left;">

Harris:1980ua

</td>

<td style="text-align:left;">

Relationships between speech perception and speech production in normal
hearing and hearing impaired subjects

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

513

</td>

<td style="text-align:left;">

Mackain1981

</td>

<td style="text-align:left;">

Categorical Perception of English /r/ and /l/ by Japanese Bilinguals

</td>

<td style="text-align:left;">

PAGES, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

514

</td>

<td style="text-align:left;">

Colina:1997uu

</td>

<td style="text-align:left;">

Identity constraints and Spanish resyllabification\* 1

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

515

</td>

<td style="text-align:left;">

Bickerton:1996vf

</td>

<td style="text-align:left;">

I chat, thereby I groom

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

516

</td>

<td style="text-align:left;">

levey2004

</td>

<td style="text-align:left;">

Discrimination and Production of English Vowels by Bilingual Speakers of
Spanish and English

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

517

</td>

<td style="text-align:left;">

MartinezGil:1997vt

</td>

<td style="text-align:left;">

Obstruent vocalization in Chilean Spanish: A serial versus a
constraint-based approach

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

518

</td>

<td style="text-align:left;">

Grice:1961ud

</td>

<td style="text-align:left;">

The Causal Theory of Perception

</td>

<td style="text-align:left;">

NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

519

</td>

<td style="text-align:left;">

Cubillos:2012jq

</td>

<td style="text-align:left;">

The Impact of Study Abroad on Students’ Self-Efficacy Perceptions

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

520

</td>

<td style="text-align:left;">

Polka:2011fp

</td>

<td style="text-align:left;">

Natural Referent Vowel (NRV) framework: an emerging view of early
phonetic development

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

521

</td>

<td style="text-align:left;">

Kuhl:2008kx

</td>

<td style="text-align:left;">

Phonetic learning as a pathway to language: New data and native language
magnet theory expanded (NLM-e)

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

522

</td>

<td style="text-align:left;">

Lev-Ari2010

</td>

<td style="text-align:left;">

Why don’t we believe non-native speakers? The influence of accent on
credibility

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

523

</td>

<td style="text-align:left;">

Batstone:2002kt

</td>

<td style="text-align:left;">

Contexts of engagement: a discourse perspective on {}intake{} and
{}pushed output{}

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

524

</td>

<td style="text-align:left;">

Morrison:2005hp

</td>

<td style="text-align:left;">

An appropriate metric for cue weighting in L2 speech perception:
response to Escudero and Boersma (2004)

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

525

</td>

<td style="text-align:left;">

Bialystok2008

</td>

<td style="text-align:left;">

Bilingualism: The good, the bad, and the indifferent

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

526

</td>

<td style="text-align:left;">

mora2012l2

</td>

<td style="text-align:left;">

L2 effects on the perception and production of a native vowel contrast
in early bilinguals

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

527

</td>

<td style="text-align:left;">

Beaudrie2005

</td>

<td style="text-align:left;">

Beginning Level University Heritage Programs: Creating a Space for All
Heritage Language Learners

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

528

</td>

<td style="text-align:left;">

Ohala:1986tk

</td>

<td style="text-align:left;">

Against the direct realist view of speech perception.

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

529

</td>

<td style="text-align:left;">

broersma2011competition

</td>

<td style="text-align:left;">

Competition dynamics of second-language listening

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

530

</td>

<td style="text-align:left;">

Piske2001

</td>

<td style="text-align:left;">

Factors affecting degree of foreign accent in an L2 : a review

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

531

</td>

<td style="text-align:left;">

Costa:2009zr

</td>

<td style="text-align:left;">

On the bilingual advantage in conflict processing: now you see it, now
you don’t

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

532

</td>

<td style="text-align:left;">

DeKeyser:2013dn

</td>

<td style="text-align:left;">

Age Effects in Second Language Learning: Stepping Stones Toward Better
Understanding

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

533

</td>

<td style="text-align:left;">

zubizarreta2011encoding

</td>

<td style="text-align:left;">

Encoding discourse-based meaning: Prosody vs. syntax. Implications for
second language acquisition

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

534

</td>

<td style="text-align:left;">

chladkova2011context

</td>

<td style="text-align:left;">

Context-specific acoustic differences between Peruvian and Iberian
Spanish vowels

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

535

</td>

<td style="text-align:left;">

rivera2000intraethnic

</td>

<td style="text-align:left;">

Intraethnic attitudes among Hispanics in a Northern California community

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

536

</td>

<td style="text-align:left;">

Alispahic:2014us

</td>

<td style="text-align:left;">

Difficulty in discriminating non-native vowels: Are Dutch vowels easier
for Australian English than Spanish listeners?

</td>

<td style="text-align:left;">

VOLUME, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

537

</td>

<td style="text-align:left;">

escudero2009cross

</td>

<td style="text-align:left;">

A cross-dialect acoustic description of vowels: Brazilian and European
Portuguese

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, MONTH

</td>

</tr>

<tr>

<td style="text-align:right;">

538

</td>

<td style="text-align:left;">

Tatman:2013um

</td>

<td style="text-align:left;">

A Very Brief Introduction To Bayesian Statistics for Linguists

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

539

</td>

<td style="text-align:left;">

Au:2002dga

</td>

<td style="text-align:left;">

Overhearing a language during childhood

</td>

<td style="text-align:left;">

MONTH

</td>

</tr>

</tbody>

</table>

``` r
unlink("cache", recursive = TRUE)
unlink("figure", recursive = TRUE)
```
