
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

    ## Warning in bib2df_tidy(bib, separate_names): NAs introduced by coercion

    ## Column `YEAR` contains character strings.
    ##               No coercion to numeric applied.

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

<img src="https://i.imgur.com/rlpGIbI.png" width="960" />

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

<img src="https://i.imgur.com/HpxqDTQ.png" width="960" />

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

<img src="https://i.imgur.com/Vp8qwW1.png" width="960" />

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

<img src="https://i.imgur.com/SXG1bpE.png" width="960" />

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

<img src="https://i.imgur.com/kOmUHDf.png" width="960" />

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

<img src="https://i.imgur.com/RnF74fJ.png" width="960" />

## Publication Years

``` r
bib %>% 
  ggplot(., aes(x = YEAR)) + 
    geom_histogram(binwidth = 1, color = 'black', fill = 'grey60', stat = "count") + 
    scale_x_discrete(breaks = seq(1900, 2020, 10))
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

<img src="https://i.imgur.com/DgJZ633.png" width="960" />

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

<img src="https://i.imgur.com/bpD0wke.png" width="960" />

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
            JOURNAL, AUTHOR) %>%   
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

broersma2010perception

</td>

<td style="text-align:left;">

Perception of final fricative voicing: Native and nonnative listeners’
use of vowel duration

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

Cutler:2009mz

</td>

<td style="text-align:left;">

Greater sensitivity to prosodic goodness in non-native than in native
listeners

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

barron2006learning

</td>

<td style="text-align:left;">

Learning to say “you” in German: The acquisition of sociolinguistic
competence in a study abroad context

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

4

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

5

</td>

<td style="text-align:left;">

Riggs:1949ui

</td>

<td style="text-align:left;">

Alternate Phonemic Analyses of Comanche

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

6

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

7

</td>

<td style="text-align:left;">

Best:2003wq

</td>

<td style="text-align:left;">

Cross-language perception of nonnative vowels: Phonological and phonetic
effects of listeners{} native languages

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

8

</td>

<td style="text-align:left;">

Munro:1993tf

</td>

<td style="text-align:left;">

Productions of English vowels by native speakers of Arabic: acoustic
measurements and accentedness ratings.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:left;">

Nishi:2008rt

</td>

<td style="text-align:left;">

Acoustic and perceptual similarity of Japanese and American English
vowels

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

10

</td>

<td style="text-align:left;">

Hualde:1989tt

</td>

<td style="text-align:left;">

Procesos conson{'a}nticos y estructuras geom{'e}tricas en espa{~n}ol

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

11

</td>

<td style="text-align:left;">

Sundara:2008ys

</td>

<td style="text-align:left;">

Development of coronal stop perception: Bilingual infants keep pace with
their monolingual peers

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

12

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

13

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

14

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

15

</td>

<td style="text-align:left;">

Rauber:2005uo

</td>

<td style="text-align:left;">

The interrelation between the perception and production of English
vowels by native speakers of Brazilian Portuguese

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

16

</td>

<td style="text-align:left;">

Leon:1903uv

</td>

<td style="text-align:left;">

Los comanches y el dialecto Cahuillo de la baja California

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

17

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

18

</td>

<td style="text-align:left;">

isabelli2006study

</td>

<td style="text-align:left;">

Study abroad social networks, motivation and attitudes: Implications for
second language acquisition

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

19

</td>

<td style="text-align:left;">

Dunbar:1993ws

</td>

<td style="text-align:left;">

Coevolution of neocortex size, group size and language in humans

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

20

</td>

<td style="text-align:left;">

Hickok:2003ui

</td>

<td style="text-align:left;">

Auditory-motor interaction revealed by fMRI: speech, music, and working
memory in area Spt

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:left;">

ruiz2009reorienting

</td>

<td style="text-align:left;">

Reorienting Language-as-Resource

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

22

</td>

<td style="text-align:left;">

Samuel2011

</td>

<td style="text-align:left;">

Speech perception.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

23

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

24

</td>

<td style="text-align:left;">

Anonymous:-7kf7cVY

</td>

<td style="text-align:left;">

L1-Spanish Speakers’ Acquisition of the English/i/{}/I/Contrast:
Duration-based Perception is Not the Initial Developmental Stage

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

25

</td>

<td style="text-align:left;">

Face:2003fk

</td>

<td style="text-align:left;">

Intonation in spanish declaratives : differences between lab speech and
spontaneous speech.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

26

</td>

<td style="text-align:left;">

Michnowicz:2007uz

</td>

<td style="text-align:left;">

El habla de Yucat{'a}m: Final \[m\] in a Dialect in Contact

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

27

</td>

<td style="text-align:left;">

ringen2012voicing

</td>

<td style="text-align:left;">

The voicing contrast in Fenno-Swedish stops

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

28

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

29

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

30

</td>

<td style="text-align:left;">

broersma2008flexible

</td>

<td style="text-align:left;">

Flexible cue use in nonnative phonetic categorization

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

31

</td>

<td style="text-align:left;">

Akustyk

</td>

<td style="text-align:left;">

AKUSTYK for Praat

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

32

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

33

</td>

<td style="text-align:left;">

Ingvalson:2011dx

</td>

<td style="text-align:left;">

Years of Exposure to English Predicts Perception and Production of /r/
and /l/ by Native Speakers of Japanese

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

34

</td>

<td style="text-align:left;">

Correa:2008p87

</td>

<td style="text-align:left;">

Metalinguistic knowledge and the acquisition of the Spanish subjunctive
by learners at three proficiency levels

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

35

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

36

</td>

<td style="text-align:left;">

Brady:1978tf

</td>

<td style="text-align:left;">

A range effect in the perception of voicing

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

37

</td>

<td style="text-align:left;">

antoniou2013focusing

</td>

<td style="text-align:left;">

Focusing the lens of language experience: Perception of Ma’di stops by
Greek and English bilinguals and monolinguals

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

38

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

39

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

40

</td>

<td style="text-align:left;">

Escudero:2014gd

</td>

<td style="text-align:left;">

Magnitude of phonetic distinction predicts success at early word
learning in native and non-native accents.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

41

</td>

<td style="text-align:left;">

Caramazza:1973gk

</td>

<td style="text-align:left;">

The acquisition of a new phonological contrast: The case of stop
consonants in French-English bilinguals

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

42

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

43

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

44

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

45

</td>

<td style="text-align:left;">

Gaies1991

</td>

<td style="text-align:left;">

The Matched-Guise Technique for Measuring Attitudes and Their
Implications for Language Education: A Critical Assessment

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

46

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

47

</td>

<td style="text-align:left;">

bills2005comunidades

</td>

<td style="text-align:left;">

Las comunidades ling{"u}{'}sticas y el mantenimiento del espa{~n}ol en
Estados Unidos

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

48

</td>

<td style="text-align:left;">

vanLeussen:2011ur

</td>

<td style="text-align:left;">

Acoustic properties of Dutch steady-state vowels: Contextual effects and
a comparison with previous studies

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

49

</td>

<td style="text-align:left;">

myers2005vowel

</td>

<td style="text-align:left;">

Vowel duration and neutralization of vowel length contrasts in
Kinyarwanda

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

SchillingEstes:2002tc

</td>

<td style="text-align:left;">

Investigating Stylistic Variation

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

51

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

52

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

53

</td>

<td style="text-align:left;">

Bakovic:2007un

</td>

<td style="text-align:left;">

Hiatus resolution and incomplete identity

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

54

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

55

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

56

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

57

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

58

</td>

<td style="text-align:left;">

anderson2008static

</td>

<td style="text-align:left;">

Static palatography for language fieldwork

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

59

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

60

</td>

<td style="text-align:left;">

HancinBhatt:wi

</td>

<td style="text-align:left;">

Optimal L2 syllables: Interaction of transfer and developmental effects

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

61

</td>

<td style="text-align:left;">

Bullock:2008us

</td>

<td style="text-align:left;">

Kreyol incursions into Dominican Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

62

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

63

</td>

<td style="text-align:left;">

Harris:1984wx

</td>

<td style="text-align:left;">

La espirantizaci{'o}n en castellano y la representaci{'o}n fonol{'o}gica
autosegmental

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

64

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

65

</td>

<td style="text-align:left;">

Corral:2010p1789

</td>

<td style="text-align:left;">

La ‘ch’ fricativa en Granada: un sonido del habla masculina

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

66

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

67

</td>

<td style="text-align:left;">

McKee:1983vo

</td>

<td style="text-align:left;">

The Effects of Intensive Language Instruction on Student Performance in
Beginning College French. (Report no. FL013910)

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

68

</td>

<td style="text-align:left;">

Hovland1952

</td>

<td style="text-align:left;">

Source credibility and effective communication

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

69

</td>

<td style="text-align:left;">

Torreira:2014vw

</td>

<td style="text-align:left;">

Quasi-neutralization of stress contrasts in Spanish

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

70

</td>

<td style="text-align:left;">

Flege:2007tt

</td>

<td style="text-align:left;">

Language contact in bilingualism: phonetic system interactions

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

Jaeger:ed

</td>

<td style="text-align:left;">

Mixed effect models for genetic and areal dependencies in linguistic
typology

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

72

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

73

</td>

<td style="text-align:left;">

leyden2006prosody

</td>

<td style="text-align:left;">

On the prosody of Orkney and Shetland dialects

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

74

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

75

</td>

<td style="text-align:left;">

myers2003f0

</td>

<td style="text-align:left;">

F0 timing in Kinyarwanda

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

76

</td>

<td style="text-align:left;">

Shively:2008wa

</td>

<td style="text-align:left;">

Development of Spanish requests and apologies during study abroad

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

77

</td>

<td style="text-align:left;">

LevAri:2014ii

</td>

<td style="text-align:left;">

Comprehending non-native speakers: theory and evidence for adjustment in
manner of processing.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

78

</td>

<td style="text-align:left;">

face2003effects

</td>

<td style="text-align:left;">

Effects of syntactic constituency on the intonational marking of Spanish
contrastive focus

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

79

</td>

<td style="text-align:left;">

Pallier:2012uy

</td>

<td style="text-align:left;">

TEX et BibTEX a\` la puissance Emacs

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

80

</td>

<td style="text-align:left;">

RCoreTeam2012

</td>

<td style="text-align:left;">

R: A Language and Environment for Statistical Computing

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

81

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

82

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

83

</td>

<td style="text-align:left;">

Dunbar:1998us

</td>

<td style="text-align:left;">

Theory of mind and the evolution of language

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

84

</td>

<td style="text-align:left;">

Warren1970

</td>

<td style="text-align:left;">

Perceptual Restoration of Missing Speech Sounds

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

85

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

86

</td>

<td style="text-align:left;">

winter\_2013lr

</td>

<td style="text-align:left;">

Linear models and linear mixed effects models in R with linguistic
applications

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

87

</td>

<td style="text-align:left;">

Flemming:2003vr

</td>

<td style="text-align:left;">

Speech perception in phonology

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

88

</td>

<td style="text-align:left;">

iverson2010cross

</td>

<td style="text-align:left;">

Cross-language effects for non-speech analogs: A comparison of English/
w/-/ v/ perception by native speakers of Hindi and English.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

89

</td>

<td style="text-align:left;">

Harris:1991wm

</td>

<td style="text-align:left;">

The exponence of gender in Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

90

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

91

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

92

</td>

<td style="text-align:left;">

Rivers:1991p102

</td>

<td style="text-align:left;">

Sancho y la duquesa: Una nota socioliteraria

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

93

</td>

<td style="text-align:left;">

Achugar:2008wb

</td>

<td style="text-align:left;">

Counter-hegemonic language practices and ideologies Creating a new space
and value for Spanish in Southwest Texas

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

94

</td>

<td style="text-align:left;">

Lacabex:ty

</td>

<td style="text-align:left;">

Relationship between perception and production in non-native speech

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

95

</td>

<td style="text-align:left;">

Colina:2006uf

</td>

<td style="text-align:left;">

Output-to-output Correspondence and the Emergence of the Unmarked in
Spanish Plural Formation

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

96

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

97

</td>

<td style="text-align:left;">

Diehl2004

</td>

<td style="text-align:left;">

Speech perception

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

98

</td>

<td style="text-align:left;">

ocampo2003notion

</td>

<td style="text-align:left;">

On the notion of focus in spoken Spanish: An empirical approach

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

99

</td>

<td style="text-align:left;">

katz2012compression

</td>

<td style="text-align:left;">

Compression effects in English

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

100

</td>

<td style="text-align:left;">

Kohler:2002tu

</td>

<td style="text-align:left;">

Hearing sounds, understanding actions: action representation in mirror
neurons

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

101

</td>

<td style="text-align:left;">

Samuel2003

</td>

<td style="text-align:left;">

Memory and Language Lexical activation ( and other factors ) can mediate
compensation for coarticulation

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

102

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

103

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

104

</td>

<td style="text-align:left;">

Face:2009vi

</td>

<td style="text-align:left;">

Acquisition of the Spanish voiced spirants by second language learners

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

105

</td>

<td style="text-align:left;">

tsukada2011perception

</td>

<td style="text-align:left;">

The perception of Arabic and Japanese short and long vowels by native
speakers of Arabic, Japanese, and Persian

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

106

</td>

<td style="text-align:left;">

Lynch:2001vq

</td>

<td style="text-align:left;">

Social factors and language proficiency in postsecondary Spanish
immersion: Issues and implications

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

107

</td>

<td style="text-align:left;">

van2002relationship

</td>

<td style="text-align:left;">

The relationship between vowel and consonant duration in Orkney and
Shetland dialects

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

108

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

109

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

110

</td>

<td style="text-align:left;">

Kinginger:2009tc

</td>

<td style="text-align:left;">

Language learning and study abroad: A critical reading of research

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

111

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

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

112

</td>

<td style="text-align:left;">

Fowler:1986vb

</td>

<td style="text-align:left;">

An event approach to the study of speech perception from a
direct-realist perspective

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

113

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

114

</td>

<td style="text-align:left;">

Boersma:1999wf

</td>

<td style="text-align:left;">

On the need for a separate perception grammar

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

115

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

116

</td>

<td style="text-align:left;">

Neary:tk1980

</td>

<td style="text-align:left;">

On the physical interpretation of vowel quality: cinefluorographic and
acoustic evidence

</td>

<td style="text-align:left;">

VOLUME

</td>

</tr>

<tr>

<td style="text-align:right;">

117

</td>

<td style="text-align:left;">

Torreira:2011jo

</td>

<td style="text-align:left;">

Realization of voiceless stops and vowels in conversational French and
Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

118

</td>

<td style="text-align:left;">

Moyer:2008tz

</td>

<td style="text-align:left;">

Research as Practice: Linking Theory, Method, and Data

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

119

</td>

<td style="text-align:left;">

Eisend2006

</td>

<td style="text-align:left;">

Source Credibility Dimensions in Marketing Communication {} A
Generalized Solution

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

120

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

121

</td>

<td style="text-align:left;">

Botello:2010p108

</td>

<td style="text-align:left;">

El refr{'a}n como texto oral y escrito

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

122

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

123

</td>

<td style="text-align:left;">

Spolsky:2011qy

</td>

<td style="text-align:left;">

Does the United States need a language policy?

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

124

</td>

<td style="text-align:left;">

McKee:1983vb

</td>

<td style="text-align:left;">

The Effects of Intensive Language Instruction on Student Performance in
Beginning College French.

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

125

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

126

</td>

<td style="text-align:left;">

Barrios:IFybV93w

</td>

<td style="text-align:left;">

Native Language Constraints on L2 Perception

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

127

</td>

<td style="text-align:left;">

Lotto:2004wt

</td>

<td style="text-align:left;">

Mapping the task for the second language learner: The case of Japanese
acquisition of /r/ and /l/

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

128

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

129

</td>

<td style="text-align:left;">

AkahaneYamada:1996kn

</td>

<td style="text-align:left;">

Does training in speech perception modify speech production?

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

130

</td>

<td style="text-align:left;">

olarrea2012

</td>

<td style="text-align:left;">

Word Order and Information Structure

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

131

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

132

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

133

</td>

<td style="text-align:left;">

silva2004spanish

</td>

<td style="text-align:left;">

Spanish in the Southwest

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

134

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

135

</td>

<td style="text-align:left;">

Ruiz:1984rt

</td>

<td style="text-align:left;">

Orientations in language planning

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

136

</td>

<td style="text-align:left;">

Ainsworth:1972ui

</td>

<td style="text-align:left;">

Duration as a cue in the recognition of synthetic vowels

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

137

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

138

</td>

<td style="text-align:left;">

ordonez2000clausal

</td>

<td style="text-align:left;">

The clausal architecture of Spanish: a comparative study

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

139

</td>

<td style="text-align:left;">

Boersma:2001uj

</td>

<td style="text-align:left;">

Phonology-semantics interaction in OT, and its acquisition

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

140

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

141

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

142

</td>

<td style="text-align:left;">

Harris:1982vd

</td>

<td style="text-align:left;">

Syllable structure and stress in Spanish: a nonlinear analysis

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

143

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

144

</td>

<td style="text-align:left;">

Sisinni:2014vb

</td>

<td style="text-align:left;">

The perception of American English vowels by Salento Italian adult
listeners: Longitudinal development in the classroom context

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

145

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

146

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

147

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

148

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

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

149

</td>

<td style="text-align:left;">

Boersma:2008tk

</td>

<td style="text-align:left;">

Learning to perceive a smaller L2 vowel inventory: an Optimality Theory
account

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

150

</td>

<td style="text-align:left;">

Williams:2014tc

</td>

<td style="text-align:left;">

Native and Non-Native Speech Perception

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

151

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

152

</td>

<td style="text-align:left;">

Crowhurst:1992ve

</td>

<td style="text-align:left;">

Diminutives and augmentatives in Mexican Spanish: a prosodic analysis

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

153

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

154

</td>

<td style="text-align:left;">

Maclagan:2007vp

</td>

<td style="text-align:left;">

Getting fed up with our feet: Contrast maintenance and the New Zealand
English {}short{} front vowel shift

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

155

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

156

</td>

<td style="text-align:left;">

Bennett:2011uh

</td>

<td style="text-align:left;">

Neural correlates of interspecies perspective taking in the post-mortem
atlantic salmon: an argument for proper multiple comparisons correction

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

157

</td>

<td style="text-align:left;">

parrell2010b

</td>

<td style="text-align:left;">

How /b, d, g/ differ from /p, t, k/ in Spanish: A dynamic account

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

158

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

159

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

160

</td>

<td style="text-align:left;">

escudero2006phonological

</td>

<td style="text-align:left;">

The phonological and phonetic development of new vowel contrasts in
Spanish learners of English

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

161

</td>

<td style="text-align:left;">

Hay:2006wp

</td>

<td style="text-align:left;">

Factors influencing speech perception in the context of a
merger-in-progress

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

162

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

163

</td>

<td style="text-align:left;">

Ladefoged:1972wl

</td>

<td style="text-align:left;">

An auditory-motor theory of speech production

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

164

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

165

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

166

</td>

<td style="text-align:left;">

Milinski:1997tj

</td>

<td style="text-align:left;">

How to avoid seven deadly sins in the study of behavior

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

167

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

168

</td>

<td style="text-align:left;">

Cross:2001ur

</td>

<td style="text-align:left;">

Music, mind and evolution

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

169

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

170

</td>

<td style="text-align:left;">

Llisterri:1995wb

</td>

<td style="text-align:left;">

Relationships between speech production and speech perception in a
second language

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

171

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

172

</td>

<td style="text-align:left;">

hualde2008lexical

</td>

<td style="text-align:left;">

Lexical tone and stress in Goizueta Basque

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

173

</td>

<td style="text-align:left;">

bataller2010making

</td>

<td style="text-align:left;">

Making a request for a service in Spanish: Pragmatic development in the
study abroad setting

</td>

<td style="text-align:left;">

JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

174

</td>

<td style="text-align:left;">

Correa:vo

</td>

<td style="text-align:left;">

Heritage Language Learner Programs and Life after the Classroom{}A Not
So Critical Approach

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

175

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

176

</td>

<td style="text-align:left;">

segalowitz2004comparison

</td>

<td style="text-align:left;">

A Comparison of Spanish Second Language Acquisition in Two Different
Learning Contexts: Study Abroad and the Domestic Classroom.

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

177

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

178

</td>

<td style="text-align:left;">

Lee:2011cg

</td>

<td style="text-align:left;">

An OT account of the precedence relationship between perception and
production in the acquisition of English stress

</td>

<td style="text-align:left;">

PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

179

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

180

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

181

</td>

<td style="text-align:left;">

chang2011production

</td>

<td style="text-align:left;">

Production of phonetic and phonological contrast by heritage speakers of
Mandarin

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

182

</td>

<td style="text-align:left;">

beckman2011rate

</td>

<td style="text-align:left;">

Rate effects on Swedish VOT: Evidence for phonological overspecification

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

183

</td>

<td style="text-align:left;">

Winter:2011uz

</td>

<td style="text-align:left;">

Pseudoreplication in phonetic research

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

184

</td>

<td style="text-align:left;">

Ito:2009gf

</td>

<td style="text-align:left;">

Perception of allophonic cues to English word boundaries by Japanese
second language learners of English

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

185

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

186

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

187

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

188

</td>

<td style="text-align:left;">

mehotcheva2010after

</td>

<td style="text-align:left;">

After the fiesta is over: foreign language attrition of Spanish in Dutch
and German Erasmus Students

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

189

</td>

<td style="text-align:left;">

repp1984categorical

</td>

<td style="text-align:left;">

Categorical perception: Issues, methods, findings

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

190

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

191

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

192

</td>

<td style="text-align:left;">

Magloire:2000ud

</td>

<td style="text-align:left;">

A cross-language comparison of speaking rate effects on the production
of voice onset time in English and Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

193

</td>

<td style="text-align:left;">

long1996role

</td>

<td style="text-align:left;">

The role of the linguistic environment in second language acquisition

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

194

</td>

<td style="text-align:left;">

hernandez2010

</td>

<td style="text-align:left;">

The impact of bilingualism on the executive control and orienting
networks of attention

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

195

</td>

<td style="text-align:left;">

Merker:2000uv

</td>

<td style="text-align:left;">

Synchronous chorusing and human origins

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

196

</td>

<td style="text-align:left;">

Harris:1992tr

</td>

<td style="text-align:left;">

With respect to metrical constituents in Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

197

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

198

</td>

<td style="text-align:left;">

Prieto:2007tw

</td>

<td style="text-align:left;">

The segmental anchoring hypothesis revisited: Syllable structure and
speech rate effects on peak timing in Spanish

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

199

</td>

<td style="text-align:left;">

OrtegaLlebaria:2001wl

</td>

<td style="text-align:left;">

Auditory-visual L2 speech perception: Effects of visual cues and
acoustic-phonetic context for Spanish learners of English

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

200

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

201

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

202

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

203

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

204

</td>

<td style="text-align:left;">

sundara2005acoustic

</td>

<td style="text-align:left;">

Acoustic-phonetics of coronal stops: A cross-language study of Canadian
English and Canadian French

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

205

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

206

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

207

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

208

</td>

<td style="text-align:left;">

lisker1967

</td>

<td style="text-align:left;">

Some effects of context on Voice Onset Time in English stops

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

209

</td>

<td style="text-align:left;">

Nadeu:2012ht

</td>

<td style="text-align:left;">

Lenition and Phonemic Overlap in Rome Italian

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

210

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

211

</td>

<td style="text-align:left;">

Hualde:1989ve

</td>

<td style="text-align:left;">

Silabeo y estructura morf{'e}mica en espa{~n}ol

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

212

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

213

</td>

<td style="text-align:left;">

McMurray:2010ep

</td>

<td style="text-align:left;">

Unmasking the acoustic effects of vowel-to-vowel coarticulation: A
statistical modeling approach

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

214

</td>

<td style="text-align:left;">

Amengual

</td>

<td style="text-align:left;">

Bilingual language profile: an easy-to-use instrument to assess
bilingualism

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

215

</td>

<td style="text-align:left;">

mueller2011learners

</td>

<td style="text-align:left;">

Learners’ identity negotiations and beliefs about pronunciation in study
abroad contexts

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

216

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

217

</td>

<td style="text-align:left;">

schneider2011reaction

</td>

<td style="text-align:left;">

Reaction time and decision difficulty in the perception of intonation

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

218

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

219

</td>

<td style="text-align:left;">

Fowler:2005ih

</td>

<td style="text-align:left;">

The relation of speech perception and speech production

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

220

</td>

<td style="text-align:left;">

Wiley:2008wb

</td>

<td style="text-align:left;">

Emacs and R Integration via ESS: Installation How-To

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

221

</td>

<td style="text-align:left;">

Tyler:2009dq

</td>

<td style="text-align:left;">

Cross-language differences in cue use for speech segmentation

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

222

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

223

</td>

<td style="text-align:left;">

bajuniemi2013teaching

</td>

<td style="text-align:left;">

Teaching Intervention on the Pronunciation of Spanish Intervocalic /d/

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

224

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

225

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

226

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

227

</td>

<td style="text-align:left;">

Escudero:2007wn

</td>

<td style="text-align:left;">

Multilingual sound perception and word recognition

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

228

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

229

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

230

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

231

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

232

</td>

<td style="text-align:left;">

Hualde:1997tq

</td>

<td style="text-align:left;">

Spanish /i/ and related sounds: An exercise in phonemic analysis

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

233

</td>

<td style="text-align:left;">

Rogers2006

</td>

<td style="text-align:left;">

Effects of bilingualism , noise , and reverberation on speech perception
by listeners with normal hearing

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

234

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

235

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

236

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

237

</td>

<td style="text-align:left;">

Nakagawa2013

</td>

<td style="text-align:left;">

A general and simple method for obtaining R2 from generalized linear
mixed-effects models

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

238

</td>

<td style="text-align:left;">

praat

</td>

<td style="text-align:left;">

Praat: doing phonetics by computer

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

239

</td>

<td style="text-align:left;">

BlasArroyo:1995wq

</td>

<td style="text-align:left;">

De nuevo el espa{~n}ol y el catal{'a}n, juntos y en contraste: estudio
de actitudes ling{"u}{'}sticas

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

240

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

241

</td>

<td style="text-align:left;">

RalloFabra:2005ub

</td>

<td style="text-align:left;">

Predicting ease of acquisition of L2 speech sounds. a perceived
dissimilarity test

</td>

<td style="text-align:left;">

VOLUME

</td>

</tr>

<tr>

<td style="text-align:right;">

242

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

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

243

</td>

<td style="text-align:left;">

Lisker:1964wl

</td>

<td style="text-align:left;">

A Cross-language Study of Voicing in Initial Stops: Acoustical
Measurements

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

244

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

245

</td>

<td style="text-align:left;">

Smalley:1953tq

</td>

<td style="text-align:left;">

Phonemic Rhythm in Comanche

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

Casagrande:1989wz

</td>

<td style="text-align:left;">

Comanche Linguistic Acculturation

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

247

</td>

<td style="text-align:left;">

hualde2002intonation

</td>

<td style="text-align:left;">

Intonation in Spanish and the other Ibero-Romance languages: overview
and status quaestionis

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

248

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

249

</td>

<td style="text-align:left;">

Gulian:2007vc

</td>

<td style="text-align:left;">

Supervision hampers distributional learning of vowel contrasts

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

250

</td>

<td style="text-align:left;">

Saldivar:1980p121

</td>

<td style="text-align:left;">

Don Quijote’s Metaphors and the Grammar of Proper Language

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

251

</td>

<td style="text-align:left;">

mccarthy2007

</td>

<td style="text-align:left;">

What is Optimality Theory?

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

252

</td>

<td style="text-align:left;">

diaz2012individual

</td>

<td style="text-align:left;">

Individual differences in late bilinguals’ L2 phonological processes:
From acoustic-phonetic analysis to lexical access

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

253

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

254

</td>

<td style="text-align:left;">

swain1995three

</td>

<td style="text-align:left;">

Three functions of output in second language learning

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

255

</td>

<td style="text-align:left;">

Recasens:2010vc

</td>

<td style="text-align:left;">

Lingual kinematics and coarticulation for alveolopalatal and velar
consonants in Catalan

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

256

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

257

</td>

<td style="text-align:left;">

simonet2011intonational

</td>

<td style="text-align:left;">

Intonational convergence in language contact: Utterance-final F0
contours in Catalan{}Spanish early bilinguals

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

258

</td>

<td style="text-align:left;">

Morrison:2007ur

</td>

<td style="text-align:left;">

A cross-dialect comparison of Peninsula-and Peruvian-Spanish vowels

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

259

</td>

<td style="text-align:left;">

Elvin:2014ua

</td>

<td style="text-align:left;">

Perception of Brazilian Portuguese Vowels by Australian English and
Spanish Listeners

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

260

</td>

<td style="text-align:left;">

Piske:2002to

</td>

<td style="text-align:left;">

The production of English vowels by fluent early and late
Italian-English bilinguals

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

261

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

262

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

263

</td>

<td style="text-align:left;">

LoureiroRodriguez:2008ua

</td>

<td style="text-align:left;">

Conflicting values at a conflicting age

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

264

</td>

<td style="text-align:left;">

Guion:1998wq

</td>

<td style="text-align:left;">

The role of perception in the sound change of velar palatalization

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

265

</td>

<td style="text-align:left;">

Chomsky:1959tf

</td>

<td style="text-align:left;">

Review of B.F. Skinner, Verbal Behavior

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

266

</td>

<td style="text-align:left;">

tuinman2011perception

</td>

<td style="text-align:left;">

Perception of intrusive/r/in English by native, cross-language and
cross-dialect listeners

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

267

</td>

<td style="text-align:left;">

Woolard:1997uj

</td>

<td style="text-align:left;">

Between friends: Gender, peer group structure, and bilingualism in urban
Catalonia

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

268

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

269

</td>

<td style="text-align:left;">

Montaruli:2011ue

</td>

<td style="text-align:left;">

Identity, language, and ethnic relations in the Bilingual Autonomous
Communities of Spain

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

270

</td>

<td style="text-align:left;">

Ernestus:2011ky

</td>

<td style="text-align:left;">

Vowel elision in casual French: The case of vowel/e/in the word
c’{'e}tait

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

271

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

272

</td>

<td style="text-align:left;">

Ramachandra:2009tx

</td>

<td style="text-align:left;">

The Role of Mirror Neurons in Processing Vocal Emotions: Evidence from
Psychophysiological Data

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

273

</td>

<td style="text-align:left;">

Hualde:1989vr

</td>

<td style="text-align:left;">

Autosegmental and metrical spreading in the vowel-harmony systems of
northwestern Spain

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

274

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

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

275

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

276

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

277

</td>

<td style="text-align:left;">

Munro1996

</td>

<td style="text-align:left;">

The effects of age of second language learning on the production of
English vowels

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

278

</td>

<td style="text-align:left;">

cutler2008consonant

</td>

<td style="text-align:left;">

Consonant identification in noise by native and non-native listeners:
Effects of local context

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

279

</td>

<td style="text-align:left;">

Lombardi:2003vt

</td>

<td style="text-align:left;">

Second language data and constraints on manner: explaining substitutions
for the English interdentals

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

280

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

281

</td>

<td style="text-align:left;">

Colina:2006ug

</td>

<td style="text-align:left;">

No double plurals in dominican spanish: an optimality-theoretic account

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

282

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

283

</td>

<td style="text-align:left;">

gordon2001memory

</td>

<td style="text-align:left;">

Memory interference during language processing

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

284

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

285

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

286

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

287

</td>

<td style="text-align:left;">

Flege1999b

</td>

<td style="text-align:left;">

Age Constraints on Second-Language Acquisition

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

288

</td>

<td style="text-align:left;">

forrest1988statistical

</td>

<td style="text-align:left;">

Statistical analysis of word-initial voiceless obstruents: Preliminary
data

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

289

</td>

<td style="text-align:left;">

PallierGalles1999

</td>

<td style="text-align:left;">

Phonological representations and repetition priming

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

290

</td>

<td style="text-align:left;">

Bohn:1993tc

</td>

<td style="text-align:left;">

Perceptual switching in Spanish/English bilinguals: Evidence for
universal factors in stop voicing judgments

</td>

<td style="text-align:left;">

VOLUME

</td>

</tr>

<tr>

<td style="text-align:right;">

291

</td>

<td style="text-align:left;">

Recasens:2009vc

</td>

<td style="text-align:left;">

An articulatory investigation of lingual coarticulatory resistance and
aggressiveness for consonants and vowels in Catalan

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

292

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

293

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

294

</td>

<td style="text-align:left;">

Nadeu:2011vx

</td>

<td style="text-align:left;">

Consonant lenition and phonological recategorization

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

295

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

296

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

297

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

298

</td>

<td style="text-align:left;">

felix2013refusing

</td>

<td style="text-align:left;">

Refusing in L2 Spanish: The effects of the context of learning during a
short-term study abroad program

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

299

</td>

<td style="text-align:left;">

simonet2011vx

</td>

<td style="text-align:left;">

Consonant lenition and phonological recategorization

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

300

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

301

</td>

<td style="text-align:left;">

simonet2011

</td>

<td style="text-align:left;">

Technology in Phonetic Science: Setting Up a Basic Phonetics Laboratory.

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

302

</td>

<td style="text-align:left;">

Harris:1980ua

</td>

<td style="text-align:left;">

Relationships between speech perception and speech production in normal
hearing and hearing impaired subjects

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

303

</td>

<td style="text-align:left;">

Mackain1981

</td>

<td style="text-align:left;">

Categorical Perception of English /r/ and /l/ by Japanese Bilinguals

</td>

<td style="text-align:left;">

PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

304

</td>

<td style="text-align:left;">

Colina:1997uu

</td>

<td style="text-align:left;">

Identity constraints and Spanish resyllabification\* 1

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

305

</td>

<td style="text-align:left;">

Bickerton:1996vf

</td>

<td style="text-align:left;">

I chat, thereby I groom

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

306

</td>

<td style="text-align:left;">

MartinezGil:1997vt

</td>

<td style="text-align:left;">

Obstruent vocalization in Chilean Spanish: A serial versus a
constraint-based approach

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

307

</td>

<td style="text-align:left;">

Grice:1961ud

</td>

<td style="text-align:left;">

The Causal Theory of Perception

</td>

<td style="text-align:left;">

NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

308

</td>

<td style="text-align:left;">

Ohala:1986tk

</td>

<td style="text-align:left;">

Against the direct realist view of speech perception.

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

309

</td>

<td style="text-align:left;">

Piske2001

</td>

<td style="text-align:left;">

Factors affecting degree of foreign accent in an L2 : a review

</td>

<td style="text-align:left;">

VOLUME, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

310

</td>

<td style="text-align:left;">

chladkova2011context

</td>

<td style="text-align:left;">

Context-specific acoustic differences between Peruvian and Iberian
Spanish vowels

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

311

</td>

<td style="text-align:left;">

rivera2000intraethnic

</td>

<td style="text-align:left;">

Intraethnic attitudes among Hispanics in a Northern California community

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

312

</td>

<td style="text-align:left;">

Alispahic:2014us

</td>

<td style="text-align:left;">

Difficulty in discriminating non-native vowels: Are Dutch vowels easier
for Australian English than Spanish listeners?

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

313

</td>

<td style="text-align:left;">

escudero2009cross

</td>

<td style="text-align:left;">

A cross-dialect acoustic description of vowels: Brazilian and European
Portuguese

</td>

<td style="text-align:left;">

VOLUME, PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

314

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

315

</td>

<td style="text-align:left;">

Waller:aa

</td>

<td style="text-align:left;">

Documenting and evaluating {Data} {Science} contributions in academic
promotion in {Departments} of {Statistics} and {Biostatistics

</td>

<td style="text-align:left;">

YEAR, VOLUME, PAGES, NUMBER, JOURNAL

</td>

</tr>

<tr>

<td style="text-align:right;">

316

</td>

<td style="text-align:left;">

Escudero:2014aa

</td>

<td style="text-align:left;">

Magnitude of phonetic distinction predicts success at early word
learning in native and non-native accents

</td>

<td style="text-align:left;">

PAGES, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

317

</td>

<td style="text-align:left;">

McCarthy:2014aa

</td>

<td style="text-align:left;">

Speech {Perception} and {Production} by {Sequential} {Bilingual}
{Children}: {A} {Longitudinal} {Study} of {Voice} {Onset} {Time}
{Acquisition

</td>

<td style="text-align:left;">

VOLUME, NUMBER

</td>

</tr>

<tr>

<td style="text-align:right;">

318

</td>

<td style="text-align:left;">

Antoniou:2019aa

</td>

<td style="text-align:left;">

The {Advantages} of {Bilingualism} {Debate

</td>

<td style="text-align:left;">

PAGES

</td>

</tr>

<tr>

<td style="text-align:right;">

319

</td>

<td style="text-align:left;">

Wilson:2014aa

</td>

<td style="text-align:left;">

Using ultrasound for teaching and researching articulation

</td>

<td style="text-align:left;">

YEAR

</td>

</tr>

</tbody>

</table>
