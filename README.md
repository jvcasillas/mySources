
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
bib %>% 
  filter(., CATEGORY == "ARTICLE") %>% 
  select(., CATEGORY, YEAR, TITLE, PAGES, JOURNAL, AUTHOR) %>% 
  filter_all(., any_vars(is.na(.))) %>% 
  select(., AUTHOR, TITLE) %>% 
  kable(., output = 'html')
```

| AUTHOR                                                                                                          | TITLE                                                                                                                                                              |
| :-------------------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Colina, Sonia                                                                                                   | Clases naturales Ej.                                                                                                                                               |
| Zahn, Ista                                                                                                      | Learning to Sweave in APA style                                                                                                                                    |
| Hualde, Jos{'e} Ignacio                                                                                         | Procesos conson{'a}nticos y estructuras geom{'e}tricas en espa{~n}ol                                                                                               |
| c(“Sundara, M”, “Polka, L”, “Molnar, M”)                                                                        | Development of coronal stop perception: Bilingual infants keep pace with their monolingual peers                                                                   |
| Bosque, Ignacio                                                                                                 | La negaci{'o}n y el principio de las categor{'}as vac{'}as                                                                                                         |
| Colina, Sonia                                                                                                   | Repaso sistema fonol{'o}gico                                                                                                                                       |
| Schertz, Jessamyn                                                                                               | Exaggeration of featural contrasts in clarifications of misheard speech in English                                                                                 |
| Dunbar, Robin I M                                                                                               | Coevolution of neocortex size, group size and language in humans                                                                                                   |
| c(“Hickok, G”, “Buchsbaum, B”, “Humphries, C”)                                                                  | Auditory-motor interaction revealed by fMRI: speech, music, and working memory in area Spt                                                                         |
| MacKay, D G                                                                                                     | Asynlmetries in the Relationsh ip Between Speech Perception and Production                                                                                         |
| c(“Lakens, Dani{\\"e}l”, “Scheel, Anne M”, “Isager, Peder M”)                                                   | Equivalence Testing for Psychological Research: A Tutorial                                                                                                         |
| Morrison, Geoffrey Stewart                                                                                      | L1-Spanish Speakers’ Acquisition of the English/i/{}/I/Contrast: Duration-based Perception is Not the Initial Developmental Stage                                  |
| Michnowicz, J                                                                                                   | El habla de Yucat{'a}m: Final \[m\] in a Dialect in Contact                                                                                                        |
| c(“Ringen, C”, “Suomi, Kari”)                                                                                   | The voicing contrast in Fenno-Swedish stops                                                                                                                        |
| Vogel, Ralf                                                                                                     | xyling {} LATEX macros for linguistic graphics using the xypic module                                                                                              |
| Plichta, Bartlomiej                                                                                             | AKUSTYK for Praat                                                                                                                                                  |
| Casillas, Joseph Vincent                                                                                        | EL USO DE LOS REFRANES EN                                                                                                                                          |
| Correa, Maite                                                                                                   | Metalinguistic knowledge and the acquisition of the Spanish subjunctive by learners at three proficiency levels                                                    |
| Healy, Kieran                                                                                                   | Choosing Your Workflow Applications                                                                                                                                |
| Teetor, Paul                                                                                                    | R~Cookbook                                                                                                                                                         |
| c(“Caramazza, A”, “Yeni-Komshian, GH”, “Zurif, E B”, “Carbone, E”)                                              | The acquisition of a new phonological contrast: The case of stop consonants in French-English bilinguals                                                           |
| Peng, Roger D                                                                                                   | R Programming for Data Science                                                                                                                                     |
| Peng, Roger D                                                                                                   | Exploratory Data Analysis with R                                                                                                                                   |
| Pakin, Scott                                                                                                    | The Comprehensive LaTeX Symbol List                                                                                                                                |
| c(“van Leussen, J W”, “Williams, D”, “Escudero, Paola”)                                                         | Acoustic properties of Dutch steady-state vowels: Contextual effects and a comparison with previous studies                                                        |
| Myers, Scott                                                                                                    | Vowel duration and neutralization of vowel length contrasts in Kinyarwanda                                                                                         |
| Colina, Sonia                                                                                                   | Conceptos estructuralistas - terminlolog{'}a                                                                                                                       |
| Bakovi{'c}, E                                                                                                   | Hiatus resolution and incomplete identity                                                                                                                          |
| Colina, Sonia                                                                                                   | Rasgos distintivos lecture                                                                                                                                         |
| Olarrea, Antxon                                                                                                 | problema1                                                                                                                                                          |
| Hanssen, Ferdy                                                                                                  | Installing fonts in LaTEX: a user{}s experience                                                                                                                    |
| Drager, Katie                                                                                                   | A Novice{}s Guide to Understanding Mixed Effects Models                                                                                                            |
| Anderson, Victoria B                                                                                            | Static palatography for language fieldwork                                                                                                                         |
| Colina, Sonia                                                                                                   | Trancripci{'o}n de laterales - Ej                                                                                                                                  |
| Bullock, BE                                                                                                     | Kreyol incursions into Dominican Spanish                                                                                                                           |
| Colina, Sonia                                                                                                   | Neutralizaci{'o}n de constrastres - Lecture                                                                                                                        |
| Alonso, A                                                                                                       | Una ley fonol{'o}gica del espa{~n}ol                                                                                                                               |
| Harris, James W.                                                                                                | La espirantizaci{'o}n en castellano y la representaci{'o}n fonol{'o}gica autosegmental                                                                             |
| Xie, Yihui                                                                                                      | knitr: A General-Purpose Tool for Dynamic Report Generation in R                                                                                                   |
| Pallier, Christophe                                                                                             | Premiers pas en Emacs                                                                                                                                              |
| McKee, E                                                                                                        | The Effects of Intensive Language Instruction on Student Performance in Beginning College French. (Report no. FL013910)                                            |
| c(“Jaeger, T Florian”, “Graff, Peter”, “Croft, William”, “Pontillo, Daniel”)                                    | Mixed effect models for genetic and areal dependencies in linguistic typology                                                                                      |
| Llisterri, Joaquim                                                                                              | Fon{'e}tica y fonolog{'}a del espa{~n}ol                                                                                                                           |
| c(“van Leyden, Klaske”, “van Heuven, Vincent J”)                                                                | On the prosody of Orkney and Shetland dialects                                                                                                                     |
| Myers, Scott                                                                                                    | F0 timing in Kinyarwanda                                                                                                                                           |
| c(“Shively, R L”, “Cohen, A D”)                                                                                 | Development of Spanish requests and apologies during study abroad                                                                                                  |
| Pallier, Christophe                                                                                             | TEX et BibTEX a\` la puissance Emacs                                                                                                                               |
| R Core Team                                                                                                     | R: A Language and Environment for Statistical Computing                                                                                                            |
| Dalmaijer, Edwin                                                                                                | Is the low-cost EyeTribe eye tracker any good for research?                                                                                                        |
| Ricci, Vito                                                                                                     | R FUNCTIONS FOR REGRESSION ANALYSIS                                                                                                                                |
| Dunbar, Robin I M                                                                                               | Theory of mind and the evolution of language                                                                                                                       |
| Burns, Patrick                                                                                                  | The R Inferno                                                                                                                                                      |
| Harris, James W.                                                                                                | The exponence of gender in Spanish                                                                                                                                 |
| Escudero, Paola                                                                                                 | Native and Non-Native Speech Perception                                                                                                                            |
| Achugar, Mariana                                                                                                | Counter-hegemonic language practices and ideologies Creating a new space and value for Spanish in Southwest Texas                                                  |
| Lacabex, E G                                                                                                    | Relationship between perception and production in non-native speech                                                                                                |
| Colina, Sonia                                                                                                   | Output-to-output Correspondence and the Emergence of the Unmarked in Spanish Plural Formation                                                                      |
| Gray, James                                                                                                     | Textmate Power Editing for the Mac (Pragmatic, 2007)                                                                                                               |
| Katz, J                                                                                                         | Compression effects in English                                                                                                                                     |
| c(“Kohler, E”, “Keysers, C”, “Umilta, MA”, “Fogassi, L”)                                                        | Hearing sounds, understanding actions: action representation in mirror neurons                                                                                     |
| Colina, Sonia                                                                                                   | Inventario de los fonemas y alofonos del espanol estandar                                                                                                          |
| Colina, Sonia                                                                                                   | SILABIFICACI{'O}N Y LA TEOR{'I}A DE LA OPTIMIDAD                                                                                                                   |
| Face, Timothy L                                                                                                 | Acquisition of the Spanish voiced spirants by second language learners                                                                                             |
| c(“Klee, C A”, “Lynch, A”, “Tedick, D J”)                                                                       | Social factors and language proficiency in postsecondary Spanish immersion: Issues and implications                                                                |
| van Leyden, Klaske                                                                                              | The relationship between vowel and consonant duration in Orkney and Shetland dialects                                                                              |
| Pascal van Lieshout, Ph D                                                                                       | Short Tutorial                                                                                                                                                     |
| c(“Peterson, Gordon E”, “Barney, Harold L”)                                                                     | Control Methods Used in a Study of the Vowels                                                                                                                      |
| Morrison, Geoffrey Stewart                                                                                      | Towards a Quantitative Speech Learning Model (QSLM)                                                                                                                |
| Kinginger, Celeste                                                                                              | Language learning and study abroad: A critical reading of research                                                                                                 |
| Faraway, Julian                                                                                                 | Using R                                                                                                                                                            |
| SCARPACE, DANIEL                                                                                                | The Acquisition of Resyllabification in Spanish by English Speakers                                                                                                |
| c(“Ernestus, Mirjam”, “Torreira, Francisco”)                                                                    | Realization of voiceless stops and vowels in conversational French and Spanish                                                                                     |
| Moyer, Melissa G                                                                                                | Research as Practice: Linking Theory, Method, and Data                                                                                                             |
| Botello, Maria Teresa Perez                                                                                     | El refr{'a}n como texto oral y escrito                                                                                                                             |
| Tremblay, Marie-Claude                                                                                          | Cross-Linguistic Influence in Third Language Acquisition: The Role of L2 Proficiency and L2 Exposure                                                               |
| McKee, E                                                                                                        | The Effects of Intensive Language Instruction on Student Performance in Beginning College French.                                                                  |
| Figueredo, Aurelio                                                                                              | Psych 507a                                                                                                                                                         |
| Barrios, Shannon                                                                                                | Native Language Constraints on L2 Perception                                                                                                                       |
| c(“Lotto, A J”, “Sato, M”, “Diehl, Randy L”)                                                                    | Mapping the task for the second language learner: The case of Japanese acquisition of /r/ and /l/                                                                  |
| Murphy, Shirin                                                                                                  | Second Language Transfer During Third Language Acquisition                                                                                                         |
| Kawahara, Shigeto                                                                                               | Praat Scripting for dummies                                                                                                                                        |
| Colina, Sonia                                                                                                   | Acentuaci{'o}n                                                                                                                                                     |
| Sternefeld, Wolfgand                                                                                            | linguex.sty Documentation                                                                                                                                          |
| Pease, Emma                                                                                                     | Tree Macros                                                                                                                                                        |
| Ord{'o}{~n}ez, Francisco                                                                                        | The clausal architecture of Spanish: a comparative study                                                                                                           |
| Hutchins, Sean                                                                                                  | The Linked Dual Representation model of vocal perception and production                                                                                            |
| Harris, James W.                                                                                                | Syllable structure and stress in Spanish: a nonlinear analysis                                                                                                     |
| Colina, Sonia                                                                                                   | Transcripci{'o}n de nasales - EJ                                                                                                                                   |
| Casillas, Joseph Vincent                                                                                        | La vibrante m{'u}ltiple intervoc{'a}lica. Los ejercicios de canto como ayuda a su pronunciaci{'o}n en espa{~n}ol                                                   |
| Torreira, Francisco                                                                                             | Aspirated Stops in Andalusian Spanish                                                                                                                              |
| Soskuthy, Marton                                                                                                | Generalised additive mixed models for dynamic analysis in linguistics: a practical introduction                                                                    |
| c(“Boersma, Paul”, “Escudero, Paola”, “Hayes, R”)                                                               | Learning abstract phonological from auditory phonetic categories: An integrated model for the acquisition of language-specific sound categories                    |
| c(“Williams, D”, “Escudero, Paola”)                                                                             | Native and Non-Native Speech Perception                                                                                                                            |
| Morrison, Geoffrey Stewart                                                                                      | Manuscript: DE070483 Revised Date: 28 January 2008L1-Spanish speakers{} acquisition of the English                                                                 |
| Crowhurst, MJ                                                                                                   | Diminutives and augmentatives in Mexican Spanish: a prosodic analysis                                                                                              |
| Maclagan, M                                                                                                     | Getting fed up with our feet: Contrast maintenance and the New Zealand English {}short{} front vowel shift                                                         |
| H, Swaroop C                                                                                                    | A Byte of Python                                                                                                                                                   |
| Parrell, Benjamin                                                                                               | How /b, d, g/ differ from /p, t, k/ in Spanish: A dynamic account                                                                                                  |
| Gillespie, Maureen                                                                                              | Categorical Variables in Regression Analyses                                                                                                                       |
| c(“Siskind, Jeffrey Mark”, “Dimitriadis, Alexis”)                                                               | Qtree, a LATEX tree-drawing package                                                                                                                                |
| c(“Hay, Jennifer”, “Warren, P”)                                                                                 | Factors influencing speech perception in the context of a merger-in-progress                                                                                       |
| Zivanovic, Saso                                                                                                 | Forest: a pgf/TikZ-based package for drawing linguistic trees                                                                                                      |
| Silva, Thais Crist{'o}faro                                                                                      | Fon{'e}tica e fonologia do portugu{^e}s                                                                                                                            |
| Leisch, Friedrich                                                                                               | Sweave users manual                                                                                                                                                |
| Cross, I                                                                                                        | Music, mind and evolution                                                                                                                                          |
| c(“Ernestus, Mirjam”, “Warner, Natasha”)                                                                        | An introduction to reduced pronunciation variants                                                                                                                  |
| Sanders, Nathan                                                                                                 | Documentation for the                                                                                                                                              |
| c(“Hualde, Jos{\\’e} Ignacio”, “Lujanbio, Oihana”)                                                              | Lexical tone and stress in Goizueta Basque                                                                                                                         |
| Bataller, R                                                                                                     | Making a request for a service in Spanish: Pragmatic development in the study abroad setting                                                                       |
| Correa, Maite                                                                                                   | Heritage Language Learner Programs and Life after the Classroom{}A Not So Critical Approach                                                                        |
| Caffo, Brian                                                                                                    | Developing Data Products in R                                                                                                                                      |
| Colina, Sonia                                                                                                   | Tabla de consonantes                                                                                                                                               |
| c(“Lee, Shinsook”, “Cho, Mi-Hui”)                                                                               | An OT account of the precedence relationship between perception and production in the acquisition of English stress                                                |
| Meijer, Erik                                                                                                    | The apacite package                                                                                                                                                |
| c(“McMurray, Bob”, “Beckman, Jill”, “Helgason, P”, “Ringen, C”)                                                 | Rate effects on Swedish VOT: Evidence for phonological overspecification                                                                                           |
| Winter, Bodo                                                                                                    | Pseudoreplication in phonetic research                                                                                                                             |
| Simonet, Miquel                                                                                                 | Introduction to R                                                                                                                                                  |
| Manning, Christopher                                                                                            | Logistic regression (with R)                                                                                                                                       |
| Baker, Adam                                                                                                     | The ot-tableau package                                                                                                                                             |
| Mehotcheva, Teodora H                                                                                           | After the fiesta is over: foreign language attrition of Spanish in Dutch and German Erasmus Students                                                               |
| Jaeger, T Florian                                                                                               | Praat scripting tutorial Basics                                                                                                                                    |
| Fukui, Rei                                                                                                      | TIPA Manual                                                                                                                                                        |
| Magloire, J                                                                                                     | A cross-language comparison of speaking rate effects on the production of voice onset time in English and Spanish                                                  |
| Merker, B                                                                                                       | Synchronous chorusing and human origins                                                                                                                            |
| Harris, James W.                                                                                                | With respect to metrical constituents in Spanish                                                                                                                   |
| c(“Prieto, Pilar”, “Torreira, Francisco”)                                                                       | The segmental anchoring hypothesis revisited: Syllable structure and speech rate effects on peak timing in Spanish                                                 |
| c(“Ortega-Llebaria, M”, “Faulkner, A”, “Hazan, V”)                                                              | Auditory-visual L2 speech perception: Effects of visual cues and acoustic-phonetic context for Spanish learners of English                                         |
| Colina, Sonia                                                                                                   | La s{'}laba handout                                                                                                                                                |
| Hattori, Kota                                                                                                   | Examination of the Relationship between L2 Perception and Production: An Investigation of English /r/-/l/ Perception and Production by Adult Japanese Speakers     |
| Locklin, Jason                                                                                                  | R notes for experimental psychology                                                                                                                                |
| Schmidt, Walter                                                                                                 | Font selection in LATEX: The most frequently asked questions                                                                                                       |
| Sundara, M                                                                                                      | Acoustic-phonetics of coronal stops: A cross-language study of Canadian English and Canadian French                                                                |
| Colina, Sonia                                                                                                   | Acentuaci{'o}n - handout08                                                                                                                                         |
| c(“Giannakopoulou, A”, “Uther, M”, “Ylinen, S”)                                                                 | Enhanced plasticity in spoken language acquisition for child learners: Evidence from phonetic training studies in child and adult learners of English              |
| Gorman, Kyle                                                                                                    | On VARBRUL {} Or, The Spirit of {}74                                                                                                                               |
| c(“Nadeu, Marianna”, “Hualde, Jos{\\’e} Ignacio”)                                                               | Lenition and Phonemic Overlap in Rome Italian                                                                                                                      |
| Hualde, Jos{'e} Ignacio                                                                                         | Silabeo y estructura morf{'e}mica en espa{~n}ol                                                                                                                    |
| Lipski, J                                                                                                       | Aspects of Ecuadorian vowel reduction                                                                                                                              |
| c(“Cole, J”, “McMurray, Bob”, “Munson, Cheyenne”, “Linebaugh, G”)                                               | Unmasking the acoustic effects of vowel-to-vowel coarticulation: A statistical modeling approach                                                                   |
| c(“Birdsong, D”, “Gertken, L M”, “Amengual, M”)                                                                 | Bilingual language profile: an easy-to-use instrument to assess bilingualism                                                                                       |
| Mueller, Mareike                                                                                                | Learners’ identity negotiations and beliefs about pronunciation in study abroad contexts                                                                           |
| Llisterri, Joaquim                                                                                              | Las tecnolog{'}as del habla                                                                                                                                        |
| c(“Laks, John Goldsmith”, “Bernard”)                                                                            | Generative phonology: its origins, its principles, and its successors                                                                                              |
| Wiley, Caroline R H                                                                                             | Emacs and R Integration via ESS: Installation How-To                                                                                                               |
| c(“Oetiker, Tobias”, “Partl, Hubert”, “Hyna, Irene”, “Schlegl, Elizabeth”)                                      | The Not So Short Introduction to LATEX2\(\varepsilon\)                                                                                                             |
| Bajuniemi, Abby                                                                                                 | Teaching Intervention on the Pronunciation of Spanish Intervocalic /d/                                                                                             |
| Colina, Sonia                                                                                                   | Cuadro de rasgos                                                                                                                                                   |
| Hirschorn, Philip                                                                                               | Using the exam document class                                                                                                                                      |
| Moore, Ryan T                                                                                                   | How to Insert BibTEX Entries into a CV, Syllabus, . . .                                                                                                            |
| Escudero, Paola                                                                                                 | Multilingual sound perception and word recognition                                                                                                                 |
| Colina, Sonia                                                                                                   | Rasgos distintivos-summary fall 2010                                                                                                                               |
| Park, Hanyong                                                                                                   | Detecting foreign accent in monosyllables: The role of L1 phonotactics                                                                                             |
| Hualde, Jos{'e} Ignacio                                                                                         | Spanish /i/ and related sounds: An exercise in phonemic analysis                                                                                                   |
| Ernestus, Mirjam                                                                                                | PartB2\_ForeignCasualSpeech                                                                                                                                        |
| Colina, Sonia                                                                                                   | Transcripci{'o}n EJ - Hualde et al                                                                                                                                 |
| c(“Boersma, Paul”, “Weenink, David”)                                                                            | Praat: doing phonetics by computer                                                                                                                                 |
| c(“Mertz, Andrew”, “Slough, William”)                                                                           | Beamer by Example                                                                                                                                                  |
| Escudero, Paola                                                                                                 | Developmental patterns in the adult L2 acquisition of new contrasts: the acoustic cue weighting in the perception of Scottish tense/lax vowels by Spanish speakers |
| c(“Burgaleta, Miguel”, “Baus, Cristina”, “D{\\’\\i}az, Bego{\\~n}a”, “Sebasti{\\’a}n-Gall{\\’e}s, N{\\’u}ria”)  | Brain structure is related to speech perception abilities in bilinguals                                                                                            |
| Simonet, Miquel                                                                                                 | textbf{The phonetics and phonology of bilingualism}                                                                                                                |
| c(“Gulian, M”, “Escudero, Paola”, “Boersma, Paul”)                                                              | Supervision hampers distributional learning of vowel contrasts                                                                                                     |
| c(“D{\\’\\i}az, Bego{\\~n}a”, “Mitterer, Holger”, “Broersma, Mirjam”, “Sebasti{\\’a}n-Gall{\\’e}s, N{\\’u}ria”) | Individual differences in late bilinguals’ L2 phonological processes: From acoustic-phonetic analysis to lexical access                                            |
| Bereznak, Catherine                                                                                             | Review of: a grammar of Comanche, by Jean Ormsbee Charney                                                                                                          |
| Recasens, Daniel                                                                                                | Lingual kinematics and coarticulation for alveolopalatal and velar consonants in Catalan                                                                           |
| Kartushina, Natalia                                                                                             | On the effects of L2 perception and of individual differences in L1 production on L2 pronunciation                                                                 |
| Simonet, Miquel                                                                                                 | Intonational convergence in language contact: Utterance-final F0 contours in Catalan{}Spanish early bilinguals                                                     |
| c(“Morrison, G”, “Escudero, Paola”)                                                                             | A cross-dialect comparison of Peninsula-and Peruvian-Spanish vowels                                                                                                |
| c(“Piske, Thorsten”, “Flege, James E”, “MacKay, Ian R. A.”)                                                     | The production of English vowels by fluent early and late Italian-English bilinguals                                                                               |
| Lawrence, Michael A                                                                                             | Package {}ez{}                                                                                                                                                     |
| Caffo, Brian                                                                                                    | Regression Models for Data Science in R                                                                                                                            |
| Loureiro-Rodr{'}guez, Ver{'o}nica                                                                               | Conflicting values at a conflicting age                                                                                                                            |
| Guion, Susan G                                                                                                  | The role of perception in the sound change of velar palatalization                                                                                                 |
| Woolard, KA                                                                                                     | Between friends: Gender, peer group structure, and bilingualism in urban Catalonia                                                                                 |
| Xie, Yihui                                                                                                      | knitr Graphics Manual                                                                                                                                              |
| c(“Montaruli, E”, “Bourhis, RY”)                                                                                | Identity, language, and ethnic relations in the Bilingual Autonomous Communities of Spain                                                                          |
| c(“Ernestus, Mirjam”, “Torreira, Francisco”)                                                                    | Vowel elision in casual French: The case of vowel/e/in the word c’{'e}tait                                                                                         |
| Morrison, Geoffrey Stewart                                                                                      | L1 {&} L2 Production and Perception of English and Spanish Vowels A Statistical Modelling Approach                                                                 |
| c(“Ramachandra, V”, “Depalma, N”)                                                                               | The Role of Mirror Neurons in Processing Vocal Emotions: Evidence from Psychophysiological Data                                                                    |
| Hualde, Jos{'e} Ignacio                                                                                         | Autosegmental and metrical spreading in the vowel-harmony systems of northwestern Spain                                                                            |
| Wood, Denise                                                                                                    | A Primer for Linguists6 October 2009                                                                                                                               |
| c(“Flege, James E”, “Munro, Murray J”, “MacKay, Ian R. A.”)                                                     | The effects of age of second language learning on the production of English vowels                                                                                 |
| c(“Cooper, Natalie”, “Hsing, Pen-Yuan”)                                                                         | A Guide to Reproducible Code in Ecology and Evolution                                                                                                              |
| Colina, Sonia                                                                                                   | No double plurals in dominican spanish: an optimality-theoretic account                                                                                            |
| ROBINSON, DAVID                                                                                                 | INTRODUCTION TO EMPIRICAL BAYES                                                                                                                                    |
| c(“Gordon, P C”, “Hendrick, Randall”, “Johnson, Marcus”)                                                        | Memory interference during language processing                                                                                                                     |
| Colina, Sonia                                                                                                   | Estructura sil{'a}bica                                                                                                                                             |
| Colina, Sonia                                                                                                   | Silabificaci{'o}n y diptongos - Ej                                                                                                                                 |
| Cumming, Geoff                                                                                                  | Understanding The New Statistics: Effect Sizes, Confidence Intervals, and Meta-Analysis                                                                            |
| c(“Flege, James E”, “Yeni-Komshian, Grace H”, “Liu, Serena”)                                                    | Age Constraints on Second-Language Acquisition                                                                                                                     |
| Recasens, Daniel                                                                                                | An articulatory investigation of lingual coarticulatory resistance and aggressiveness for consonants and vowels in Catalan                                         |
| Ernestus, Mirjam                                                                                                | European Research Council                                                                                                                                          |
| Colina, Sonia                                                                                                   | Transcripci{'o}n EJ - Hualde et al.                                                                                                                                |
| c(“Simonet, Miquel”, “Nadeu, Marianna”, “Hualde, Jos{\\’e} Ignacio”)                                            | Consonant lenition and phonological recategorization                                                                                                               |
| Beaudrie, Sara                                                                                                  | Articles                                                                                                                                                           |
| c(“Colina, Edited by Fernando Mart{\\’\\i}nez-Gil”, “Sonia”)                                                    | Optimality-Theoretic Studies in Spanish Phonology                                                                                                                  |
| c(“Simonet, Miquel”, “Nadeu, Marianna”, “Hualde, Jos{\\’e} Ignacio”)                                            | Consonant lenition and phonological recategorization                                                                                                               |
| Torres-Reyna, Oscar                                                                                             | Data Preparation/Descriptive Statistics                                                                                                                            |
| Simonet, Miquel                                                                                                 | Technology in Phonetic Science: Setting Up a Basic Phonetics Laboratory.                                                                                           |
| c(“Harris, K”, “McGarr, N”)                                                                                     | Relationships between speech perception and speech production in normal hearing and hearing impaired subjects                                                      |
| c(“Mackain, Kristine S”, “Best, Catherine T”, “Strange, Winifred”)                                              | Categorical Perception of English /r/ and /l/ by Japanese Bilinguals                                                                                               |
| Colina, Sonia                                                                                                   | Identity constraints and Spanish resyllabification\* 1                                                                                                             |
| Bickerton, D                                                                                                    | I chat, thereby I groom                                                                                                                                            |
| Mart{'}nez-Gil, F                                                                                               | Obstruent vocalization in Chilean Spanish: A serial versus a constraint-based approach                                                                             |
| c(“Piske, Thorsten”, “Flege, James E”, “MacKay, Ian R. A.”)                                                     | Factors affecting degree of foreign accent in an L2 : a review                                                                                                     |
| c(“Escudero, Paola”, “Chl{\\’a}dkov{\\’a}, K”, “Boersma, Paul”)                                                 | Context-specific acoustic differences between Peruvian and Iberian Spanish vowels                                                                                  |
| c(“Escudero, Paola”, “Rauber, Andr{\\’e}ia Schurt”, “Boersma, Paul”)                                            | A cross-dialect acoustic description of vowels: Brazilian and European Portuguese                                                                                  |
| Tatman, Rachael                                                                                                 | A Very Brief Introduction To Bayesian Statistics for Linguists                                                                                                     |

``` r
unlink("cache", recursive = TRUE)
unlink("figure", recursive = TRUE)
```
