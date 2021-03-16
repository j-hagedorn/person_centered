## cooccur.R

library(tidyverse); library(textclean); library(tidygraph)
library(quanteda);library(text2vec); library(tidytext)

dtm_sentences <- read_csv("data/reg_dtm_sentences.csv")

# Identify terms associated with key concepts
terms <- concepts %>% filter(!is.na(concept)) %>% select(term) %>% .$term

# Expand memory limits and parallelism for computation
memory.limit(100000)
quanteda_options(threads = 4)

tcm_sections <- 
  dtm_sections %>%
  cast_dfm(document,feature,frequency) %>%
  quanteda::fcm(context = "document", count = "frequency") %>%
  # Subset terms to those mapped to concepts
  fcm_select(terms,"keep","fixed") %>%
  tidy() %>%
  filter(count > 0) %>%
  rename(from = document, to = term, section_cooc = count) %>%
  # Remove occurrence of term with itself
  filter(from != to)

tcm_sentences <- 
  dtm_sentences %>%
  cast_dfm(document,feature,frequency) %>%
  quanteda::fcm(context = "document", count = "frequency") %>%
  # Subset terms to those mapped to concepts
  fcm_select(terms,"keep","fixed") %>%
  tidy() %>%
  filter(count > 0) %>%
  rename(from = document, to = term, sentence_cooc = count) %>%
  # Remove occurrence of term with itself
  filter(from != to)

tcm <-
  tcm_sections %>%
  full_join(tcm_sentences, by = c("from","to")) %>%
  left_join(concepts %>% select(term,from_concept = concept), by = c("from" = "term")) %>%
  left_join(concepts %>% select(term,to_concept   = concept), by = c("to"   = "term")) %>%
  select(from,to,from_concept,to_concept,section_cooc,sentence_cooc)

write_csv(tcm,"data/tcm.csv")
