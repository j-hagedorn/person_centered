#### global.R

# load libraries

library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(visNetwork)
library(DT)
library(digest)

# load data

pcp_nodes <- readxl::read_excel("data/pcp_concept_map.xlsx",sheet = "nodes")
pcp_edges <- readxl::read_excel("data/pcp_concept_map.xlsx",sheet = "edges")

corpus_concepts <- read_csv("data/corpus_concepts.csv")
reg_list <- read_csv("data/reg_list.csv")

domain_qm_bhdda <- read_csv("data/domain_qm_bhdda.csv")

# create id column for nodes

pcp_nodes <- pcp_nodes %>%
  mutate(concept_id = row_number()) %>%
  rename(id = concept_id)

pcp_edges <- pcp_edges %>%
  rename(
    to_concept = to,
    from_concept = from
  ) %>%
  left_join(pcp_nodes %>% select(id,concept_name),
            by = c("to_concept" = "concept_name")
  ) %>%
  rename(to = id) %>%
  left_join(pcp_nodes %>% select(id,concept_name),
            by = c("from_concept" = "concept_name")
  ) %>%
  rename(from = id)

# create elements to capture feedback

fieldsAll <- c("name", "org", "text")

responsesDir <- file.path("responses")

epochTime <- function() {
  as.integer(Sys.time())
}
  
