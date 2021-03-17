#### global.R

# load libraries

library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(visNetwork)
library(digest)
library(DT)
library(reactable)
library(AzureStor)

# load data

pcp_nodes <- readxl::read_excel("data/pcp_concept_map.xlsx",sheet = "nodes")
pcp_edges <- readxl::read_excel("data/pcp_concept_map.xlsx",sheet = "edges")

corpus_concepts <- read_csv("data/corpus_concepts.csv")
reg_list <- read_csv("data/reg_list.csv")
reqs <- read_csv("data/reqs.csv")

domain_qm_bhdda <- read_csv("data/domain_qm_bhdda.csv")

# create id column for nodes and format concept names

pcp_nodes <- pcp_nodes %>%
  mutate(concept_id = row_number()) %>%
  rename(id = concept_id) %>%
  mutate(
    # format concept names displayed in application
    concept = gsub("_"," ", concept_name),
    concept = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", concept, perl = TRUE),
    concept = case_when(
      concept == "Pcp Facilitation" ~ "PCP Facilitation",
      concept == "Pcp Process"      ~ "PCP Process",
      concept == "Describe Pcp"     ~ "Describe PCP",
      concept == "Pcp Meeting"      ~ "PCP Meeting",
      TRUE                          ~ concept
    )
  )

pcp_edges <- pcp_edges %>%
  rename(
    to_concept_name = to,
    from_concept_name = from
  ) %>%
  left_join(pcp_nodes %>% select(id,concept_name),
            by = c("to_concept_name" = "concept_name")
  ) %>%
  rename(to = id) %>%
  left_join(pcp_nodes %>% select(id,concept_name),
            by = c("from_concept_name" = "concept_name")
  ) %>%
  rename(from = id) %>%
  mutate(
    # format concept names displayed in application
    to_concept = gsub("_"," ", to_concept_name),
    to_concept = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", to_concept, perl = TRUE),
    from_concept = gsub("_"," ", from_concept_name),
    from_concept = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", from_concept, perl = TRUE),
    to_concept = case_when(
      to_concept == "Pcp Facilitation" ~ "PCP Facilitation",
      to_concept == "Pcp Process"      ~ "PCP Process",
      to_concept == "Describe Pcp"     ~ "Describe PCP",
      to_concept == "Pcp Meeting"      ~ "PCP Meeting",
      TRUE                             ~ to_concept
    ),
    from_concept = case_when(
      from_concept == "Pcp Facilitation" ~ "PCP Facilitation",
      from_concept == "Pcp Process"      ~ "PCP Process",
      from_concept == "Describe Pcp"     ~ "Describe PCP",
      from_concept == "Pcp Meeting"      ~ "PCP Meeting",
      TRUE                               ~ from_concept
    )
  )

# format concept names in reqs
reqs <- reqs %>%
  mutate(
    # format concept names displayed in application
    concept = gsub("_"," ", concept),
    concept = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", concept, perl = TRUE),
    concept = case_when(
      concept == "Pcp Facilitation" ~ "PCP Facilitation",
      concept == "Pcp Process"      ~ "PCP Process",
      concept == "Describe Pcp"     ~ "Describe PCP",
      concept == "Pcp Meeting"      ~ "PCP Meeting",
      TRUE                          ~ concept
    )
  )

# create elements to capture feedback

fieldsAll <- c("name","org","email","page","text")

responsesDir <- file.path("responses")

epochTime <- function() {
  
  as.integer(Sys.time())
  
}
  


# Make Azure storage connections for dumping CSV files created 
# by feedback page 



fl_endp_sas <- file_endpoint(Sys.getenv('file_url'), key= Sys.getenv('key'))


cont<-file_share(fl_endp_sas,"personal-mdhhs")




