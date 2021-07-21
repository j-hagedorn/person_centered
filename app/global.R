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

umls <- read_csv("data/umls.csv")

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
      concept == "Ipos Development" ~ "IPOS Development",
      concept == "Endorse Ipos"     ~ "Endorse IPOS",
      concept == "Cm Signed"        ~ "CM Signed",
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
      to_concept == "Ipos Development" ~ "IPOS Development",
      to_concept == "Endorse Ipos"     ~ "Endorse IPOS",
      to_concept == "Cm Signed"        ~ "CM Signed",
      TRUE                             ~ to_concept
    ),
    from_concept = case_when(
      from_concept == "Pcp Facilitation" ~ "PCP Facilitation",
      from_concept == "Pcp Process"      ~ "PCP Process",
      from_concept == "Describe Pcp"     ~ "Describe PCP",
      from_concept == "Pcp Meeting"      ~ "PCP Meeting",
      from_concept == "Ipos Development" ~ "IPOS Development",
      from_concept == "Endorse Ipos"     ~ "Endorse IPOS",
      from_concept == "Cm Signed"        ~ "CM Signed",
      TRUE                               ~ from_concept
    )
  )

# format concept names in reqs
reqs <- reqs %>%
  mutate(
    concept = gsub("_"," ", concept),
    concept = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", concept, perl = TRUE),
    concept = case_when(
      concept == "Pcp Facilitation" ~ "PCP Facilitation",
      concept == "Pcp Process"      ~ "PCP Process",
      concept == "Describe Pcp"     ~ "Describe PCP",
      concept == "Pcp Meeting"      ~ "PCP Meeting",
      concept == "Ipos Development" ~ "IPOS Development",
      concept == "Endorse Ipos"     ~ "Endorse IPOS",
      concept == "Cm Signed"        ~ "CM Signed",
      TRUE                          ~ concept
    )
  )

# format concept names in umls
umls <- umls %>%
  left_join(pcp_nodes %>% select(id,concept_name),
            by = c("concept" = "concept_name")
  ) %>%
  mutate(
    concept = gsub("_"," ", concept),
    concept = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", concept, perl = TRUE),
    concept = case_when(
      concept == "Pcp Facilitation" ~ "PCP Facilitation",
      concept == "Pcp Process"      ~ "PCP Process",
      concept == "Describe Pcp"     ~ "Describe PCP",
      concept == "Pcp Meeting"      ~ "PCP Meeting",
      concept == "Ipos Development" ~ "IPOS Development",
      concept == "Endorse Ipos"     ~ "Endorse IPOS",
      concept == "Cm Signed"        ~ "CM Signed",
      TRUE                          ~ concept
    )
  )

# create elements to capture feedback

fieldsAll <- c("name","org","email","page","text")

responsesDir <- file.path("responses")

epochTime <- function() {
  
  as.integer(Sys.time())
  
}
  


#File paths change when operating between windows and linux operating systems. When building 
#locally I need the Windows paths, but need to change those when dockerizing and deploying


# Specifying where to pull the Renviron file when launching. When deployed to the cloud, 
# it uses a storage mount, which is on linux path.

if(Sys.info()[['sysname']] == 'Windows'){
  
  readRenviron("../.Renviron")
  
}else{ readRenviron("/srv/shiny-server/datafiles/.Renviron")}


#file_url<-'https://tbdopen.file.core.windows.net/'
#key = 'V0IIcwlEGyVKZVqqHpgCo/RItxNjVO6fOkWHP15Plqfz0tAvU8ZANehOtdRs4PrKJNJiIB6Zye7Q0Eplm4/E7g=='

# Azure connection to storage

#fl_endp_access_key <- file_endpoint(file_url, key= key)

fl_endp_access_key<- file_endpoint(Sys.getenv('file_url'), key= Sys.getenv('key'))


cont<-file_share(fl_endp_access_key,"personal-mdhhs")





