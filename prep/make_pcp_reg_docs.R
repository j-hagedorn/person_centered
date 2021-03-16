# make_pcp_reg_docs.R

library(tidyverse); library(quanteda); library(textclean)

corpus_concepts <- read_csv("data/corpus_concepts.csv")
concepts <- read_csv("data/concepts.csv")
reg_list <- read_csv("data/reg_list.csv")

# Define cleaning routine
clean_text <- function(x){
  
  x %>%
    str_to_lower() %>%
    str_replace_all("\\[|\\]"," ") %>% # remove square brackets
    str_replace_all( "[^\\s]*[0-9][^\\s]*"," ") %>% # remove mixed string & number
    str_replace_all("\\-"," ") %>% # replace dashes, shd be picked up in noun phrases
    str_replace_all("_"," ") %>%
    str_replace_all("§|§§","rule ") %>%
    str_squish() %>%
    replace_number(remove = T) %>% # remove number
    replace_html(replacement = "") 
  
}

# Get n separate policy docs for each concept
regs <- 
  corpus_concepts %>%
  filter(has_concept == T) %>%
  select(doc_id,assessment:has_concept) %>%
  select(-has_concept) %>%
  group_by(doc_id) %>%
  summarise_at(vars(!matches("doc_id")),list(~sum(.,na.rm = T))) %>%
  pivot_longer(-doc_id,names_to = "concept") %>%
  filter(value > 0) %>%
  group_by(concept) %>%
  summarize(n_regs = n_distinct(doc_id)) 

concept_glance <- 
  concepts %>%
  group_by(concept) %>%
  summarise(
    n_phrases  = n_distinct(term),
    n_occur    = sum(term_count),
    n_sections = sum(doc_count)
  ) %>%
  right_join(regs,by = "concept") %>%
  filter(!concept %in% c("person","services","service_provider"))

rm(regs)

# For each concept, grab context and store

df <- tibble()

for (i in 1:nrow(concept_glance)) {
  
  print(i)
  concept_name <- concept_glance$concept[i]
  
  # Get specific phrases which makes up the concept to search text
  concept_phrases <- 
    concepts %>% filter(concept == concept_name) %>% 
    mutate(term = str_replace_all(term,"_"," ")) %>%.$term
  
  occurs <- 
    corpus_concepts %>%
    filter_at(vars(matches(concept_name)),all_vars(!is.na(.))) %>%
    select(doc_id,id,section,start_page,text) %>%
    mutate(text = clean_text(text))
  
  snippet <-
    occurs %>%
    corpus(docid_field = "id",text_field = "text") %>%
    kwic(phrase(concept_phrases), window = 40) %>%
    as_tibble() %>%
    mutate(snippet = str_squish(paste(pre,keyword,post,sep = " "))) %>%
    select(id = docname, snippet)
  
  occurs <- 
    occurs %>% 
    left_join(snippet, by = "id") %>% 
    select(-text) %>%
    left_join(
      reg_list %>% select(doc_id = document_number, title, short_title, url = pdf_url), 
      by = "doc_id"
    ) %>%
    mutate(concept = concept_name)
  
  rm(snippet)
  
  df <- bind_rows(occurs,df)
  
}

occurs <- 
  df %>%
  group_by(id) %>%
  mutate(passage_id = paste0(id,"_",row_number())) %>%
  select(ends_with("id"),concept,snippet,title,short_title,section,start_page,url)

rm(df)

write_csv(occurs,"analyses/data/occurs.csv")

# Render reports for each concept
for (iter in unique(occurs$concept)) {
  rmarkdown::render(
    input = "analyses/fidelity_poc.Rmd", 
    output_file = paste0(iter,".docx"),
    output_dir = path,
    params = list(concept = iter)
  )
}
