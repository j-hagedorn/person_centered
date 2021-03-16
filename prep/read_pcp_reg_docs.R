# read_pcp_reg_docs.R

library(textreadr);library(stringdist);library(tidystringdist)

# list of word docs and associated concept
file_df <-
  tibble(
    doc_name = list.files(path),
    path = list.files(path, full.names = T)
  ) %>%
  filter(str_detect(doc_name,"^rev_")) %>% # Only manually revised versions
  mutate(concept = str_remove_all(doc_name,"^rev_|.docx$"))

notes_df <- tibble() # notes from word docs and associated concept
doc_df <- tibble() # id, requirement and associated concept from word docs

for (i in 1:length(file_df$path)) {
  
  print(i)
  
  notes <- 
    tibble(text = read_docx(file_df$path[i])) %>%
    filter(str_detect(text,"^notes:")) %>% 
    mutate(concept = file_df$concept[i])
  
  notes_df <- bind_rows(notes_df,notes)
  
  doc <- 
    tibble(text = read_docx(file_df$path[i])) %>%
    # Keep 
    filter(
      stringdist("id:",str_sub(text,1,4)) <= 1 |
        stringdist("requirement:",str_sub(text,1,13)) <= 1 |
        substr(text,1,1) == "F"
    ) %>%
    mutate(
      field = case_when(
        stringdist("id:",str_sub(text,1,4)) <= 1           ~ "id",
        stringdist("requirement:",str_sub(text,1,13)) <= 1 ~ "requirement",
        substr(text,1,1) == "F"                            ~ "title"
      )
    )
  
  tst <- doc %>%
    rbind(doc %>% # create duplicate field to pull page number
            filter(field == "title") %>%
            mutate(field = "page")
    ) %>%
    mutate(
      text = case_when(
        field == "title" ~ gsub("be.*$", "", text),
        field == "page"  ~ sub(".*page ", "", text),
        TRUE             ~ text
      ),
      text = trimws(text, which = "left"),
      text = case_when(
        field %in% c("id", "requirement") ~ str_remove(text,".+?(?<=:)"), # remove until 1st colon
        field == "title"                  ~ gsub(".*From the","",text), # remove phrase "From the"
        field == "page"                   ~ gsub(" .*$", "", text)
      ),
      text = str_squish(text)
    ) %>%
    pivot_wider(names_from = "field",values_from = "text") %>%
    unnest(c(id,requirement,title,page)) %>%
    filter(!requirement %in% c("","NA")) %>% 
    mutate(concept = file_df$concept[i])
  
  doc_df <- bind_rows(doc_df,tst)
  
  rm(notes);rm(doc);rm(tst)
}

clean_title <- readxl::read_excel("data/doc_df_clean.xlsx",sheet = "title")
clean_reqs <- readxl::read_excel("data/doc_df_clean.xlsx",sheet = "requirement")

# additional formatting
doc_df <- doc_df %>%
  left_join(clean_title,
            by = "title") %>%
  select(-title) %>%
  rename(title = title_formatted) %>%
  left_join(clean_reqs,
            by = "requirement") %>%
  select(-requirement) %>%
  rename(requirement = requirement_formatted)

write_csv(doc_df,"data/doc_df_test.csv")

rm(doc_df); rm(file_df); rm(notes_df); rm(i)

