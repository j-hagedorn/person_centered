## make_flow.R

library(openxlsx)

tbdsuser <- Sys.info()[["login"]]

path <- paste0("C:/Users/",tbdsuser,"/Documents/GitHub/person_centered/data")

# load data

pcp_nodes <- readxl::read_excel("app/data/pcp_concept_map.xlsx",sheet = "nodes")
pcp_edges <- readxl::read_excel("app/data/pcp_concept_map.xlsx",sheet = "edges")

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

process_map <- pcp_edges %>%
  select(from, from_concept, to) %>%
  rename(
    `Process Step ID` = from,
    `Process Step Description` = from_concept,
    `Next Step ID` = to
  ) %>%
  group_by(`Process Step ID`, `Process Step Description`) %>%
  summarize(`Next Step ID` = str_c(`Next Step ID`, collapse = ", "))

to_only <- pcp_edges %>%
  filter(!to %in% from) %>%
  select(to, to_concept) %>%
  distinct() %>%
  rename(
    `Process Step ID` = to,
    `Process Step Description` = to_concept
  ) %>%
  mutate(`Next Step ID` = "")

process_map <- process_map %>%
  rbind(to_only) %>%
  mutate(
    `Connector Label` = "",
    `Shape Type` = ""
  )


openxlsx::write.xlsx(process_map, file = paste0(path,"/process_map.xlsx"))
