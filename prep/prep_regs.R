## prep_regs.R

tbdsuser <- Sys.info()[["login"]]

# group concepts from vocab
source("prep/concepts.R")

# content analysis
source("prep/cooccur.R")

# fidelity requirements
path <- paste0("C:/Users/",tbdsuser,"/TBD Solutions LLC/PCP Framework - General/reg_refs")

source("prep/make_pcp_reg_docs.R") # formats data as snippets of regs and creates .doc files
source("prep/read_pcp_reg_docs.R") # associates fidelity requirements entered manually into .doc files with source documents
source("prep/join_regs_to_concepts.R") # combine requirements with concepts

# write necessary files to app data folder
write_csv(corpus_concepts,"app/data/corpus_concepts.csv")
write_csv(reg_list,"app/data/reg_list.csv")

# Remove objects
rm(list = c(
  # data
  "concept_summary","concepts","corpus_concepts","corpus_sections",
  "dtm_concepts","dtm_sections","dtm_sentences",
  "tcm","tcm_sections","tcm_sentences","vocab",
  # values
  "terms"
  )
)
