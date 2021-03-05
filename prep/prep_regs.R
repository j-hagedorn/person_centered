## prep_regs.R

# group concepts from vocab
source("prep/concepts.R")

# content analysis
source("prep/cooccur.R")

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
