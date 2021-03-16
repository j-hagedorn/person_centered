# join_regs_to_concepts.R

library(tidyverse); library(quanteda); library(textclean); library(tidygraph)

occurs <- read_csv("data/occurs.csv")
doc_df <- read_csv("data/doc_df.csv")


reqs <- doc_df %>%
  left_join(occurs %>%
              ungroup() %>%
              select(title, url) %>%
              distinct(),
            by = "title"
  ) %>%
  select(requirement, title, concept, page, url) %>%
  mutate(
    url = case_when(
      title == "Medicare Shared Savings Program-Accountable Care Organizations-Pathways to Success; and Expanding the Use of Telehealth Services for the Treatment of Opioid Use Disorder Under the Substance Use-Disorder Prevention That Promotes Opioid Recovery and Treatment (SUPPORT) for Patients and Communities Act" ~ "https://www.govinfo.gov/content/pkg/FR-2018-11-23/pdf/2018-24170.pdf",
      title == "Medicaid and Children’s Health Insurance Program (CHIP) Programs; Medicaid Managed Care, CHIP Delivered in Managed Care, and Revisions Related to Third Party Liability" ~ "https://www.govinfo.gov/content/pkg/FR-2016-05-06/pdf/2016-09581.pdf",
      TRUE ~ url
    )
  )

write_csv(reqs,"app/data/reqs.csv")
