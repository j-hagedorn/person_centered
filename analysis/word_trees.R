x <- "plan"
y <- "planning"
docs <- c("CCHBC","HCBS","MHCode","PCP","Self-D")

filt <- 
  collapsed %>% filter(map_lgl(concepts_incl, ~ x %in% .)) %>% filter(doc_id %in% docs) %>%
  bind_rows(
    collapsed %>% filter(map_lgl(concepts_incl, ~ y %in% .)) %>% filter(doc_id %in% docs)
  ) %>%
  distinct(doc_id,sid, .keep_all = T) %>%
  mutate(text = str_to_lower(text))


p <- 
  filt %>% 
  googleVis::gvisWordTree(
    textvar = "text",
    options = list(wordtree="{word:'plan', type:'double', format:'implicit'}")
  )

p <- 
  filt %>% 
  googleVis::gvisWordTree(
    textvar = "text",
    options = list(
      wordtree="{word:'planning', type:'prefix', format:'implicit'}",
      fontName="Roboto Condensed"
    )
  )


p$html$footer <- NULL
p$html$jsFooter <- NULL
p$html$caption <- NULL
plot(p)

# Remove comments