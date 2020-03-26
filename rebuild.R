# Rebuild book in all needed formats

bookdown::render_book("index.Rmd", "bookdown::gitbook",new_session = F)
bookdown::render_book("index.Rmd", "bookdown::pdf_book",new_session = F)
bookdown::render_book("index.Rmd", "bookdown::word_document2",new_session = F)
bookdown::render_book("index.Rmd", "bookdown::epub_book",new_session = F)
