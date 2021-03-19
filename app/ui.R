#### ui.R

ui <- 
  navbarPage(
    "Personal",
    theme = shinytheme("flatly"),
    tabPanel("Purpose", tags$head(includeHTML(("google-analytics.html"))),
             uiOutput("why")
    ),
    tabPanel("Explore",
             uiOutput("search"),
             tags$style(
               type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
             )
    ),
    tabPanel("Inform",
             uiOutput("advise")
             # tags$style(
             #   type = "text/css",
             #   ".shiny-output-error { visibility: hidden; }",
             #   ".shiny-output-error:before { visibility: hidden; }"
             # )
    ),
    tabPanel("Quality",
             uiOutput("standard")
             # tags$style(
             #   type = "text/css",
             #   ".shiny-output-error { visibility: hidden; }",
             #   ".shiny-output-error:before { visibility: hidden; }"
             # )
    ),
    # tabPanel("Documentation"
             # tags$style(
             #   type = "text/css",
             #   ".shiny-output-error { visibility: hidden; }",
             #   ".shiny-output-error:before { visibility: hidden; }"
             # )
    # ),
    tabPanel("Feedback",
             uiOutput("response")
             # tags$style(
             #   type = "text/css",
             #   ".shiny-output-error { visibility: hidden; }",
             #   ".shiny-output-error:before { visibility: hidden; }"
             # )
    )
  )
