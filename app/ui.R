#### ui.R

ui <- 
  navbarPage(
    h4("Personal"),
    tabPanel(h4("Purpose"),
                tags$head(includeHTML(("google-analytics.html"))),
                uiOutput("why")
    ),
    tabPanel(h4("Explore"),
             uiOutput("search"),
             tags$style(
               type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
             )
    ),
    tabPanel(h4("Inform"),
             uiOutput("advise")
             # tags$style(
             #   type = "text/css",
             #   ".shiny-output-error { visibility: hidden; }",
             #   ".shiny-output-error:before { visibility: hidden; }"
             # )
    ),
    tabPanel(h4("Quality"),
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
    tabPanel(h4("Feedback"),
             uiOutput("response")
             # tags$style(
             #   type = "text/css",
             #   ".shiny-output-error { visibility: hidden; }",
             #   ".shiny-output-error:before { visibility: hidden; }"
             # )
    )
  )
