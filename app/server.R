#### server.R

shinyServer(function(input, output) {

  ## ui for purpose page
  
  output$why <- renderUI({

      fluidRow(
        tags$img(
          src = "retrosupply-jLwVAUtLOAQ-unsplash.jpg",
          width = "100%", style = 'position:absolute;margin-top: -21px'
        ),
        tags$head(
          tags$style(HTML('#why_blank {opacity: 0}; #why_main {opacity: .85}'))
        ),
        sidebarPanel(
          id = "why_blank", width = 9,
        ),
        sidebarPanel(
          id = "why_main", width = 3,
          h3("Person Centered Care"),
          br(),
          p("An approach to care that consciously adopts the perspectives of
          individuals, families and communities, and sees them as participants
          as well as beneficiaries of trusted health systems that respond to their
          needs and preferences in humane and holistic ways."),
          uiOutput("url"),
          br(),
          p("The various sections of this application can be used to: "),
          tags$li(strong(em("Explore"))," concepts related to person-centered planning and their relationships to one another"),
          br(),
          tags$li("Review a collection of state and federal requirements to ", strong(em("inform"))," training"),
          br(),
          tags$li("Identify and implement measurable outcomes to ", strong(em("ensure"))," quality and consistency"),
          br(),
          br()
        )
      )
      

  })

  ## ui for explore page
  
  output$search <- renderUI({
    
    fluidRow(
      column(
        width = 12,
        column(
          width = 3,
          strong(em("Developing a common language")),
          br(),
          br(),
          p("Language is one of our primary means of communication and interaction,
          which emphasizes the importance of using consistent terminology."),
          br(),
          p("Using a common language creates a collective understanding and allows for
          clear and consistent communication. It also provides a foundation to develop
          training and measurable outcomes."),
          br(),
          p("Use the network graph and table below to explore concepts related 
          to person-centered planning. Each line represents a link between two concepts.
          The size of the circle is relative to the number of documents, or regulations,
          that particular a concept is mapped to."),
          p("Total # of concepts: ",length(unique(pcp_nodes$concept_name))),
          p("Total # of documents: ",length(unique(corpus_concepts$doc_id)))
        ),
        column(
          width = 9,
          p("Select a concept:"),
          visNetworkOutput("netVis")
        )
      ),
      column(
        width = 12,
        uiOutput("netText"),
        br(),
        br(),
        dataTableOutput("netTbl")
      )
    )

  })
  
  ## ui for inform page
  
  output$advise <- renderUI({
    
      fluidRow(
        column(
          width = 12,
          column(
            width = 3,
            uiOutput("select_concept"),
            br(),
            uiOutput("regText"),
            br()
          ),
          column(
            width = 9,
            strong(em("Exploring relevant policies and regulations")),
            br(),
            br(),
            p("The taxonomy of concepts being used to develop a common language around
            the person-centered planning process were mapped to both state and federal
            regulations."),
            p("Selecting a concept from the drop down menu will update the table below to include
            all documents with content related to the selected concept. The document name,
            specific section and page numbers are included as well as a link to the full
            document. Concepts that are not mapped to a regulation will result in a table with
            no available data."),
            br()
          )
        ),
        column(
          width = 12,
          dataTableOutput("regTbl")
        )
      )
    
  })
  
  ## ui for assure page
  
  output$guarantee <- renderUI({
    
    fluidRow(
      column(
        width = 12,
        column(
          width = 3,
          strong(em("Measuring success")),
          p(),
          p("Creating valid and reliable methods of evaluation is crucial in
            identfying areas that require improvement and determining whether the
            intended goals and objectives have been achieved."),
          br(),
          p("The table to the right is an inventory of CMS measures related to
            behavioral health. They includes measures within the following
            content areas:"),
          tags$li("access"),
          tags$li("decision-making"),
          tags$li("planning"),
          tags$li("risk"),
          tags$li("unrestricted settings"),
          tags$li("ER visits"),
          tags$li("care coordination"),
          tags$li("continuity of care"),
          tags$li("readmission"),
          tags$li("substance use"),
          tags$li("mental health")
        ),
        column(
          width = 9,
          dataTableOutput("cmsTbl")
        )
      )
    )
    
  })
  
  ## ui for feedback page
  
  ## https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
  
  output$response <- renderUI({
    
    fluidPage(
      titlePanel("Feedback Form"),
      div(
        id = "form",
        textInput("name", "Name", ""),
        textInput("org", "Organization"),
        textInput("text", "Feedback"),
        # checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
        # sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
        # selectInput("os_type", "Operating system used most frequently",
        #             c("",  "Windows", "Mac", "Linux")),
        actionButton("submit", "Submit", class = "btn-primary")
      )
    )
    
  })
  
  ## ui elements for purpose
  
  url <- a("People-centred health services", 
           href="https://www.who.int/servicedeliverysafety/areas/people-centred-care/ipchs-what/en/")
  
  output$url <- renderUI({
    
    tagList("- World Health Organization:", url)
    
  })
  
  ## ui elements for explore
  
  output$netVis <- renderVisNetwork({
    
    # determine size of node based on occurrence in concepts
    size <- corpus_concepts %>%
      select(assessment:crisis_plan) %>%
      colSums(., na.rm = T) %>%
      data.frame() %>%
      rownames_to_column() %>%
      rename(
        "concept_name" = "rowname",
        "occur" = "."
      ) %>%
      arrange(occur) %>%
      mutate(
        # set row_number relative to occurrence
        rn = row_number() + 10,
        rn = as.numeric(rn)
      )
    
    # join nodes to concepts within regulation docs
    pcp_nodes <- pcp_nodes %>%
      left_join(size, by = "concept_name")
    
    # set rn if not present in concepts 
    pcp_nodes$rn <- if_else(is.na(pcp_nodes$rn) == T, min(pcp_nodes$rn, na.rm = T) / 2, pcp_nodes$rn)
    
    vis.nodes <- pcp_nodes
    vis.nodes$label <- vis.nodes$concept_name
    vis.nodes$title <- vis.nodes$concept_definition
    vis.nodes$size  <- vis.nodes$rn
    vis.nodes$color <- "teal"
    
    vis.edges <- pcp_edges
    
    visNetwork(
      nodes = vis.nodes, 
      edges = vis.edges
    ) %>%
      visOptions(
        highlightNearest = T,
        nodesIdSelection = list(enabled = T),
        autoResize = T
      ) %>%
      visHierarchicalLayout(sortMethod = "directed")
    
  })
  
  output$netText <- renderUI({
    
    if(input$netVis_selected > 0){
      
      concept <- pcp_nodes %>%
        filter(id == input$netVis_selected) %>%
        select(concept_name, concept_definition) %>%
        mutate(concept_definition = if_else(is.na(concept_definition) == T,
                                            "This concept is not currently defined",
                                            paste0("This concept is defined as: ",concept_definition,".")))
      
      paste0("You selected the concept ",concept$concept_name,". ",concept$concept_definition)
      
    } else
      
      paste0("A concept has not been selected. Please use the drop down menu above to
             make a selection.")
    
  })

  
  output$netTbl <- renderDataTable({
    
    pcp_edges %>%
      filter(from %in% input$netVis_selected) %>%
      select(to_concept, from_concept) %>%
      rename(
        selected = from_concept,
        related = to_concept
      ) %>%
      rbind(pcp_edges %>% 
              filter(to %in% input$netVis_selected) %>%
              select(to_concept, from_concept) %>%
              rename(
                selected = to_concept,
                related = from_concept
              )
      ) %>%
      left_join(pcp_nodes %>%
                  select(concept_name, concept_definition), 
                by = c("related" = "concept_name")
      ) %>%
      select(selected, related, concept_definition) %>%
      datatable(
        rownames = F,
        colnames = c("Selected concept", "Related concept(s)", "Related concept defintion(s)")
      )
    
  })
  
  ## ui elements for inform
  
  output$select_concept <- renderUI({
    
    selectInput(
      "select_concept",
      label = "Select a concept:",
      choices = unique(pcp_nodes$concept_name)
    )
    
  })
  
  output$regText <- renderUI({
    
    concept <- pcp_nodes %>%
      filter(concept_name %in% input$select_concept) %>%
        select(concept_name, concept_definition) %>%
        mutate(concept_definition = if_else(is.na(concept_definition) == T,
                                            "The concept you selected is not currently defined.",
                                            paste0(concept_name,": ",concept_definition,".")))
    
    paste0(concept$concept_definition)
    
  })
  
  output$regTbl <- renderDataTable({
    
    corpus_concepts %>%
      filter(has_concept == TRUE) %>%
      select(-has_concept) %>%
      pivot_longer(cols = c(assessment:crisis_plan), names_to = "concept") %>%
      filter(is.na(value) == FALSE) %>%
      select(doc_id, section, start_page, stop_page, concept, value) %>%
      left_join(pcp_nodes %>%
                  select(id, concept_name),
                by = c("concept" = "concept_name")
      ) %>%
      filter(concept %in% input$select_concept) %>%
      left_join(reg_list,
                by = c("doc_id" = "document_number")
      ) %>%
      select(doc_id, title, section, start_page, stop_page, pdf_url) %>%
      mutate(pdf_url = paste0("<a href=",pdf_url,">",pdf_url,"</a>")) %>%
      datatable(
        rownames = F,
        escape = F,
        caption = "Regulations Associated with Person-Centered Planning Concepts",
        colnames = c("ID", "Document", "Section", "Starts on page...",
                     "Ends on page...", "Link")
      )
    
  })
  
  ## ui elements for assure
  
  output$cmsTbl <- renderDataTable({
    
    domain_qm_bhdda %>%
      select(cmit_id, nqf_id, measure_title, measure_description) %>%
      datatable(
        rownames = F,
        caption = "CMS Measures Inventory for BHDDA",
        colnames = c("CMIT Ref No", "NQF ID", "Measure Title", "Measure Description")
      )
    
  })
  
  
  
  ## ui elements for feedback
  
  formData <- reactive({
    
    data <- sapply(fieldsAll, function(x) input[[x]])
    
    data <- c(data, timestamp = epochTime())
    
    data <- t(data)
    
    data
    
  })
  
  saveData <- function(data) {
    
    humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
    
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    
    saveData(formData())
    
  })
  
})
