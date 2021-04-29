#### server.R

shinyServer(function(input, output) {

  ## ui for purpose page
  
  output$why <- renderUI({

      fluidRow(
        tags$img(
          src = "photo-1465343161283-c1959138ddaa.jpg",
          width = "100%", style = 'position:absolute;margin-top: -21px'
        ),
        tags$head(
           includeHTML(("google-analytics.html")),
          # Note the wrapping of the string in HTML()
          tags$style(HTML("
                          @import url('//fonts.googleapis.com/css?family=Montserrat:400,700|Open+Sans:400,300,600,700,800');
                          body {
                          background-color: white;
                          color: black; /* text color */
                          }
                          
                          .navbar { 
                          background-color: #008080;
                          color: #000000;
                          }

                          /* Change header text to imported font */
                          h2 {
                          font-family: 'Montserrat', sans-serif;
                          font-weight:bold;
                          }
                          
                          h4 {
                          font-family: 'Montserrat', sans-serif;
                          font-weight:bold;
                          color: black;
                          }
                          
                          p {
                          font-family: 'Montserrat', sans-serif;
                          }
                          
                          li {
                          font-family: 'Montserrat', sans-serif;
                          }
                          
                          em {
                          font-family: 'Montserrat', sans-serif;
                          }
                          
                          /* Make text visible on inputs */
                          .shiny-input-container {
                          color: #474747;
                          }"
                          )
                      )
        ),
        fixedRow(
          column(12,
                 tags$head(
                   tags$style(HTML('#why_side {opacity: 0.90} #why_main {opacity: 1}'))
                 ))),
        sidebarPanel(
          id = "why_main",
          width = 6,
          h2("Person Centered Practices"),
          p("An approach to care that consciously adopts the perspectives of
          individuals, families and communities, and sees them as participants
          as well as beneficiaries of trusted health systems that respond to their
          needs and preferences in humane and holistic ways."),
          uiOutput("url"),
          br(),
          p("The various sections of this application can be used to: "),
          tags$li(strong(em("Explore"))," concepts related to person-centered planning and their relationships to one another"),
          tags$li("Review a collection of state and federal requirements to ", strong(em("inform"))," training"),
          tags$li("Identify and implement measurable outcomes to ensure ",strong(em("quality")), "and consistency"),
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
          h4(em("Developing a common language")),
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
          p("Total # of documents: ",length(unique(corpus_concepts$doc_id))),
          br()
        ),
        column(
          width = 9,
          p("Select a concept:"),
          visNetworkOutput("netVis")
        )
      ),
      column(
        width = 12,
        tags$head(tags$style("
                  #conceptText * {display: inline; font-family: 'Montserrat', sans-serif;} 
                             ")),
        div(id="conceptText",textOutput("netConcept"), tags$b(textOutput("conceptText")),paste0(".")),
        tags$em(textOutput("defintionText")),
        br(),
        textOutput("netText_parent"),
        tags$head(tags$style("#netText_parent{font-family: 'Montserrat', sans-serif;}")),
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
            # tags$em(textOutput("regText")),
            br()
          ),
          column(
            width = 9,
            h4(em("Exploring relevant policies and regulations")),
            br(),
            p("The taxonomy of concepts being used to develop a common language around
            the person-centered planning process were mapped to both state and federal
            regulations. These regulations were reviewed and then associated with specific
            person-centered planning requirements."),
            p("Selecting a concept from the drop down menu will update the table below to include
            person-centered planning requirements with content related to the selected concept. 
            The document name and page number is included as well as a link to the full text. 
            Concepts that are not mapped to a state or federal regulation document, and subsequently
            a person-centered planning requirement, are not available in the drop down menu."),
            br()
          )
        ),
        column(
          width = 12,
          downloadButton('downloadData', 'Download Data'),
          br(),
          br(),
          reactableOutput("reqTbl")
        )
      )
    
  })
  
  ## ui for standard page
  
  output$standard <- renderUI({
    
    fluidRow(
      column(
        width = 6,
        h4(em("Measuring success")),
        p(),
        p("Creating valid and reliable methods of evaluation is crucial in
            identfying areas that require improvement and determining whether the
            intended goals and objectives have been achieved."),
        p("The table below is an inventory of CMS measures related to
            planning and decision making. It includes measures with the 
            following key phrases: care plan, plan of care, safety plan, follow-up plan,
            shared decision, and personal priorities.")
      ),
      column(
        width = 12,
        dataTableOutput("cmsTbl")
      )
    )
    
  })
  
  ## ui for feedback page
  
  ## https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
  
  output$response <- renderUI({
    
    fluidPage(
      shinyjs::useShinyjs(),
        titlePanel("Feedback Form"),
        div(
          id = "form",
          
          textInput("name", "Name", ""),
          textInput("org", "Organization"),
          textInput("email", "Email Address"),
          selectInput("page", "My feedback is related to the following section:",
                      c("","Explore",  "Inform", "Quality")),
          textAreaInput("text", "Feedback"),
          actionButton("submit", "Submit", class = "btn-primary")
        ),
        shinyjs::hidden(
          div(
            id = "thankyou_msg",
            h3("Thanks, your response was submitted successfully!"),
            actionLink("submit_another", "Submit another response")
          )
        )
    )
    
    
  })
  
  ## ui elements for purpose
  
  url <- a("People-centred health services", 
           href="https://www.who.int/servicedeliverysafety/areas/people-centred-care/ipchs-what/en/")
  
  output$url <- renderUI({
    
    tagList(tags$p("- World Health Organization:", url))
    
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
    vis.nodes$label <- vis.nodes$concept
    vis.nodes$title <- vis.nodes$concept_definition
    vis.nodes$size  <- vis.nodes$rn
    # vis.nodes$color <- "teal"

    vis.edges <- pcp_edges
    
    visNetwork(
      nodes = vis.nodes, 
      edges = vis.edges
      ) %>%
      visNodes(color = list(background = "teal", border = "teal", highlight = 'orange')) %>%
      visEvents(click="function(nodes){
                Shiny.onInputChange('current_node_id',
                nodes);
      }") %>%
      visOptions(
        highlightNearest = T,
        nodesIdSelection = list(enabled = T),
        autoResize = T
      ) %>%
      visHierarchicalLayout(sortMethod = "directed")
    
  })
  
  output$netConcept <- renderText({
    
    if(input$netVis_selected > 0){
      
      paste0("You selected the concept ")
      
    } else
      
      paste0("A concept has not been selected. Please use the drop down menu above to
             make a selection")
    
  })
  
  output$conceptText <- renderText({
    
    if(input$netVis_selected > 0){
      
      text <- pcp_nodes %>%
        filter(id == input$netVis_selected) %>%
        select(concept)
      
      HTML(paste0("'",text,"'"))
      
    } else
      
      paste0("")
    
  })
  
  output$defintionText <- renderText({
    
    if(input$netVis_selected > 0){
      
      text <- pcp_nodes %>%
        filter(id == input$netVis_selected) %>%
        select(concept_definition) %>%
        mutate(concept_definition = case_when(
          is.na(concept_definition) == T ~ "This concept is not yet defined.",
          TRUE ~ paste0('This concept is defined as: ',concept_definition)
        ))
      
      HTML(paste0(text))
      
    } else
      
      paste0("")
    
  })
  
  output$netText_parent <- renderText({
    
    parent <- pcp_edges %>%
      filter(to %in% input$netVis_selected) %>%
      select(to_concept, from_concept) %>%
      rename(
        selected = to_concept,
        related = from_concept
      ) %>%
      left_join(pcp_nodes %>%
                  select(concept, concept_definition), 
                by = c("related" = "concept")
      ) %>%
      select(selected, related, concept_definition)
    
    if(input$netVis_selected == 1){
      
      paste0("'Person' is the central concept within the Person Centered Planning process. 
             All other concepts extend from the person and illustrate the various aspects
             of an individual.")

    } else if(input$netVis_selected > 1) {
      
      text <- paste0("The selected concept ", parent$selected," is a subset of the broader construct ", 
                     parent$related,". This relationship illustrates that ",parent$selected, 
                     " is one aspect of ", parent$related,".")
      
      HTML(paste0(text))
      
    } else
      
      paste0("")
      
      

  })
  

  output$netTbl <- renderDataTable({
    
    to <- pcp_edges %>%
      filter(from %in% input$netVis_selected) %>%
      select(to_concept, from_concept) %>%
      rename(
        selected = from_concept,
        related = to_concept
      ) %>%
      mutate(relationship = 'Child') %>%
      left_join(pcp_nodes %>%
                  select(concept, concept_definition), 
                by = c("related" = "concept")
      )
    
    from <- pcp_edges %>%
      filter(to %in% input$netVis_selected) %>%
      select(to_concept, from_concept) %>%
      rename(
        selected = to_concept,
        related = from_concept
      ) %>%
      mutate(relationship = 'Parent') %>%
      left_join(pcp_nodes %>%
                  select(concept, concept_definition), 
                by = c("selected" = "concept")
      )
    
    from %>%
      rbind(to) %>%
      select(selected, related, relationship, concept_definition) %>%
      datatable(
        rownames = F,
        caption = "Aspect(s) of the selected concept",
        colnames = c("Selected concept", "Related concept(s)","Relationship","Related concept defintion(s)"),
        options = list(
          searching = F,
          bLengthChange = F,
          info = F,
          bPaginate = F,
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Montserrat'});","}"
            )
        )
        
      )
    
  })
  
  ## ui elements for inform
  
  output$select_concept <- renderUI({
    
    selectInput(
      "select_concept",
      label = "Select a concept:",
      choices = unique(reqs$concept),
      selected = unique(reqs$concept),
      multiple = TRUE
    )
    
  })
  
  output$regText <- renderText({
    
    if(input$select_concept == unique(reqs$concept))
    
    concept <- pcp_nodes %>%
      filter(concept %in% input$select_concept) %>%
        select(concept, concept_definition) %>%
        mutate(concept_definition = if_else(is.na(concept_definition) == T,
                                            "The concept you selected is not currently defined.",
                                            paste0(concept,": ",concept_definition)))
    
    paste0(concept$concept_definition)
    
  })
  
  output$downloadData <- downloadHandler(

    filename = function() {
      paste("reqs", Sys.Date(),".csv", sep="")
    },
    content = function(file) {
      write.csv(reqReactive(), file)
    }

  )
  
  reqReactive <- reactive({
    
   reqs %>%
      filter(concept %in% input$select_concept) %>%
      select(concept,requirement,title,page,url) %>%
      rename(
        Concept = concept,
        Requirement = requirement
      )
      
    
  })
  
  output$reqTbl <- renderReactable({
    
    reqReactive() %>%
      select(-url) %>%
      reactable(filterable = TRUE,
                defaultPageSize = 10,
                groupBy = c("Concept","Requirement"),
                columns = list(
                  title = colDef(
                    header = "Document Title",
                    html = TRUE,
                    cell = function(value, index){
                      sprintf('<a href="%s" target="_blank">%s</a>', reqReactive()$url[index], value)
                    }
                  ),
                  page = colDef(
                    header = "Starts on Page..."
                  )
                )
      )
    
    
  })
  
  ## ui elements for standard
  
  output$cmsTbl <- renderDataTable({
    
    domain_qm_bhdda %>%
      filter(key_planning == T | key_decision == T) %>%
      select(cmit_id, nqf_id, measure_title, measure_description) %>%
      datatable(
        rownames = F,
        caption = "CMS Measures Inventory",
        colnames = c("CMIT Ref No", "NQF ID", "Measure Title", "Measure Description"),
        options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$('body').css({'font-family': 'Montserrat'});","}"
          )
        #   searching = F,
        #   bLengthChange = F,
        #   info = F
        )
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
    
    
    #write.csv(x = data, file = file.path(responsesDir, fileName),
    #          row.names = FALSE, quote = TRUE)
    
    
    x<-data.frame(data)
    
   # write_csv(x,paste0("data/",fileName))
    
    storage_write_csv(x, cont, fileName)
    
  }
  
#  action to take when submit button is pressed
   observeEvent(input$submit, {

    saveData(formData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")

  })
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })

})
