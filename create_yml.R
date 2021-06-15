#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(yaml)


# define functions --------------
print_yaml <- function(){yaml_obj[names(yaml_obj) %>% order_names()]}

update_name <- function(name){yaml_obj$`project_name` <- name; yaml_obj[names(yaml_obj) %>% order_names()]}

order_names <- function(nm){nm %>%
        str_replace_all(pattern = "project_name", "01_project_name") %>%
        str_replace_all(pattern = "person_", "02_person_") %>%
        str_replace_all(pattern = "species_", "03_species_") %>%
        str_replace_all(pattern = "location_", "04_location_") %>%
        str_replace_all(pattern = "data_file_type_overview", "05_data_file_type_overview") %>%
        str_replace_all(pattern = "data_description", "06_data_description") %>%
        str_replace_all(pattern = "dates", "07_dates") %>%
        str_replace_all(pattern = "funding_sources", "08_funding_sources") %>% 
        order()
    }

new_person <- function(name, inst, email, role, date){
    new_id <- str_c("person_", (sum(str_count(names(yaml_obj), "person_")) + 1) %>%
                        str_pad(width = 2,pad = 0))
    yaml_obj[[new_id]] <- list(name = name,
                               institute = inst,
                               email = email,
                               role = role,
                               date = str_c(date, collapse = " - "))
    yaml_obj <<- yaml_obj[names(yaml_obj) %>% order_names()]
}

new_species <- function(name_eng, name_loc, name_sci){
    new_id <- str_c("species_", sum(str_count(names(yaml_obj),"species_")) + 1)
    yaml_obj[[new_id]] <- list(name_eng = name_eng,
                               name_loc = name_loc,
                               name_sci = name_sci)
    yaml_obj <<- yaml_obj[names(yaml_obj) %>% order_names()]
}

new_location <- function(country, region, park, field_station, lat_log){
    new_id <- str_c("location_", sum(str_count(names(yaml_obj),"location_")) + 1)
    
    list_loc <- list(country = country,
                     region = region,
                     park = park,
                     field_station = field_station,
                     lat_log = lat_log)
    
    yaml_obj[[new_id]] <- list_loc[sapply(list_loc, function(x){ x != "" })]
    yaml_obj <<- yaml_obj[names(yaml_obj) %>% order_names()]
}

new_data_type <- function(data_type){
    if( sum(str_count(names(yaml_obj),"data_file_type_overview")) == 0 ){ 
        yaml_obj$data_file_type_overview <- list()
    }
    
    new_id <- str_c("file_type_",sum(str_count(names(yaml_obj$data_file_type_overview),"file_type_")) + 1)
    
    yaml_obj$data_file_type_overview <- c(yaml_obj$data_file_type_overview, new_id = data_type)
    
    names(yaml_obj$data_file_type_overview)[names(yaml_obj$data_file_type_overview) == "new_id"] <- new_id
    yaml_obj <<- yaml_obj[names(yaml_obj) %>% order_names()]
}

new_data_desc <- function(data_desc){
    if( sum(str_count(names(yaml_obj),"data_description")) == 0 ){ 
        yaml_obj$data_description <- list()
    }
    
    new_id <- str_c("data_desc_",sum(str_count(names(yaml_obj$data_description),"data_desc_")) + 1)
    
    yaml_obj$data_description <- c(yaml_obj$data_description, new_id = data_desc)
    
    names(yaml_obj$data_description)[names(yaml_obj$data_description) == "new_id"] <- new_id
    yaml_obj <<- yaml_obj[names(yaml_obj) %>% order_names()]
}

new_fund <- function(fund_id){
    if( sum(str_count(names(yaml_obj),"funding_sources")) == 0 ){ 
        yaml_obj$funding_sources <- list()
    }
    
    new_id <- str_c("fund_",sum(str_count(names(yaml_obj$funding_sources),"fund_")) + 1)
    
    yaml_obj$funding_sources <- c(yaml_obj$funding_sources, new_id = fund_id)
    
    names(yaml_obj$funding_sources)[names(yaml_obj$funding_sources) == "new_id"] <- new_id
    yaml_obj <<- yaml_obj[names(yaml_obj) %>% order_names()]
}

new_date <- function(date_id, date_date){
    if( sum(str_count(names(yaml_obj),"dates")) == 0 ){ 
        yaml_obj$dates <- list()
    }
    
    # yaml_obj$dates <- c(yaml_obj$dates, new_id = date_date)
    yaml_obj$dates <- c(yaml_obj$dates, new_id = str_c(date_date, collapse = " - "))
    
    names(yaml_obj$dates)[names(yaml_obj$dates) == "new_id"] <- date_id
    yaml_obj <<- yaml_obj[names(yaml_obj) %>% order_names()]
}

add_custom_field <- function(custom_name, custom_content){
    cat(str_c(custom_name, ": ", custom_content, "\\n"))
}


# define objects --------------
yaml_obj <- list( project_name = "")
proj_name <- ''

# define UI for application (the user input panel) ------------
ui <- fluidPage(
    
    # App title ----
    titlePanel("Downloading Data"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            selectInput("new_project", "Project",
                        c(load = "load",
                          modify = "modify",
                          add_custom_entry = "add_custom_entry"
                        )),
            conditionalPanel(
                condition = "input.new_project == 'load'",
                fileInput("load_file", "Open Yaml", multiple = FALSE, width = NULL)),
            conditionalPanel(
                condition = "input.new_project == 'modify'",
                selectInput("next_field", "Next Field",
                            c(proj_name = "project_name",
                              person = "person", location = "location", 
                              species = "species",
                              file_types = "data_file_type_overview",
                              data_description = "data_description",
                              dates = "dates",
                              funding_sources = "funding_sources"
                            )
                ),
                conditionalPanel(
                    condition = "input.next_field == 'project_name'",
                textInput("proj_lab", "Project Name", value = "new_project", 
                          width = NULL, placeholder = NULL),
                actionButton("add_proj_name", "Update Project Name")
                ),
                conditionalPanel(
                    condition = "input.next_field == 'person'",
                    textInput("person_name", "Person Name", value = "", width = NULL, placeholder = NULL),
                    textInput("person_institution", "Institution", value = "", width = NULL, placeholder = NULL),
                    textInput("person_email", "Email", value = "", width = NULL, placeholder = NULL),
                    selectInput("person_role", "Role", c("PI", "PhD Student", "Masters Student", "Collaborator", "HIWI", "Field Assistant", "Lab Assistant", "Analyst")),
                    dateRangeInput("person_date", "Field Date"),
                    actionButton("add_person", "Add Field")
                ),
                conditionalPanel(
                    condition = "input.next_field == 'species'",
                    textInput("species_eng", "Common Name (English)", value = "", width = NULL, placeholder = NULL),
                    textInput("species_loc", "Local Name", value = "", width = NULL, placeholder = NULL),
                    textInput("species_sci", "Latin Name (Genus species)", value = "", width = NULL, placeholder = NULL),
                    actionButton("add_spec", "Add Field")
                ),
                conditionalPanel(
                    condition = "input.next_field == 'location'",
                    textInput("loc_country", "Country", value = "", width = NULL, placeholder = NULL),
                    textInput("loc_region", "State/Province/Region", value = "", width = NULL, placeholder = NULL),
                    textInput("loc_park", "Park/Protected Area", value = "", width = NULL, placeholder = NULL),
                    textInput("loc_field_station", "Field Station", value = "", width = NULL, placeholder = NULL),
                    numericInput("loc_long",
                                 "Long",
                                 0,
                                 min = -180,
                                 max = 180,
                                 step = .001),
                    numericInput("loc_lat",
                                 "Lat",
                                 0,
                                 min = -90,
                                 max = 90,
                                 step = .001),
                    actionButton("add_location", "Add Field")
                ),
                conditionalPanel(
                    condition = "input.next_field == 'data_file_type_overview'",
                    selectizeInput(inputId = "data_type", 
                                   label =  "Data Type (select or free text)",
                                   options = list(create = TRUE),
                                   choices = c("csv", "gpgk", "gpx", "json", 
                                               "md", "mp3", "mp4", "pdf", "py",
                                               "R", "Rmd", "shp", "tex", "tiff",
                                               "tsv", "txt", "yaml/yml","jpg","paper")
                                   ),
                    actionButton("add_type", "Add Field")
                ),
                conditionalPanel(
                    condition = "input.next_field == 'data_description'",
                    # textInput("data_desc", "Data description", value = "", width = NULL, placeholder = NULL),
                    selectizeInput(inputId = "data_desc", 
                                   label =  "Data description (select or free text)",
                                   options = list(create = TRUE),
                                   choices = c("Behavioral (focal follows, scans, etc., annotations)",
                                               "Movement (gps collar, telemetry data, accelerometer)",
                                               "Vocalizations",
                                               "Ecological (i.e. species, plant surveys, soil samples)",
                                               "Climatic",
                                               "Camera Trap",
                                               "Spatial (i.e. GPS)",
                                               "Drone Imagery",
                                               "DNA Samples",
                                               "Tissue Samples")
                    ),
                    actionButton("add_desc", "Add Field")
                ),
                conditionalPanel(
                    condition = "input.next_field == 'dates'",
                    # textInput("date_id", "Date Specifier", value = "", width = NULL, placeholder = NULL),
                    selectizeInput(inputId = "date_id", 
                                   label =  "Date Specifier (select or free text)",
                                   options = list(create = TRUE),
                                   choices = c("Field Trip",
                                               "Data Collection",
                                               "Project Duration")
                    ),
                    # dateInput(inputId = "date_date", label = "Date"),
                    dateRangeInput(inputId = "date_date", label = "Date"),
                    actionButton("add_date", "Add Field")
                ),
                conditionalPanel(
                    condition = "input.next_field == 'funding_sources'",
                    textInput("fund_src", "Funding Source", value = "", width = NULL, placeholder = NULL),
                    actionButton("add_fund", "Add Field")
                ),
               # Button
                downloadButton("downloadData", "Download")
                
            ),
            conditionalPanel(
                condition = "input.new_project == 'add_custom_entry'",
                selectInput("custom_select", "Select Field",choices = names(yaml_obj),width = NULL),
                textInput("custom_name", "Field name", value = "", width = NULL, placeholder = NULL),
                textInput("custom_content", "Field name", value = "", width = NULL, placeholder = NULL))
            ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            tags$h3("current yml content:"),
            tags$code(htmlOutput("yaml"))
        )
        
    ))

# define server logic (actually calling the yaml update functions) ------------
server <- function(input, output, session) {
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$proj_lab, ".yml", sep = "")
        },
        content = function(file) {
            tmp_file <- tempfile()
            write_lines(x = str_c("# Project Metadata: ", input$proj_lab), file = file)
            write_yaml(x = update_name(input$proj_lab), file = tmp_file)
            write_lines(read_lines(tmp_file), file = file, append = TRUE)
            unlink(tmp_file)
        }
    )
    
    output$title <- renderText(paste0("Project Title: ", proj_name))
    
    observeEvent(input$load_file, {
        file <- input$load_file
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext %in% c("yml", "yaml"), "Please load a yml file"))
        
        yaml_obj <<- read_yaml(file$datapath)
        proj_name <<- yaml_obj$project_name
        
        output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>%
                                      str_replace_all(" ", "&nbsp"))
        
    })
    
    observeEvent(input$add_proj_name, {
        output$yaml <- renderText(str_replace_all(string = as.yaml(update_name(input$proj_lab),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>% 
                                      str_replace_all(" ", "&nbsp"))
    })
    
    observeEvent(input$add_person, {
        new_person(name = input$person_name,
                   inst = input$person_institution, 
                   email = input$person_email,
                   role = input$person_role,
                   date = input$person_date)
        
        output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>% 
                                      str_replace_all(" ", "&nbsp"))
    })

    observeEvent(input$add_spec, {
        new_species(name_eng = input$species_eng,
                    name_loc = input$species_loc, 
                    name_sci = input$species_sci)
        
        output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>% 
                                      str_replace_all(" ", "&nbsp"))
    })

    observeEvent(input$add_location, {
        new_location(country = input$loc_country,
                     region = input$loc_region, 
                     park = input$loc_park,
                     field_station = input$loc_field_station,
                     lat_log = str_c(input$loc_lat, " N; ", input$loc_long," E")#input$loc_lat_log
        )
        
        output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>% 
                                      str_replace_all(" ", "&nbsp"))
    })
    
    observeEvent(input$add_type, {
        new_data_type(data_type = input$data_type)
        
        output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>% 
                                      str_replace_all(" ", "&nbsp"))
    })
    
    observeEvent(input$add_desc, {
        new_data_desc(data_desc = input$data_desc)
        
        output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>% 
                                      str_replace_all(" ", "&nbsp"))
    })
    
    observeEvent(input$add_date, {
        new_date(date_id = input$date_id, date_date = as.character(input$date_date))
        
        output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>% 
                                      str_replace_all(" ", "&nbsp"))
    })
    
    observeEvent(input$add_fund, {
        new_fund(fund_id = input$fund_src)
        
        output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>% 
                                      str_replace_all(" ", "&nbsp"))
    })
    
    observeEvent(yaml_obj,{
        updateSelectInput(session, "custom_select",
                      label = paste("Select input label", names(yaml_obj)),
                      choices = names(yaml_obj),
                      selected = NULL
    )})
    
    observeEvent(input$custom_content, {
        add_custom_field(custom_name = input$custom_name, custom_content = input$custom_content)
        
        output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                                   indent = 6),
                                                  pattern = "\\n", replacement = "<br>") %>% 
                                      str_replace_all(" ", "&nbsp"))
    })
    
    # output$yaml <- renderText(as.yaml(list(foo = list(bar = 'baz')), indent = 3))
    output$yaml <- renderText(str_replace_all(string = as.yaml(print_yaml(),
                                                               indent = 6),
                                              pattern = "\\n", replacement = "<br>") %>% 
                                  str_replace_all(" ", "&nbsp"))
}

# Run the application 
shinyApp(ui = ui, server = server)
