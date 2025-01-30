# library(shiny)
# library(htmlwidgets)
# library(sortable)
# library(shinyWidgets)
# library(shinycssloaders)
# library(shinybusy)
# library(here)
# library(janitor)
# library(rio)
# library(tidyverse)
#
# ui <- navbarPage(title = 'TESTING',
#     tabPanel(
#       "Cheminformatics",
#       fluidPage(
#         fluidRow(
#           column(
#             width = 3,
#             selectInput("haz_cust_resp",
#                         label = "Hazard profile",
#                         choices = c("Full", "Emergency Response", "Site-Specific"),
#                         selected = "Full"
#             )
#           ),
#           column(width = 3,
#                  uiOutput("haz_endpoints_input")
#         )
#     ),
#     tags$hr(),
#     fluidRow(textOutput('filt_output'))
#   )
# )
# )
#
# server <- function(input, output, session) {
#
#   endpoints_list <- list(
#     'Full' = list(
#       "acuteMammalianOral",
#       "acuteMammalianDermal",
#       "acuteMammalianInhalation",
#       "developmental",
#       "reproductive",
#       "endocrine",
#       "genotoxicity",
#       "carcinogenicity",
#       "neurotoxicitySingle",
#       "neurotoxicityRepeat",
#       "systemicToxicitySingle",
#       "systemicToxicityRepeat",
#       "eyeIrritation",
#       "skinIrritation",
#       "skinSensitization",
#       "acuteAquatic",
#       "chronicAquatic",
#       "persistence",
#       "bioaccumulation",
#       "exposure"
#     ),
#     "Emergency Response" = list(
#       "acuteMammalianOral",
#       "acuteMammalianDermal",
#       "acuteMammalianInhalation",
#       "genotoxicity",
#       "neurotoxicitySingle",
#       "systemicToxicitySingle",
#       "eyeIrritation",
#       "skinIrritation",
#       "skinSensitization",
#       "acuteAquatic"
#     ),
#     "Site-Specific" = list(
#       "developmental",
#       "reproductive",
#       "endocrine",
#       "genotoxicity",
#       "carcinogenicity",
#       "neurotoxicityRepeat",
#       "systemicToxicityRepeat",
#       "chronicAquatic",
#       "persistence",
#       "bioaccumulation"
#     )
#   )
#
#
#   output$haz_endpoints_input <- renderUI({
#     pickerInput('haz_sort_list',
#                 label = 'Endpoints',
#                 choices = endpoints_list[[input$haz_cust_resp]],
#                 selected = endpoints_list[[input$haz_cust_resp]],
#                 multiple = T)
#   })
#
#   output$filt_output <- renderText({
#
#     input$haz_sort_list
#
#   })
#
# }
#
# shinyApp(ui, server)

# library(shiny)
# library(DT)
# library(ComptoxR)
#
# # Define a list of hex colors
# hex_colors <- ComptoxR::cust_pal %>% unlist() %>% paste0(., '80')
#
# ui <- fluidPage(
#   titlePanel("Colored DataTable Example"),
#   DTOutput("my_table")
# )
#
# server <- function(input, output) {
#   output$my_table <- renderDT({
#     # Create a sample dataset
#     data <- data.frame(
#       ID = 1:10,
#       Name = paste("Person", 1:10),
#       Value = runif(10, 1, 100)
#     )
#
#     # Create the datatable
#     datatable(
#       data,
#       class = list(stripe = FALSE),
#       options = list(
#         columnDefs = list(
#           list(
#             targets = 0,  # Target the first column (0-indexed)
#             render = JS(
#               sprintf(
#                 "function(data, type, row, meta) {
#                   var colors = %s;
#                   var colorIndex = meta.row %% colors.length;
#                   return '<div style=\"background-color: ' + colors[colorIndex] + '; width: 100%%; height: 100%%; display: flex; align-items: center; justify-content: center;\">' + data + '</div>';
#                 }",
#                 jsonlite::toJSON(hex_colors)
#               )
#             )
#           )
#         )
#       )
#     )
#   })
# }
# shinyApp(ui, server)

library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 5),

  tooltip(
    span("Hover over me", style = "text-decoration: underline; cursor: pointer;"),
    HTML(
      "<ul>
        <li>First item</li>
        <li>Second item</li>
        <li>Third item</li>
      </ul>"
    ),
    placement = "right"
  )
)

server <- function(input, output) {}

shinyApp(ui, server)
