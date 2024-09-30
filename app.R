# Packages ------------------------------------------------------------------
{
  library(shiny)
  library(htmlwidgets)
  library(sortable)
  library(shinyWidgets)
  library(shinycssloaders)
  library(shinybusy)
  library(here)
  library(janitor)
  library(rio)
  library(tidyverse)
  library(DT)
  library(plotly)
  library(toxpiR)

  library(ComptoxR)
  library(grateful)
}

# Options ------------------------------------------------------------------
options(shiny.maxRequestSize = 30 * 1024^2)
options(tinytex.verbose = TRUE)
options(knitr.table.format = "latex")
options(kableExtra.latex.load_packages = TRUE)
options(dplyr.summarise.inform = FALSE)

# Data ------------------------------------------------------------------
sswqs <- list.files(pattern = "^sswqs_curated_*")

if (length(sswqs == 1L)) {
  sswqs <- rio::import(file = sswqs)

  cli::cli_alert_info(paste0("SSWQS loaded: ", list.files(pattern = "^sswqs_curated_*")))
} else {
  cli::cli_abort("Issue with SSWQS files: more than one or zero detected!")
}


# Variables ------------------------------------------------------------------

haz_labels <- list(
  "dtxsid",
  "name",
  "acuteMammalianOral",
  "acuteMammalianDermal",
  "acuteMammalianInhalation",
  "developmental",
  "reproductive",
  "endocrine",
  "genotoxicity",
  "carcinogenicity",
  "neurotoxicitySingle",
  "neurotoxicityRepeat",
  "systemicToxicitySingle",
  "systemicToxicityRepeat",
  "eyeIrritation",
  "skinIrritation",
  "skinSensitization",
  "acuteAquatic",
  "chronicAquatic",
  "persistence",
  "bioaccumulation",
  "exposure"
)

`%ni%` <- Negate(`%in%`)


{
  user_filters <- list()

  user_filters$unitname <- unique(sswqs$unit_name)

  user_filters$range <- unique(sswqs$is_range)

  user_filters$protection <- unique(sswqs$protection)

  user_filters$sourcewater <- unique(sswqs$sourcewater)

  user_filters$duration <- unique(sswqs$duration)

  user_filters$enduse <- unique(sswqs$enduse)

  user_filters$origin_category <-
    unique(sswqs$origin_category)

  user_filters <- user_filters %>%
    map(., sort)

  user_filters$enduse <- user_filters$enduse[order(match(user_filters$enduse, c("Organism", "Water & Organism", "UNC")))]

  user_filters$origin_category <- user_filters$origin_category[order(match(user_filters$origin_category, c("Federal", "State", "Other")))]

  user_filters$data_category <- unique(sswqs$data_category)
}

cust_pal <- ComptoxR::cust_pal

qhrtet_ver <- paste0("QHRTET ver ", packageVersion("ComptoxR"))

# UI ------------------------------------------------------------------
ui <- navbarPage(qhrtet_ver,
  id = "nav",
  header = shinybusy::add_busy_bar(color = "#0dc5c1"),
  tags$script(src = "https://kit.fontawesome.com/c64da12c89.js"),

  ## 1. Landing ------------------------------------------------------------------
  tabPanel(
    "Landing",
    fluidRow(
      column(
        12,
        h2("Explanation of tool")
      )
    ),
    fluidRow(
      column(
        12,
        h4("The Quantitative Hazard, Risk, Toxicological Evaluation Tool (QHRTET) was developed for the exploration of user-supplied datasets as well as the FracFocus database for environmental exposure and risk characterization.")
      )
    ),
    fluidRow(
      column(
        12,
        h4("QHRTET provides access to experimental and predicted physico-chemical properties, environmental fate and transport information, and appropriately linked toxicity data. This research improves the understanding of chemical fate and activity in both organisms (humans and wildlife) and the environment to support chemical safety decision making.")
      )
    ),
    fluidRow(
      column(
        12,
        h4("QHRTET is a prototype and uses a compilation of information sourced from many sites, databases and sources including U.S. Federal and state sources and international bodies that saves the user time by providing information in one location. The data are not reviewed by USEPA – the user must apply judgment in use of the information. The results do not indicate EPA’s position on the use or regulation of these chemicals. EPA is distributing this information solely as a public service.")
      )
    ),
    fluidRow(
      column(
        12,
        h2("Current QHRTET Modules")
      )
    ),
    # fluidRow(
    #   column(
    #     12,
    #     h3("Produced Water Exploration")
    #   )
    # ),
    # fluidRow(
    #   column(
    #     12,
    #     h4("This module explores the FracFocus disclosure database from the Ground Water Protection Council. It current contains information from reports from over 1,600 companies for over 189,000 wells. Here, QHRTET has processed the data to remove erroneous data, allowing a user to explore validated information on a given compound. Of note, the ability to compare a given compound against an aggregated chemical list from the CompTox Chemical Dashboard to see geo-spatial prevalence.")
    #   )
    # ),
    fluidRow(
      column(
        12,
        h3("Water Quality Standards")
      )
    ),
    fluidRow(
      column(
        12,
        h4("The Water Quality Standard Module takes all available promulgated surface water standards from US EPA, and allows a user to compare these standards against the FracFocus data or a user provided dataset. Where a standard is not available, users can upload a customized standards dataset, or use another federal/state/tribal standard.")
      )
    ),
    fluidRow(
      column(
        12,
        h3("Hazard Comparsion")
      )
    ),
    fluidRow(
      column(
        12,
        h4("This module allow user to access a simplified and localized version of the Cheminformatics 'Hazard' module, allowing a user to explore data from the Produced Water or a user-uploaded dataset.")
      )
    ),
    fluidRow(
      column(
        12,
        h3("HRT Prioritization")
      )
    ),
    fluidRow(
      column(
        12,
        h4("Utilizing a min-max feature scaling normalization scheme, a set of compounds are ranked and assigned a relative risk score based on data from the Hazard Comparison module, as well as from the Cheminformatics 'Toxprint' module. This unitless score provides a way for a user to compare similar or disparate sets of compounds. Where data is lacking, an interpolation value option is provided as a conservative option. A more advanced option that incorporates subjective weighing of factors, geo-location, and concentration of observed compounds is also provided.")
      )
    )
  ),
  ## 2. Upload ------------------------------------------------------------------
  navbarMenu(
    "User Upload",
    ### User Data --------------------------------------------------------------------
    tabPanel(
      "Data Upload",
      fluidRow(
        column(
          12,
          h5("Malformed user input could result in data not being analyzed. Users are suggested to use included templates for local data.")
        )
      ),
      fileInput("user_data", "Upload user data",
        multiple = FALSE,
        accept = c(".xlsx", ".csv")
      ),
      fluidRow(
        column(
          12,
          actionButton("user_parse_button", "Parse data"),
          actionButton("data_template_download", "Download template"),
          actionButton("cleaned_user_data", "Download curated data")
        )
      ),
      tags$hr(),
      DTOutput("user_df") %>% withSpinner(color = "#0dc5c1")
      # tags$hr(),
      # verbatimTextOutput('user_df')
    ),
    ### User Benchmarks ------------------------------------------------------------------
    tabPanel(
      "Benchmark Upload",
      fluidRow(
        column(
          12,
          h5("Malformed user input could result in data not being analyzed. Users are suggested to use included templates for local data.")
        )
      ),
      fileInput("wqs_upload", "Upload benchmarks",
        multiple = FALSE,
        accept = c(".xlsx", ".csv")
      ),
      fluidRow(
        actionButton("wqs_parse_button", "Parse Benchmark data"),
        actionButton("wqs_download", "Download template"),
      ),
      tags$hr(),
      DTOutput("user_wqs") %>% withSpinner(color = "#0dc5c1"),
      tags$hr(),
      DTOutput("user_b_list_tbl") %>% withSpinner(color = "#0dc5c1")
    ),
  ),
  ## 3. Cheminformatics ------------------------------------------------------------------
  navbarMenu(
    "Hazard Comparison",

    ### Hazard ---------------------------------------------------------

    tabPanel(
      "Cheminformatics",
      fluidPage(
        fluidRow(
          column(
            width = 3,
            selectInput("haz_cust_resp",
                        label = "Hazard profile",
                        choices = c("Full", "Emergency Response", "Site-Specific"),
                        selected = "Full"
            )
          ),
          column(
            width = 3,
            tags$h5(strong('Import user endpoint data?')),
            switchInput("haz_user_cust_import",
                        size = "mini",
                        onLabel = "TRUE",
                        offLabel = "FALSE"
                        #,labelWidth = "120px"
            ),
            align = "center"
            ,style = "margin-top: -5px;"
          ),
          column(width = 3,
                 tags$h5(strong('Query compounds')),
                 actionButton("haz_button",label = NULL, icon =tags$i(class = "fa-solid fa-cart-shopping"))
                 ,align = "center"
                 ,style = "margin-top: -5px;"
          ),
          column(width = 3,
                 uiOutput("haz_endpoints_input")
          )
        ),
        tags$hr(),
        fluidRow(
          DTOutput("hazard_table") %>%
            withSpinner(color = "#0dc5c1")
        ),
        tags$hr(),
        fluidRow(
          tags$h4("Endpoint Data Coverage"),
          tags$h5("Assign a bias weight for each endpoint of interest"),
          actionButton("tp_score_button", "Relative Risk Ranking"),
          tags$hr(),
          DTOutput("endpoint_coverage") %>%
            withSpinner(color = "#0dc5c1")
        )
      )
    ),

    ### Relative Risk Ranking -------------------------------------------------------------------
    tabPanel(
      "Relative Risk Ranking",
      tags$hr(),
      fluidPage(
        fluidRow(
          #### Table ranking ----------------------------------------------------
          column(
            width = 6,
            DTOutput("tp_table") %>%
              withSpinner(color = "#0dc5c1"),
          ),

          #### Ranked plot -------------------------------------------------------------
          column(
            width = 6,
            plotlyOutput("tp_table_plot") %>%
              withSpinner(color = "#0dc5c1")
          )
        ),
        tags$hr(),
        fluidRow(

          #### Per Compound Details ----------------------------------------------------
          column(
            width = 6,
            DTOutput("tp_single_table") %>%
              withSpinner(color = "#0dc5c1")
          ),

          #### Per Compound Plot -------------------------------------------------------
          column(
            width = 6,
            plotOutput("tp_single_plot") %>%
              withSpinner(color = "#0dc5c1")
          )
        )
      )
    )
  ),
  ## 4. Exploration -------------------------------------------------------------
  navbarMenu(
    "Exploration",
    ### Benchmark Prioritization ------------------------------------------------------------------
    tabPanel(
      "Benchmark Priortization",
      sidebarLayout(

        #### side panel --------------------------------------------------------------
        sidebarPanel(
          actionButton("filt_benchmark_data", "Filter Benchmarks"),
          checkboxInput("toggle_restrict_data",
            label = "Restrict benchmarks to user data?",
            value = TRUE
          ),
          tags$hr(),
          pickerInput(
            inputId = "origin_category",
            label = "Origin Category",
            choices = user_filters$origin_category,
            selected = user_filters$origin_category,
            multiple = T
          ),
          pickerInput(
            inputId = "data_category",
            label = "Data Category",
            choices = user_filters$data_category,
            selected = user_filters$data_category,
            multiple = T
          ),
          pickerInput(
            inputId = "filt_unit",
            label = "Unit Name",
            choices = user_filters$unitname,
            selected = user_filters$unitname,
            multiple = TRUE
          ),
          pickerInput(
            inputId = "filt_range",
            label = "Ranged Values",
            choices = user_filters$range,
            selected = user_filters$range,
            multiple = TRUE
          ),
          pickerInput(
            inputId = "filt_protection",
            label = "Level of Protection",
            choices = user_filters$protection,
            selected = user_filters$protection,
            multiple = TRUE
          ),
          pickerInput(
            inputId = "filt_sourcewater",
            label = "Source water",
            choices = user_filters$sourcewater,
            selected = user_filters$sourcewater,
            multiple = TRUE
          ),
          pickerInput(
            inputId = "filt_duration",
            label = "Duration",
            choices = user_filters$duration,
            selected = user_filters$duration,
            multiple = TRUE
          ),
          pickerInput(
            inputId = "filt_enduse",
            label = "Enduse",
            choices = user_filters$enduse,
            selected = user_filters$enduse,
            multiple = TRUE
          )
        ),
        #### main panel --------------------------------------------------------------

        mainPanel(
          tags$head(
            tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
          ),
          fluidRow(
            column(
              width = 12,
              uiOutput("benchmark_bucket")
            )
          ),
          # tags$hr(),
          fluidRow(
            column(
              width = 12,
              DTOutput("benchmark_summary") %>% withSpinner(color = "#0dc5c1")
            )
          )
        )
      )
    ),

    ### Site Exploration --------------------------------------------------------
    # tabPanel('Site Exploration', fluidPage(
    #   fluidRow(
    #     # group filter
    #
    #     pickerInput(inputId = 'filt_group',
    #                 label = 'Group',
    #                 choices = user_data_list$group),
    #
    #     # compound filter
    #
    #     pickerInput(inputId = 'filt_compound',
    #                 label = 'Select compound',
    #                 choices = user_data_list$compound),
    #
    #     #date slider
    #
    #     # sliderInput(inputId = 'filt_date',
    #     #             label = 'Filter date',
    #     #             min = ,
    #     #             max = , )
    #   ),
    #   fluidRow(
    #     # site list table? or list?
    #
    #     # Map
    #
    #     # Plots
    #   ),
    #   fluidRow(
    #     #benchmark table -> row selection to hline on plots
    #   )
    # )),

    ### Site Summary ------------------------------------------------------------
    tabPanel("Site Summary", fluidPage()),

    ### Site Prioritization ------------------------------------------------------
    tabPanel("Site Priortization", fluidPage())
  ),


  ## 5. Citations -----------------------------------------------------------------
  navbarMenu(
    "Citations and Contact",
    tabPanel(
      "Citations",
      fluidPage(
        htmlOutput("grateful_report")
      )
    ),
    tabPanel(
      "Contact",
      # h2("Hecho mit liebe"),
      h6("Sean Thimons"),
      h6("ORISE Reseach Fellow"),
      h6("Water Infrastructure Division"),
      h6("Center for Environmental Solutions and Emergency Response"),
      h6("Office of Research and Development"),
      h6("U.S. Environmental Protection Agency"),
      h6("thimons.sean@epa.gov"),
      h3("Disclaimer"),
      h6("This tool was supported in part by an appointment to the Research Participation Program at the Office of Research and Development, Center for Environmental Solutions and Emergency Response, U.S. Environmental Protection Agency, administered by the Oak Ridge Institute for Science and Education through an interagency agreement between the U.S. Department of Energy and EPA.")
    )
  )



  #### UI Ends ------------------------------------------------------------------
)
# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  ## Data --------------------------------------------------------------------

  ### User Site Data ----------------------------------------------------------
  {
    user_data_list <- reactiveValues()

    user_data_list$upload <- NULL
    user_data_list$cleaned <- NULL

    user_data_list$hazard <- NULL
    user_data_list$expo <- NULL

    user_data_list$compound <- NULL

    user_data_list$site <- NULL
    user_data_list$group <- NULL
    user_data_list$date <- list(min = NA, max = NA)
  }

  observeEvent(input$user_data, {
    user_data_list$upload <- rio::import(input$user_data$datapath) %>%
      janitor::clean_names() %>%
      filter(result_flag == "DETECT" &
        tic == "NOT TIC" &
        surrogate == "NOT SURROGATE" &
        !str_detect(site_name, "DUP")) %>%
      mutate(
        site_name = fct_reorder(site_name, dist),
        date = as_date(date)
      )

    user_data_list$site <- unique(user_data_list$upload$site)

    user_data_list$group <- unique(user_data_list$upload$group)

    user_data_list$date <- list(
      min = user_data_list$upload$date,
      max = user_data_list$upload$date
    )
  })

  ### Cleaning upload ---------------------------------------------------------

  observeEvent(input$user_parse_button, {
    req(user_data_list$upload)

    if ("preferred_name" %in% colnames(user_data_list$upload) & "dtxsid" %in% colnames(user_data_list$upload)) {
      cli::cli_alert_info("Curated data detected!")
      cli::cli_text()
      user_data_list$upload_cleaned <- user_data_list$upload %>% rename(preferredName = preferred_name)
      user_data_list$compound <- unique(user_data_list$upload_cleaned$dtxsid)
      user_data_list$curated <- user_data_list$upload_cleaned %>%
        filter(!is.na(cas_chk)) %>%
        distinct(analyte, cas_number, .keep_all = T) %>%
        select(analyte, cas_number, cas_chk, dtxsid, preferredName)
    } else {
      user_data_list$cleaned <-
        user_data_list$upload %>%
        distinct(., analyte, cas_number, .keep_all = F)

      user_data_list$cleaned$cas_chk <- webchem::is.cas(user_data_list$cleaned$cas_number)

      user_data_list$cleaned <- user_data_list$cleaned %>%
        arrange(cas_number, cas_chk, analyte) %>%
        filter(!is.na(cas_number) & !is.na(analyte))

      # CAS
      cas_search <- user_data_list$cleaned %>%
        filter(., cas_chk == TRUE) %>%
        select(cas_number) %>%
        ungroup()

      cas_raw <- ComptoxR::ct_search(
        type = "string",
        search_param = "equal",
        query = cas_search$cas_number,
      )

      cas_search <- cas_raw %>%
        distinct(., searchValue, .keep_all = T) %>%
        select(dtxsid, preferredName, searchValue, rank)

      # Name
      name_search <- user_data_list$cleaned %>%
        filter(., cas_chk == TRUE) %>%
        select(analyte) %>%
        ungroup()

      name_raw <- ComptoxR::ct_search(
        type = "string",
        search_param = "equal",
        query = name_search$analyte,
      )

      name_search <- name_raw %>%
        distinct(., searchValue, .keep_all = T) %>%
        select(dtxsid, preferredName, searchValue, rank)

      user_data_list$curated <- left_join(user_data_list$cleaned, cas_search, by = c("cas_number" = "searchValue")) %>%
        left_join(., name_search, by = c("analyte" = "searchValue")) %>%
        pivot_longer(., cols = c(dtxsid.x, dtxsid.y), values_to = "dtxsid", names_to = "dtx", values_drop_na = T) %>%
        pivot_longer(., cols = c(preferredName.x, preferredName.y), values_to = "preferredName", names_to = "pref", values_drop_na = T) %>%
        select(-c(rank.x:dtx, pref)) %>%
        distinct(analyte, cas_number, .keep_all = T)

      cli::cli_alert_success("Cleaned!\n")

      # Compounds
      user_data_list$compound <- unique(user_data_list$curated$dtxsid)

      print(user_data_list$compound)
      cli::cli_text()

      user_data_list$upload_cleaned <- user_data_list$upload %>%
        left_join(., user_data_list$curated, by = (c("analyte", "cas_number")))
    }

    # Hazard query from CCD ------------------------------------------------------------------
    # user_data_list$hazard <-
    #
    #   ComptoxR::ct_hazard(query = user_data_list$search)
    #
    # user_data_list$expo <- user_data_list$hazard %>%
    #   filter(supercategory == 'Exposure Limit' | supercategory == 'Screening Limit')
    #
    # In-vitro query from CCD ------------------------------------------------------------------
    #
  })

  output$user_df <- renderDT(user_data_list$curated)

  observeEvent(input$cleaned_user_data, {
    req(user_data_list$upload_cleaned)
    rio::export(user_data_list$upload_cleaned, file = paste0("curated_user_data_", Sys.Date(), ".xlsx"))
  })

  ### User Benchmark ------------------------------------------------------------------
  wqs_raw_upload <- reactive({
    req(input$wqs_upload)
    wqs_up <- rio::import(input$wqs_upload$datapath) %>%
      janitor::clean_names()
    return(wqs_up)
  })


  output$user_wqs <- renderDT(
    wqs_raw_upload()
  )


  #### Benchmark Template ------------------------------------------------------

  observeEvent(input$wqs_download, {
    # FIX THIS-----
    wqs_temp <- tribble(
      ~REGION, ~ENTITY_NAME, ~ENTITY_ABBR, ~CAS_NO, ~STD_POLLUTANT_NAME, ~CRITERION_VALUE, ~UNIT_NAME, ~IS_RANGE, ~PROTECTION, ~SOURCEWATER, ~DURATION, ~ENDUSE, ~LOCAL, ~META, ~DATA_SOURCE, ~SHORT_CIT, ~CIT,
    ) %>%
      as.data.frame()
    wqs_temp[1, ] <- NA_character_
    wqs_temp[, 1:17] <- as.character(wqs_temp[, 1:17])

    rio::export(list("template" = wqs_temp), file = "benchmark_template.xlsx")
  })


  #### User Benchmark List names ------------------------------------------------------------------

  user_b_list <- reactive({
    req(input$wqs_raw_upload)
    user_b_list <- wqs_raw_upload() %>%
      filter(!is.na(source)) %>%
      distinct(source)
    return(user_b_list)
  })

  output$user_b_list_tbl <- renderDT(
    user_b_list()
  )

  ### Benchmark list ------------------------------------------------------------------

  benchmark_rv <- reactiveValues()
  benchmark_rv$list <- list(
    "State-Specific Water Quality Standards",
    "CCD Exposure and Screening Limits",
    "Cheminformatics Hazard endpoints"
    # ,'ToxCast in-vitro endpoints'
    # TODO
  )
  benchmark_rv$summary_table <- sswqs

  # creates list with new values
  observeEvent(input$wqs_upload, {
    if (!is.null(user_b_list())) {
      update <- isolate(user_b_list()$source)
      benchmark_rv$list <- append(benchmark_rv$list, update)
    }
  })

  # builds super table
  observeEvent(input$wqs_upload, {
    if (!is.null(user_b_list())) {
      raw_user <-
        raw_user_upload() %>%
        filter(!is.na(source))

      benchmark_rv$summary_table <- bind_rows(sswqs, raw_user)
    }
  })

  output$benchmark_bucket <- renderUI({
    bucket_list(
      header = "Drag the items into desired bucket",
      group_name = "bucket_list_group",
      orientation = "horizontal",
      add_rank_list(
        text = "Available benchmark sources",
        labels = benchmark_rv$list,
        input_id = "rank_list_1"
      ),
      add_rank_list(
        text = "Priortization Order",
        labels = NULL,
        input_id = "rank_list_2"
      )
    )
  })

  #### Benchmark Summary Table, Prioritized -------------------------------------

  observeEvent(input$filt_benchmark_data, {
    benchmark_rv$summary_table_filt <-
      benchmark_rv$summary_table %>%
      select(-c(
        criterion_id,
        cit,
        origin_supercategory,
        origin_agency
      )) %>%
      filter(
        source %in% input$rank_list_2 &
          origin_category %in% input$origin_category &
          is_range %in% input$filt_range &
          protection %in% input$filt_protection &
          sourcewater %in% input$filt_sourcewater &
          duration %in% input$filt_duration &
          enduse %in% input$filt_enduse &
          data_category %in% input$data_category &
          unit_name %in% input$filt_unit
      )

    if (input$toggle_restrict_data == TRUE) {
      benchmark_rv$summary_table_filt <- benchmark_rv$summary_table_filt %>%
        filter(dtxsid %in% user_data_list$upload_cleaned$dtxsid)
    }
  })

  output$benchmark_summary <- renderDT({
    benchmark_rv$summary_table_filt
  })

  ## Cheminformatics ------------------------------------------------------------------

  ### Hazard Comparison -------------------------------------------------------

  haz_df <- reactiveValues()
  haz_df$data <- NULL
  haz_df$cache <- NULL
  haz_df$tp_end <- NULL
  haz_df$tp_end_cache <- NULL
  haz_df$tp_var <- NULL
  haz_df$search <- NULL

  ##### Hazard query button -----------------------------------------------------

  observeEvent(input$haz_button, {
    req(user_data_list$compound)

    haz_df$data <- ComptoxR::chemi_hazard(user_data_list$compound)
    haz_df$data$joined <- left_join(haz_df$data$headers, haz_df$data$score, by = "dtxsid")
    haz_df$cache <- haz_df$data$joined
    haz_df$tp_end <- ComptoxR::tp_endpoint_coverage(haz_df$data$records, id = "dtxsid", filter = 0.1)
    haz_df$tp_end_cache <- haz_df$tp_end
  })

  ##### Profile selection -------------------------------------------------------

  endpoints_list <- list(
    'Full' = list(
      "acuteMammalianOral",
      "acuteMammalianDermal",
      "acuteMammalianInhalation",
      "developmental",
      "reproductive",
      "endocrine",
      "genotoxicity",
      "carcinogenicity",
      "neurotoxicitySingle",
      "neurotoxicityRepeat",
      "systemicToxicitySingle",
      "systemicToxicityRepeat",
      "eyeIrritation",
      "skinIrritation",
      "skinSensitization",
      "acuteAquatic",
      "chronicAquatic",
      "persistence",
      "bioaccumulation",
      "exposure"
    ),
    "Emergency Response" = list(
      "acuteMammalianOral",
      "acuteMammalianDermal",
      "acuteMammalianInhalation",
      "genotoxicity",
      "neurotoxicitySingle",
      "systemicToxicitySingle",
      "eyeIrritation",
      "skinIrritation",
      "skinSensitization",
      "acuteAquatic"
    ),
    "Site-Specific" = list(
      "developmental",
      "reproductive",
      "endocrine",
      "genotoxicity",
      "carcinogenicity",
      "neurotoxicityRepeat",
      "systemicToxicityRepeat",
      "chronicAquatic",
      "persistence",
      "bioaccumulation"
    )
  )


  output$haz_endpoints_input <- renderUI({
    pickerInput('haz_sort_list',
                label = 'Endpoints',
                choices = endpoints_list[[input$haz_cust_resp]],
                selected = endpoints_list[[input$haz_cust_resp]],
                multiple = T)
  })

  haz_table_filt <- reactive({
    if (!identical(
      colnames(haz_df$data$joined),
      input$haz_sort_list)
    ) {
      haz_df$data$joined <- haz_df$cache
    }

    filter_list <- c('dtxsid', 'name', input$haz_sort_list)

    haz_df$data$joined[filter_list]
  })

  output$hazard_table <- renderDT({
    if (is.null(haz_df$data)) {
      default_tbl <- tribble(
        ~dtxsid,
        ~name,
        ~acuteMammalianOral,
        ~acuteMammalianDermal,
        ~acuteMammalianInhalation,
        ~developmental,
        ~reproductive,
        ~endocrine,
        ~genotoxicity,
        ~carcinogenicity,
        ~neurotoxicitySingle,
        ~neurotoxicityRepeat,
        ~systemicToxicitySingle,
        ~systemicToxicityRepeat,
        ~eyeIrritation,
        ~skinIrritation,
        ~skinSensitization,
        ~acuteAquatic,
        ~chronicAquatic,
        ~persistence,
        ~bioaccumulation,
        ~exposure
      )
      return(DT::datatable(default_tbl))
    }

    DT::datatable(
      haz_table_filt()
      # ,extensions = "Buttons",
      # options = list(
      #   dom = "Bfrtip",
      #   buttons = c("copy", "csv", "excel", "pdf", "print")
      # )
    ) %>%
      formatStyle(names(haz_table_filt()),
        textAlign = "center"
      ) %>%
      formatStyle(names(haz_table_filt()),
        backgroundColor = styleEqual("VH", "#f5c6cb")
      ) %>%
      formatStyle(names(haz_table_filt()),
        backgroundColor = styleEqual("H", "#ff8c0066")
      ) %>%
      formatStyle(names(haz_table_filt()),
        backgroundColor = styleEqual("M", "#ffeeba")
      ) %>%
      formatStyle(names(haz_table_filt()),
        backgroundColor = styleEqual("L", "#c3e6cb")
      ) %>%
      formatStyle(names(haz_table_filt()),
        backgroundColor = styleEqual("I", "#d6d8db")
      ) %>%
      formatStyle(names(haz_table_filt()),
        backgroundColor = styleEqual("ND", "SlateGray")
      )
  })

  endpoint_filt <- reactive({
    req(haz_table_filt())

    colnames(haz_table_filt()) %>%
      str_subset(., pattern = "dtxsid|name", negate = T)
  })

  output$endpoint_coverage <- renderDT({
    req(haz_df$data)

    haz_df$tp_end %>%
      select(endpoint_filt()) %>%
      DT::datatable(.,
        options = list(pageLength = 20),
        editable = list(
          target = "cell",
          disable = list(columns = c(1, 2))
        )
      ) %>%
      formatRound("score", 2) %>%
      formatPercentage("score", 0)
  })

  ### Relative Risk Ranking ---------------------------------------------------
  tp <- reactiveValues()
  tp$data <- NULL
  tp$bias <- NULL

  observeEvent(input$tp_score_button, {
    req(haz_df$data)

    tp$bias <- haz_df$tp_end

    tp$data <- ComptoxR::tp_combined_score(
      table = haz_df$data$records,
      bias = tp$bias
    )
  })

  #### TP table ----------------------------------------------------------------

  output$tp_table <- renderDT({
    req(tp$data)

    tp$data$tp_scores %>%
      left_join(., haz_df$data$headers, by = "dtxsid") %>%
      select(name, score) %>%
      arrange(desc(score)) %>%
      DT::datatable(.)
  })


  #### TP plot -----------------------------------------------------------------

  output$tp_table_plot <- renderPlotly({
    req(tp$data)

    s <- input$tp_table_rows_selected

    df_dat <- tp$data$tp_scores %>%
      left_join(., haz_df$data$headers, by = "dtxsid") %>%
      left_join(., tp$data$variable_coverage, by = "dtxsid") %>%
      arrange(desc(score)) %>%
      # select(name, score, data_coverage) %>%
      mutate(dtxsid = forcats::fct_reorder(dtxsid, score))

    p1 <- ggplot(df_dat) +
      aes(x = score, y = dtxsid, colour = data_coverage, text = name) +
      geom_point(shape = "circle", size = 3.45, alpha = 0.5) +
      scale_color_viridis_c(option = "viridis", direction = 1) +
      labs(
        x = "Score",
        y = "Compound",
        title = "Relative Risk Ranking",
        caption = "Color indicates data coverage on [0-1] scale",
        color = "Coverage Score"
      ) +
      theme_classic() +
      theme(
        axis.title.y = element_text(size = 14L),
        axis.title.x = element_text(size = 14L)
      )

    if (length(s)) {
      p1 <- p1 +
        geom_point(data = df_dat[s, , drop = FALSE], shape = "23", size = 4, color = "black")
    }

    ggplotly(p1, height = 500)
  })


  #### Compound Details --------------------------------------------------------

  output$tp_single_table <- renderDT({
    req(tp$data)

    s <- input$tp_table_rows_selected

    df_data <- tp$data$tp_scores %>%
      left_join(., haz_df$data$headers, by = "dtxsid") %>%
      left_join(., tp$data$variable_coverage, by = "dtxsid") %>%
      arrange(desc(score)) %>%
      mutate(name = forcats::fct_reorder(name, score)) %>%
      select(!c(score, data_coverage, dtxsid))

    df_data[s, , drop = FALSE] %>%
      # rename(compound = name) %>%
      pivot_longer(cols = !c(name), names_to = "compound", values_to = "endpoint") %>%
      pivot_wider(id_cols = "compound", names_from = "name", values_from = "endpoint") %>%
      rename(Endpoint = compound)
  })

  #### Compound Plot -----------------------------------------------------------

  output$tp_single_plot <- renderPlot({
    req(tp$data)

    s <- input$tp_table_rows_selected

    df_data <- tp$data$tp_scores %>%
      left_join(., haz_df$data$headers, by = "dtxsid") %>%
      left_join(., tp$data$variable_coverage, by = "dtxsid") %>%
      arrange(desc(score)) %>%
      mutate(name = forcats::fct_reorder(name, score)) %>%
      select(!c(score, data_coverage, dtxsid))

    df_data <- df_data[s, , drop = FALSE] %>%
      rename(compound = name) %>%
      pivot_longer(., cols = !c(compound), values_to = "score") %>%
      ggplot(.) +
      aes(x = name, y = score, fill = name) +
      geom_bar(stat = "identity") +
      scale_fill_discrete(type = cust_pal) +
      coord_polar() +
      theme_void() +
      xlab(NULL) +
      labs(
        title = "Per compound breakdown",
        y = "Score",
        fill = "Endpoint"
      )

    if (length(s)) {
      df_data + facet_wrap(vars(compound), ncol = 3)
    } else {
      df_data
    }
  })


  ## Citations ---------------------------------------------------------------

  output$grateful_report <- renderUI({
    # tags$iframe(
    #   src = here('grateful-report.html'),
    #   seamless = TRUE
    # )
    includeHTML(here("grateful-report.html"))
  })

  # Server End ------------------------------------------------------------------
}
shinyApp(ui, server)

# END ---------------------------------------------------------------------
