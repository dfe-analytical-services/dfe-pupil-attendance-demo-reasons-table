server <- function(input, output, session) {
  
  # Setting up reactive levels for dropdown ------------------------------------------------------------
  
  # Overarching geographic levels
  geog_levels <- reactive({
    geog_lookup %>%
      dplyr::select(geographic_level) %>%
      unique() %>%
      as.data.table()
  })
  
  output$levels_filtered <- renderUI({
    selectInput(
      inputId = "geography_choice",
      label = "Choose geographic breakdown level:",
      choices = geog_levels(),
      selected = head(geog_levels, 1)
    )
  })
  
  
  # Regional geographies
  reg_geog <- reactive({
    geog_lookup %>%
      dplyr::filter(geographic_level == input$geography_choice) %>%
      dplyr::select(region_name) %>%
      unique() %>%
      as.data.table()
  })
  
  observe({
    if (input$geography_choice != "National") {
      reg_geog <- geog_lookup %>%
        dplyr::filter(geographic_level == input$geography_choice) %>%
        dplyr::select(region_name) %>%
        unique() %>%
        as.data.table()
    }
    updateSelectInput(session, "region_choice",
                      choices = reg_geog()
    )
  })
  
  output$reg_filtered <- renderUI({
    selectInput(
      inputId = "region_choice",
      label = "Choose region:",
      choices = reg_geog(),
      selected = head(reg_geog, 1)
    )
  })
  
  
  # Local authority geographies
  la_geog <- reactive({
    geog_lookup %>%
      dplyr::filter(geographic_level == input$geography_choice, region_name == input$region_choice) %>%
      dplyr::select(la_name) %>%
      unique() %>%
      as.data.table()
  })
  
  observe({
    if (input$geography_choice == "Local authority") {
      la_geog <- geog_lookup %>%
        dplyr::filter(geographic_level == input$geography_choice, region_name == input$region_choice) %>%
        dplyr::select(la_name) %>%
        unique() %>%
        as.data.table()
    }
    updateSelectInput(session, "la_choice",
                      choices = la_geog()
    )
  })
  
  output$la_filtered <- renderUI({
    selectInput(
      inputId = "la_choice",
      label = "Choose local authority:",
      choices = la_geog(),
      selected = head(la_geog, 1)
    )
  })
  
  
  
  
  # School types
  schools <- reactive({
    (school_type_lookup %>%
       dplyr::filter(geographic_level == input$geography_choice))$school_type %>%
      unique()
  })
  
  observe({
    choicesSchools <- schools()
    updateSelectInput(session, "school_choice",
                      choices = schools()
    )
  })
  
  output$schools_filtered <- renderUI({
    selectInput(
      inputId = "school_choice",
      label = "Choose school type:",
      choices = schools(),
      selected = head(schools, 1)
    )
  })
  
  
  # Setting up reactive data for plots and tables ------------------------------------------------------------
  
  # YTD data for reasons tables
  live_attendance_data_ytd_reasons_tables <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data_ytd, geographic_level == "National",
        school_type == input$school_choice
      ) %>% mutate(
        illness_perc = illness_perc / 100,
        appointments_perc = appointments_perc / 100,
        auth_religious_perc = auth_religious_perc / 100,
        auth_study_perc = auth_study_perc / 100,
        auth_grt_perc = auth_grt_perc / 100,
        auth_holiday_perc = auth_holiday_perc / 100,
        auth_excluded_perc = auth_excluded_perc / 100,
        auth_other_perc = auth_other_perc / 100,
        unauth_hol_perc = unauth_hol_perc / 100,
        unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
        unauth_oth_perc = unauth_oth_perc / 100,
        unauth_not_yet_perc = unauth_not_yet_perc / 100
        #,pa_perc = pa_perc / 100
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data_ytd, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice
      ) %>% mutate(
        illness_perc = illness_perc / 100,
        appointments_perc = appointments_perc / 100,
        auth_religious_perc = auth_religious_perc / 100,
        auth_study_perc = auth_study_perc / 100,
        auth_grt_perc = auth_grt_perc / 100,
        auth_holiday_perc = auth_holiday_perc / 100,
        auth_excluded_perc = auth_excluded_perc / 100,
        auth_other_perc = auth_other_perc / 100,
        unauth_hol_perc = unauth_hol_perc / 100,
        unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
        unauth_oth_perc = unauth_oth_perc / 100,
        unauth_not_yet_perc = unauth_not_yet_perc / 100
        #,pa_perc = pa_perc / 100
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data_ytd, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice
      ) %>% mutate(
        illness_perc = illness_perc / 100,
        appointments_perc = appointments_perc / 100,
        auth_religious_perc = auth_religious_perc / 100,
        auth_study_perc = auth_study_perc / 100,
        auth_grt_perc = auth_grt_perc / 100,
        auth_holiday_perc = auth_holiday_perc / 100,
        auth_excluded_perc = auth_excluded_perc / 100,
        auth_other_perc = auth_other_perc / 100,
        unauth_hol_perc = unauth_hol_perc / 100,
        unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
        unauth_oth_perc = unauth_oth_perc / 100,
        unauth_not_yet_perc = unauth_not_yet_perc / 100
        #,pa_perc = pa_perc / 100
      )
    } else {
      NA
    }
  })
  

  # Creating tables ------------------------------------------------------------
  
  # authorised reasons ytd
  output$absence_auth_reasons_table_ytd <- renderDT({
    
    absence_auth_reasons_ytd_dt <- live_attendance_data_ytd_reasons_tables() %>%
      dplyr::select(illness_perc, appointments_perc, auth_religious_perc, auth_study_perc, auth_grt_perc, auth_holiday_perc, auth_excluded_perc, auth_other_perc) %>%
      rename(
        "Illness" = illness_perc,
        "Medical or dental appointments" = appointments_perc,
        "Religious observance" = auth_religious_perc,
        "Study leave" = auth_study_perc,
        "Traveller" = auth_grt_perc,
        "Holiday" = auth_holiday_perc,
        "Excluded" = auth_excluded_perc,
        "Other" = auth_other_perc
      )
    
    absence_auth_reasons_ytd_dt <- datatable(absence_auth_reasons_ytd_dt,
                                             selection = "none",
                                             escape = FALSE,
                                             rownames = FALSE,
                                             class = "cell-border stripe",
                                             options = list(
                                               scrollX = TRUE,
                                               ordering = F,
                                               searching = FALSE,
                                               lengthChange = FALSE,
                                               dom = "t",
                                               columnDefs = list(list(className = "dt-center", targets = 0:7))
                                             )
    ) %>%
      formatPercentage(c(0:7), 1)
  })
  
  
  # unauthorised reasons ytd
  output$absence_unauth_reasons_table_ytd <- renderDT({
    validate(need(nrow(live_attendance_data_ytd_reasons_tables()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd_reasons_tables()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))
    
    absence_unauth_reasons_ytd_dt <- live_attendance_data_ytd_reasons_tables() %>%
      dplyr::select(unauth_hol_perc, unauth_late_registers_closed_perc, unauth_oth_perc, unauth_not_yet_perc) %>%
      rename(
        "Holiday" = unauth_hol_perc,
        "Late after registers closed" = unauth_late_registers_closed_perc,
        "Other" = unauth_oth_perc,
        "No reason yet" = unauth_not_yet_perc
      )
    
    absence_unauth_reasons_ytd_dt <- datatable(absence_unauth_reasons_ytd_dt,
                                               selection = "none",
                                               escape = FALSE,
                                               rownames = FALSE,
                                               class = "cell-border stripe",
                                               options = list(
                                                 scrollX = TRUE,
                                                 ordering = F,
                                                 searching = FALSE,
                                                 lengthChange = FALSE,
                                                 dom = "t",
                                                 columnDefs = list(list(className = "dt-center", targets = 0:3))
                                               )
    ) %>%
      formatPercentage(c(0:3), 1)
  })
  

  
}