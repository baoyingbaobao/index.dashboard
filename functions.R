readData<-function(table_name,key){
  data = httr::POST(
    url = paste0("https://community.dsparkanalytics.com.au/content/60/tblreq?tblreq=", table_name),
    config = httr::add_headers(Authorization = paste0("Key ", key)),
    verbose())
  table = httr::content(data, as = "parsed")
  df<-matrix(unlist(table),byrow=TRUE,ncol=length(table[[1]]))
  colnames(df) <- names(table[[1]])
  df %>% as.data.frame() 
}

dspark_navbar <- function(logo, height, ...) {
  
  other_dboard <- "https://realtime.dsparkanalytics.com.au/Covid19/"
  realtime_dboard <- "https://realtime.dsparkanalytics.com.au/realtime/"
  
  fluidPage(
    
    tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-size: 25px;
      }
    "))
    ),
    
    left = 0,
    right = 0,
    top = 0,
    height = height,
    id = "navbar",
    div(
      id = "logo",
      img(id ="nav_logo", src = logo),
      span(
        id = "navlinks",
        a(href = paste0(other_dboard, "/?tab=TripsPanel"), span("Trips")),
        a(href = paste0(other_dboard, "/?tab=TimeAtHomePanel"), span("Time At Home")),
        a(href = paste0(other_dboard, "/?tab=DomVisitorsPanel"), span("Domestic Visitors")),
        a(href = paste0(other_dboard, "/?tab=IntVisitorsPanel"), span("International Visitors")),
        a(href = realtime_dboard, span("Realtime")), 
        a(href = "/", span("Indicies")),
        a(href = paste0(other_dboard, "/?tab=SummaryPanel"), span("Summary"))
      ),
    ),
    div(id = "nav_options", ...)
  )
  
}

generate_plot <- function(data, colour, title) {
  data %>% 
    mutate(date = as.Date(date)) %>% 
    arrange(date) %>% 
    plot_ly(x = ~date, y = ~pr) %>%
    add_lines(line=list(color= colour),
              hovertemplate = paste('%{x}', '<br>%{y:.2%}<br>'),
              name = "") %>% 
    layout(xaxis = list(type = "date", title = "", colour = "white"),
           yaxis = list(tickformat = "%", title = "",  range = c(0, 1)),
           showlegend = FALSE,
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           # paper_bgcolor = "rgba(0, 0, 0, 0)",
           # fig_bgcolor   = "rgba(0, 0, 0, 0)",
           annotations = list(
             x = (data %>% mutate(date = as.Date(date)) %>% arrange(desc(date)) %>% slice(1))$date, 
             y = (data %>% mutate(date = as.Date(date)) %>% arrange(desc(date)) %>% slice(1))$pr - 0.1, 
             text = paste0(round((data %>% mutate(date = as.Date(date)) %>% arrange(desc(date)) %>%slice(1))$pr*100, 2), "%"),
             showarrow = FALSE, 
             xanchor = "bottom right",
             font = list(size = 15, color = colour)
           ))
}

summary_table <- function(data, start, end){
  data_processed<-data %>% filter(date >= as.character(start) & date <= as.character(end)) %>% 
    dplyr::mutate(max_value = max(value), pr = value/max_value) %>% 
    dplyr::mutate(weeknum = case_when(date >= as.character(end - 6) & date <= as.character(end) ~ "current", 
                                      date >= as.character(end - 13) & date <= as.character(end - 7) ~ "prev",
                                      TRUE ~ "0")) %>% 
    dplyr::filter(weeknum != "0") %>% 
    dplyr::group_by(weeknum) %>%
    dplyr::summarise(avg_key_stat = mean(value)) %>%
    dplyr::ungroup() %>% 
    tidyr::spread(weeknum, avg_key_stat) %>% 
    dplyr::mutate(pr_change = paste0(round((current - prev)/prev*100, 2), "%"))
  
  data_processed$pr_change
  
}



indexUI <- function(id, region_mapping) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Options",
        width = 6,
        dateRangeInput(
          inputId = ns("daterange"),
          label = "Date Range",
          start = as.Date("2020-02-17"),
          end = Sys.Date() -10
        ),
        selectInput(
          inputId = ns("region"),
          label = "Please Select GCC",
          choices = region_mapping,
          multiple = FALSE
        )
      ),
      box(
        # title = textOutput(ns("table_title")),
        title = "",
        width = 6,
        background = "navy",
        solidHeader = TRUE,
        gt_output(ns("metric_table"))
        # tableOutput(ns("metric_table"))
      ),
      tags$style(".dropdown-menu {color: black;}")
    ),
    fluidRow(
    uiOutput(ns("charts"))
    )
  )
}


indexServer <- function(id, app_data, metric_colours, region_mapping) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    plot_data <- reactive({
      app_data %>%
        dplyr::filter(
          location_value %in% !!input$region,
          date >= as.character(!!input$daterange[1]),
          date <= as.character(!!input$daterange[2])
        ) %>%
        dplyr::group_by(metric) %>%
        dplyr::mutate(
          max_value = max(value),
          pr = value / max_value
        ) %>%
        dplyr::ungroup() %>%
        base::split(.$metric)
    })
  
    table_data <- reactive({
      purrr::map_dfr(plot_data(), ~{
        summary_table(.x, start = input$daterange[1], end = input$daterange[2])
      }, .id = "Measure")
      })

    # generating plots iteratively
    observe({
      purrr::imap(plot_data(), ~{
        name <- .y %>%
          {gsub("\\(|\\)", "", .)} %>%
          {gsub(" ", "_", .)} %>%
          tolower()
        output[[name]] <- renderPlotly({
          generate_plot(
            data = .x,
            colour = metric_colours[[.y]],
            title = names(region_mapping[region_mapping == input$region])
          )
        })
      })
    })
  
    # generate table of metrics
    output$metric_table <- render_gt({

      validate(
        need(as.character(input$daterange[2]) <= min((app_data%>% group_by(metric) %>% summarise(max_date = max(date)) %>% ungroup())$max_date),
             "date out of range !")
      )
      
      table_data() %>% 
        gt() %>% 
        tab_header(
          title = paste0("Compare Week ", as.character(input$daterange[2] - 6), " ~ ", as.character(input$daterange[2]), " to Previous Week")
          # subtitle = paste0(as.character(input$daterange[2] - 6), " ~ ", as.character(input$daterange[2]), " ")
        ) %>% 
        cols_align(
          align = "center",
          columns = colnames(table_data())
        ) %>% 
        tab_style(
          style = list(
            cell_text(weight = "bold", size = "xx-large", color = "#fc03db")
          ),
          locations = cells_body(
            columns = colnames(table_data())
          )) %>% 
        tab_options(
          # column_labels.background.color ="lightcyan",
          table.background.color = "rgba(0, 0, 0, 0)",
          column_labels.font.size = 15,
          column_labels.font.weight = "bold"
        )
      
    })
    
    # render plots in a 2 * n layout
    output$charts <- renderUI({
      
      # how many rows of plots are required
      rows_required <- ceiling(length(plot_data()) / 2)
      
      # iterate over rows
      row_uis <- purrr::map(seq_len(rows_required), ~{
        
        # get required plots and names for this row
        row_plots_idx <- seq((.x * 2) - 1, by = 1, length.out = 2)
        row_plots_name <- names(plot_data())[row_plots_idx]
        
        # create each box element on the row
        plot_boxes <- row_plots_name %>%
          na.omit() %>% # there will be times with odd number of plots causing NA
          purrr::map(~{
            
            # sanitise name
            row_plot <- .x %>%
              {gsub("\\(|\\)", "", .)} %>%
              {gsub(" ", "_", .)} %>%
              tolower()
            
            # make box
            box(
              title = .x,
              background = "navy",
              solidHeader = TRUE,
              width = 6, 
              plotlyOutput(ns(row_plot), height = 250)
            )
            
          })
        
        # add boxes to row
        do.call(fluidRow, plot_boxes)
        
      })
      
      # adding all rows into tagList so that UI can be dynamically grown
      do.call(tagList, row_uis)
      
    })
    
  })
  
}
