fluidPage(
  
  shinyjs::useShinyjs(debug = TRUE),
  
  tags$head(
    tags$title("DSpark Indicies"),
    tags$link(rel = "stylesheet", type = "text/css", href = "dspark-dark-theme.css"),
    tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon.ico")
  ),

  fluidPage(
    dspark_navbar(
      logo = "dspark_logo.png",
      height = "50px"
    ),
    indexUI(id = "indexes", region_mapping = gccmapping)
  )
  
)