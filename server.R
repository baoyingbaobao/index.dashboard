function(input, output, session) {
  
  indexServer(
    id = "indexes",
    app_data = app_data,
    metric_colours = metric_colours,
    region_mapping = gccmapping
  )
  
}

# things to improve: 
# 1. add date range feedback
# 4. update index table before run app