library(shiny)
library(plotly)
library(gt)

app_data %>% filter(metric == "International Visitors" , agg_value == "1GSYD") %>% 
  mutate(max_value = max(value),pr = value / max_value)->international

app_data %>% filter(metric == "Total Distance Travelled (km)" , agg_value == "1GSYD") %>% 
  mutate(max_value = max(value),pr = value / max_value)->travel_distance

colour = '#000000'
title = "Test"

international %>% 
  mutate(datedate = as.Date(datedate)) %>% 
  arrange(datedate) %>% 
  plot_ly(x = ~datedate, y = ~pr) %>%
  add_lines(line=list(color= "red"),
            hovertemplate = paste('%{x}', '<br>%{y:.2%}<br>'),
            name = "") %>% 
  layout(xaxis = list(type = "date", title = ""),
         yaxis = list(tickformat = "%", title = "", range = c(0, 1)),
         showlegend = FALSE,
         annotations = list(
           x = (international %>% mutate(datedate = as.Date(datedate)) %>% arrange(desc(datedate)) %>%slice(1))$datedate, 
           y = (international %>% mutate(datedate = as.Date(datedate)) %>% arrange(desc(datedate)) %>%slice(1))$pr, 
           text = paste0(round((international %>% mutate(datedate = as.Date(datedate)) %>% arrange(desc(datedate)) %>%slice(1))$pr*100, 2), "%"),
           showarrow = FALSE,
           xanchor = "right",
           font = list(size = 15, color = "red")
         ))


# test GT table
table<-international %>% 
  mutate(datedate = as.Date(datedate)) %>% 
  arrange(datedate) %>% 
  summarise(value = sum(value), n_days = n_distinct(datedate))

gt_tbl <- table%>% 
  gt() %>% 
  cols_align(
  align = "center",
  columns = vars(value, n_days)
) %>% 
  # data_color(
  #   columns = vars(value),
  #   colors = scales::col_factor(
  #     c("red"), domain = NULL
  # )) %>%
  # data_color(
  #   columns = vars(n_days),
  #   colors = scales::col_factor(
  #     c("blue"), domain = NULL
  #   )) %>%
  tab_style(
    style = list(
      # cell_fill(color = "lightcyan"),
      cell_text(weight = "bold", size = 15, color = "black")
    ),
    locations = cells_body(
      columns = colnames(table)
  )) %>%
  tab_style(
    style = list(
      # cell_fill(color = "#F9E3D6"),
      cell_text(weight = "bold", size = 15, color = "white")
    ),
    locations = cells_body(
      columns = vars(n_days)
  )) %>% 
  tab_options(
    column_labels.background.color ="lightcyan",
    column_labels.font.size = 15,
    column_labels.font.weight = "bold"
    # column_labels.text_transform = NULL,
    # olumn_labels.vlines.style = NULL,
    # column_labels.vlines.width = NULL,
    # column_labels.vlines.color = NULL,
  )


# test read data from index API
trip_volume<-readData(table_name ="tripsIndexData",key = "sYp4cTwEKYhuZV5bAJxxefFuoLVqXL6p")
domestic<-readData(table_name ="domesticTravellersIndexData",key = "sYp4cTwEKYhuZV5bAJxxefFuoLVqXL6p")
international<-readData(table_name ="internationalTravellersIndexData",key = "sYp4cTwEKYhuZV5bAJxxefFuoLVqXL6p")
stay_metric<-readData(table_name ="staymetricsIndex",key = "sYp4cTwEKYhuZV5bAJxxefFuoLVqXL6p")

Sys.time()
data = httr::POST(
  url = paste0("https://community.dsparkanalytics.com.au/content/60/tblreq?tblreq=", table_name),
  config = httr::add_headers(Authorization = paste0("Key ", key)),
  verbose())
Sys.time()
table = httr::content(data, as = "parsed")
Sys.time()
test<-table %>% bind_rows()
Sys.time()

Sys.time()
test1<-matrix(unlist(table),byrow=TRUE,ncol=length(table[[1]]))
colnames(test1) <- names(table[[1]])
test1<-test1 %>% as.data.frame()
Sys.time()

# line 21
#nav_options {
display: inline-flex;
float: right;
margin-top: -48px;
padding: 0px 40px 0px 40px;
}