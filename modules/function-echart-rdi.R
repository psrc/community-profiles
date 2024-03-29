echarts4r::e_common(font_family = "Poppins")

echart_rdi <- function(data, filter_type = NULL, desc_col, str_wrap_num, group, x, y, ymax = NULL, stack = NULL, title, egrid_left, palette_colors) {
  # function to produce an echart for RDI subtabs
  
  js <- "function(params, ticket, callback) {
                                       var fmt = new Intl.NumberFormat('en', {\"style\":\"percent\",\"minimumFractionDigits\":1,\"maximumFractionDigits\":1,\"currency\":\"USD\"});\n
                                       var idx = 0;\n
                                       if (params.name == params.value[0]) {\n
                                       idx = 1;\n        }\n
                                       return(params.marker + ' ' +\n
                                              params.seriesName + ': ' + fmt.format(parseFloat(params.value[idx]))
                                              )
                                       }"
  
  if(!is.null(filter_type)) {
    e <- data %>%
      filter(type == filter_type) %>%
      mutate("{{desc_col}}" := str_wrap({{desc_col}}, str_wrap_num)) %>%
      group_by({{group}})
  } else {
    e <- data %>%
      mutate("{{desc_col}}" := str_wrap({{desc_col}}, str_wrap_num)) %>%
      group_by({{group}})
  }
  
  e %>% 
    e_charts_(x = x, stack = stack) |>
    e_bar_(y) |>
    e_y_axis(max = ymax) |>
    # e_y_axis(splitNumber = 3, max = ymax) |>
    e_x_axis(axisLabel = list(interval = 0L),
             axisTick = list(alignWithLabel = TRUE)) |>
    e_flip_coords() |>
    e_grid(left = egrid_left, top = '10%') |>
    e_title(text = title,
            left = 'center',
            textStyle = list(fontSize = 12)) |>
    e_color(palette_colors) |>
    # e_color(psrc_colors$obgnpgy_5) |>
    e_tooltip(formatter =  e_tooltip_item_formatter("percent", digits = 1)) |>
    e_tooltip(formatter =  htmlwidgets::JS(js)) |>
    e_x_axis(formatter = e_axis_formatter("percent", digits = 0))
 
}