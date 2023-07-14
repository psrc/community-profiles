echart_rdi <- function(data, filter_type, desc_col, str_wrap_num, group, x, y, title, egrid_left) {
  # function to produce an echart for RDI subtabs
  
  data %>%
    filter(type == filter_type) %>% 
    mutate("{{desc_col}}" := str_wrap({{desc_col}}, str_wrap_num)) %>%
    group_by({{group}}) %>%
    e_charts_(x = x) |>
    e_bar_(y) |>
    e_x_axis(axisLabel = list(interval = 0L),
             axisTick = list(alignWithLabel = TRUE)) |>
    e_flip_coords() |>
    e_grid(left = egrid_left, top = '10%') |>
    e_title(text = title,
            left = 'center',
            textStyle = list(fontSize = 12)) |>
    e_color(psrc_colors$obgnpgy_5) %>%
    e_tooltip(formatter =  e_tooltip_item_formatter("percent", digits = 1)) |> 
    e_x_axis(formatter = e_axis_formatter("percent", digits = 0))
}