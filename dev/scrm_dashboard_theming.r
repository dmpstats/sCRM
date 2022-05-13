fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#434C5E", 
    aqua = "#edd205",
    #yellow = "#1500fc" # "#fc830a"
    #red = "#E13A36"
  ),
  fresh::adminlte_sidebar(
    width = "265px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440",
    dark_submenu_color ="#2E3440",
    dark_submenu_hover_color = "#3981e6" # "#2a7ddb"
  ),
  fresh::adminlte_global(
    content_bg = "#FFF",
    box_bg = "white", 
    info_box_bg = "#D8DEE9"
  ),
  
  output_file = "inst/app/www/scrm-custom-theme.css"
)



# fresh::create_theme(
#   theme = "yeti", 
#   output_file = "inst/app/www/scrm-custom-theme.css"
# )
