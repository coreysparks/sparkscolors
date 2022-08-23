#theme_set(theme_minimal())
carsey_colors <- c(
  "primary blue"        = "#004f9e",
  "orange"       = "#e36c2c",
  "green"     = "#4eaf49",
  "ltblue"     = "#80a7cf",
  "dkorange" = "#723616",
  "white"      = "#FFFFFF")


carsey_pal <- function(
  primary = 'primary blue', 
  other ="orange", 
  direction = 1
) {
  stopifnot(primary %in% names(carsey_colors))
  
  function(n) {
    if (n > 6) warning("Carsey Color Palette only has 6 colors.")
    
    if (n == 2) {
      other <- if (!other %in% names(carsey_colors)) {
        other
      } else {
        carsey_colors[other]
      }
      color_list <- c(other, carsey_colors[primary])
    } else {
      color_list <- carsey_colors[1:n]
    }
    
    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}


scale_colour_carsey <- function(
  primary = 'primary blue', 
  other = "orange", 
  direction = 1, 
  ...
) {
  ggplot2::discrete_scale(
    "colour", "carsey", 
    carsey_pal(primary, other, direction), 
    ...
  )
}

scale_color_carsey <- scale_colour_carsey
