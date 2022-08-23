#theme_set(theme_minimal())
utsa_colors <- c(
  "primary blue"        = "#0c2340 ",
  "orange"       = "#f15a22",
  "dkorange"     = "#D3430D",
  "web2"     = "#495A70",
  "web3" = "#DBDEE3",
  "white"      = "#FFFFFF")


utsa_pal <- function(
  primary = 'primary blue', 
  other ="orange", 
  direction = 1
) {
  stopifnot(primary %in% names(utsa_colors))
  
  function(n) {
    if (n > 6) warning("utsa Color Palette only has 6 colors.")
    
    if (n == 2) {
      other <- if (!other %in% names(utsa_colors)) {
        other
      } else {
        utsa_colors[other]
      }
      color_list <- c(other, utsa_colors[primary])
    } else {
      color_list <- utsa_colors[1:n]
    }
    
    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}


scale_colour_utsa <- function(
  primary = 'primary blue', 
  other = "orange", 
  direction = 1, 
  ...
) {
  ggplot2::discrete_scale(
    "colour", "utsa", 
    utsa_pal(primary, other, direction), 
    ...
  )
}

scale_color_utsa <- scale_colour_utsa
