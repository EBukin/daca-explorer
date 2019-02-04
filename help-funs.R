
wb_col <- function(...) {
  wb_colors <- c(
    `Solid Blue` = "#002244",
    `Bright Blue` = "#009FDA",
    `orange` = "#F05023",
    `yellow` = "#FDB714",
    `red` = "#EB1C2D",
    `brown` = "#F78D28",
    `blue` = "#009CA7",
    `green` = "#00AB51",
    `purple` = "#872B90",
    `light_blue` = "#00A996"
  )
  
  cols <- c(...)
  
  if (is.null(cols)) {
    return(wb_colors)
  }
  
  wb_colors[cols]
}

wb_pal <- function(palette = "main", reverse = FALSE, ...) {
  wb_palettes <- list(
    `main` = wb_col("red", "yellow", "green", "blue", "purple")
  )
  
  pal <- wb_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}
scale_color_wb <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- wb_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("wb_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_wb <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- wb_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("wb_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



# Developing function for calculating distance to the origin
dist_origin <- function(x, y) {
  # browser()
  sqrt(x^2 + y^2)
}
