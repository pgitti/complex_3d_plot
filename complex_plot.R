complex_3d_plot <- function(
    FUN = function(z) return((z ^ 2 + 1) / (z ^ 2 - 1)),
    range = -5:5,
    res = 1,
    n_color = 1000,
    color_range = c("red", "blue"),
    cutoff = 9/10) {
  
  # calculate values
  re <- seq(min(range), max(range), by = res)
  im <- seq(min(range), max(range), by = res)
  z <- outer(re, im, function(x, y) complex(real = x, imaginary = y))
  y <- FUN(z)
  mod_y <- Mod(y)
  arg_y <- Arg(y)
  
  # generate color scale
  color_scale <- colorRampPalette(color_range)
  
  # generate desired number of color breaks (lightgrey = NA)
  colors <- c(color_scale(n_color), "lightgrey")
  
  # compute the arg-value at the facet centres
  arg_facet <- arg_y[-1, -1] + arg_y[-1, -nrow(z)] + arg_y[-nrow(z), -1] + arg_y[-nrow(z), -ncol(z)] 
  
  # recode arg_facet into a color index
  arg_col <- forcats::fct_na_value_to_level(cut(arg_facet, n_color))
  
  # cutoff
  max_mod_y <- max(mod_y[!is.infinite(mod_y)])
  min_mod_y <- min(mod_y[!is.infinite(mod_y)])
  mod_y <- replace(mod_y, mod_y > max_mod_y * cutoff, max_mod_y * cutoff)
  mod_y <- replace(mod_y, mod_y < min_mod_y * cutoff, min_mod_y * cutoff)
  
  persp(
    re,
    im,
    mod_y,
    col = colors[arg_col],
    main = gsub("return\\(|(\\)$)", "", deparse(FUN)[2]),
    zlim = c(0, max_mod_y * cutoff),
    ticktype = "detailed",
    theta = 30,
    phi = 50,
    zlab = "Mod(z)"
  )
  
  legend(
    "topright",
    legend = c("-π", 0, "π"),
    fill = color_scale(3),
    title = "Arg(z)"
  )
  
  data <- as.data.frame(
    lapply(list(z = z, y = y, mod_y = mod_y, arg_y = arg_y),
           as.vector))
  
  return(data)
}

