# plotting results


# Setup -------------------------------------------------------------------

# devtools::install_github("EBukin/ggwb")
library(tidyverse)
library(ggplot2)
library(ggwb)


# library(ggthemr)
# # ggthemr_reset()
# # Setting ggthemr palet
# wb_colours <-c(
#   `red`              = "#EB1C2D",
#   `orange`           = "#F05023",
#   `yellow`           = "#FDB714",
#   `light_blue`       = "#00A996",
#   `blue`             = "#009CA7",
#   `green`            = "#00AB51"
#   )
# 
# wb <- 
#   define_palette(
#   swatch = set_swatch(wb_colours), 
#   gradient = c(lower = wb_colours[1L], upper = wb_colours[length(wb_colours)]), 
#   background = "#EEEEEE" 
# )
# 
# ggthemr(wb, layout = "scientific", spacing = 1)
# ggthemr_reset()


# Functions ---------------------------------------------------------------


plot_scattered_countries <- function(plot_data, x_var, y_var, radius_var, colour_var, shape_var = NULL) {
  plot_data %>% 
    drop_na() %>% 
    ggplot() + 
    aes_string(x = x_var, y = y_var, size = radius_var, colour = colour_var, fill = colour_var) + 
    geom_point(position="jitter", aes(alpha = 0.9)) + 
    scale_color_wb(name = names(colour_var), discrete = TRUE)+
    scale_radius(name = names(radius_var)) +
    guides(
      fill = 'none',
      alpha = 'none',
      # shape = guide_legend(order = 2, override.aes = list(size=5, colour=wb_col("red"))),
      size = guide_legend(order = 1, override.aes = list(colour=wb_col("red"))),
      colour = guide_legend(order = 2, override.aes = list(size=5))
    ) +
    xlab(names(x_var)) + 
    ylab(names(y_var)) + 
    theme_bw()
}


# data --------------------------------------------------------------------

pd_one <- 
  read_rds("clean-data/analysis_data.rds") %>% 
  filter(!is.na(country_code)) %>% 
  mutate(income = factor(income, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))) 

# Ploting -----------------------------------------------------------------

axis_vars <- 
  set_names(
    distinct(pd_one, var, var_code)$var_code,
    distinct(pd_one, var, var_code)$var
  )

group_vars <-
  c(
    `Income group` = "income",
    `Region` = "region",
    `Level of ag.\ndevelpment` = 'cluster'
  )
  
# Filter X axis
x_var <- axis_vars[axis_vars == "SL.AGR.EMPL.ZS"]
y_var <- axis_vars[axis_vars == "twog.cvrg.gsm.ndctrs"]
radius_var <- axis_vars[axis_vars == "ndx.scr.gsm.ndx"]
colour_var <- group_vars[group_vars == "income"]
active_year = 2008

# Filter Y axis

# Filter Year
filter_data <-
  pd_one %>% 
  filter(var_code %in% c(x_var, y_var, radius_var)) %>%
  filter(year == active_year)

export_data <-
  filter_data %>%
  mutate(var = str_c(Source, "-", str_trunc(var, 25))) %>%
  select(country, region, income, year, val, flag, var) %>%
  {
    dta <- .
    ident <- dta %>% distinct(country, region, income, year)
    vals <- dta %>%  select(country, year, val, var) %>% 
      spread(var, val)
    flags <- dta %>%  select(country, year, flag, var) %>% 
      mutate(var = str_c(var, " - FLAG")) %>% spread(var, flag)
    ident %>% 
      left_join(vals, c("country", "year")) %>% 
      left_join(flags, c("country", "year")) %>% 
      mutate_at(vars(contains("flag")), funs(ifelse(is.na(.), "Missing", .)))
  }

zero_vars <- 
  filter_data %>% 
  group_by(var, var_code, flag) %>% 
  count %>% 
  spread(flag, n) %>% 
  ungroup() %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
  rename(Variable = var)

if (!"Imputed" %in% names(zero_vars)) {
  zero_vars <-
    zero_vars %>%
    mutate(Imputed = 0)
}

zero_vars <-
  zero_vars %>% 
  mutate(
    no_data = Original == 0 & Imputed == 0
  )

zero_vars %>% 
  filter(Original == 0 & Imputed == 0) %>% 
  pull(Variable)

orig_imput_values <-
  zero_vars %>%
  filter(var_code == radius_var) %>%
  gather(var, val, Original, Imputed) %>%
  pull(val)
all(orig_imput_values == 0)

zero_vars %>%
  as.list() %>% 
  transpose() %>% 
  map(.f = function(x) {
    tags$p(
      str_c("Variable ", x$Variable," contains: ", x$Original, " original, ", x$Imputed, " imputed and ", x$Missing, " missing values." )
    )
  }) %>% 
  tagList()




flt_data <-
  filter_data %>% 
  filter(var_code %in% c(x_var, y_var, radius_var)) %>%
  filter(year == active_year) 


contains_data <-
  zero_vars %>%
  filter(!no_data) %>%
  pull(var_code)

contains_zero <-
  zero_vars %>%
  filter(no_data) %>%
  pull(var_code)

if (!is.null(contains_data) & length(contains_data) < 2) return(NULL)

# flt_data <- filter_data()

if (!is.null(contains_data) & length(contains_zero) > 0) {
  flt_data <-
    flt_data %>%
    filter(var_code != contains_zero)
}

pd <- 
  flt_data %>%
  select(-var) %>%
  {
    dta <- .
    flags <-
      dta %>%
      group_by(country, year) %>%
      summarise(flag = ifelse(all(flag == "Original"), "Original",
                              ifelse(all(flag == "Imputed"), "Imputed", "Partial")
      ))
    dta %>%
      select(-flag, -`Variable group`, -Source) %>%
      spread(var_code, val) %>%
      left_join(flags, by = c("country", "year"))
  } %>%
  mutate(
    flag = factor(flag, levels = c("Original", "Partial", "Imputed"))
  ) %>%
  drop_na()

clusters <- 
  pd %>% 
  select(matches("\\.")) %>% 
  kmeans(5)

pd <- 
  pd %>% 
  mutate(cluster = factor(clusters$cluster) )
#%>% 
 # drop_na() 



# %>% 
#   mutate(ag_development = kmeans(.[c(x_var, y_var, radius_var)], 5)$cluster,
#          ag_development = as.factor(ag_development))
# cluster <- 
#   plot_data[c(x_var, y_var, radius_var)] %>% 
#   drop_na() %>% 
#   kmeans(5) 
# cluster$cluster
# 
# cluster$centers
# kmeans(plot_data[c(x_var, y_var, radius_var)], 5)$cluster %>% 
#   str(level = 1)


plot_scattered_countries(pd, x_var, y_var, radius_var, "cluster", "flag") 


#   # devtools::install_github("jcheng5/googleCharts")
# library(googleCharts)
# 
# googleBubbleChart

# 
# devtools::install_github("barrosm/google-motion-charts-with-r")
# install.packages("googleVis")
library(googleVis)


gviz_data <-
  read_rds("clean-data/analysis_data.rds") %>% 
  filter(year > 1995) %>% 
  mutate(
    `Variable group` = ifelse(is.na(`Variable group`), "", `Variable group`),
    var = str_c(`Source`, " - ", ifelse(`Variable group` == '', '', str_c(`Variable group`, " - ")), var)) %>% 
  select(-var_code, - Source, -`Variable group`, -flag) %>%
  # slice(c(329624, 329625))
  spread(var, val) 
  
gviz_data %>% 
  arrange(country, year)
names(gviz_data)
  
gvp <- 
  
  gvisMotionChart(
    data = gviz_data,
    idvar = "country",
    timevar = "year", 
    xvar = "WDI - Employment in agriculture (% of total employment) (modeled ILO estimate)" ,
    yvar = "WDI - Agriculture, forestry, and fishing, value added (% of GDP)" ,
    colorvar = "region", 
    sizevar = "Digital Adoption Index - Digital Adoption Index", 
    date.format = "YYYY",
    options=list(width="1200", height="700")
  )

plot(gvp)

# x_var <- axis_vars[axis_vars == "SL.AGR.EMPL.ZS"]
# y_var <- axis_vars[axis_vars == "twog.cvrg.gsm.ndctrs"]
# radius_var <- axis_vars[axis_vars == "ndx.scr.gsm.ndx"]
# colour_var <- group_vars[group_vars == "income"]
