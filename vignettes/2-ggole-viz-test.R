
library(tidyverse)
library(googleVis)
# install.packages("ggrepel")
library(ggrepel)

pd <-
  read_rds("clean-data/analysis_data.rds") %>%
  filter(year > 1990, !is.na(country_code)) %>%
  mutate(
    income = factor(income, levels = c("Low income", "Lower middle income", "Upper middle income", "High income")),
    country = factor(country, levels = sort(unique(pull(., country)))),
    region = factor(region),
    `Variable group` = ifelse(is.na(`Variable group`), "", `Variable group`),
    var = str_c(`Source`, " - ", ifelse(`Variable group` == "", "", str_c(`Variable group`, " - ")), var)
  ) %>% 
  arrange(Source, var, country, year)

bubles_data <- 
  pd %>% 
  select(-var_code, - Source, -`Variable group`, -flag) %>%
  spread(var, val) 

bubles_calc_data <- 
  pd %>% 
  select(-var, - Source, -`Variable group`, -flag) %>%
  spread(var_code, val) 

axis_vars <-
  c(
    `NOTHING` = "nothing",
    `.Agricultural transformation index` = "ag_transform",
    set_names(
      distinct(pd, var, Source, var_code)$var_code,
      distinct(pd, var, var_code)$var
    )
  )

countries_list <-
  c(
    set_names(
      distinct(pd, country, country_code)$country_code,
      distinct(pd, country, country_code)$country
    )
  )

region_list <-
  distinct(pd, country, country_code, region) %>%
  mutate(region2 = as.character(region)) %>% 
  group_by(region) %>%
  nest() %>%
  as.list() %>%
  .$data %>% 
  map(.f = function(.x) {
    # browser()
    a <- list()
    a[[unique(.x$region2)]] <- 
      set_names(
        .x$country_code,
        .x$country
      )
    names(a) <- unique(.x$region2)
    a
  })


get_var <- function(var_code, ax_var = axis_vars) {
  ax_var[ax_var%in%var_code]
}

# Developing function for calculating distance to the origin
dist_origin <- function(x, y) {
  # browser()
  sqrt(x^2 + y^2)
}

source("help-funs.R")

# c(5, 8, 9, 15) %>% 
#   reduce(dist_origin)
# 
# list(
#   c(5, 9, 8),
#   c(2, 3, 4),
#   c(1, 6, 9)
#   ) %>% 
#   reduce(dist_origin)
# 
# n <- 1000000
# test <- 
#   tibble(
#   a = rnorm(n),
#   b = rnorm(n),
#   x = rnorm(n)
# )
# 
# test %>% 
#   mutate(
#     y = select(., a, b, x) %>% reduce(dist_origin)
#   )
# 
# test %>% 
#   rowwise() %>% 
#   mutate(y = c(a, b, x) %>% reduce(dist_origin))
# 
# dist_origin2(test$a, test$b)

# Calculating Level of argicultural transformation
# state_string <- 
#   '{"nonSelectedAlpha":0.4,"time":"2008","dimensions":{"iconDimensions":["dim0"]},
# "playDuration":15000,"iconType":"BUBBLE","xZoomedIn":false,"yAxisOption":"63",
# "xZoomedDataMin":0.01099999994,"showTrails":true,"iconKeySettings":[],
# "xZoomedDataMax":92.84200287,"orderedByX":false,"xAxisOption":"66",
# "yZoomedDataMin":0.02647090715,"yZoomedIn":false,"uniColorForNonSelected":false,
# "yLambda":1,"xLambda":1,"colorOption":"4","duration":{"timeUnit":"Y","multiplier":1},
# "yZoomedDataMax":79.04236246,"sizeOption":"100","orderedByY":false}'

p_buble_data <- 
  bubles_data %>% 
  mutate(
    `.Agricultural transformation index` = select(bubles_calc_data, x_var, y_var) %>% reduce(dist_origin),
    `.Agricultural transformation index` = if_else(`.Agricultural transformation index` < 1, 1, `.Agricultural transformation index`),
    `.Agricultural transformation index` = 1 / `.Agricultural transformation index`
  )

gvp <-

  gvisMotionChart(
    data = p_buble_data,
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



# Plotly ------------------------------------------------------------------

# library(plotly)

# Inputs 
x_var <-get_var("SL.AGR.EMPL.ZS")
y_var <- get_var("NV.AGR.TOTL.ZS")
size_var <- get_var("ag_transform")
# size_var <- "NOTHING"
year_show <- 2002
trace_countries <- c("UKR", "URY", "USA")
# trace_countries <- unique(bubles_calc_data$country_code)
# trace_countries <- NA#c("UKR", "URY", "USA")
colour_var <- "region_code"

if (size_var == "NOTHING") size_var <- NULL
if (trace_countries[[1]] == "" | is.null(trace_countries[[1]]) | is.na(trace_countries[[1]])) trace_countries <- NULL

# bubles_data

test <-
  bubles_calc_data  %>% 
  filter(country_code %in% c("UKR", "URY", "USA")) %>% 
    arrange(country_code, year) %>% 
  mutate(
    ag_transform = NA,
    ag_transform = select(., x_var, y_var) %>% reduce(dist_origin),
    ag_transform = if_else(ag_transform < 1, 1, ag_transform),
    ag_transform = 1 / ag_transform * 100
  ) %>% 
  dplyr::select(year, country_code, country, region_code, region, 
                income_code, income, c(x_var, y_var, size_var)) %>% 
  filter(year == year_show | country_code %in% trace_countries[[1]] & year <= year_show) %>% 
  mutate(
    alpha_val = 1,
    tracable = !is.null(trace_countries[[1]]) & country_code %in% trace_countries,
    alpha_val = if_else(tracable & year == year_show, 1, 0.4, 1),
    alpha_val = if_else(tracable & year != year_show, 0.7, alpha_val, 1),
    traces_lable = if_else(tracable & year == year_show, country_code, NA_character_),
    missing_size_point = !is.null(size_var) & is.na(eval(parse(text = size_var))),
    x_missing = if_else(missing_size_point, eval(parse(text = x_var)), NA_real_),
    y_missing = if_else(missing_size_point, eval(parse(text = y_var)), NA_real_))

# test %>% 
#   plot_ly() %>% 
#   add_trace(
#     x = ~eval(parse(text = x_var)),
#     y = ~eval(parse(text = y_var)),
#     size = ~eval(parse(text = size_var)),
#     type = 'scatter',
#     mode = 'markers'
#   )


test %>%
  ggplot() +
  aes_string(
    x = x_var, 
    y = y_var, 
    fill = colour_var,
    colour = colour_var
  ) +
  geom_point(aes_string(size = size_var, alpha = "alpha_val")) +
  geom_point(aes_string(x = "x_missing", y = "y_missing", alpha = "alpha_val"), shape = 8 , show.legend = FALSE) +
  geom_path(aes_string(group = "country"), show.legend = FALSE) +
  geom_label_repel(aes_string(label = "traces_lable", fill = colour_var), colour = "white", fontface = "bold", size = 3, direction = "y")+
  scale_alpha(range = c(0.3, 1)) +
  scale_color_wb(name = names(colour_var), discrete = TRUE) +
  scale_size(name = names(size_var)) + 
    guides(
      fill = "none",
      alpha = "none",
      group = "none",
      size = guide_legend(order = 1, override.aes = list(colour = wb_col("red"))),
      colour = guide_legend(order = 2, override.aes = list(size = 5))
    ) +
    xlab(names(x_var)) +
    ylab(names(y_var)) +
    theme_bw()
