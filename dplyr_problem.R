
tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5)
) %>% 
  select(a)

tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5)
) %>% 
  select("b")


tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5)
) %>% 
  select(a, "b")

b_to_select <- c(`B Name` = "b")
c_to_select <- c(`c Name` = "c")

tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5)
) %>% 
  select(a, b_to_select)

tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5)
) %>% 
  select(a, c_to_select)

tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5)
) %>% 
  select(a, b_to_select, c_to_select)

two_var <- c(b_to_select, c_to_select)
tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5)
) %>% 
  select(a, two_var)
