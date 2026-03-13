library(tidyverse)
library(broom)

model_paths <- list.files(path = "out_glm", pattern = "^glm", full.names = TRUE)
models <- model_paths %>%
  map(read_rds) %>%
  set_names(stringr::str_extract(model_paths, "glm .*?-\\d{2}"))

map(models, function(x) {
  broom::tidy(x) %>%
    mutate(
      odds = exp(estimate),
      sig = ifelse(p.value <= 0.05, "*", "")
    )
})
