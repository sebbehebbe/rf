library(tidyverse)
library(tidymodels)
library(furrr)

set.seed(123)
plan(multiprocess)

# Help functions ----------------------------------------------------------


#' tibble with random data
#' @param rows number of rows
#' @param cols number of columns
rdata <- function(cols = 1e3, rows = 1e3) {

  stopifnot(cols > 3)

  # Random functions chosen randomly to generate random data
  funs  <- sample(list(rnorm, rexp, runif), cols, replace = TRUE)

  X <- sapply(funs, exec, rows)
  Y <- X %*% rnorm(cols) # linear comb with random coefs

  cbind(Y, X[, seq_len(cols - 3)]) %>% as_tibble()
}

#' Fit a random forrest model
#' @param dat data frame
#' @param trees number of trees in the forrest
rf <- function(dat, trees = 1e3) {
  rand_forest(mode = "regression", mtry = .preds(), trees = trees) %>%
  set_engine("ranger", importance = 'impurity') %>%
  fit(V1 ~ ., data = dat)
}



# Data management and analysis --------------------------------------------


# Data, predictions and metrics for increasingly large datasets
rf_all <-
  tibble(p = seq(5, 100, 10)) %>%
  mutate(
    data    = future_map(p, rdata),
    rf      = future_map(data, rf),
    obspred = map2(data, rf, ~ bind_cols(obs = .x$V1, predict.model_fit(.y, .x))),
    metr    = map(obspred, metrics, obs, .pred)
  )


# Results -----------------------------------------------------------------


# Make plot
unnest(rf_all, metr) %>%
  ggplot(aes(p, .estimate)) +
  geom_line() +
  facet_wrap(~ .metric, ncol = 1, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Increasing error with nunmber of parameters",
    x     = "No of independent variables",
    y     = "Estimated error metric"
  )

ggsave("metrics.png")