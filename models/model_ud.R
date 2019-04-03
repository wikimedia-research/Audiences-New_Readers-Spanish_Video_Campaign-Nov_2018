# devtools::install_github("chelsyx/CausalImpact", force = TRUE)
# install.packages("dtw")

library(CausalImpact)
library(dtw)

source("refine.R")
source("functions.R")

# es ud from Mexico

cat("Data preparation...\n")
test_series <- unique_devices %>%
  filter(country_code == 'MX', project == 'es.wikipedia', type == 'total') %>%
  group_by(date) %>%
  summarize(MX_es.wikipedia = sum(uniques)) %>%
  ungroup() %>%
  complete(date, fill = list(MX_es.wikipedia = 0))
control_series <- unique_devices %>%
  filter(country_code != 'MX', type == 'total') %>%
  group_by(date, country, project) %>%
  summarize(uniques = sum(uniques)) %>%
  ungroup() %>%
  complete(date, nesting(country, project), fill = list(uniques = 0)) %>%
  unite(metrics, country, project) %>%
  mutate(metrics = gsub(" ", ".", metrics, fixed = TRUE)) %>%
  spread(metrics, uniques)
# control_series <- left_join(control_series, internet_users, by = "date")

holiday.list <- list(HolyWeek, Christmas, Newyear)
# preselect_controls <- "internet_users"

# Choose models
model_spec <- expand.grid(
  trend = c("local_level", "local_linear", "semi_local", "static"),
  train_length = c(84, 126, 183, 400, 600, 800),
  ar = FALSE
)
n_control_candidates <- 20

# Model selection

library(foreach)
library(doParallel)

cl <- makeCluster(10)
registerDoParallel(cl)

model_compare <- foreach(i=1:nrow(model_spec), .combine=rbind,
                         .packages = c("dplyr", "magrittr", "CausalImpact", "dtw"),
                         .export=ls(envir=globalenv())) %dopar% {
  param <- model_spec[i, ]
  y <- test_series
  x <- control_series

  cv_results <- bsts_cv_loop(x, y, train_length = param$train_length, cv_end = online_start - 1, horizon = 70, nfold = 10, step = 14,
                             log_transformed = FALSE, trend = param$trend, autoAR = param$ar, prior_level_sd = 0.01, weekly_seasonality = TRUE,
                             yearly_seasonality = TRUE, holiday.list = holiday.list, dynamic_regression = FALSE,
                             niter = 5000, n_control_candidates = n_control_candidates)

  output <- c(
    i, mean(cv_results$rmse, na.rm = TRUE), sd(cv_results$rmse, na.rm = TRUE), mean(cv_results$mape, na.rm = TRUE), sd(cv_results$mape, na.rm = TRUE), mean(cv_results$rsquare, na.rm = TRUE),
    mean(abs(cv_results$AbsEffect), na.rm = TRUE), mean(cv_results$AbsEffect_CI_width, na.rm = TRUE), mean(cv_results$AbsEffect_sd, na.rm = TRUE), mean(cv_results$contain_zero, na.rm = TRUE)
    )
  output
}

stopCluster(cl)
colnames(model_compare) <- c('ID', 'RMSE', 'RMSE_sd', 'MAPE', 'MAPE_sd', 'Rsquare', 'AbsEffect', 'AbsEffect_CI_width', 'AbsEffect_sd', 'contain_zero')
model_compare <- cbind(model_compare, model_spec)
save(model_compare, file = "data/eswiki_ud_model_compare.RData")

# Check model compare results
load("data/eswiki_ud_model_compare.RData")
model_rank <- model_compare %>%
  dplyr::select(RMSE, RMSE_sd, MAPE, MAPE_sd, AbsEffect, AbsEffect_sd) %>%
  dplyr::mutate_all(rank) %>%
  rowMeans() %>%
  cbind(ID = model_compare$ID, model_compare$contain_zero, model_spec)
# best model is 24 MAPE=0.07829526


# Visual Validation
n_iters <- 1e4
train_length <- 800
trend <- "static"
autoAR <- FALSE
validation_end <- online_start - 1
validation_start <- validation_end - 10*7 + 1
train_end <- validation_start - 1
train_start <- train_end - train_length + 1
y <- test_series
x <- control_series

selected_controls <- control_candidates(
  test = y, control = x,
  match_period_start = train_start, match_period_end = train_end,
  n_candidates = 30)
selected_controls <- unique(c(head(selected_controls$dtw_all, n_control_candidates), head(selected_controls$dtw_eswiki, n_control_candidates),
  head(selected_controls$corr_all, n_control_candidates), head(selected_controls$corr_eswiki, n_control_candidates)))
model <- run_bsts_model(x, y, train_start, train_end, validation_start, validation_end,
  selected_controls, trend = trend, autoAR = autoAR, weekly_seasonality=TRUE, yearly_seasonality=TRUE,
  holiday.list = holiday.list, dynamic_regression = FALSE, prior_level_sd = 0.01, niter = n_iters)
model_impact <- CausalImpact(bsts.model = model$model,
                             post.period.response = model$UnStandardize(as.numeric(model$post.period.response)),
                             UnStandardize = model$UnStandardize)
plot(model$model, "comp")
plot(model_impact)


# Model Impact
test_end <- online_start + 10*7 - 1
test_start <- online_start
train_end <- online_start - 1
train_start <- train_end - train_length + 1
n_iters <- 1e4
y <- test_series
x <- control_series
selected_controls <- control_candidates(
  test = y, control = x,
  match_period_start = train_start, match_period_end = train_end,
  n_candidates = 30)
selected_controls <- unique(c(head(selected_controls$dtw_all, n_control_candidates), head(selected_controls$dtw_eswiki, n_control_candidates),
  head(selected_controls$corr_all, n_control_candidates), head(selected_controls$corr_eswiki, n_control_candidates)))
model <- run_bsts_model(x, y, train_start, train_end, test_start, test_end,
  selected_controls, trend = trend, autoAR = autoAR, weekly_seasonality=TRUE, yearly_seasonality=TRUE,
  holiday.list = holiday.list, dynamic_regression = FALSE, prior_level_sd = 0.01, niter = n_iters)
model_impact <- CausalImpact(bsts.model = model$model,
                             post.period.response = model$UnStandardize(as.numeric(model$post.period.response)),
                             UnStandardize = model$UnStandardize)
p <- plot(model_impact) +
  scale_x_date(name = "Date", date_breaks = "3 months", date_labels = "%b %Y") +
  labs(title = "Impact of the video campaign in 10 weeks on Spanish Wikipedia unique devices from Mexico",
  subtitle = "Vertical dashed line represents the start date of the campaign 15 November 2018")
ggsave("eswiki_ud_impact.png", plot = p, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 14)
summary(model_impact)
summary(model_impact, "report")
png('docs/figures/eswiki_ud_inclusion_prob.png', units = "in", res = 300, height = 8, width = 10)
plot(model$model, "coef")
dev.off()

# inclusion prob
burn <- SuggestBurn(0.1, model)
sort(colMeans(model$model$coefficients[-(1:burn),] != 0))
# Peru_es.wikipedia      Dominican.Republic_es.wikipedia
#            0.99969997                           1.00000000

# average coefficient
sort(apply(model$model$coefficients[-(1:burn),], 2, PositiveMean))
# Peru_es.wikipedia      Dominican.Republic_es.wikipedia
#          0.199875751                        0.508764880
