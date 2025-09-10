library(terra)
library(amt)
library(tidyverse)
library(circular)

# Env covariates
forest <- get_sh_forest()

# Distance to forest
forest <- subst(forest, 0, NA)
dist_forest <- scale(distance(forest))
names(dist_forest) <- "dist_forest"

# Tracking data
data(deer)

dat1 <-
  deer |>
  steps_by_burst() |>
  random_steps() |>
  extract_covariates(dist_forest) |>
  time_of_day(where = "both")


m0 <- fit_clogit(
  case_ ~ # This is the response
    dist_forest + # distance to forest
    strata(step_id_), # each stratum is an observed step with its random steps
  data = dat1, # the data
  model = TRUE # to save the input data
)

summary(m0)

x1 <- data.frame(dist_forest = 1)
x2 <- data.frame(dist_forest = 0)

exp(log_rss(m0, x1, x2)$df$log_rss)

w1 <- exp(coef(m0) * 1)
w2 <- exp(coef(m0) * 0)

s1 <- sample(1:2, 1e6, TRUE, prob = c(w1, w2))
(s2 <- table(s1))
s2[1] / s2[2]
exp(coef(m0))


x1 <- data.frame(dist_forest = seq(-1, 3, len = 100))
x2 <- data.frame(dist_forest = 0)

log_rss(m0, x1, x2)$df |>
  mutate(rss = exp(log_rss)) |>
  ggplot(aes(dist_forest_x1, rss)) +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2) +
  theme_minimal() +
  labs(x = "Distance to forest (scaled)", y = "RSS")



m1 <- fit_clogit(
  case_ ~ # This is the response
    dist_forest + # distance to forest
    dist_forest:tod_end_ + # as interaction with time of day
    strata(step_id_), # each stratum is an observed step with its random steps
  data = dat1, # the data
  model = TRUE # to save the input data
)

summary(m1)


# Day
x1 <- data.frame(dist_forest = seq(-1, 3, len = 100),
                 tod_end_ = factor("day", levels = c("day", "night")))
x2 <- data.frame(dist_forest = 0,
                 tod_end_ = factor("day", levels = c("day", "night")))

day <- log_rss(m1, x1, x2, ci = "se")$df |>
  mutate(rss = exp(log_rss), lwr = exp(lwr),
         upr = exp(upr), when = "day")


# night
x1 <- data.frame(dist_forest = seq(-1, 3, len = 100),
                 tod_end_ = factor("night", levels = c("day", "night")))
x2 <- data.frame(dist_forest = 0,
                 tod_end_ = factor("night", levels = c("day", "night")))

night <- log_rss(m1, x1, x2, ci = "se")$df |>
  mutate(rss = exp(log_rss), lwr = exp(lwr), upr = exp(upr),
         when = "night")

bind_rows(day, night) |>
  ggplot(aes(dist_forest_x1, rss, col = when)) +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = when, col = NULL),
              alpha = 0.2) +
  theme_minimal() +
  labs(x = "Distance to forest (scaled)", y = "RSS")


# One step back
dat1 |> filter(case_) |>
  ggplot(aes(sl_)) +
  geom_density() +
  geom_function(
    fun = dgamma,
    args = list(
      shape = sl_distr(dat1)$params$shape,
      scale = sl_distr(dat1)$params$scale),
    col = "red", lty = 2) +
  scale_y_continuous(limits = c(0, 0.003)) +
  theme_minimal()


m2 <- fit_clogit(
  case_ ~ # This is the response
    dist_forest + # distance to forest
    dist_forest:tod_end_ + # as interaction with time of day

    # movement model
    sl_ +
    log(sl_) +

    strata(step_id_), # each stratum is an observed step with its random steps
  data = dat1, # the data
  model = TRUE # to save the input data
)

updated_sl <- update_sl_distr(m2, beta_log_sl = "log(sl_)")

summary(m2)

dat1 |> filter(case_) |>
  ggplot(aes(sl_)) +
  geom_density() +
  geom_function(
    aes(col = "Tentative"),
    fun = dgamma,
    args = list(
      shape = sl_distr(dat1)$params$shape,
      scale = sl_distr(dat1)$params$scale),
    lty = 2) +
  geom_function(
    aes(col = "Updated"),
    fun = dgamma,
    args = list(
      shape = updated_sl$params$shape,
      scale = updated_sl$params$scale),
    lty = 2) +
  scale_y_continuous(limits = c(0, 0.003)) +
  theme_minimal()


m3 <- fit_clogit(
  case_ ~ # This is the response
    dist_forest + # distance to forest
    dist_forest:tod_end_ + # as interaction with time of day

    # Movement kernel
    sl_ +
    log(sl_) +

    # Is movement different for different times?
    sl_:tod_start_ +
    log(sl_):tod_start_ +
    strata(step_id_), # each stratum is an observed step with its random steps
  data = dat1, # the data
  model = TRUE # to save the input data
)

summary(m3)

tent_shape <- sl_distr_params(m3)$shape
tent_scale <- sl_distr_params(m3)$scale

day_shape <- tent_shape + coef(m3)["log(sl_)"]
night_shape <- tent_shape + coef(m3)["log(sl_)"] + coef(m3)["log(sl_):tod_start_night"]

day_scale <- 1/ (1 / tent_scale - (coef(m3)["sl_"]))
night_scale <- 1/ (1 / tent_scale - (coef(m3)["sl_"] + coef(m3)["sl_:tod_start_night"]))

dat1 |> filter(case_) |>
  ggplot(aes(sl_)) +
  geom_density() +
  geom_function(
    aes(col = "Tentative"),
    fun = dgamma,
    args = list(
      shape = sl_distr(dat1)$params$shape,
      scale = sl_distr(dat1)$params$scale),
    lty = 2) +
  geom_function(
    aes(col = "Day"),
    fun = dgamma,
    args = list(
      shape = day_shape,
      scale = day_scale),
    lty = 2) +
  geom_function(
    aes(col = "Night"),
    fun = dgamma,
    args = list(
      shape = night_shape,
      scale = night_scale),
    lty = 2) +
  scale_y_continuous(limits = c(0, 0.003)) +
  theme_minimal() +
  labs(col = "When")

# Night, day and twilight steps
dat2 <- dat1 |> mutate(
  tod2 = case_when(
    tod_start_ == "night" & tod_end_ == "night" ~ "night",
    tod_start_ == "day" & tod_end_ == "day" ~ "day",
    .default = "twilight"
  )
)

dat2 |> filter(case_) |> pull(tod2) |> table()

m4 <- fit_clogit(
  case_ ~ # This is the response
    dist_forest + # distance to forest
    dist_forest:tod2 + # as interaction with time of day

    # Movement model
    sl_ +
    log(sl_) +

    sl_:tod2 +
    log(sl_):tod2 +
    strata(step_id_),
  data = dat2, # the data
  model = TRUE # to save the input data
)

summary(m4)

tent_shape <- sl_distr_params(m4)$shape
tent_scale <- sl_distr_params(m4)$scale

day_shape <- tent_shape + coef(m4)["log(sl_)"]
night_shape <- tent_shape + coef(m4)["log(sl_)"] + coef(m4)["log(sl_):tod2night"]
twilight_shape <- tent_shape + coef(m4)["log(sl_)"] + coef(m4)["log(sl_):tod2twilight"]

day_scale <- 1/ (1 / tent_scale - (coef(m4)["sl_"]))
night_scale <- 1/ (1 / tent_scale - (coef(m4)["sl_"] + coef(m4)["sl_:tod2night"]))
twilight_scale <- 1/ (1 / tent_scale - (coef(m4)["sl_"] + coef(m4)["sl_:tod2twilight"]))

dat1 |> filter(case_) |>
  ggplot(aes(sl_)) +
  geom_density() +
  geom_function(
    aes(col = "Tentative"),
    fun = dgamma,
    args = list(
      shape = sl_distr(dat1)$params$shape,
      scale = sl_distr(dat1)$params$scale),
    lty = 2) +
  geom_function(
    aes(col = "Day"),
    fun = dgamma,
    args = list(
      shape = day_shape,
      scale = day_scale),
    lty = 2) +
  geom_function(
    aes(col = "Night"),
    fun = dgamma,
    args = list(
      shape = night_shape,
      scale = night_scale),
    lty = 2) +
  scale_y_continuous(limits = c(0, 0.003)) +
  geom_function(
    aes(col = "Twilight"),
    fun = dgamma,
    args = list(
      shape = twilight_shape,
      scale = twilight_scale),
    lty = 2) +
  scale_y_continuous(limits = c(0, 0.003)) +
  theme_minimal() +
  labs(col = "When")

# Including the turn angle

# Do dummy encoding manually
dat2$night <- as.numeric(dat2$tod2 == "night")
dat2$twilight <- as.numeric(dat2$tod2 == "twilight")

m5 <- fit_clogit(
  case_ ~ # This is the response
    dist_forest + # distance to forest
    dist_forest:night +
    dist_forest:twilight +

    # Movement model
    sl_ +
    log(sl_) +
    cos(ta_) +
    sl_:night +
    log(sl_):night +
    cos(ta_):night +
    sl_:twilight +
    log(sl_):twilight +
    cos(ta_):twilight +

    strata(step_id_),
  data = dat2, # the data
  model = TRUE # to save the input data
)

summary(m5)

tent_shape <- sl_distr_params(m5)$shape
tent_scale <- sl_distr_params(m5)$scale

day_shape <- tent_shape + coef(m5)["log(sl_)"]
night_shape <- tent_shape + coef(m5)["log(sl_)"] + coef(m5)["log(sl_):night"]
twilight_shape <- tent_shape + coef(m5)["log(sl_)"] + coef(m5)["log(sl_):twilight"]

day_scale <- 1/ (1 / tent_scale - (coef(m5)["sl_"]))
night_scale <- 1/ (1 / tent_scale - (coef(m5)["sl_"] + coef(m5)["sl_:night"]))
twilight_scale <- 1/ (1 / tent_scale - (coef(m5)["sl_"] + coef(m5)["sl_:twilight"]))

dat1 |> filter(case_) |>
  ggplot(aes(sl_)) +
  geom_density() +
  geom_function(
    aes(col = "Tentative"),
    fun = dgamma,
    args = list(
      shape = sl_distr(dat1)$params$shape,
      scale = sl_distr(dat1)$params$scale),
    lty = 2) +
  geom_function(
    aes(col = "Day"),
    fun = dgamma,
    args = list(
      shape = day_shape,
      scale = day_scale),
    lty = 2) +
  geom_function(
    aes(col = "Night"),
    fun = dgamma,
    args = list(
      shape = night_shape,
      scale = night_scale),
    lty = 2) +
  scale_y_continuous(limits = c(0, 0.003)) +
  geom_function(
    aes(col = "Twilight"),
    fun = dgamma,
    args = list(
      shape = twilight_shape,
      scale = twilight_scale),
    lty = 2) +
  scale_y_continuous(limits = c(0, 0.003)) +
  theme_minimal() +
  labs(col = "When", title = "Step-length distribution")

# turn-angle dist
tent_concentration <- ta_distr_params(m5)$kappa

day_concentration <- tent_concentration + coef(m5)["cos(ta_)"]
night_concentration <- tent_concentration +
  coef(m5)["cos(ta_)"] + coef(m5)["cos(ta_):night"]
twilight_concentration <- tent_concentration + coef(m5)["cos(ta_)"] +
  coef(m5)["cos(ta_):twilight"]

dat1 |> filter(case_) |>
  ggplot(aes(ta_)) +
  geom_density() +
  geom_function(
    aes(col = "Tentative"),
    fun = dvonmises,
    args = list(
      kappa = tent_concentration,
      mu = 0),
    lty = 2) +
  geom_function(
    aes(col = "Day"),
    fun = dvonmises,
    args = list(
      kappa = abs(day_concentration),
      mu = 0),
    lty = 2) +
  geom_function(
    aes(col = "Night"),
    fun = dvonmises,
    args = list(
      kappa = night_concentration,
      mu = 0),
    lty = 2) +
  geom_function(
    aes(col = "Twilight"),
    fun = dvonmises,
    args = list(
      kappa = abs(twilight_concentration),
      mu = 0),
    lty = 2) +
#  scale_y_continuous(limits = c(0.01, 0.2)) +
  theme_minimal() +
  labs(col = "When", title = "Turn-angle distribution")

# Habitat selection (m5)
# Day
x1 <- data.frame(dist_forest = seq(-1, 3, len = 100),
                 night = 0, twilight = 0,
                 sl_ = 1, ta_ = 1)
x2 <- data.frame(dist_forest = 0,
                 night = 0, twilight = 0,
                 sl_ = 1, ta_ = 1)

day <- log_rss(m5, x1, x2, ci = "se")$df |>
  mutate(rss = exp(log_rss), lwr = exp(lwr),
         upr = exp(upr), when = "day")


# night
x1 <- data.frame(dist_forest = seq(-1, 3, len = 100),
                 night = 1, twilight = 0,
                 sl_ = 1, ta_ = 1)
x2 <- data.frame(dist_forest = 0,
                 night = 1, twilight = 0,
                 sl_ = 1, ta_ = 1)

night <- log_rss(m5, x1, x2, ci = "se")$df |>
  mutate(rss = exp(log_rss), lwr = exp(lwr), upr = exp(upr),
         when = "night")

# twilight
x1 <- data.frame(dist_forest = seq(-1, 3, len = 100),
                 night = 0, twilight = 1,
                 sl_ = 1, ta_ = 1)
x2 <- data.frame(dist_forest = 0,
                 night = 0, twilight = 1,
                 sl_ = 1, ta_ = 1)

twilight <- log_rss(m5, x1, x2, ci = "se")$df |>
  mutate(rss = exp(log_rss), lwr = exp(lwr), upr = exp(upr),
         when = "twilight")

bind_rows(day, night, twilight) |>
  ggplot(aes(dist_forest_x1, rss, col = when)) +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = when, col = NULL),
              alpha = 0.2) +
  theme_minimal() +
  labs(x = "Distance to forest (scaled)", y = "RSS")


AIC(m0$model, m1$model, m2$model, m3$model, m4$model, m5$model) |>
  mutate(dAIC = AIC - min(AIC))
