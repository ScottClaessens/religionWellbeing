# set default ggplot theme
theme_set(theme_classic())

# custom functions

# load data and manipulate to long-format
loadData <- function() {
  # load data
  read.csv("data/MARP_data_blinded.csv") %>%
    # reduced slice of data for toy models (4 people per country)
    group_by(country) %>%  # remove these 3
    slice(1:4) %>%         # lines before
    ungroup() %>%          # real analysis
    # remove participants who failed attention check
    filter(attention_check == 1) %>%
    # remove composite wellbeing items for pivot
    select(-wb_overall_mean, -wb_phys_mean, -wb_psych_mean, -wb_soc_mean) %>%
    # pivot wellbeing items longer
    pivot_longer(starts_with("wb_"),
                 names_to = "item",
                 values_to = "wellbeing") %>%
    # create subscale variable
    mutate(subscale = substr(item, 4, nchar(item) - 2))
}

# fit random-intercept-only model
fitModel1 <- function(d) {
  brm(wellbeing ~ 1 + (1 | subscale/item) + (1 | country/subject),
      data = d, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept), # prior choices from prior predictive checks
                prior(exponential(3), class = sd)),     # these priors give every ordinal outcome (1-5) equal prior plausibility
      iter = 3000, cores = 4,
      control = list(adapt_delta = 0.99))
}

# fit random-intercept and random-slope model
# note: using rel_5 as a toy example, can be changed
fitModel2 <- function(d) {
  brm(wellbeing ~ 1 + rel_5 + (1 + rel_5 | subscale/item) + (1 | country/subject),
      data = d, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(3), class = sd)),
      iter = 3000, cores = 4,
      control = list(adapt_delta = 0.99))
}

# fit interaction model
# note: using cnorm_1 as a toy example, can be changed
fitModel3 <- function(d) {
  brm(wellbeing ~ 1 + rel_5*cnorm_1 + (1 + rel_5*cnorm_1 | subscale/item) + (1 | country/subject),
      data = d, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(3), class = sd)),
      iter = 3000, cores = 4,
      control = list(adapt_delta = 0.99))
}

# plot conditional effects
cond <- function(model, effects = NULL) {
  plot(conditional_effects(model, effects = effects), plot = FALSE)[[1]] +
    scale_y_continuous(name = "Wellbeing score (1-5)", limits = c(1, 5))
}

# forest plot 1 for model 2
forest2.1 <- function(m2) {
  m2 %>%
    spread_draws(b_rel_5, r_subscale[subscale,parameter]) %>%
    filter(parameter == "rel_5") %>%
    mutate(subscale_mean = b_rel_5 + r_subscale) %>%
    ggplot(aes(y = subscale, x = subscale_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "rel_5 parameter") +
    ggtitle("Effect of rel_5 split by wellbeing subscale")
}

# forest plot 2 for model 2
forest2.2 <- function(m2) {
  m2 %>%
    spread_draws(b_rel_5, `r_subscale:item`[item,parameter]) %>%
    filter(parameter == "rel_5") %>%
    mutate(item_mean = b_rel_5 + `r_subscale:item`) %>%
    ggplot(aes(y = item, x = item_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "rel_5 parameter") +
    ggtitle("Effect of rel_5 split by wellbeing item")
}

# forest plot 1 for model 3
forest3.1 <- function(m3) {
  m3 %>%
    spread_draws(`b_rel_5:cnorm_1`, r_subscale[subscale,parameter]) %>%
    filter(parameter == "rel_5:cnorm_1") %>%
    mutate(subscale_mean = `b_rel_5:cnorm_1` + r_subscale) %>%
    ggplot(aes(y = subscale, x = subscale_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "Interaction parameter") +
    ggtitle("Interaction effect split by wellbeing subscale")
}

# forest plot 2 for model 3
forest3.2 <- function(m3) {
  m3 %>%
    spread_draws(`b_rel_5:cnorm_1`, `r_subscale:item`[item,parameter]) %>%
    filter(parameter == "rel_5:cnorm_1") %>%
    mutate(item_mean = `b_rel_5:cnorm_1` + `r_subscale:item`) %>%
    ggplot(aes(y = item, x = item_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "Interaction parameter") +
    ggtitle("Interaction effect split by wellbeing item")
}