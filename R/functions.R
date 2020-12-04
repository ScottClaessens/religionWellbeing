# set default ggplot theme
theme_set(theme_classic())

# load wide data
loadWideData <- function() {
  # load data
  read.csv("data/MARP_data_blinded.csv") %>%
    # reduced slice of data for toy models (4 people per country)
    group_by(country) %>%  # remove these 3
    slice(1:4) %>%         # lines before
    ungroup() %>%          # real analysis
    # remove participants who failed attention check
    filter(attention_check == 1) %>%
    # standardise age and create cnorm composite
    mutate(age = as.numeric(scale(age)),
           cnorm = (cnorm_1 + cnorm_2) / 2)
}

# principal components analysis for religion items
fitPCA <- function(dWide, nfactors) {
  dWide %>%
    # lump together non-religious and atheist people
    mutate(rel_3 = ifelse(rel_3 == 0.5, 0, 1)) %>%
    # fit pca to all religion variables
    select(starts_with("rel_")) %>%
    pca(nfactors = nfactors)
}

# pca scree plot
getScreePlot <- function(pca) {
  tibble(Factor = 1:length(pca$values), Eigenvalues = pca$values) %>%
    ggplot(aes(x = Factor, y = Eigenvalues)) +
    geom_point() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_x_continuous(breaks = 1:length(pca$values))
}

# pivot data longer for modelling
pivotLong <- function(dWide, pca) {
  dWide %>%
    # add religion principal component to dataset
    mutate(religion = pca$scores[,1]) %>%
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
fitModel1 <- function(dLong) {
  brm(wellbeing ~ 1 + (1 | subscale/item) + (1 | country) + (1 | subject) + age + gender + gdp_scaled,
      data = dLong, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept), # prior choices from prior predictive checks
                prior(exponential(3), class = sd)),     # these priors give every ordinal outcome (1-5) equal prior plausibility
      iter = 3000, cores = 4,
      control = list(adapt_delta = 0.99, max_treedepth = 15))
}

# fit random-intercept and random-slope model
fitModel2 <- function(dLong) {
  brm(wellbeing ~ 1 + religion + (1 + religion | subscale/item) + (1 + religion | country) + (1 | subject) + age + gender + gdp_scaled,
      data = dLong, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(3), class = sd)),
      iter = 3000, cores = 4,
      control = list(adapt_delta = 0.99, max_treedepth = 15))
}

# fit interaction model
fitModel3 <- function(dLong) {
  brm(wellbeing ~ 1 + religion*cnorm + (1 + religion*cnorm | subscale/item) + (1 + religion*cnorm | country) + (1 | subject) + age + gender + gdp_scaled,
      data = dLong, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(3), class = sd)),
      iter = 3000, cores = 4,
      control = list(adapt_delta = 0.99, max_treedepth = 15))
}

# plot conditional effects
cond <- function(model, effects = NULL) {
  plot(conditional_effects(model, effects = effects), plot = FALSE)[[1]] +
    scale_y_continuous(name = "Wellbeing score (1-5)", limits = c(1, 5))
}

# forest plot 1 for model 2
forest2.1 <- function(m2) {
  m2 %>%
    spread_draws(b_religion, r_subscale[subscale,parameter]) %>%
    filter(parameter == "religion") %>%
    mutate(subscale_mean = b_religion + r_subscale) %>%
    ggplot(aes(y = subscale, x = subscale_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "religion parameter") +
    ggtitle("Effect of religion split by wellbeing subscale")
}

# forest plot 2 for model 2
forest2.2 <- function(m2) {
  m2 %>%
    spread_draws(b_religion, `r_subscale:item`[item,parameter]) %>%
    filter(parameter == "religion") %>%
    mutate(item_mean = b_religion + `r_subscale:item`) %>%
    ggplot(aes(y = item, x = item_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "religion parameter") +
    ggtitle("Effect of religion split by wellbeing item")
}

# forest plot 3 for model 2
forest2.3 <- function(m2) {
  m2 %>%
    spread_draws(b_religion, `r_country`[country,parameter]) %>%
    filter(parameter == "religion") %>%
    mutate(country_mean = b_religion + `r_country`) %>%
    ggplot(aes(y = fct_rev(country), x = country_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "religion parameter") +
    ggtitle("Effect of religion split by country")
}

# forest plot 1 for model 3
forest3.1 <- function(m3) {
  m3 %>%
    spread_draws(`b_religion:cnorm`, r_subscale[subscale,parameter]) %>%
    filter(parameter == "religion:cnorm") %>%
    mutate(subscale_mean = `b_religion:cnorm` + r_subscale) %>%
    ggplot(aes(y = subscale, x = subscale_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "Interaction parameter") +
    ggtitle("Interaction effect split by wellbeing subscale")
}

# forest plot 2 for model 3
forest3.2 <- function(m3) {
  m3 %>%
    spread_draws(`b_religion:cnorm`, `r_subscale:item`[item,parameter]) %>%
    filter(parameter == "religion:cnorm") %>%
    mutate(item_mean = `b_religion:cnorm` + `r_subscale:item`) %>%
    ggplot(aes(y = item, x = item_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "Interaction parameter") +
    ggtitle("Interaction effect split by wellbeing item")
}

# forest plot 3 for model 3
forest3.3 <- function(m3) {
  m3 %>%
    spread_draws(`b_religion:cnorm`, `r_country`[country,parameter]) %>%
    filter(parameter == "religion:cnorm") %>%
    mutate(country_mean = `b_religion:cnorm` + `r_country`) %>%
    ggplot(aes(y = fct_rev(country), x = country_mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = "Interaction parameter") +
    ggtitle("Interaction effect split by country")
}