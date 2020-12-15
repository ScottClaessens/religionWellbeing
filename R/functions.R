# set default ggplot theme
theme_set(theme_classic())

# load wide data
loadWideData <- function(file) {
  # get iso codes
  iso <- c("Australia" = "AU", "Belgium" = "BE", "Brazil" = "BR", "Canada" = "CA", "Chile" = "CL",
           "China" = "CN", "Croatia" = "HR", "Denmark" = "DK", "France" = "FR", "Germany" = "DE",
           "India" = "IN", "Ireland" = "IE", "Israel" = "IL", "Italy" = "IT", "Japan" = "JP",
           "Lithuania" = "LT", "Morocco" = "MA", "Netherlands" = "NL", "Romania" = "RO",
           "Singapore" = "SG", "Spain" = "ES", "Turkey" = "TR", "UK" = "GB", "US" = "US")
  # load data
  read.csv(file) %>%
    mutate(
      # fill in denomination
      # NOTE: we currently leave religious denomination groupings as they
      # appear in the dataset, but we will create higher-level groupings
      # if this approach creates model fitting functions
      denomination = factor(ifelse(is.na(denomination), 
                                   "No denomination specified",
                                   denomination)),
      # add iso codes to dataset
      iso = factor(as.character(iso[country]))
      ) %>%
    # reduced slice of data for toy models (1 person per country and denomination)
    group_by(iso, denomination) %>%  # remove these 3
    slice(1) %>%                     # lines before
    ungroup() %>%                    # real analysis
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

# load covariance matrix
loadMat <- function(file) {
  out <-
    # get linguistic proximity matrix
    read_xlsx(file) %>%
    # ISO as rownames
    column_to_rownames("ISO") %>%
    # as matrix
    as.matrix() %>%
    # avoid rounding error
    round(4)
  # positive definite covariance matrix
  # where covariance is linearly related
  # to linguistic proximity
  diag(out) <- 1
  return(out)
}

# generic model fitting function
fitModel <- function(dLong, formula, cov = NULL) {
  brm(
    # model formula
    formula,
    # fit to long data
    data = dLong,
    # covariance matrix for iso random intercepts
    data2 = cov,
    # cumulative family for ordinal model
    family = cumulative,
    # set identical priors for every model
    prior = c(prior(normal(0, 2), class = Intercept), # prior choices from prior predictive checks
              prior(normal(0, 0.5), class = b),       # these priors give every ordinal outcome (1-5)
              prior(exponential(3), class = sd)),     # equal prior plausibility
    # run for 3000 iterations (1500 warmup) with 4 cores
    iter = 3000, cores = 4,
    # additional control parameters for stan
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )
}

# plot conditional effects
cond <- function(model, effects = NULL) {
  plot(conditional_effects(model, effects = effects), plot = FALSE)[[1]] +
    scale_y_continuous(name = "Wellbeing score (1-5)", limits = c(1, 5))
}

# forest plot
forest <- function(model, parameter, group) {
  model %>%
    # use tidybayes to dynamically spread posterior samples
    # for particular parameter-group combination
    spread_draws(!!sym(paste0("b_", parameter)), 
                 (!!sym(paste0("r_", group)))[grp,par]) %>%
    # filter to parameter of interest
    filter(par == parameter) %>%
    # get means for each group
    mutate(mean = !!sym(paste0("b_", parameter)) + !!sym(paste0("r_", group))) %>%
    # create forest plot
    ggplot(aes(y = grp, x = mean)) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(y = NULL, x = parameter)
}