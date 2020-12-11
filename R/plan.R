# drake plan
plan <- drake_plan(
  # load wide data
  dWide = loadWideData(file_in("data/MARP_data_blinded.csv")),
  # religion pca
  pca1 = fitPCA(dWide, nfactors = 9),
  pca2 = fitPCA(dWide, nfactors = 1),
  scree = getScreePlot(pca1),
  # pivot data to long format for modelling
  # and add religion pca score to dataset
  dLong = pivotLong(dWide, pca2),
  # load geographic matrix
  geo = loadMat(file_in("data/MARP_geodistlog.xlsx")),
  # fit models
  m1.1 = fitModel(dLong, 
                  formula = bf(wellbeing ~ 1 + (1 | subscale/item) + (1 | iso) + 
                                 (1 | denomination) + (1 | subject) + age + gender + gdp_scaled)),
  m1.2 = fitModel(dLong, 
                  formula = bf(wellbeing ~ 1 + religion + (1 + religion | subscale/item) + 
                    (1 + religion | iso) + (1 + religion | denomination) + (1 | subject) + 
                    age + gender + gdp_scaled)),
  m1.3 = fitModel(dLong, formula = bf(wellbeing ~ 1 + religion*cnorm + (1 + religion*cnorm | subscale/item) + 
                    (1 + religion*cnorm | iso) + (1 + religion*cnorm | denomination) + (1 | subject) + 
                    age + gender + gdp_scaled)),
  m2.1 = fitModel(dLong, 
                  formula = bf(wellbeing ~ 1 + (1 | subscale/item) + 
                    (1 | gr(iso, cov = geo)) + (1 | denomination) + (1 | subject) + 
                    age + gender + gdp_scaled), 
                  cov = list(geo = geo)),
  m2.2 = fitModel(dLong, 
                  formula = bf(wellbeing ~ 1 + religion + (1 + religion | subscale/item) + 
                    (1 | gr(iso, cov = geo, id = "iso")) + (0 + religion | gr(iso, id = "iso")) + 
                    (1 + religion | denomination) + (1 | subject) + 
                    age + gender + gdp_scaled), 
                  cov = list(geo = geo)),
  m2.3 = fitModel(dLong, 
                  formula = bf(wellbeing ~ 1 + religion*cnorm + (1 + religion*cnorm | subscale/item) + 
                    (1 | gr(iso, cov = geo, id = "iso")) + (0 + religion*cnorm | gr(iso, id = "iso")) + 
                    (1 + religion*cnorm | denomination) + (1 | subject) + 
                    age + gender + gdp_scaled), 
                  cov = list(geo = geo)),
  # leave-one-out cross-validation
  loo1.1 = loo(m1.1),
  loo1.2 = loo(m1.2),
  loo1.3 = loo(m1.3),
  loo2.1 = loo(m2.1),
  loo2.2 = loo(m2.2),
  loo2.3 = loo(m2.3),
  # model comparison
  looCompare1 = loo_compare(loo1.1, loo1.2),
  looCompare2 = loo_compare(loo1.2, loo1.3),
  looCompare3 = loo_compare(loo2.1, loo2.2),
  looCompare4 = loo_compare(loo2.2, loo2.3),
  # model summaries
  summary1.1 = summary(m1.1),
  summary1.2 = summary(m1.2),
  summary1.3 = summary(m1.3),
  summary2.1 = summary(m2.1),
  summary2.2 = summary(m2.2),
  summary2.3 = summary(m2.3),
  # conditional effects
  cond1.2 = cond(m1.2, effects = "religion"),
  cond1.3 = cond(m1.3, effects = "religion:cnorm"),
  cond2.2 = cond(m2.2, effects = "religion"),
  cond2.3 = cond(m2.3, effects = "religion:cnorm"),
  # forest plots
  f1.2.1 = forest(m1.2, parameter = "religion", group = "subscale"),
  f1.2.2 = forest(m1.2, parameter = "religion", group = "subscale:item"),
  f1.2.3 = forest(m1.2, parameter = "religion", group = "iso"),
  f1.2.4 = forest(m1.2, parameter = "religion", group = "denomination"),
  f1.3.1 = forest(m1.3, parameter = "religion:cnorm", group = "subscale"),
  f1.3.2 = forest(m1.3, parameter = "religion:cnorm", group = "subscale:item"),
  f1.3.3 = forest(m1.3, parameter = "religion:cnorm", group = "iso"),
  f1.3.4 = forest(m1.3, parameter = "religion:cnorm", group = "denomination"),
  f2.2.1 = forest(m2.2, parameter = "religion", group = "subscale"),
  f2.2.2 = forest(m2.2, parameter = "religion", group = "subscale:item"),
  f2.2.3 = forest(m2.2, parameter = "religion", group = "iso"),
  f2.2.4 = forest(m2.2, parameter = "religion", group = "denomination"),
  f2.3.1 = forest(m2.3, parameter = "religion:cnorm", group = "subscale"),
  f2.3.2 = forest(m2.3, parameter = "religion:cnorm", group = "subscale:item"),
  f2.3.3 = forest(m2.3, parameter = "religion:cnorm", group = "iso"),
  f2.3.4 = forest(m2.3, parameter = "religion:cnorm", group = "denomination"),
  # render report
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)