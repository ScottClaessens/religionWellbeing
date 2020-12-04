# drake plan
plan <- drake_plan(
  # load wide data
  dWide = loadWideData(),
  # religion pca
  pca1 = fitPCA(dWide, nfactors = 9),
  pca2 = fitPCA(dWide, nfactors = 1),
  scree = getScreePlot(pca1),
  # pivot data to long format for modelling
  # and add religion pca score to dataset
  dLong = pivotLong(dWide, pca2),
  # fit models
  m1 = fitModel1(dLong),
  m2 = fitModel2(dLong),
  m3 = fitModel3(dLong),
  # leave-one-out cross-validation
  loo1 = loo(m1),
  loo2 = loo(m2),
  loo3 = loo(m3),
  # model comparison
  looCompare1 = loo_compare(loo1, loo2),
  looCompare2 = loo_compare(loo2, loo3),
  # model summaries
  summary1 = summary(m1),
  summary2 = summary(m2),
  summary3 = summary(m3),
  # conditional effects
  cond2 = cond(m2, effects = "religion"),
  cond3 = cond(m3, effects = "religion:cnorm"),
  # forest plots
  f2.1 = forest2.1(m2),
  f2.2 = forest2.2(m2),
  f2.3 = forest2.3(m2),
  f3.1 = forest3.1(m3),
  f3.2 = forest3.2(m3),
  f3.3 = forest3.3(m3),
  # render report
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)