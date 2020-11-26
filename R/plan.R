# drake plan
plan <- drake_plan(
  # load data
  d = loadData(),
  # fit models
  m1 = fitModel1(d),
  m2 = fitModel2(d),
  m3 = fitModel3(d),
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
  cond2 = cond(m2, effects = "rel_5"),
  cond3 = cond(m3, effects = "rel_5:cnorm_1"),
  # forest plots
  f2.1 = forest2.1(m2),
  f2.2 = forest2.2(m2),
  f3.1 = forest3.1(m3),
  f3.2 = forest3.2(m3),
  # render report
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)