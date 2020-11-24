# drake plan
plan <- drake_plan(
  # load data
  d = read.csv("data/MARP_data_blinded.csv"),
  # render report
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)