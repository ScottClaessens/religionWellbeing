# make file
source("R/packages.R")
source("R/functions.R")
source("R/plan.R")
options(
  clustermq.scheduler = "slurm", 
  clustermq.template = "slurm_clustermq.tmpl")
make(plan,
     jobs = 6,
     parallelism = "clustermq",
     lock_envir = FALSE)
