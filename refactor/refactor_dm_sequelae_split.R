###
#Code to calculate split of outcomes using the ratio of T1 and T2
###
library("data.table")
source("masked/get_draws.R")


#### arguments
basedir <- "maksed"
outdir <- file.path(basedir, "gbd_2020_test_type2_age_15_plus")

# input model entity ids
# dm: diabetes, vl: vision loss
me_id <- list(
  dm_parent = 24632,
  dm_t1 = 24633,
  dm_t2 = 24634,
  vl_mod = 2014,
  vl_sev = 2015,
  vl_blind = 2016
)

# get draw arguments
location_id <- 102
age_group_id <- c(2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                  30, 31, 32, 34, 235, 238, 388, 389)
default_get_draws_args <- list(
  gbd_id_type = "modelable_entity_id",
  source = "epi",
  decomp_step = "iterative",
  gbd_round_id = 7,
  measure_id = 5,
  location_id = location_id,
  age_group_id = age_group_id
)


#### load files
# convenient function
get_draws_as_dt <- function(...) {
  get_draws_args <- modifyList(default_get_draws_args, val = list(...))
  setDT(do.call(get_draws, get_draws_args))
}

# read files
dt_prev <- lapply(me_id, function(x) get_draws_as_dt(gbd_id = x))


#### computation of dm_d2
# format prevalence
col_draws <- paste("draw", 0:999, sep = "_")
col_ids <- c("location_id", "age_group_id", "sex_id", "year_id")
col_ids_all <- c(col_ids, "draw_id")
for (me in c("dm_parent", "dm_t1")) {
  dt_prev[[me]] <- melt(
    dt_prev[[me]],
    measure.vars = col_draws,
    variable.name = "draw_id",
    value.name = me
  )[, c(col_ids_all, me), with = FALSE]
}

dm_prev <- merge(dt_prev$dm_parent, dt_prev$dm_t1, by = col_ids_all, all = TRUE)
dm_prev[dm_parent < dm_t1, dm_parent := dm_t1]
dm_prev[, dm_t2 := dm_parent - dm_t1]

# squeeze
age_group_id_less_than_15 <- c(2, 3, 388, 389, 238, 34, 6, 7)
dm_prev[, dm_t1_ratio := dm_t1 / dm_parent]
dm_prev[age_group_id %in% age_group_id_less_than_15, dm_t1_ratio := 1]
dm_prev[, `:=`(dm_t1 = dm_t1_ratio * dm_parent,
               dm_t2 = (1 - dm_t1_ratio) * dm_parent)]


#### save results
dm_t2_prev <- dm_prev[, c(col_ids_all, "dm_t2"), with = FALSE]
dm_t2_prev <- dcast(dm_t2_prev,
                    paste(paste(col_ids, collapse = "+"), "draw_id", sep = "~"),
                    value.var = "dm_t2")
filename <- file.path(paste0("me_", me_id$dm_t2),
                      paste0("5_", location_id, ".csv"))
fwrite(dm_t2_prev, filename)
