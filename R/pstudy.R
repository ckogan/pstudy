#' @examples
#' df <- tbl_df(data.frame(id = c(1,2,3,4,5,6), x = c(1,2,3,4,5,6), grp = c(1L, 1L, 1L, 2L, 1L, 2L)))
#' pst_df <- pstudy(df, id, grp, c(1L, 2L), c(0.25, 0.5))
#' df %>% training(pst_df, .)
pstudy <- function(df, id_var, group_var, groups, p_train) {
  gexpr <- substitute(group_var)
  grp <- eval(gexpr, df, parent.frame())
  iexpr <- substitute(id_var)
  id <- eval(iexpr, df, parent.frame())
  ngrp <- table(grp)
  in_train <- Map(split_tt, ngrp, p_train)
  ix <- order(do.call(c,tapply(1:nrow(df), grp, I)))
  in_train <- do.call(c, in_train)[ix]
  obj <- list(train_id = id[in_train], group_var = gexpr, id_var = iexpr, groups = groups, p_train = p_train)
  structure(obj, class = "pstudy")
}


training <- function(x, ...) {
  UseMethod("training")
}

testing <- function(x, ...) {
  UseMethod("testing")
}

subset.pstudy <- function(obj, df, ftrain) {
  id <- eval(obj$id_var, df, parent.frame())
  id_var <- deparse(obj$id_var)
  keep <- setdiff(names(df), id_var)
  df[ftrain(id %in% obj$train_id), keep]
}

training.pstudy <- function(obj, df) {
  subset(obj, df=df, ftrain=I)
}

testing.pstudy <- function(obj, df) {
  subset(obj, df, function(ix) !ix)
}

split_tt <- function(n, p_train) {
  n_train <- trunc(n * p_train)
  n_test <- n - n_train
  sample(c(rep(T, n_train), rep(F, n_test)))
}
