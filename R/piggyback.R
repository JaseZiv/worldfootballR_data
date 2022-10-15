library(purrr)
library(readr)
library(piggyback)

write_csv2 <- partial(
  write_csv,
  na = "",
  ... = 
)

worldfootballr_repo <- "JaseZiv/worldfootballR_data"
write_worldfootballr <- function(x, name, tag, ext = c("rds", "csv")) {
  ext <- match.arg(ext)
  dir <- tempdir(check = TRUE)
  basename <- sprintf("%s.%s", name, ext)
  path <- file.path(dir, basename)
  f <- switch(
    ext,
    "rds" = write_rds,
    "csv" = write_csv2
  )
  f(x, path)
  pb_upload(
    path,
    repo = worldfootballr_repo,
    tag = tag
  )
}

write_worldfootballr_rds_and_csv <- function(x, name, tag) {
  walk(
    c("csv", "rds"),
    ~write_worldfootballr(
      x = x,
      name = name,
      tag = tag,
      ext = .x
    )
  )
}

read_worldfootballr_release_rds <- function(name, tag) {
  path <- sprintf("https://github.com/%s/releases/download/%s/%s.rds", worldfootballr_repo, tag, name)
  readRDS(url(path))
}
