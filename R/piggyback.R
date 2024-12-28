library(purrr)
library(readr)
library(piggyback)

write_csv2 <- purrr::partial(
  readr::write_csv,
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
    "rds" = readr::write_rds,
    "csv" = write_csv2
  )
  f(x, path)
  piggyback::pb_upload(
    path,
    repo = worldfootballr_repo,
    tag = tag
  )
}

write_worldfootballr_rds_and_csv <- function(x, name, tag) {
  purrr::walk(
    c("rds", "csv"),
    ~write_worldfootballr(
      x = x,
      name = name,
      tag = tag,
      ext = .x
    )
  )
}

read_worldfootballr_rds <- function(name, tag) {
  path <- sprintf("https://github.com/%s/releases/download/%s/%s.rds", worldfootballr_repo, tag, name)
  readRDS(url(path))
}

read_worldfootballr_csv <- function(name, tag) {
  path <- sprintf("https://github.com/%s/releases/download/%s/%s.csv", worldfootballr_repo, tag, name)
  read.csv(path)
}

safely_read_worldfootballr_rds <- purrr::safely(read_worldfootballr_rds)

read_worldfootballr <- function(name, tag) {
  res <- safely_read_worldfootballr_rds(name, tag)
  if (is.null(res$error)) {
    return(res$value)
  }
  message(
    sprintf(
      'Missing RDS file at `name = "%s"` (`tag: "%s"`).\nTrying to read from the CSV.',
      name, 
      tag
    )
  )
  read_worldfootballr_csv(name, tag)
}
