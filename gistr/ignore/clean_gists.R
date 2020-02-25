pages <- length(seq(0, 3200, by = 100))
out <- list()
for (i in seq_len(pages)) {
  out[[i]] <- gists(what = "minepublic", per_page = 100, page = i)
}
length(out)
out2 <- unlist(out, FALSE)

library(dplyr)
library(tibble)
`%||%` <- function (x, y) if (is.null(x) || length(x) == 0) y else x


# gists with `description` "gist gist gist"
df <- tbl_df(bind_rows(lapply(out2, function(z) 
  data.frame(id = z$id %||% NA_character_, title = z$description %||% NA_character_, 
    stringsAsFactors=FALSE))))
df
todel <- filter(df, title == "gist gist gist") %>% .$id
w <- lapply(todel, delete)

# for file based lookups
df2 <- tbl_df(bind_rows(lapply(out2, function(z) 
  data.frame(id = z$id %||% NA_character_, file_name = paste(names(z$files), collapse = ",") %||% NA_character_, 
    stringsAsFactors=FALSE))))
df2

# gist with `file` names containing "pleiades" and ending with ".geojson"
todel <- filter(df2, grepl("pleiades", file_name)) %>% .$id
w <- lapply(todel, delete)

# gist with `file` names containing "file" then numbers and letters then ".geojson"
todel <- filter(df2, grepl("file[0-9A-Za-z]+\\.geojson", file_name)) %>% .$id
w <- lapply(todel, delete)

# gist with `file` names == "myfile.geojson" 
todel <- filter(df2, grepl("myfile\\.geojson", file_name)) %>% .$id
w <- lapply(todel, delete)

# gist with `file` names containing "file" then numbers and letters then ".md"
todel <- filter(df2, grepl("file[0-9A-Za-z]+\\.md", file_name)) %>% .$id
w <- lapply(todel, delete)


pages <- length(seq(0, 700, by = 100))
out <- list()
for (i in seq_len(pages)) {
  out[[i]] <- gists(what = "minepublic", per_page = 100, page = i)
}
length(out)
out2 <- unlist(out, FALSE)
df2 <- tbl_df(bind_rows(lapply(out2, function(z) 
  data.frame(id = z$id %||% NA_character_, file_name = paste(names(z$files), collapse = ",") %||% NA_character_, 
    stringsAsFactors=FALSE))))
df2

# gist with `file` names matching various things
mm <- c("file\\.txt", "example1\\.md", "stuff\\.md", "example1\\.R", 
  "stuff\\.Rmd", "plots.md", "my_cool_code\\.R", "hello_world\\.md", 
  "foobar\\.txt", "plots_imgur\\.md", "file[0-9A-Za-z]+", "code\\.R",
  "iris\\.csv", "\\.gistr", "mtcars\\.csv", "file\\.png", 
  "ggplot_imgur\\.md", "my_markdown\\.md", "rnw_example\\.Rnw",
  "rmarkdown_eg\\.md")
todel <- filter(df2, grepl(paste(mm, collapse = "|"), file_name)) %>% .$id
w <- lapply(todel, delete)
