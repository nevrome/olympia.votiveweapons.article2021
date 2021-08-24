code_files_raw <- list.files("code", full.names = TRUE)
code_files <- code_files_raw[!grepl("00A|00B|00C|00D", code_files_raw)]

purrr::walk(
  code_files,
  function(y) {
    message("\n###### ", y, " ######\n")
    source(y)
    rm(list = ls())
  }
)
