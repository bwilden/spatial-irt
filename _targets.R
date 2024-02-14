
library(targets)

tar_option_set(
  packages = c("tibble")
)

tar_source()

list(
  tar_target(
    ces2022_file,
    here::here("data-raw", "CCES22_Common_OUTPUT_vv_topost.csv"),
    format = "file"
  ),
  tar_target(
    ces2022,
    readr::read_csv(ces2022_file)
  )
  

)
