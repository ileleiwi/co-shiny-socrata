pkgs <- c("shiny","httr","jsonlite","dplyr","tibble","purrr","glue","yaml","lubridate","ggplot2","DT")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
