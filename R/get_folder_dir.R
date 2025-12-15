get_folder_dir <- function(server = "P", folder = "41001581_egenutvikling_anders_kolstad/data/") {
  server <- toupper(server)
  if (!server %in% c("P", "R")) {
    stop("server must be 'P' or 'R'")
  }
  if (.Platform$OS.type == "windows") {
    base <- switch(server,
                   P = "P:/",
                   R = "R:/")
  } else {
    base <- switch(server,
                   P = "/data/P-Prosjekter2/",
                   R = "/data/R/")
  } 
  base <- paste0(base, folder)
  return(base)
}

