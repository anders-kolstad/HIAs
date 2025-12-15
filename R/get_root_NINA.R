get_root_NINA <- function(server = "P") {
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
  return(base)
}

