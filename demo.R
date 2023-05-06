library(httr)
library(rvest)
library(data.table)
library(tidyr)
library(dplyr)

city <- "vigo"


##### Step 1: make the POST request to get the content of the first table with
##### some info but not everything

query <- paste0("cadena=", city, "&tb=SPH_MATCH_ALL&rangoa=1826%2C1966&indexes%5B%5D=privilegios&indexes%5B%5D=patentes&indexes%5B%5D=patentes_upm&indexes%5B%5D=marcas")

x <- POST(
  "http://historico.oepm.es/logica/search_lib.php",
  add_headers(
    "accept" = "*/*",
    "accept-language" = "en-GB,en-US;q=0.9,en;q=0.8",
    "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
    "x-requested-with" = "XMLHttpRequest"
  ),
  body = query,
  verbose()
)


##### Step 2: from this first table, extract the parameters to perform the 
##### GET requests to access the more detailed tables (these parameters are
##### attributes of the "+" button in the first table)

obtain_GET_params <- function(x) {
  list_attrs <- content(x, "parsed") |> 
    html_nodes("td > a") |> 
    html_attrs()
  
  info <- lapply(list_attrs, function(x) {
    out <- x[names(x) %in% c("data-id", "data-db")]
    if (length(out) == 0) return(NULL)
    data.frame(id = out[1], db = out[2])
  })
  
  info <- Filter(Negate(is.null), info)
  rbindlist(info)
}

GET_inputs <- obtain_GET_params(x)


##### Step 3: loop through all parameters to perform the GET requests and obtain
##### all the detailed tables

GET_outputs <- list()
for (i in 1:nrow(GET_inputs)) {
  message(paste("GET request for row", i))
  GET_outputs[[i]] <- read_html(
    paste0("http://historico.oepm.es/logica/ficha.php?id=", 
           GET_inputs[[1]][i], "&db=", GET_inputs[[2]][i])
  ) |> 
    html_table()
}


##### Step 4: clean and put this in a unique data.frame

foo <- GET_outputs
for (i in seq_along(foo)) {
  if (length(foo[[i]]) > 1) {
    foo[[i]] <- rbindlist(foo[[i]])
  } else {
    foo[[i]] <- foo[[i]][[1]]
  }
  
  foo[[i]] <- foo[[i]] |> 
    pivot_wider(
      names_from = "X1",
      values_from = "X2"
    )
}

final <- as_tibble(rbindlist(foo, fill = TRUE))
