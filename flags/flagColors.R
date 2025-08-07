flagColors <- function(country){
  temp <- flagColors_json[grepl(country, flagColors_json)][[1]]$colors %>%
    enframe %>%
    unnest %>%
    group_by(name) %>%
    mutate(variable = c("hex", "percent")) %>%
    ungroup %>%
    spread(variable, value) %>%
    mutate(hex = unlist(hex),
           percent = unlist(percent)) %>%
    as_tibble %>%
    select(-name) %>%
    mutate(Flag = gsub(" ", "-", str_to_lower(country)),
           Flag = paste0('<img src="../../icon/flag/vsmall/', Flag, '.png" alt="Flag">')) %>%
    select(Flag, everything())
    
  attr(temp, "groups") <- NULL
  return(temp)
}
