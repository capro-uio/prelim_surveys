library(patchwork)
library(tidyverse)
library(ggwordcloud)

theme_set(theme_minimal())
PAL <- see::palette_okabeito()(3)

get_data <- function(form, online = FALSE) {
  metaf <- here::here("data", paste0(form, "_meta.json"))
  dataf <- here::here("data", paste0(form, "_data.tsv"))
  if(online){
    nettskjemar::nettskjema_get_data(form) |>
      type_convert() |> 
      mutate(across(where(is.character),
                          ~ gsub("\\r|\\n|\\t", "", .x))) |> 
      select(-any_of(c("email", "name"))) |> 
      write_tsv(dataf, escape = "backslash")
    jsonlite::write_json(nettskjemar::nettskjema_get_meta(form), 
                         metaf,
                         pretty = TRUE,
                         auto_unbox = TRUE)
  }
  return(list(
    dt = read_tsv(dataf),
    meta = jsonlite::read_json(metaf)
  ))
}

percent <- function(x){
  scales::label_percent(accuracy = 2)(x)
}

thousand <- function(x){
  formatC(x, format="f", big.mark = " ", digits=0)
}

ggbar <- function(data, col){
  labnm <- rlang::quo_name(enquo(col))
  labnm <- str_replace_all(labnm, "_", " ")
  
  data |> 
    separate_rows({{col}}) |> 
    group_by({{col}}) |> 
    tally() |> 
    arrange(n) |> 
    mutate(
      {{col}} := fct_inorder({{col}}),
      pc = n/sum(n),
      pclab = percent(pc)
    ) |> 
    
    ggplot(aes(y = {{col}} , x = n, label = pclab)) + 
    geom_histogram(stat = "identity",
                   aes(fill = pc,
                       colour = pc)) +
    scale_fill_gradientn(colours = PAL, guide = "none") +
    scale_colour_gradientn(colours = PAL, guide = "none") +
    geom_label(alpha = .8) +
    labs(
      title = tools::toTitleCase(labnm),
      x = "Count",
      y = ""
    ) +
    coord_cartesian(clip = "off")
}

dateform <- function(date){
  format(as.Date(date), "%B %d, %Y")
}