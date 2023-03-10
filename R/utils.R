library(patchwork)
library(tidyverse)
library(ggwordcloud)

theme_capro <- function(){
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#2A2F36"),
    plot.background = element_rect(fill = "#2A2F36"), 
    text = element_text(colour = "#c0c2c4"),
    axis.text = element_text(colour = "#c0c2c4"),
    panel.grid = element_line(colour = "#20242b")
  )
}

theme_set(theme_capro())
PAL <- c("#CC4C4C", "#85bfbf", "#6fd66f", "#6f6fd6", "#d66fd6", "#c0c2c4", "#a285bf", "#bfa285" )
PALC <- colorRampPalette(c("#CC4C4C", "#c0c2c4", "#85bfbf"))

scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = PAL)
}

scale_colour_continuous <- function(...) {
  scale_colour_gradientn(colours = PALC(100), ...)
}

scale_fill_continuous <- function(...) {
  scale_fill_gradientn(colours = PALC(100), ...)
}


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