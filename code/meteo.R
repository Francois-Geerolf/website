# meteo_station_heure_par_heure --------


meteo_station_heure_par_heure <- function(station_id = "07156",
                                          from = Sys.Date() - lubridate::days(1),
                                          to   = Sys.Date(),
                                          interactive_limit = 12,
                                          output = c("table", "data")) {
  
  output <- match.arg(output)
  
  data <- tibble(date = seq.Date(from = from, to = to, by = "day")) %>%
    
    mutate(
      url = paste0(
        "https://www.meteo60.fr/stations-releves/station-jour-csv?station_id=-",
        station_id,
        "&date=",
        format(date, "%d/%m/%Y")
      )
    ) %>%
    
    mutate(data = purrr::map(url, readr::read_delim, show_col_types = FALSE)) %>%
    
    mutate(data = purrr::map(data, \(df) {
      df$Vent_moyen <- as.character(df$Vent_moyen)
      df$Nebulosite <- as.character(df$Nebulosite)
      df
    })) %>%
    
    tidyr::unnest(data) %>%
    select(-url, -...15) %>%
    arrange(desc(date), desc(Heure_UTC)) %>%
    
    rename(
      heure = Heure_legale,
      T = Temperature,
      Vent = Vent_moyen,
      Humidité = Humidite_relative
    ) %>%
    
    mutate(
      heure = stringr::str_replace(heure, "^([0-9]{1,2}).*$", "\\1h"),
      
      Temps = tidyr::replace_na(as.character(Temps), ""),
      
      icon = dplyr::case_when(
        stringr::str_detect(Temps, regex("soleil|clair", TRUE)) ~ "☀️",
        stringr::str_detect(Temps, regex("nuage", TRUE)) ~ "⛅",
        stringr::str_detect(Temps, regex("pluie|averse|bruine", TRUE)) ~ "🌧️",
        stringr::str_detect(Temps, regex("orage", TRUE)) ~ "⛈️",
        stringr::str_detect(Temps, regex("brouillard|brume", TRUE)) ~ "🌫️",
        stringr::str_detect(Temps, regex("neige", TRUE)) ~ "❄️",
        TRUE ~ ""
      ),
      Temps = paste(icon, Temps)
    ) %>%
    
    select(
      date,
      heure,
      T,
      Temps,
      Vent,
      Vent_rafales,
      Direction,
      Humidité,
      everything()
    ) %>%
    select(
      -icon, -Heure_UTC
    ) %>%
    mutate(date = format(date, "%d/%m"))
  
  # 👉 si l'utilisateur veut les données brutes
  if (output == "data") {
    return(data)
  }
  
  interactive_mode <- nrow(data) > interactive_limit
  
  gt_tbl <- data %>%
    gt::gt() %>%
    
    gt::fmt(
      columns = T,
      fns = function(x) sprintf("%.1f°", x)
    ) %>%
    
    gt::data_color(
      columns = T,
      colors = scales::col_numeric(
        palette = c("#2c7bb6", "#abd9e9", "#66bd63", "#fdae61", "#d7191c"),
        domain = NULL
      )
    ) %>%
    
    gt::cols_label(T = "T°",
                   heure = "h",
                   Vent_rafales = "Rafales") %>%
    
    gt::cols_align(
      "center",
      everything()
    ) %>%
    
    gt::cols_width(
      -c(Temps, Precipitations, Variation_pression,Pression_mer, date) ~ gt::px(60),
      c(Pression_mer) ~ gt::px(80),
      c(Variation_pression) ~ gt::px(90),
      c(date) ~ gt::px(60),
      c(Temps) ~ gt::px(250),
      c(Precipitations) ~ gt::px(230)
    ) %>%
    
    gt::tab_options(
      table.width = gt::pct(100),
      data_row.padding = gt::px(4)
    ) %>%
    
    gt::tab_footnote(
      footnote = gt::md(
        paste0(
          "Généré le ",
          format(Sys.time(), "%A %d %B %Y, %Hh%M"),
          " • Source : https://www.infoclimat.fr"
        )
      )
    )
  
  if (interactive_mode) {
    gt_tbl <- gt_tbl |>
      gt::opt_interactive(
        use_search = FALSE,
        use_pagination = TRUE,
        use_compact_mode = TRUE,
        page_size_default = interactive_limit
      )
  }
  
  gt_tbl
}

# meteo_station_last_days --------

meteo_station_last_days <- function(station_id = "07156",
                                    n_days = 30,
                                    sun_threshold = 6,
                                    interactive_limit = 15,
                                    output = c("table", "data")) {
  
  output <- match.arg(output)
  
  stopifnot(n_days > 0)
  
  today <- Sys.Date()
  start_date <- today - n_days + 1
  
  months_needed <- seq(lubridate::floor_date(start_date, "month"),
                       lubridate::floor_date(today, "month"),
                       by = "1 month")
  
  data <- purrr::map_dfr(months_needed, \(d) {
    
    url <- paste0(
      "https://www.meteo60.fr/stations-releves/station-mois-csv?station_id=",
      station_id,
      "&mois=", lubridate::month(d),
      "&annee=", lubridate::year(d) - 2000
    )
    
    readr::read_delim(url, show_col_types = FALSE) %>%
      tibble::as_tibble() %>%
      dplyr::select(-7) %>%
      dplyr::mutate(
        jour = readr::parse_number(Jour),
        date = as.Date(ISOdate(lubridate::year(d),
                               lubridate::month(d),
                               jour)),
        Temperature_mini = as.numeric(Temperature_mini),
        Temperature_moy = as.numeric(Temperature_moy),
        Temperature_maxi = as.numeric(Temperature_maxi),
        Precipitations = as.character(Precipitations)
      )
  }) %>%
    dplyr::filter(date >= start_date & date <= today) %>%
    dplyr::arrange(desc(date)) %>%
    
    dplyr::mutate(
      # 📅 Jour compact
      date = format(date, "%d/%m", locale = "fr_FR.UTF-8"),
      
      # 🌡️ noms courts
      `T° min` = Temperature_mini,
      `T° moy` = Temperature_moy,
      `T° max` = Temperature_maxi,
      
      # ☔ pluie
      pluie_icon = ifelse(!(Precipitations %in% c("0.0", "0.0*", "0*", "0")), "☔", ""),
      
      # ☀️ ensoleillement élevé
      sun_minutes = lubridate::period_to_seconds(lubridate::hm(gsub("h", ":", Ensoleillement))) / 60,
      soleil_icon = dplyr::if_else(sun_minutes >= sun_threshold * 60, "☀️", "", missing = ""),
      
      Meteo = stringr::str_trim(paste(pluie_icon, soleil_icon))
    ) %>%
    
    dplyr::select(
      date,
      `T° min`,
      `T° moy`,
      `T° max`,
      Meteo,
      Precipitations,
      Ensoleillement
    )
  
  # 👉 sortie données
  if (output == "data") {
    return(data)
  }
  
  interactive_mode <- nrow(data) > interactive_limit
  
  gt_tbl <- data %>%
    gt::gt() %>%
    
    gt::cols_align(
      "center",
      everything()
    ) %>%
    
    gt::data_color(
      columns = `T° moy`,
      colors = scales::col_numeric(
        palette = c("#2c7bb6", "#abd9e9", "#66bd63", "#fdae61", "#d7191c"),
        domain = NULL
      )
    ) %>%
    
    gt::cols_label(
      Meteo = "",
      Precipitations = "Pluie (mm)",
      Ensoleillement = "Soleil"
    ) %>%
    
    gt::tab_options(
      table.width = gt::pct(100),
      data_row.padding = gt::px(4)
    ) %>%
    
    gt::cols_width(
      -c(Precipitations, Ensoleillement, date, Meteo) ~ gt::px(55),
      c(Meteo) ~ gt::px(65),
      c(date) ~ gt::px(60),
      c(Precipitations, Ensoleillement) ~ gt::px(80)
    ) %>%
    
    gt::tab_footnote(
      footnote = gt::md(
        paste0("Généré le ",
               format(Sys.time(), "%A %d %B %Y, %Hh%M"),
               " • Source : https://www.infoclimat.fr"
        )
      )
    )
  
  if (interactive_mode) {
    gt_tbl <- gt_tbl |>
      gt::opt_interactive(
        use_search = FALSE,
        use_pagination = TRUE,
        use_compact_mode = TRUE,
        page_size_default = interactive_limit
      )
  }
  
  gt_tbl
}



