# meteo_station_heure_par_heure --------


meteo_station_heure_par_heure <- function(station_id = "07156",
                                          from = Sys.Date() - lubridate::days(1),
                                          to   = Sys.Date(),
                                          interactive_limit = 12,
                                          output = c("table", "data", "raw")) {
  
  output <- match.arg(output)
  
  raw <- tibble(date = seq.Date(from = from, to = to, by = "day")) %>%
    
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
      df$Direction <- as.character(df$Direction)
      df
    })) %>%
    tidyr::unnest(data) %>%
    rename(Heure_UTC = 3)
  
  # 👉 si l'utilisateur veut les données raw
  if (output == "raw") {
    return(raw)
  }
  
   data <- raw %>%
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
                                    output = c("table", "data", "raw")) {
  
  output <- match.arg(output)
  
  stopifnot(n_days > 0)
  
  today <- Sys.Date()
  start_date <- today - n_days + 1
  
  months_needed <- seq(lubridate::floor_date(start_date, "month"),
                       lubridate::floor_date(today, "month"),
                       by = "1 month")
  
  raw <- purrr::map_dfr(months_needed, \(d) {
    
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
  })
  
  
  # 👉 si l'utilisateur veut les données raw
  if (output == "raw") {
    return(raw)
  }
  
  data <- raw %>%
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





# Graphiques --------

suppressMessages({library(ggplot2); library(patchwork)})

meteo_temp_pal <- c("#313695", "#4575b4", "#74add1", "#abd9e9", "#fee090",
                    "#fdae61", "#f46d43", "#d73027", "#a50026")

meteo_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          plot.title.position = "plot", legend.position = "top",
          plot.title = element_text(face = "bold"))
}

# "Tr." (traces) -> 0.05 mm ; "0.0/1h" -> 0
meteo_num <- function(x) {
  suppressWarnings(readr::parse_number(ifelse(grepl("^Tr", x), "0.05", as.character(x))))
}

# meteo_daily : données journalières de plusieurs stations, format long --------

meteo_daily <- function(stations = c("07156" = "Paris"), n_days = 30) {
  purrr::imap_dfr(stations, \(nm, id)
    suppressWarnings(suppressMessages(meteo_station_last_days(id, n_days, output = "raw"))) |>
      dplyr::filter(date >= Sys.Date() - n_days + 1, date <= Sys.Date()) |>
      dplyr::transmute(
        station = nm, date,
        tmin = Temperature_mini, tmax = Temperature_maxi, tmoy = Temperature_moy,
        pluie = meteo_num(Precipitations),
        soleil = lubridate::period_to_seconds(lubridate::hm(sub("h", ":", Ensoleillement))) / 3600
      ) |>
      dplyr::filter(!is.na(tmax))) |>
    dplyr::mutate(station = factor(station, unname(stations)))
}

# meteo_plot_heatmap : jours (lignes) × stations (colonnes) --------

meteo_plot_heatmap <- function(daily, var = "tmax", rain_threshold = 1) {
  daily <- dplyr::mutate(daily, v = .data[[var]])
  lib <- c(tmax = "maximales", tmin = "minimales", tmoy = "moyennes")[[var]]
  ggplot(daily, aes(station, date, fill = v)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.0f°", v),
                  color = dplyr::if_else(v > 27 | v < 5, "white", "grey15")), size = 3.4) +
    geom_text(aes(label = ifelse(pluie >= rain_threshold, "💧", "")), nudge_x = 0.33, size = 2.6) +
    scale_color_identity() +
    scale_fill_gradientn(colours = meteo_temp_pal, limits = c(-5, 35),
                         oob = scales::squish, guide = "none") +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_date(date_labels = "%a %d",
                 breaks = seq(max(daily$date), min(daily$date), by = "-1 day"),
                 expand = c(0, 0)) +
    labs(title = paste0("Températures ", lib, " (°C)"),
         subtitle = paste0("💧 pluie ≥ ", rain_threshold, " mm"), x = NULL, y = NULL) +
    meteo_theme() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold"))
}

# meteo_plot_lines : température moyenne journalière, une courbe par station --------

meteo_plot_lines <- function(daily, var = "tmoy") {
  daily <- daily |> dplyr::mutate(v = .data[[var]]) |> dplyr::filter(!is.na(v))
  last <- daily |> dplyr::group_by(station) |> dplyr::filter(date == max(date))
  cols <- rep(c("#1f4e9c", "#6fa8dc", "#7f7f7f", "#e8743b", "#2ca58d", "#9b59b6", "#c0392b"),
              length.out = nlevels(daily$station))
  lib <- c(tmax = "maximale", tmin = "minimale", tmoy = "moyenne")[[var]]
  ggplot(daily, aes(date, v, color = station)) +
    geom_line(linewidth = 1) +
    geom_point(data = last, size = 2) +
    ggrepel::geom_text_repel(data = last, aes(label = station), hjust = 0, nudge_x = 0.6,
                             direction = "y", segment.color = NA, size = 3.6, fontface = "bold") +
    scale_color_manual(values = cols, guide = "none") +
    scale_x_date(date_labels = "%d/%m", expand = expansion(mult = c(0.02, 0.18))) +
    scale_y_continuous(labels = \(x) paste0(x, "°")) +
    labs(title = paste("Température", lib, "journalière"), x = NULL, y = NULL) +
    meteo_theme()
}

# meteo_plot_days : une station, barres min → max + pluie + soleil --------

meteo_plot_days <- function(daily, title = NULL) {
  d <- dplyr::filter(daily, !is.na(tmin))
  top <- ggplot(d, aes(date)) +
    geom_linerange(aes(ymin = tmin, ymax = tmax, color = tmoy), linewidth = 3.2, lineend = "round") +
    geom_text(aes(y = tmax, label = sprintf("%.0f", tmax)), vjust = -0.7, size = 2.6, color = "grey30") +
    geom_text(aes(y = tmin, label = sprintf("%.0f", tmin)), vjust = 1.6, size = 2.6, color = "grey50") +
    scale_color_gradientn(colours = meteo_temp_pal, limits = c(-5, 30),
                          oob = scales::squish, guide = "none") +
    scale_y_continuous(labels = \(x) paste0(x, "°"), expand = expansion(mult = 0.12)) +
    scale_x_date(date_labels = "%d/%m", expand = c(0.01, 0)) +
    labs(title = title, subtitle = "Températures min → max", x = NULL, y = NULL) +
    meteo_theme() + theme(axis.text.x = element_blank())
  bot <- d |>
    tidyr::pivot_longer(c(pluie, soleil)) |>
    dplyr::group_by(name) |> dplyr::filter(any(!is.na(value))) |> dplyr::ungroup() |>
    dplyr::mutate(name = dplyr::recode(name, pluie = "Pluie (mm)", soleil = "Soleil (h)")) |>
    ggplot(aes(date, value, fill = name)) +
    geom_col(width = 0.75, na.rm = TRUE) +
    facet_wrap(~name, ncol = 1, scales = "free_y", strip.position = "left") +
    scale_fill_manual(values = c("#3a86c8", "#f2b705"), guide = "none") +
    scale_x_date(date_labels = "%d/%m", expand = c(0.01, 0)) +
    labs(x = NULL, y = NULL) +
    meteo_theme() +
    theme(strip.placement = "outside", strip.text = element_text(face = "bold"))
  top / bot + plot_layout(heights = c(2, 1.3))
}

# meteo_plot_hours : une station, 48 h : température, vent/rafales, pluie --------

meteo_plot_hours <- function(station_id = "07156", title = NULL, from = Sys.Date() - 2) {
  h <- suppressWarnings(suppressMessages(
    meteo_station_heure_par_heure(station_id, from = from, output = "raw"))) |>
    dplyr::mutate(t = as.POSIXct(paste(date, Heure_legale), format = "%Y-%m-%d %Hh%M",
                                 tz = "Europe/Paris"),
                  vent = meteo_num(Vent_moyen), rafales = meteo_num(Vent_rafales),
                  pluie = meteo_num(Precipitations)) |>
    dplyr::filter(!is.na(Temperature), !is.na(t))
  nights <- tibble::tibble(d = seq(min(h$date) - 1, max(h$date), by = "day")) |>
    dplyr::transmute(xmin = as.POSIXct(paste(d, "20:00"), tz = "Europe/Paris"),
                     xmax = as.POSIXct(paste(d + 1, "07:00"), tz = "Europe/Paris"))
  base <- list(
    geom_rect(data = nights, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey93"),
    scale_x_datetime(date_labels = "%a %Hh", date_breaks = "12 hours",
                     limits = range(h$t), expand = c(0, 0)),
    meteo_theme()
  )
  ext <- dplyr::slice(h, c(which.max(Temperature), which.min(Temperature)))
  pt <- ggplot(h, aes(t, Temperature)) + base +
    geom_area(fill = "#f46d43", alpha = 0.12) +
    geom_line(aes(color = Temperature), linewidth = 1.3) +
    geom_point(data = ext, size = 2) +
    geom_text(data = ext, aes(label = sprintf("%.1f°", Temperature)),
              vjust = -0.9, hjust = "inward", size = 3.4, fontface = "bold") +
    scale_color_gradientn(colours = meteo_temp_pal, limits = c(-5, 30),
                          oob = scales::squish, guide = "none") +
    coord_cartesian(ylim = c(min(h$Temperature) - 2, max(h$Temperature) + 2)) +
    scale_y_continuous(labels = \(x) paste0(x, "°")) +
    labs(title = title, subtitle = "Température (nuit en gris)", x = NULL, y = NULL) +
    theme(axis.text.x = element_blank())
  pv <- ggplot(h, aes(t)) + base +
    geom_ribbon(aes(ymin = vent, ymax = pmax(vent, rafales, na.rm = TRUE)),
                fill = "#2ca58d", alpha = 0.2) +
    geom_line(aes(y = vent), color = "#2ca58d", linewidth = 1) +
    labs(subtitle = "Vent moyen et rafales (km/h)", x = NULL, y = NULL) +
    theme(axis.text.x = element_blank())
  pp <- ggplot(h, aes(t, pluie)) + base +
    geom_col(fill = "#3a86c8", width = 3000, na.rm = TRUE) +
    scale_y_continuous(limits = c(0, max(1, max(h$pluie, na.rm = TRUE)))) +
    labs(subtitle = "Pluie (mm/h)", x = NULL, y = NULL)
  pt / pv / pp + plot_layout(heights = c(2.2, 1, 0.8))
}
