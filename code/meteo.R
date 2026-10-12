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
  purrr::imap_dfr(stations, \(nm, id) {
    src <- meteo_stations$source[meteo_stations$id == id]
    d <- if (length(src) && src == "iem") meteo_daily_iem(id, n_days) else meteo_daily_meteo60(id, n_days)
    d |>
      dplyr::filter(date >= Sys.Date() - n_days + 1, date <= Sys.Date(), !is.na(tmax)) |>
      dplyr::mutate(station = nm, .before = 1)
  }) |>
    dplyr::mutate(station = factor(station, unname(stations)))
}

meteo_daily_meteo60 <- function(id, n_days) {
  suppressWarnings(suppressMessages(meteo_station_last_days(id, n_days, output = "raw"))) |>
    dplyr::transmute(
      date,
      tmin = Temperature_mini, tmax = Temperature_maxi, tmoy = Temperature_moy,
      pluie = meteo_num(Precipitations),
      soleil = suppressWarnings(
        lubridate::period_to_seconds(lubridate::hm(sub("h", ":", Ensoleillement))) / 3600)
    )
}

# Stations hors de France : Iowa Environmental Mesonet (relevés ASOS/METAR,
# sans clé). Résumés journaliers en °F / pouces -> °C / mm ; journée locale.
meteo_daily_iem <- function(id, n_days, network = "CA_ASOS") {
  tz <- meteo_stations$tz[meteo_stations$id == id]
  today <- as.Date(lubridate::with_tz(Sys.time(), tz))
  years <- unique(lubridate::year(c(today - n_days, today)))
  purrr::map_dfr(years, \(y) {
    j <- jsonlite::fromJSON(paste0(
      "https://mesonet.agron.iastate.edu/api/1/daily.json?station=", id,
      "&network=", network, "&year=", y))
    tibble::as_tibble(j$data)
  }) |>
    dplyr::transmute(
      date = as.Date(date),
      tmin = round((min_tmpf - 32) * 5 / 9, 1),
      tmax = round((max_tmpf - 32) * 5 / 9, 1),
      tmoy = round((tmin + tmax) / 2, 1),
      pluie = round(precip * 25.4, 1),
      soleil = NA_real_
    ) |>
    dplyr::filter(date < today)   # journée en cours incomplète
}

# meteo_plot_heatmap : jours (lignes) × stations (colonnes) --------

meteo_plot_heatmap <- function(daily, var = "tmax", rain_threshold = 1) {
  daily <- dplyr::mutate(daily, v = .data[[var]])
  lib <- c(tmax = "maximales", tmin = "minimales", tmoy = "moyennes")[[var]]
  ggplot(daily, aes(station, date, fill = v)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.0f°", v),
                  color = dplyr::if_else(v > 27 | v < 5, "white", "grey15")), size = 3.4) +
    geom_point(data = dplyr::filter(daily, pluie >= rain_threshold),
               position = position_nudge(x = 0.36), color = "#1f5fa8", size = 1.6) +
    scale_color_identity() +
    scale_fill_gradientn(colours = meteo_temp_pal, limits = c(-5, 35),
                         oob = scales::squish, guide = "none") +
    scale_x_discrete(position = "top", expand = c(0, 0),
                     labels = \(x) stringr::str_wrap(x, 8)) +
    scale_y_date(date_labels = "%a %d",
                 breaks = seq(max(daily$date), min(daily$date), by = "-1 day"),
                 expand = c(0, 0)) +
    labs(title = paste0("Températures ", lib, " (°C)"),
         subtitle = paste0("<span style='color:#1f5fa8'>●</span> pluie ≥ ", rain_threshold, " mm"), x = NULL, y = NULL) +
    meteo_theme() +
    theme(panel.grid = element_blank(), plot.subtitle = ggtext::element_markdown(),
          axis.text.x = element_text(face = "bold", size = 9.5, lineheight = 0.9))
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

# meteo_stations : stations affichées (index + une page par station) --------

meteo_stations <- tibble::tribble(
  ~id,        ~slug,              ~court,        ~nom,                  ~lieu,                           ~source,   ~tz,
  "07156",    "paris-montsouris", "Paris",       "Paris-Montsouris",    "Paris 14e (75) · 75 m",         "meteo60", "Europe/Paris",
  "07116",    "ile-de-batz",      "Île de Batz", "Île de Batz",         "Roscoff / Santec (29) · 32 m",  "meteo60", "Europe/Paris",
  "07650",    "marseille",        "Marseille",   "Marseille-Marignane", "Bouches-du-Rhône (13) · 9 m",   "meteo60", "Europe/Paris",
  "74258002", "samoens",          "Samoëns",     "Samoëns",             "Haute-Savoie (74)",             "meteo60", "Europe/Paris",
  "LAX",      "los-angeles",      "Los Angeles", "Los Angeles (LAX)",   "Californie, États-Unis · 30 m", "iem",     "America/Los_Angeles"
)

meteo_station_info <- function(slug) {
  s <- dplyr::filter(meteo_stations, slug == !!slug)
  stopifnot(nrow(s) == 1)
  as.list(s)
}

# meteo_hourly : relevés horaires propres (heure locale calculée depuis l'UTC) --------

meteo_hourly <- function(station_id = "07156", from = Sys.Date() - 2, to = Sys.Date()) {
  src <- meteo_stations$source[meteo_stations$id == station_id]
  h <- if (length(src) && src == "iem") meteo_hourly_iem(station_id, from, to)
       else meteo_hourly_meteo60(station_id, from, to)
  h |>
    dplyr::filter(!is.na(t), !is.na(temp)) |>
    dplyr::distinct(t, .keep_all = TRUE) |>
    dplyr::arrange(t)
}

meteo_hourly_meteo60 <- function(station_id, from, to) {
  suppressWarnings(suppressMessages(
    meteo_station_heure_par_heure(station_id, from = from, to = to, output = "raw"))) |>
    dplyr::transmute(
      t = lubridate::with_tz(as.POSIXct(paste(date, Heure_UTC), format = "%Y-%m-%d %Hh%M",
                                        tz = "UTC"), "Europe/Paris"),
      temp = meteo_num(Temperature), rosee = meteo_num(Point_rosee),
      humidite = meteo_num(Humidite_relative),
      vent = meteo_num(Vent_moyen), rafales = meteo_num(Vent_rafales),
      direction = meteo_num(Direction),
      pression = meteo_num(Pression_mer),
      # à 06h UTC le CSV donne le cumul 24 h (« 65.5/24h ») : on ne garde que l'horaire
      pluie = dplyr::if_else(grepl("/1h", Precipitations), meteo_num(Precipitations), NA_real_)
    )
}

# METAR horaires (report_type=3 : relevés de routine). Vent en nœuds -> km/h.
meteo_hourly_iem <- function(station_id, from, to) {
  tz <- meteo_stations$tz[meteo_stations$id == station_id]
  url <- paste0(
    "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=", station_id,
    "&data=tmpc,dwpc,relh,sknt,gust,drct,mslp,p01m",
    "&year1=", format(from - 1, "%Y"), "&month1=", format(from - 1, "%m"), "&day1=", format(from - 1, "%d"),
    "&year2=", format(to + 1, "%Y"), "&month2=", format(to + 1, "%m"), "&day2=", format(to + 1, "%d"),
    "&tz=Etc/UTC&format=onlycomma&missing=empty&trace=0.0001&report_type=3")
  readr::read_csv(url, show_col_types = FALSE, col_types = readr::cols(.default = "c")) |>
    dplyr::transmute(
      t = lubridate::with_tz(as.POSIXct(valid, format = "%Y-%m-%d %H:%M", tz = "UTC"), tz),
      temp = as.numeric(tmpc), rosee = as.numeric(dwpc), humidite = as.numeric(relh),
      vent = as.numeric(sknt) * 1.852, rafales = as.numeric(gust) * 1.852,
      direction = as.numeric(drct), pression = as.numeric(mslp), pluie = as.numeric(p01m)
    ) |>
    dplyr::filter(t >= lubridate::force_tz(as.POSIXct(from), tz))
}

# meteo_plot_hours : une station, heure par heure --------
# detail = FALSE : température, vent/rafales, pluie (48 h, page d'accueil)
# detail = TRUE  : + point de rosée, humidité, pression (7 jours, page station)
# Les panneaux sans données (ex. pas d'anémomètre à Samoëns) sont omis.

meteo_plot_hours <- function(station_id = "07156", title = NULL, from = Sys.Date() - 2,
                             detail = FALSE, h = NULL) {
  if (is.null(h)) h <- meteo_hourly(station_id, from = from)
  tz <- attr(h$t, "tzone") %||% "Europe/Paris"
  ndays <- as.numeric(difftime(max(h$t), min(h$t), units = "days"))
  nights <- tibble::tibble(d = seq(as.Date(min(h$t), tz = tz) - 1, as.Date(max(h$t), tz = tz), by = "day")) |>
    dplyr::transmute(xmin = as.POSIXct(paste(d, "20:00"), tz = tz),
                     xmax = as.POSIXct(paste(d + 1, "07:00"), tz = tz))
  base <- list(
    geom_rect(data = nights, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey93"),
    if (ndays > 3) scale_x_datetime(date_labels = "%a %d", date_breaks = "1 day",
                                    limits = range(h$t), expand = c(0, 0), timezone = tz)
    else scale_x_datetime(date_labels = "%a %Hh", date_breaks = "12 hours",
                          limits = range(h$t), expand = c(0, 0), timezone = tz),
    meteo_theme()
  )
  no_x <- theme(axis.text.x = element_blank())
  has <- \(v) any(!is.na(h[[v]]))

  ext <- dplyr::slice(h, c(which.max(temp), which.min(temp)))
  pt <- ggplot(h, aes(t, temp)) + base +
    geom_area(fill = "#f46d43", alpha = 0.12) +
    { if (detail && has("rosee")) geom_line(aes(y = rosee), color = "#4575b4",
                                            linewidth = 0.6, linetype = "22", na.rm = TRUE) } +
    geom_line(aes(color = temp), linewidth = if (detail) 1 else 1.3) +
    geom_point(data = ext, size = 2) +
    geom_text(data = ext, aes(label = sprintf("%.1f°", temp)),
              vjust = -0.9, hjust = "inward", size = 3.4, fontface = "bold") +
    scale_color_gradientn(colours = meteo_temp_pal, limits = c(-5, 30),
                          oob = scales::squish, guide = "none") +
    coord_cartesian(ylim = range(c(h$temp, if (detail) h$rosee), na.rm = TRUE) + c(-2, 2)) +
    scale_y_continuous(labels = \(x) paste0(x, "°")) +
    labs(title = title,
         subtitle = if (detail && has("rosee")) "Température et point de rosée (pointillés) · nuit en gris"
                    else "Température (nuit en gris)",
         x = NULL, y = NULL) + no_x
  panels <- list(pt); heights <- 2.2

  if (detail && has("humidite")) {
    panels <- c(panels, list(
      ggplot(h, aes(t, humidite)) + base +
        geom_line(color = "#5e81ac", linewidth = 0.8, na.rm = TRUE) +
        scale_y_continuous(limits = c(NA, 100), labels = \(x) paste0(x, " %")) +
        labs(subtitle = "Humidité relative", x = NULL, y = NULL) + no_x))
    heights <- c(heights, 0.8)
  }
  if (has("vent")) {
    pv <- ggplot(h, aes(t)) + base +
      geom_ribbon(aes(ymin = vent, ymax = pmax(vent, rafales, na.rm = TRUE)),
                  fill = "#2ca58d", alpha = 0.2, na.rm = TRUE) +
      geom_line(aes(y = vent), color = "#2ca58d", linewidth = 1, na.rm = TRUE)
    if (detail && has("direction")) {
      # flèche = direction vers laquelle souffle le vent (1 sur 3 h)
      arr <- h |> dplyr::filter(!is.na(direction), !is.na(vent), vent > 0,
                                as.integer(format(t, "%H", tz = tz)) %% 3 == 0)
      top <- max(h$rafales, h$vent, na.rm = TRUE) * 1.18
      len <- as.numeric(diff(range(h$t)), units = "secs") / 70
      pv <- pv + geom_segment(
        data = arr,
        aes(x = t + len / 2 * sin(direction * pi / 180), xend = t - len / 2 * sin(direction * pi / 180),
            y = top + top * 0.06 * cos(direction * pi / 180), yend = top - top * 0.06 * cos(direction * pi / 180)),
        arrow = grid::arrow(length = grid::unit(0.09, "cm")), color = "grey35", linewidth = 0.35,
        na.rm = TRUE)
    }
    panels <- c(panels, list(pv + labs(subtitle = if (detail) "Vent moyen, rafales (km/h) et direction"
                                                  else "Vent moyen et rafales (km/h)",
                                       x = NULL, y = NULL) + no_x))
    heights <- c(heights, 1)
  }
  if (detail && has("pression")) {
    panels <- c(panels, list(
      ggplot(h, aes(t, pression)) + base +
        geom_line(color = "#7b5ea7", linewidth = 0.8, na.rm = TRUE) +
        labs(subtitle = "Pression au niveau de la mer (hPa)", x = NULL, y = NULL) + no_x))
    heights <- c(heights, 0.8)
  }
  panels <- c(panels, list(
    ggplot(h, aes(t, pluie)) + base +
      geom_col(fill = "#3a86c8", width = 3000, na.rm = TRUE) +
      scale_y_continuous(limits = c(0, max(1, max(h$pluie, na.rm = TRUE)))) +
      labs(subtitle = "Pluie (mm/h)", x = NULL, y = NULL)))
  heights <- c(heights, 0.8)
  patchwork::wrap_plots(panels, ncol = 1, heights = heights)
}

# meteo_now_md : dernière observation, en une ligne markdown --------

meteo_now_md <- function(h) {
  l <- dplyr::slice_tail(h, n = 1)
  p24 <- sum(h$pluie[h$t > l$t - 24 * 3600], na.rm = TRUE)
  parts <- c(
    sprintf("**%.1f°**", l$temp),
    if (!is.na(l$humidite)) sprintf("humidité %.0f %%", l$humidite),
    if (!is.na(l$vent)) sprintf("vent %.0f km/h%s", l$vent,
                                if (!is.na(l$rafales)) sprintf(" (rafales %.0f)", l$rafales) else ""),
    if (!is.na(l$pression)) sprintf("%.0f hPa", l$pression),
    sprintf("pluie sur 24 h : %.1f mm", p24)
  )
  paste0("Dernière observation, ", format(l$t, "%A %d %B à %Hh%M (heure locale)"), " : ",
         paste(parts, collapse = " · "))
}

# meteo_plot_year : une station, 12 derniers mois --------

meteo_plot_year <- function(daily, title = NULL) {
  d <- dplyr::filter(daily, date > Sys.Date() - 365)
  top <- ggplot(d, aes(date)) +
    geom_ribbon(aes(ymin = tmin, ymax = tmax), fill = "#fdae61", alpha = 0.35, na.rm = TRUE) +
    geom_line(aes(y = tmoy, color = tmoy), linewidth = 0.7, na.rm = TRUE) +
    scale_color_gradientn(colours = meteo_temp_pal, limits = c(-5, 30),
                          oob = scales::squish, guide = "none") +
    scale_y_continuous(labels = \(x) paste0(x, "°")) +
    scale_x_date(limits = range(d$date) + c(0, 1), expand = c(0.01, 0)) +
    labs(title = title, subtitle = "Température moyenne et amplitude min–max", x = NULL, y = NULL) +
    meteo_theme() + theme(axis.text.x = element_blank())
  m <- d |>
    dplyr::mutate(mois = lubridate::floor_date(date, "month")) |>
    dplyr::filter(mois >= min(d$date)) |>   # pas de mois partiel au début
    dplyr::group_by(mois) |>
    dplyr::summarise(pluie = if (all(is.na(pluie))) NA else sum(pluie, na.rm = TRUE),
                     soleil = if (all(is.na(soleil))) NA else sum(soleil, na.rm = TRUE),
                     .groups = "drop") |>
    tidyr::pivot_longer(c(pluie, soleil)) |>
    dplyr::group_by(name) |> dplyr::filter(any(!is.na(value))) |> dplyr::ungroup() |>
    dplyr::mutate(name = dplyr::recode(name, pluie = "Pluie (mm/mois)", soleil = "Soleil (h/mois)"))
  bot <- ggplot(m, aes(mois + 14, value, fill = name)) +
    geom_col(width = 24, na.rm = TRUE) +
    geom_text(aes(label = round(value)), vjust = -0.3, size = 2.6, color = "grey30", na.rm = TRUE) +
    facet_wrap(~name, ncol = 1, scales = "free_y", strip.position = "left") +
    scale_fill_manual(values = c("Pluie (mm/mois)" = "#3a86c8", "Soleil (h/mois)" = "#f2b705"),
                      guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    scale_x_date(labels = \(x) stringr::str_to_upper(substr(format(x, "%b"), 1, 1)),
                 date_breaks = "1 month",
                 limits = range(d$date) + c(0, 1), expand = c(0.01, 0)) +
    labs(x = NULL, y = NULL) + meteo_theme() +
    theme(strip.placement = "outside", strip.text = element_text(face = "bold"))
  top / bot + plot_layout(heights = c(2, if (length(unique(m$name)) > 1) 1.6 else 0.9))
}

# meteo_plot_vs_last_year : 30 derniers jours vs mêmes jours un an plus tôt --------

meteo_plot_vs_last_year <- function(daily, n_days = 30, title = NULL) {
  d <- daily |>
    dplyr::mutate(annee = dplyr::case_when(
      date > Sys.Date() - n_days ~ "cette année",
      date > Sys.Date() - n_days - 365 & date <= Sys.Date() - 365 ~ "l'an dernier"),
      jour = dplyr::if_else(annee == "l'an dernier", date + 365, date)) |>
    dplyr::filter(!is.na(annee))
  ecart <- d |> dplyr::select(jour, annee, tmoy) |>
    tidyr::pivot_wider(names_from = annee, values_from = tmoy) |>
    dplyr::summarise(e = mean(`cette année` - `l'an dernier`, na.rm = TRUE)) |> dplyr::pull(e)
  ggplot(d, aes(jour, tmoy, color = annee, linetype = annee)) +
    geom_line(linewidth = 1, na.rm = TRUE) +
    scale_color_manual(values = c("cette année" = "#d73027", "l'an dernier" = "grey55"), name = NULL) +
    scale_linetype_manual(values = c("cette année" = "solid", "l'an dernier" = "22"), name = NULL) +
    scale_y_continuous(labels = \(x) paste0(x, "°")) +
    scale_x_date(date_labels = "%d/%m") +
    labs(title = title,
         subtitle = sprintf("Température moyenne · écart moyen : %+.1f°", ecart),
         x = NULL, y = NULL) +
    meteo_theme()
}

# meteo_table_hours / meteo_table_days : tableaux communs à toutes les sources --------

meteo_gt <- function(df, page_size = 24) {
  df |>
    gt::gt() |>
    gt::sub_missing(missing_text = "–") |>
    gt::data_color(columns = dplyr::any_of(c("T°", "T° moy")),
                   fn = scales::col_numeric(meteo_temp_pal, domain = c(-5, 35))) |>
    gt::cols_align("center", dplyr::everything()) |>
    gt::opt_interactive(use_search = FALSE, use_pagination = TRUE,
                        use_compact_mode = TRUE, page_size_default = page_size)
}

meteo_table_hours <- function(h) {
  h |>
    dplyr::arrange(dplyr::desc(t)) |>
    dplyr::transmute(
      Heure = format(t, "%a %d %Hh%M"),
      `T°` = round(temp, 1), `Rosée` = round(rosee, 1), `Hum. %` = round(humidite),
      `Vent km/h` = round(vent), Rafales = round(rafales), `Dir. °` = direction,
      `Pression` = round(pression, 1), `Pluie mm` = pluie
    ) |>
    dplyr::select(dplyr::where(\(x) !all(is.na(x)))) |>
    meteo_gt(24)
}

meteo_table_days <- function(daily) {
  daily |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::transmute(
      Jour = format(date, "%a %d/%m"),
      `T° min` = tmin, `T° moy` = tmoy, `T° max` = tmax,
      `Pluie mm` = pluie, `Soleil h` = round(soleil, 1)
    ) |>
    dplyr::select(dplyr::where(\(x) !all(is.na(x)))) |>
    meteo_gt(15)
}

# meteo_pages_reactable : liste des pages du répertoire data/meteo --------

meteo_pages_reactable <- function(dir = here::here("data", "meteo")) {
  qmd <- setdiff(list.files(dir, "\\.qmd$"), "index.qmd")
  title_of <- \(f) {
    l <- grep("^title:", readLines(file.path(dir, f), n = 5, warn = FALSE), value = TRUE)[1]
    gsub('^title:\\s*|"', "", l)
  }
  tibble::tibble(slug = sub("\\.qmd$", "", qmd),
                 Title = vapply(qmd, title_of, ""),
                 updated = as.Date(file.mtime(file.path(dir, qmd)))) |>
    dplyr::left_join(dplyr::select(meteo_stations, slug, Lieu = lieu, source), by = "slug") |>
    dplyr::mutate(
      Title = sub("^Météo — ", "", Title),
      Source = dplyr::recode(source, meteo60 = "Météo-France (meteo60)",
                             iem = "NOAA ASOS (Iowa Mesonet)", .missing = "Météo-France"),
      Type = dplyr::if_else(is.na(source), "Données", "Station"),
      page = sprintf('<a href="https://fgeerolf.com/data/meteo/%s.html">%s</a>', slug, slug)
    ) |>
    dplyr::arrange(Type == "Données", match(slug, meteo_stations$slug), slug) |>
    dplyr::select(page, Title, Lieu, Source) |>
    reactable::reactable(
      searchable = TRUE, pagination = TRUE, defaultPageSize = 10,
      compact = TRUE, resizable = TRUE, highlight = TRUE, wrap = TRUE,
      defaultColDef = reactable::colDef(
        align = "left", na = "",
        headerStyle = list(fontWeight = "bold"),
        style = list(whiteSpace = "normal", wordBreak = "break-word")
      ),
      columns = list(
        page  = reactable::colDef(html = TRUE, minWidth = 90, maxWidth = 170),
        Title = reactable::colDef(minWidth = 140)
      )
    )
}
