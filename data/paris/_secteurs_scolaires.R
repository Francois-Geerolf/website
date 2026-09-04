# Fonctions communes aux 3 pages data/paris/secteurs-scolaires-*.qmd
# (maternelles, écoles élémentaires, collèges). Les .parquet sont réduits au
# dernier millésime et leur géométrie simplifiée par data/paris/_datasets.R.

source(here::here("data", "paris", "_paris.R"))   # wkb2sfc(), paris_basemap(), arrondissements_sf()

# Charge un jeu secteurs-scolaires-<...> en objet sf (WGS84).
secteurs_load <- function(id) {
  d <- arrow::read_parquet(here::here("data", "paris", paste0(id, ".parquet")))
  d$geometry <- wkb2sfc(d$geo_shape)
  d |>
    dplyr::select(-dplyr::any_of(c("geo_shape", "geo_point_2d"))) |>
    dplyr::mutate(
      n_etab = rowSums(!is.na(dplyr::across(dplyr::matches("^lib_etab_[1-4]$")))),
      superficie_ha = st_area_shape / 1e4
    ) |>
    sf::st_as_sf(crs = 4326) |>
    sf::st_make_valid()
}

# Contours des 20 arrondissements (fond de carte) — alias de _paris.R.
arrondissements_load <- arrondissements_sf

# Carte statique : un aplat par secteur, contour d'arrondissement par-dessus.
secteurs_carte <- function(sect, arr, titre) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = sect, ggplot2::aes(fill = superficie_ha),
                     colour = "white", linewidth = 0.15) +
    ggplot2::geom_sf(data = arr, fill = NA, colour = "grey30", linewidth = 0.3) +
    ggplot2::scale_fill_viridis_c(option = "viridis", trans = "sqrt",
                                  name = "Superficie\n(ha)") +
    ggplot2::labs(title = titre,
                  subtitle = sprintf("%d secteurs — %s",
                                     nrow(sect), unique(sect$annee_scol))) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.subtitle = ggplot2::element_text(colour = "grey35"),
                   plot.margin = ggplot2::margin(5, 15, 5, 5),
                   legend.title = ggplot2::element_text(size = 10))
}

# Carte interactive : survol = école(s) du secteur.
secteurs_leaflet <- function(sect, arr) {
  pal <- leaflet::colorNumeric("viridis", domain = sqrt(sect$superficie_ha))
  leaflet::leaflet(sect, options = leaflet::leafletOptions(minZoom = 11)) |>
    paris_basemap() |>
    leaflet::addPolygons(data = arr, fill = FALSE, color = "#666", weight = 1) |>
    leaflet::addPolygons(
      fillColor = ~pal(sqrt(superficie_ha)), fillOpacity = 0.6,
      color = "white", weight = 1,
      highlightOptions = leaflet::highlightOptions(weight = 2, color = "#333",
                                                   bringToFront = TRUE),
      label = ~lib_etab_1,
      popup = ~sprintf("<b>%s</b><br>%s<br>Superficie : %.1f ha",
                       lib_etab_1,
                       ifelse(is.na(adr_etab_1), "", adr_etab_1),
                       superficie_ha)
    )
}

# Bloc « superficie + établissements » commun.
secteurs_stats <- function(sect) {
  ggplot2::ggplot(sect, ggplot2::aes(superficie_ha)) +
    ggplot2::geom_histogram(bins = 30, fill = "#3b6f8a") +
    ggplot2::labs(title = "Distribution de la superficie des secteurs",
                  x = "Superficie (ha)", y = "Nombre de secteurs") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   panel.grid.major.x = ggplot2::element_blank())
}
