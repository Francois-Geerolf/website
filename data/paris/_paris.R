# Helpers communs aux pages data/paris/*.qmd.

suppressMessages({library(sf); library(leaflet)})

# geo_shape / geo_point_2d / geom d'opendata.paris.fr sont du WKB (GeoParquet).
wkb2sfc <- function(x) {
  sf::st_as_sfc(structure(lapply(x, as.raw), class = "WKB"), EWKB = FALSE)
}

# Fond de carte des leaflet : « World Light Gray » d'Esri — clair, discret,
# SANS clé d'API (les tuiles CARTO Positron affichent désormais un bandeau
# « API_KEY required » depuis une origine non enregistrée / file://).
paris_basemap <- function(map) {
  leaflet::addProviderTiles(
    map, leaflet::providers$Esri.WorldGrayCanvas,
    options = leaflet::providerTileOptions(maxZoom = 19)
  )
}

# Contours des 20 arrondissements (fond) en objet sf.
arrondissements_sf <- function() {
  d <- arrow::read_parquet(here::here("data", "paris", "arrondissements.parquet"))
  d |>
    dplyr::transmute(c_ar, nom = l_aroff, geometry = wkb2sfc(geom)) |>
    sf::st_as_sf(crs = 4326)
}
