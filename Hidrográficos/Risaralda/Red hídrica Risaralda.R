# ============================
# 1. LIBRERÍAS
# ============================
pacman::p_load(
  terra,
  sf,
  geodata,
  rayshader,
  tidyverse,
  magick,
  elevatr
)
sessionInfo()

# ============================
# 2. FRONTERAS DE RISARALDA
# ============================
get_risaralda_sf <- function() {
  main_path <- getwd()
  col_admin1 <- geodata::gadm(
    country = "COL",
    level = 1,
    path = main_path
  ) |> sf::st_as_sf()
  
  # Filtra solo Risaralda
  risaralda_sf <- col_admin1 |> 
    dplyr::filter(NAME_1 == "Risaralda") |> 
    sf::st_union() |> 
    sf::st_as_sf()
  
  return(risaralda_sf)
}

risaralda_sf <- get_risaralda_sf()
plot(sf::st_geometry(risaralda_sf))

# ============================
# 3. DESCARGA DE RÍOS (HydroRIVERS)
# ============================
url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_sa_shp.zip"
destfile <- basename(url)
if (!file.exists(destfile)) {
  download.file(url = url, destfile = destfile, mode = "wb")
  unzip(destfile)
}

# ============================
# 4. CARGA DE RÍOS Y FILTRO POR RISARALDA
# ============================
filename <- list.files(path = "HydroRIVERS_v10_sa_shp", pattern = "\\.shp$", full.names = TRUE)
hydro_rivers_all <- sf::st_read(filename)

rivers_risaralda <- sf::st_intersection(hydro_rivers_all, risaralda_sf)

# ============================
# 5. CÁLCULO DEL ANCHO DE LOS RÍOS
# ============================
crs_risaralda <- "+proj=laea +lat_0=4.5 +lon_0=-75.7 +datum=WGS84 +units=m +no_defs"

rivers_width <- rivers_risaralda |>
  dplyr::mutate(
    width = as.numeric(ORD_FLOW),
    width = dplyr::case_when(
      width == 4 ~ 16,
      width == 5 ~ 14,
      width == 6 ~ 10,
      width == 7 ~ 8,
      width == 8 ~ 4,
      width == 9 ~ 2,
      TRUE ~ 0
    )
  ) |> sf::st_transform(crs = crs_risaralda)

# ============================
# 6. DEM Y MATRIZ DE ELEVACIÓN
# ============================
elev_risaralda <- elevatr::get_elev_raster(
  locations = risaralda_sf,
  z = 10,   # resolución apropiada
  clip = "locations"
)

elev_rast <- elev_risaralda |> terra::rast() |> terra::project(crs_risaralda)
elmat <- rayshader::raster_to_matrix(elev_rast)

# ============================
# 7. GENERAR OVERLAY DE RÍOS
# ============================
overlay <- rayshader::generate_line_overlay(
  geometry = rivers_width,
  extent = elev_rast,
  heightmap = elmat,
  color = "#387B9C",
  linewidth = rivers_width$width,
  data_column_width = "width"
)

# ============================
# 8. CREAR ESCENA 3D
# ============================
shaded <- rayshader::height_shade(
  elmat,
  texture = colorRampPalette(c(
    "#fef9d7",  # amarillo claro (zonas más bajas)
    "#f7d19c",  # beige-amarillo
    "#74c476",  # verde medio
    "#006d2c",  # verde intenso
    "#00441b"   # verde oscuro (zonas más altas)
  ))(512)
)



scene <- rayshader::add_overlay(shaded, overlay, alphalayer = 1)

rayshader::plot_3d(
  scene,
  elmat,
  zscale = 10,
  solid = FALSE,
  shadow = TRUE,
  shadow_darkness = 0.7,
  background = "white",
  windowsize = c(700, 700),
  zoom = 0.8,
  phi = 70,
  theta = 0
)

# ============================
# 9. RENDERIZADO EN ALTA CALIDAD
# ============================
u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(u)
download.file(url = u, destfile = hdri_file, mode = "wb")

filename <- "risaralda-river-elevation.png"
rayshader::render_highquality(
  filename = filename,
  preview = TRUE,
  environment_light = hdri_file,
  intensity_env = 1.25,
  parallel = TRUE,
  width = 3000,
  height = 3000
)

# ============================
# 10. AÑADIR TÍTULOS Y CRÉDITOS
# ============================
img <- image_read("risaralda-river-elevation.png")

# Título con sombra
img <- image_annotate(
  img,
  text = "Risaralda",
  gravity = "south",
  location = "+0+390",  
  size = 150,
  color = "#A3A2A2",
  weight = 700,
  font = "Georgia"
)

# Título principal
img <- image_annotate(
  img,
  text = "Risaralda",
  gravity = "south",
  location = "+0+390",
  size = 150,
  color = "#a6541f",
  weight = 700,
  font = "Georgia"
)

# Barra debajo del título
barra <- image_blank(width = 1300, height = 8, color = "#a6541f")
img <- image_composite(
  img,
  barra,
  operator = "over",
  gravity = "south",
  offset = "+0+370"
)

# Subtítulo
img <- image_annotate(
  img,
  text = "Red hídrica",
  gravity = "south",
  location = "+0+240",
  size = 120,
  color = "#a6541f",
  font = "Georgia"
)

# Créditos
img <- image_annotate(
  img,
  text = "© Juan D. Raigoso 2025. Created using R+Rayshader.\nData: HydroSHEDS + Elevation: AWS Terrain Tiles \nvia elevatr R package.",
  gravity = "south",
  location = "+0+40",
  size = 60,
  color = "#35707e",
  font = "Georgia"
)

# Guardar imagen final
image_write(img, "risaralda-river-elevation_final.png")

