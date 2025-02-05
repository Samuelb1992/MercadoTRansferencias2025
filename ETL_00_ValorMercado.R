
devtools::install_github("JaseZiv/worldfootballR")

# Librerias ---------------------------------------------------------------

library(tidyverse)
library(worldfootballR)

# Parametros --------------------------------------------------------------


fecha_generacion <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
year.seleccionado <- 2024
region.seleccionada <- "Americas"
# copa.libertadores.2025 <- "https://www.transfermarkt.es/copa-libertadores/startseite/pokalwettbewerb/CLI?saison_id=2024"

paises.conmebol <- c("Argentina",
                     "Bolivia",
                     "Brazil",
                     "Chile",
                     "Colombia",
                     "Ecuador",
                     "Paraguay",
                     "Peru",
                     "Uruguay",
                     "Venezuela")


# Equipos Libertadores ----------------------------------------------------

# Equipos de cada liga ----------------------------------------------------

# Se deja el ultimo torneo de cada equipo latam
ligas.github <- "https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/refs/heads/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"

url.ligas <- read.csv(ligas.github)

# Filtramos por año y region
url.ligas.year <- url.ligas |>
  filter(
    season_start_year <= year.seleccionado,
    region == region.seleccionada,
    country %in% paises.conmebol
  ) |>
  group_by(country, comp_name) |>
  filter(season_start_year == max(season_start_year)) |> 
  ungroup() |> 
  select(season_start_year, comp_url,country)

# Hay paises con duplicados que son Argentina, Paraguay y Venezuela. Se dejan los de apertura

url.ligas.year <- url.ligas.year |> 
  group_by(country) |>
  mutate(n_pais = n()) |> 
  filter(n_pais > 1) |> 
  filter(grepl("apertura", comp_url)) |>
  # union con los otros paises
  bind_rows(url.ligas.year |> filter(!country %in% c("Argentina", "Paraguay", "Venezuela","Bolivia")))

# Faltan liga boliviana

ligas.faltantes <- data.frame(
  season_start_year = 2024,
  comp_url = c(
    "https://www.transfermarkt.es/division-profesional-apertura/startseite/wettbewerb/B1AP"
  ),
  country = c("Bolivia")
)

url.ligas.final <- rbind(url.ligas.year, ligas.faltantes)

# Buscamos url de los equipos de cada liga
# Los cambios en los equipos se producen una vez por semestre o año ( ascenso o descenso)

# equipos.latam <- url.ligas.final |>
#   mutate(team_urls = map2(
#     season_start_year,
#     comp_url,
#     ~ tm_league_team_urls(start_year = .x, league_url = .y),
#     .progress = TRUE
#   )) |>
#   unnest(team_urls)
# 
# write.csv(equipos.latam, "equipos_latam.csv", row.names = F)


equipos.latam <- read.csv("equipos_latam.csv")

# Valor de Mercado Actual -------------------------------------------------


valor.mercado.equipos <- lapply(url.ligas.final$comp_url[4], function(x) {
  tryCatch(
    {
      tm_player_market_values(
        country_name = "",
        start_year = year.seleccionado,
        league_url = x
      )
    }, error = function(e) {
      message("Error en ", x)
      message(conditionMessage(e))
      return(NA)
    }
  )
})



valor.mercado.final <- bind_rows(valor.mercado.equipos)

write.csv(valor.mercado.final, paste0("data/",fecha_generacion,"_valor_mercado.csv"), row.names = F)

print("Proceso Completado")

# # Obtener Traspasos -------------------------------------------------------
# 
# fichajes2025 <- tm_team_transfers(team_url =  equipos.latam$team_urls, transfer_window = "winter")
# 
# 
# fichajes2025 |> filter(country == "Chile" & team_name == "CD Universidad Católica")
# 
# 
# # Repaso Operaciones, Estadisticas y Funciones R --------------------------------------------------------




