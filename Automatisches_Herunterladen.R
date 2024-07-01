# devtools::install_github("LAGeSo-Infektionsschutz-I-C-1/estatistikR",force = TRUE)

pacman::p_load(estatistikR,tidyverse,install = FALSE)

user_auth <- list(
  username = "mortsurv@hlfgp.hessen.de",
  password = "Ai5kF!23"
)

df_nachrichten <- get_nachrichten(user_auth = user_auth) |>
  get_anhaenge_info(user_auth = user_auth)

df_nachrichten_downloaded <- df_nachrichten |>
  filter(absender == "Sterbefalldaten RKI", versanddatum == max(versanddatum)) |>
  download_anhaenge(user_auth = user_auth, path = "Import")

df_nachrichten_downloaded |>
  markiere_nachrichten_gelesen(user_auth = user_auth)

unzip(file.path("Import",df_nachrichten_downloaded$anhang_dateiname),exdir = "Daten")

file.remove(file.path("Import",df_nachrichten_downloaded$anhang_dateiname))
