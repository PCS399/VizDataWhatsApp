#' Charger les données WhatsApp
#' @param file_path Chemin vers le fichier WhatsApp
#' @return Un dataframe contenant les données WhatsApp
#' @export
load_whatsapp_data <- function(file_path) {
  # Charger les données
  data <- rwhatsapp::rwa_read(file_path)
  
  # Filtrer les messages système
  data <- data %>%
    dplyr::filter(!is.na(author))
  
  return(data)
} 