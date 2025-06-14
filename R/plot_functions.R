#' Créer un graphique des messages par jour
#' @param data Données WhatsApp
#' @param selected_authors Auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_messages_per_day <- function(data, selected_authors) {
  # Filtrer les données
  filtered_data <- data %>%
    dplyr::filter(author %in% selected_authors)
  
  # Compter les messages par jour
  daily_counts <- filtered_data %>%
    dplyr::mutate(date = lubridate::date(time)) %>%
    dplyr::count(date)
  
  # Créer le graphique
  p <- ggplot2::ggplot(daily_counts, ggplot2::aes(x = date, y = n)) +
    ggplot2::geom_line(color = "#25D366") +
    ggplot2::geom_point(color = "#128C7E") +
    ggplot2::labs(
      title = "Messages par jour",
      x = "Date",
      y = "Nombre de messages"
    ) +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique des messages par auteur
#' @param data Données WhatsApp
#' @param selected_authors Auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_messages_by_author <- function(data, selected_authors) {
  # Filtrer les données
  filtered_data <- data %>%
    dplyr::filter(author %in% selected_authors)
  
  # Compter les messages par auteur
  author_counts <- filtered_data %>%
    dplyr::count(author) %>%
    dplyr::arrange(n)
  
  # Créer le graphique
  p <- ggplot2::ggplot(author_counts, ggplot2::aes(x = reorder(author, n), y = n)) +
    ggplot2::geom_col(fill = "#25D366") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Messages par auteur",
      x = "Auteur",
      y = "Nombre de messages"
    ) +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique des mots les plus utilisés
#' @param data Données WhatsApp
#' @param selected_authors Auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_most_used_words <- function(data, selected_authors) {
  # Filtrer les données
  filtered_data <- data %>%
    dplyr::filter(author %in% selected_authors)
  
  # Extraire les mots
  words <- filtered_data %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::filter(!word %in% stopwords::stopwords("fr")) %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::top_n(20, n)
  
  # Créer le graphique
  p <- ggplot2::ggplot(words, ggplot2::aes(x = reorder(word, n), y = n)) +
    ggplot2::geom_col(fill = "#25D366") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Mots les plus utilisés",
      x = "Mot",
      y = "Fréquence"
    ) +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique des emojis les plus utilisés
#' @param data Données WhatsApp
#' @param selected_authors Auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_most_used_emojis <- function(data, selected_authors) {
  # Filtrer les données
  filtered_data <- data %>%
    dplyr::filter(author %in% selected_authors)
  
  # Extraire les emojis
  emojis <- filtered_data %>%
    dplyr::mutate(emoji = stringr::str_extract_all(text, "[\\p{Emoji}]")) %>%
    tidyr::unnest(emoji) %>%
    dplyr::count(emoji, sort = TRUE) %>%
    dplyr::top_n(20, n)
  
  # Créer le graphique
  p <- ggplot2::ggplot(emojis, ggplot2::aes(x = reorder(emoji, n), y = n)) +
    ggplot2::geom_col(fill = "#25D366") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Emojis les plus utilisés",
      x = "Emoji",
      y = "Fréquence"
    ) +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique de l'activité par heure
#' @param data Données WhatsApp
#' @param selected_authors Auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_activity_by_hour <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data %>% dplyr::filter(author %in% selected_authors)
  }
  
  hour_data <- data %>%
    dplyr::mutate(hour = lubridate::hour(time)) %>%
    dplyr::count(hour, .drop = FALSE) %>%
    dplyr::arrange(hour)
  
  p <- ggplot2::ggplot(hour_data, ggplot2::aes(x = hour, y = n, fill = factor(hour))) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Activité par heure de la journée",
      x = "Heure",
      y = "Nombre de messages"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::theme(
      legend.position = "none"
    )
  
  plotly::ggplotly(p)
}

#' Créer un graphique de l'activité par jour de la semaine
#' @param data Les données WhatsApp
#' @param selected_authors Les auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_activity_by_weekday <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data %>% dplyr::filter(author %in% selected_authors)
  }
  
  # Créer un vecteur de jours de la semaine en français
  jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
  
  # Calculer l'activité par jour de la semaine
  weekday_data <- data %>%
    dplyr::mutate(weekday = factor(lubridate::wday(time, label = FALSE), 
                                  levels = 1:7,
                                  labels = jours)) %>%
    dplyr::count(weekday, .drop = FALSE) %>%
    dplyr::arrange(weekday)
  
  # Créer le graphique
  p <- ggplot2::ggplot(weekday_data, ggplot2::aes(x = weekday, y = n, fill = weekday)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Activité par jour de la semaine",
      x = "Jour de la semaine",
      y = "Nombre de messages"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  plotly::ggplotly(p)
}

#' Créer un graphique de la longueur moyenne des messages
#' @param data Données WhatsApp
#' @param selected_authors Auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_avg_message_length <- function(data, selected_authors) {
  # Filtrer les données
  filtered_data <- data %>%
    dplyr::filter(author %in% selected_authors)
  
  # Calculer la longueur moyenne des messages
  avg_length <- filtered_data %>%
    dplyr::mutate(length = stringr::str_length(text)) %>%
    dplyr::group_by(author) %>%
    dplyr::summarise(avg_length = mean(length)) %>%
    dplyr::arrange(avg_length)
  
  # Créer le graphique
  p <- ggplot2::ggplot(avg_length, ggplot2::aes(x = reorder(author, avg_length), y = avg_length)) +
    ggplot2::geom_col(fill = "#25D366") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Longueur moyenne des messages",
      x = "Auteur",
      y = "Longueur moyenne (caractères)"
    ) +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique de l'évolution des messages
#' @param data Les données WhatsApp
#' @param selected_authors Les auteurs sélectionnés
#' @return Un graphique plotly
plot_message_evolution <- function(data, selected_authors) {
  # Filtrer les données pour les auteurs sélectionnés
  filtered_data <- data %>%
    dplyr::filter(author %in% selected_authors)
  
  # Calculer l'évolution des messages
  message_evolution <- filtered_data %>%
    dplyr::group_by(author, month = lubridate::month(time)) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(month)
  
  # Créer le graphique
  p <- ggplot2::ggplot(message_evolution, ggplot2::aes(x = month, y = count, color = author, group = author)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb) +
    ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Évolution des messages par mois",
      x = "Mois",
      y = "Nombre de messages",
      color = "Auteur"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Convertir en graphique plotly
  plotly::ggplotly(p) %>%
    plotly::layout(
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2)
    )
}

#' Créer un graphique de la diversité lexicale
#' @param data Les données WhatsApp
#' @param selected_authors Les auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_lexical_diversity <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data %>% dplyr::filter(author %in% selected_authors)
  }
  
  # Calculer la diversité lexicale
  diversity_data <- data %>%
    dplyr::filter(!is.na(text)) %>%
    tidyr::unnest_tokens(word, text) %>%
    dplyr::filter(!word %in% tidytext::get_stopwords("fr")$word) %>%
    dplyr::count(author, word) %>%
    dplyr::group_by(author) %>%
    dplyr::summarise(
      unique_words = dplyr::n(),
      total_words = sum(n),
      diversity = unique_words / total_words
    ) %>%
    dplyr::arrange(diversity)
  
  p <- ggplot2::ggplot(diversity_data, ggplot2::aes(x = reorder(author, diversity), y = diversity, fill = author)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Diversité lexicale par auteur",
      x = "Auteur",
      y = "Diversité lexicale (mots uniques / total)"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  plotly::ggplotly(p)
}

#' Créer un graphique des messages par auteur
#' @param data Les données WhatsApp
#' @param selected_authors Les auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_message_reactions <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data %>% dplyr::filter(author %in% selected_authors)
  }
  
  # Compter le nombre total de messages par auteur
  message_data <- data %>%
    dplyr::count(author) %>%
    dplyr::arrange(desc(n))
  
  p <- ggplot2::ggplot(message_data, ggplot2::aes(x = reorder(author, n), y = n, fill = author)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Nombre de messages par auteur",
      x = "Auteur",
      y = "Nombre de messages"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  plotly::ggplotly(p)
} 