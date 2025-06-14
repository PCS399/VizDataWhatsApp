#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr count
#' @importFrom dplyr top_n
#' @importFrom dplyr left_join
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 expansion
#' @importFrom plotly ggplotly
#' @importFrom lubridate wday
#' @importFrom lubridate hour
#' @importFrom lubridate date
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr unnest
#' @importFrom stringr str_detect
#' @importFrom scales comma

#' Créer un graphique des messages par jour
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_messages_per_day <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  daily_counts <- data %>%
    dplyr::mutate(day = lubridate::date(time)) %>%
    dplyr::count(day)
  
  p <- ggplot2::ggplot(daily_counts, ggplot2::aes(x = day, y = n)) +
    ggplot2::geom_bar(stat = "identity", fill = "#25D366") +
    ggplot2::labs(title = "Messages par jour",
                  x = "Date",
                  y = "Nombre de messages") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique des messages par auteur
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_messages_by_author <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  author_counts <- data %>%
    dplyr::count(author) %>%
    dplyr::arrange(dplyr::desc(n))
  
  p <- ggplot2::ggplot(author_counts, ggplot2::aes(x = reorder(author, n), y = n)) +
    ggplot2::geom_bar(stat = "identity", fill = "#25D366") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Nombre de messages par auteur",
                  x = "Auteur",
                  y = "Nombre de messages") +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique des mots les plus utilisés
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_most_used_words <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  to_remove <- c(stopwords::stopwords(language = "fr"),
                 "media", "omitted", "ref")
  
  word_counts <- data %>%
    tidytext::unnest_tokens(input = text, output = word) %>%
    dplyr::filter(!word %in% to_remove) %>%
    dplyr::count(author, word, sort = TRUE) %>%
    dplyr::group_by(author) %>%
    dplyr::top_n(n = 6, n)
  
  p <- ggplot2::ggplot(word_counts, ggplot2::aes(x = reorder(word, n), y = n, fill = author)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~author, ncol = 2, scales = "free_y") +
    ggplot2::labs(title = "Mots les plus utilisés",
                  x = "Mot",
                  y = "Fréquence") +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique des emojis les plus utilisés
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_most_used_emojis <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  emoji_data <- rwhatsapp::emojis %>%
    dplyr::mutate(hex_runes1 = gsub("\\s.*", "", hex_runes)) %>%
    dplyr::mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                                    tolower(hex_runes1), ".png"))
  
  emoji_counts <- data %>%
    tidyr::unnest(emoji) %>%
    dplyr::count(author, emoji, sort = TRUE) %>%
    dplyr::group_by(author) %>%
    dplyr::top_n(n = 6, n) %>%
    dplyr::left_join(emoji_data, by = "emoji")
  
  p <- ggplot2::ggplot(emoji_counts, ggplot2::aes(x = reorder(emoji, n), y = n, fill = author)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~author, ncol = 2, scales = "free_y") +
    ggplot2::labs(title = "Emojis les plus utilisés",
                  x = "Emoji",
                  y = "Fréquence") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique de la diversité lexicale
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_lexical_diversity <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  to_remove <- c(stopwords::stopwords(language = "fr"),
                 "media", "omitted", "ref")
  
  lex_diversity <- data %>%
    tidytext::unnest_tokens(input = text, output = word) %>%
    dplyr::filter(!word %in% to_remove) %>%
    dplyr::group_by(author) %>%
    dplyr::summarise(lex_diversity = dplyr::n_distinct(word)) %>%
    dplyr::arrange(dplyr::desc(lex_diversity))
  
  p <- ggplot2::ggplot(lex_diversity, 
                      ggplot2::aes(x = reorder(author, lex_diversity),
                                  y = lex_diversity,
                                  fill = author)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(lex_diversity)), 
                      hjust = -0.1) +
    ggplot2::labs(title = "Diversité lexicale",
                  x = "Auteur",
                  y = "Mots uniques") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique de l'activité par heure
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_activity_by_hour <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  hourly_counts <- data %>%
    dplyr::mutate(hour = lubridate::hour(time)) %>%
    dplyr::count(hour) %>%
    dplyr::arrange(hour)
  
  p <- ggplot2::ggplot(hourly_counts, ggplot2::aes(x = hour, y = n)) +
    ggplot2::geom_line(color = "#25D366", size = 1) +
    ggplot2::geom_point(color = "#25D366", size = 2) +
    ggplot2::scale_x_continuous(breaks = 0:23) +
    ggplot2::labs(title = "Activité par heure",
                  x = "Heure",
                  y = "Nombre de messages") +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique de l'activité par jour de la semaine
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_activity_by_weekday <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  weekday_counts <- data %>%
    dplyr::mutate(weekday = lubridate::wday(time, label = TRUE, locale = "fr_FR")) %>%
    dplyr::count(weekday) %>%
    dplyr::arrange(weekday)
  
  p <- ggplot2::ggplot(weekday_counts, ggplot2::aes(x = weekday, y = n)) +
    ggplot2::geom_bar(stat = "identity", fill = "#25D366") +
    ggplot2::labs(title = "Activité par jour de la semaine",
                  x = "Jour",
                  y = "Nombre de messages") +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique de la longueur moyenne des messages
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_avg_message_length <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  avg_length <- data %>%
    dplyr::mutate(message_length = nchar(text)) %>%
    dplyr::group_by(author) %>%
    dplyr::summarise(avg_length = mean(message_length, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(avg_length))
  
  p <- ggplot2::ggplot(avg_length, ggplot2::aes(x = reorder(author, avg_length), y = avg_length)) +
    ggplot2::geom_bar(stat = "identity", fill = "#25D366") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Longueur moyenne des messages par auteur",
                  x = "Auteur",
                  y = "Longueur moyenne (caractères)") +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique de l'évolution temporelle des messages
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_message_evolution <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  monthly_counts <- data %>%
    dplyr::mutate(month = lubridate::floor_date(time, "month")) %>%
    dplyr::count(month)
  
  p <- ggplot2::ggplot(monthly_counts, ggplot2::aes(x = month, y = n)) +
    ggplot2::geom_line(color = "#25D366", size = 1) +
    ggplot2::geom_point(color = "#25D366", size = 2) +
    ggplot2::labs(title = "Évolution des messages dans le temps",
                  x = "Mois",
                  y = "Nombre de messages") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(plotly::ggplotly(p))
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