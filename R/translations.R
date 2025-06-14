#' Obtenir les traductions de l'application
#' @return Une liste de traductions
#' @export
get_translations <- function() {
  list(
    fr = list(
      title = "WhatsAppViz",
      subtitle = "Visualisation interactive des conversations WhatsApp",
      file_input = "Choisir un fichier WhatsApp",
      select_authors = "Sélectionner les auteurs",
      tabs = list(
        activity = "Activité",
        authors = "Auteurs",
        content = "Contenu",
        interactions = "Interactions"
      ),
      plots = list(
        messages_per_day = "Messages par jour",
        messages_by_author = "Messages par auteur",
        most_used_words = "Mots les plus utilisés",
        most_used_emojis = "Emojis les plus utilisés",
        activity_by_hour = "Activité par heure",
        activity_by_weekday = "Activité par jour de la semaine",
        avg_message_length = "Longueur moyenne des messages",
        message_evolution = "Évolution des messages",
        lexical_diversity = "Diversité lexicale",
        message_reactions = "Taux de réaction aux messages"
      ),
      footer = "© 2024 WhatsAppViz - Tous droits réservés"
    ),
    en = list(
      title = "WhatsAppViz",
      subtitle = "Interactive WhatsApp Conversation Visualization",
      file_input = "Choose a WhatsApp file",
      select_authors = "Select authors",
      tabs = list(
        activity = "Activity",
        authors = "Authors",
        content = "Content",
        interactions = "Interactions"
      ),
      plots = list(
        messages_per_day = "Messages per day",
        messages_by_author = "Messages by author",
        most_used_words = "Most used words",
        most_used_emojis = "Most used emojis",
        activity_by_hour = "Activity by hour",
        activity_by_weekday = "Activity by weekday",
        avg_message_length = "Average message length",
        message_evolution = "Message evolution",
        lexical_diversity = "Lexical diversity",
        message_reactions = "Message reaction rate"
      ),
      footer = "© 2024 WhatsAppViz - All rights reserved"
    )
  )
}

#' Mettre à jour les traductions de l'interface
#' @param session Session Shiny
#' @param translations Traductions à appliquer
#' @export
update_ui_translations <- function(session, translations) {
  # Mettre à jour les textes de l'interface
  shiny::updateTextInput(session, "file", label = translations$file_input)
  shiny::updateSelectInput(session, "authors", label = translations$select_authors)
  
  # Mettre à jour les titres des onglets
  shiny::updateTabsetPanel(session, "tabs",
    selected = translations$tabs$activity
  )
  
  # Mettre à jour les titres des graphiques
  shiny::updateTextInput(session, "messages_per_day", label = translations$plots$messages_per_day)
  shiny::updateTextInput(session, "messages_by_author", label = translations$plots$messages_by_author)
  shiny::updateTextInput(session, "most_used_words", label = translations$plots$most_used_words)
  shiny::updateTextInput(session, "most_used_emojis", label = translations$plots$most_used_emojis)
  shiny::updateTextInput(session, "activity_by_hour", label = translations$plots$activity_by_hour)
  shiny::updateTextInput(session, "activity_by_weekday", label = translations$plots$activity_by_weekday)
  shiny::updateTextInput(session, "avg_message_length", label = translations$plots$avg_message_length)
  shiny::updateTextInput(session, "message_evolution", label = translations$plots$message_evolution)
  shiny::updateTextInput(session, "lexical_diversity", label = translations$plots$lexical_diversity)
  shiny::updateTextInput(session, "message_reactions", label = translations$plots$message_reactions)
} 