#' Charger les données WhatsApp
#' @param file_path Chemin vers le fichier WhatsApp
#' @return Un dataframe avec les données WhatsApp
#' @export
load_whatsapp_data <- function(file_path) {
  data <- rwhatsapp::rwa_read(file_path)
  data <- data[!is.na(data$author), ]
  return(data)
}

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
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0, 0, 0.1))) +
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
  
  evolution <- data %>%
    dplyr::mutate(month = lubridate::floor_date(time, "month")) %>%
    dplyr::count(month, author) %>%
    dplyr::arrange(month)
  
  p <- ggplot2::ggplot(evolution, ggplot2::aes(x = month, y = n, color = author)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Évolution temporelle des messages",
                  x = "Date",
                  y = "Nombre de messages",
                  color = "Auteur") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(plotly::ggplotly(p))
}

#' Créer un graphique des réactions aux messages
#' @param data Données WhatsApp
#' @param selected_authors Liste des auteurs sélectionnés
#' @return Un graphique plotly
#' @export
plot_message_reactions <- function(data, selected_authors = NULL) {
  if (!is.null(selected_authors)) {
    data <- data[data$author %in% selected_authors, ]
  }
  
  reactions <- data %>%
    dplyr::filter(!is.na(reactions)) %>%
    dplyr::count(author, reactions) %>%
    dplyr::arrange(dplyr::desc(n))
  
  p <- ggplot2::ggplot(reactions, ggplot2::aes(x = reorder(reactions, n), y = n, fill = author)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Réactions aux messages",
                  x = "Type de réaction",
                  y = "Nombre de réactions") +
    ggplot2::theme_minimal()
  
  return(plotly::ggplotly(p))
}

#' Créer l'application Shiny
#' @return Une application Shiny
#' @export
run_whatsapp_viz_app <- function() {
  ui <- shiny::fluidPage(
    # Ajout des styles CSS
    shiny::tags$head(
      shiny::tags$style(HTML("
        .header {
          background-color: #25D366;
          color: white;
          padding: 20px;
          text-align: center;
          border-radius: 5px;
          margin-bottom: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 20px;
        }
        .header-logo {
          width: 60px;
          height: 60px;
        }
        .header-text {
          text-align: left;
        }
        .header h1 {
          margin: 0;
          font-size: 2.5em;
        }
        .header p {
          margin: 5px 0 0 0;
          font-size: 1.2em;
        }
        .footer {
          background-color: #128C7E;
          color: white;
          text-align: center;
          padding: 10px;
          position: fixed;
          bottom: 0;
          width: 100%;
          font-size: 0.9em;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        .footer-text {
          flex-grow: 1;
          text-align: center;
        }
        .language-selector {
          display: flex;
          gap: 10px;
          margin-right: 20px;
        }
        .language-btn {
          background: none;
          border: none;
          color: white;
          cursor: pointer;
          padding: 5px 10px;
          border-radius: 3px;
          transition: background-color 0.3s;
        }
        .language-btn:hover {
          background-color: rgba(255, 255, 255, 0.1);
        }
        .language-btn.active {
          background-color: rgba(255, 255, 255, 0.2);
        }
        .main-content {
          margin-bottom: 60px;
        }
        .control-panel {
          background-color: #f8f9fa;
          padding: 20px;
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          margin-bottom: 20px;
          display: flex;
          align-items: center;
          gap: 20px;
        }
        .file-input-wrapper {
          flex-grow: 1;
        }
        .select-input-wrapper {
          flex-grow: 2;
        }
        .box {
          background: white;
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          padding: 20px;
          margin-bottom: 20px;
          transition: all 0.3s ease;
        }
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        .tab-content {
          padding: 20px;
        }
        .nav-tabs {
          border-bottom: 2px solid #25D366;
        }
        .nav-tabs .nav-link {
          color: #128C7E;
          border: none;
          padding: 10px 20px;
          margin-right: 5px;
          border-radius: 5px 5px 0 0;
          transition: all 0.3s ease;
        }
        .nav-tabs .nav-link:hover {
          background-color: #f8f9fa;
        }
        .nav-tabs .nav-link.active {
          color: white;
          background-color: #25D366;
          border: none;
        }
      "))
    ),
    
    # En-tête
    shiny::div(class = "header",
      shiny::img(src = "logo.svg", class = "header-logo", alt = "WhatsAppViz Logo"),
      shiny::div(class = "header-text",
        shiny::h1(shiny::textOutput("title")),
        shiny::p(shiny::textOutput("subtitle"))
      )
    ),
    
    # Contenu principal
    shiny::div(class = "main-content",
      # Panneau de contrôle horizontal
      shiny::div(class = "control-panel",
        shiny::div(class = "file-input-wrapper",
          shiny::fileInput("file", shiny::textOutput("file_input"),
                          accept = c("text/plain", ".txt"))
        ),
        shiny::div(class = "select-input-wrapper",
          shinyWidgets::pickerInput("authors", shiny::textOutput("select_authors"),
                                  choices = NULL,
                                  options = list(
                                    `actions-box` = TRUE,
                                    `live-search` = TRUE
                                  ),
                                  multiple = TRUE)
        )
      ),
      
      # Onglets
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(shiny::textOutput("tab_activity"),
          shiny::div(class = "box",
            plotly::plotlyOutput("daily_plot", height = "400px")
          ),
          shiny::div(class = "box",
            plotly::plotlyOutput("hourly_plot", height = "400px")
          )
        ),
        shiny::tabPanel(shiny::textOutput("tab_authors"),
          shiny::div(class = "box",
            plotly::plotlyOutput("author_plot", height = "400px")
          ),
          shiny::div(class = "box",
            plotly::plotlyOutput("length_plot", height = "400px")
          ),
          shiny::div(class = "box",
            plotly::plotlyOutput("evolution_plot", height = "400px")
          )
        ),
        shiny::tabPanel(shiny::textOutput("tab_content"),
          shiny::div(class = "box",
            plotly::plotlyOutput("words_plot", height = "400px")
          ),
          shiny::div(class = "box",
            plotly::plotlyOutput("emojis_plot", height = "400px")
          )
        )
      )
    ),
    
    # Pied de page
    shiny::div(class = "footer",
      shiny::div(class = "footer-text",
        shiny::textOutput("footer_text")
      ),
      shiny::div(class = "language-selector",
        shiny::actionButton("fr_btn", "FR", class = "language-btn active"),
        shiny::actionButton("en_btn", "EN", class = "language-btn")
      )
    )
  )
  
  server <- function(input, output, session) {
    # Gestion des traductions
    translations <- get_translations()
    current_lang <- shiny::reactiveVal("fr")
    
    # Mettre à jour l'interface en fonction de la langue
    shiny::observeEvent(input$fr_btn, {
      current_lang("fr")
      shinyjs::addClass("fr_btn", "active")
      shinyjs::removeClass("en_btn", "active")
    })
    
    shiny::observeEvent(input$en_btn, {
      current_lang("en")
      shinyjs::addClass("en_btn", "active")
      shinyjs::removeClass("fr_btn", "active")
    })
    
    # Textes traduits
    output$title <- shiny::renderText({
      translations[[current_lang()]]$title
    })
    
    output$subtitle <- shiny::renderText({
      translations[[current_lang()]]$subtitle
    })
    
    output$file_input <- shiny::renderText({
      translations[[current_lang()]]$file_input
    })
    
    output$select_authors <- shiny::renderText({
      translations[[current_lang()]]$select_authors
    })
    
    output$tab_activity <- shiny::renderText({
      translations[[current_lang()]]$tabs$activity
    })
    
    output$tab_authors <- shiny::renderText({
      translations[[current_lang()]]$tabs$authors
    })
    
    output$tab_content <- shiny::renderText({
      translations[[current_lang()]]$tabs$content
    })
    output$footer_text <- shiny::renderText({
      translations[[current_lang()]]$footer
    })
    
    # Données et graphiques
    chat_data <- shiny::reactive({
      req(input$file)
      load_whatsapp_data(input$file$datapath)
    })
    
    # Mise à jour des choix d'auteurs
    shiny::observe({
      req(chat_data())
      authors <- unique(chat_data()$author)
      shinyWidgets::updatePickerInput(session, "authors",
                                    choices = authors,
                                    selected = authors)
    })
    
    # Graphiques d'activité
    output$daily_plot <- plotly::renderPlotly({
      req(chat_data())
      plot_messages_per_day(chat_data(), input$authors)
    })
    
    output$hourly_plot <- plotly::renderPlotly({
      req(chat_data())
      plot_activity_by_hour(chat_data(), input$authors)
    })
    
    # Graphiques d'auteurs
    output$author_plot <- plotly::renderPlotly({
      req(chat_data())
      plot_messages_by_author(chat_data(), input$authors)
    })
    
    output$length_plot <- plotly::renderPlotly({
      req(chat_data())
      plot_avg_message_length(chat_data(), input$authors)
    })
    
    output$evolution_plot <- plotly::renderPlotly({
      req(chat_data())
      plot_message_evolution(chat_data(), input$authors)
    })
    
    # Graphiques de contenu
    output$words_plot <- plotly::renderPlotly({
      req(chat_data())
      plot_most_used_words(chat_data(), input$authors)
    })
    
    output$emojis_plot <- plotly::renderPlotly({
      req(chat_data())
      plot_most_used_emojis(chat_data(), input$authors)
    })
    
  }
  
  shiny::shinyApp(ui, server)
} 