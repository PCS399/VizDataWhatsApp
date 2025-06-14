#' @importFrom shiny shinyApp fluidPage sidebarLayout sidebarPanel mainPanel tabsetPanel tabPanel
#' @importFrom shiny fileInput selectInput plotOutput renderPlot renderText reactive
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom shinyjs runjs

#' Lancer l'application WhatsAppViz
#' @export
run_whatsapp_viz_app <- function() {
  # Définir les styles CSS
  css <- "
    .logo {
      width: 40px;
      height: 40px;
      margin-right: 10px;
      background-color: white;
      border-radius: 50%;
      padding: 5px;
    }
    .header {
      display: flex;
      align-items: center;
      padding: 10px;
      background-color: #128C7E;
      color: white;
      margin-bottom: 20px;
    }
    .footer {
      text-align: center;
      padding: 10px;
      background-color: #128C7E;
      color: white;
      position: fixed;
      bottom: 0;
      width: 100%;
    }
    .sidebar {
      background-color: #f8f9fa;
      padding: 20px;
      border-right: 1px solid #dee2e6;
    }
    .main-content {
      padding: 20px;
    }
    .language-selector {
      position: absolute;
      top: 10px;
      right: 10px;
    }
    .language-btn {
      background-color: white;
      color: #128C7E;
      border: 1px solid #128C7E;
      padding: 5px 10px;
      margin-left: 5px;
      cursor: pointer;
    }
    .language-btn.active {
      background-color: #128C7E;
      color: white;
    }
  "
  
  # Interface utilisateur
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(css)
    ),
    
    # Sélecteur de langue
    shiny::div(
      class = "language-selector",
      shiny::actionButton("lang_fr", "FR", class = "language-btn active"),
      shiny::actionButton("lang_en", "EN", class = "language-btn")
    ),
    
    # En-tête
    shiny::div(
      class = "header",
      shiny::img(src = "logo.svg", class = "logo", alt = "WhatsApp Logo"),
      shiny::h2(shiny::textOutput("app_title"))
    ),
    
    # Layout principal
    shiny::sidebarLayout(
      # Panneau latéral
      shiny::sidebarPanel(
        class = "sidebar",
        shiny::fileInput("file", shiny::textOutput("file_input_label")),
        shiny::selectInput("authors", shiny::textOutput("authors_label"), 
                         choices = NULL, multiple = TRUE)
      ),
      
      # Panneau principal
      shiny::mainPanel(
        class = "main-content",
        shiny::tabsetPanel(
          # Onglet Activité
          shiny::tabPanel(
            shiny::textOutput("activity_tab"),
            shiny::plotlyOutput("weekday_plot"),
            shiny::plotlyOutput("hour_plot")
          ),
          
          # Onglet Auteurs
          shiny::tabPanel(
            shiny::textOutput("authors_tab"),
            shiny::plotlyOutput("lexical_plot"),
            shiny::plotlyOutput("reactions_plot")
          ),
          
          # Onglet Contenu
          shiny::tabPanel(
            shiny::textOutput("content_tab"),
            shiny::plotlyOutput("wordcloud_plot")
          )
        )
      )
    ),
    
    # Pied de page
    shiny::div(
      class = "footer",
      shiny::textOutput("footer_text")
    )
  )
  
  # Logique serveur
  server <- function(input, output, session) {
    # Initialiser les traductions
    translations <- reactive({
      if (input$lang_fr > input$lang_en) {
        get_translations("fr")
      } else {
        get_translations("en")
      }
    })
    
    # Mettre à jour les textes de l'interface
    observe({
      update_ui_translations(session, translations())
    })
    
    # Gérer le changement de langue
    shiny::observeEvent(input$lang_fr, {
      shinyjs::runjs("document.querySelector('.language-btn.active').classList.remove('active')")
      shinyjs::runjs("document.querySelector('#lang_fr').classList.add('active')")
    })
    
    shiny::observeEvent(input$lang_en, {
      shinyjs::runjs("document.querySelector('.language-btn.active').classList.remove('active')")
      shinyjs::runjs("document.querySelector('#lang_en').classList.add('active')")
    })
    
    # Charger les données
    data <- shiny::reactive({
      req(input$file)
      load_whatsapp_data(input$file$datapath)
    })
    
    # Mettre à jour la liste des auteurs
    shiny::observeEvent(data(), {
      authors <- unique(data()$author)
      shiny::updateSelectInput(session, "authors",
                             choices = authors,
                             selected = authors)
    })
    
    # Filtrer les données selon les auteurs sélectionnés
    filtered_data <- shiny::reactive({
      req(data(), input$authors)
      data() %>% dplyr::filter(author %in% input$authors)
    })
    
    # Rendu des graphiques
    output$weekday_plot <- plotly::renderPlotly({
      req(filtered_data())
      plot_activity_by_weekday(filtered_data(), input$authors)
    })
    
    output$hour_plot <- plotly::renderPlotly({
      req(filtered_data())
      plot_activity_by_hour(filtered_data(), input$authors)
    })
    
    output$lexical_plot <- plotly::renderPlotly({
      req(filtered_data())
      plot_lexical_diversity(filtered_data(), input$authors)
    })
    
    output$reactions_plot <- plotly::renderPlotly({
      req(filtered_data())
      plot_message_reactions(filtered_data(), input$authors)
    })
  }
  
  # Lancer l'application
  shiny::shinyApp(ui = ui, server = server)
} 