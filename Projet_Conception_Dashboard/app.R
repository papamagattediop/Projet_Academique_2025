library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(scales)
library(gridExtra)
library(WDI)
library(fresh)
library(sf)


# Fonction pour récupérer les données
load_data <- function() {
  indicators <- c(
    GDP = "NY.GDP.MKTP.CD",
    Exports = "NE.EXP.GNFS.CD",
    Imports = "NE.IMP.GNFS.CD"
  )
  data <- WDI(country = "SN", indicator = indicators, start = 1960, end = 2023)
  data$year <- as.Date(paste0(data$year, "-01-01"))
  if(!("Balance" %in% colnames(data))) {
    data$Balance <- data$Exports - data$Imports
  }
  # Supprime le dernier enregistrement s'il contient des NaN dans les colonnes clés
  if(nrow(data) > 0) {
    num_cols <- c("GDP", "Exports", "Imports")
    if(any(sapply(data[nrow(data), num_cols], is.nan))) {
      data <- data[-nrow(data), ]
    }
  }
  return(data)
}

# Fonction de décomposition (pour séries avec fréquence >= 2)
create_decomposition_plot <- function(ts_data) {
  if(frequency(ts_data) < 2 || length(ts_data) < 2 * frequency(ts_data)){
    stop("Données insuffisantes pour une décomposition saisonnière")
  }
  decomp <- decompose(ts_data)
  plot_data <- data.frame(
    Date = as.Date(time(ts_data), origin = "1970-01-01"),
    Observed = as.numeric(decomp$x),
    Trend = as.numeric(decomp$trend),
    Seasonal = as.numeric(decomp$seasonal),
    Random = as.numeric(decomp$random)
  )
  return(plot_data)
}

# Créer un thème personnalisé avec fresh
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#00796B",
    red        = "#E53935",
    green      = "#43A047",
    yellow     = "#FDD835",
    aqua       = "#29B6F6",
    blue       = "#1E88E5",
    navy       = "#0D47A1"
  ),
  adminlte_sidebar(
    dark_bg       = "#263238",
    dark_hover_bg = "#37474F",
    dark_color    = "#ECEFF1"
  ),
  adminlte_global(
    content_bg   = "#f0f2f5",
    box_bg       = "#ffffff",
    info_box_bg  = "#ffffff"
  )
)

getwd()
# Chargement des données
base_region <- read_excel("Base/base_region.xlsx")

# Correction du nom de colonne si nécessaire (sécurité)
colnames(base_region) <- tolower(colnames(base_region)) # évite les erreurs de casse

# Chargement du shapefile
shapefile <- st_read("Region/sen_admbnda_adm1_anat_20240520.shp")


  
ui <- tagList(
    fresh::use_theme(mytheme),  # Application du thème moderne
    tags$head(
      tags$style(HTML("
      .custom-navbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        width: 100%;
      }
      .navbar-title {
        font-weight: bold;
        color: white;
        font-size: 18px;
      }
      .logo-container img {
        height: 30px;
        margin-left: 10px;
      }
      .navbar {
        background-color: #003366 !important;
      }
      .navbar .navbar-nav > li > a,
      .navbar .navbar-brand {
        color: white !important;
      }
      .navbar .navbar-nav > li > a:hover,
      .navbar .navbar-brand:hover {
        color: #dcdcdc !important;
      }
    "))
    ),
    navbarPage(
      title = div(class = "custom-navbar",
                  span("Richesse du Sénégal", class = "navbar-title"),
                  span(class = "logo-container",
                       img(src = "ansd.jpg"),
                       img(src = "logo-ensae.png")
                  )
      ),
      id = "titre",
      theme = shinytheme("flatly"),
      windowTitle = "Indicateurs Sénégal",
    # Onglet Accueil
    tabPanel("Accueil", 
             # Image d'en-tête dans l'onglet Accueil
             titlePanel(
               div(
                 style = "text-align: center;",
                 tags$video(
                   src = "accueil.mp4",  # Remplace "accueil.mp4" par le nom de ta vidéo
                   style = "width: 100%; max-height: 350px; object-fit: cover; margin-bottom: 0px; border-radius: 10px; box-shadow: 0 6px 12px rgba(0,0,0,0.3);",
                   width = "100%", height = "350px", autoplay = TRUE, loop = TRUE, muted = TRUE
                 )
               ),
               "Sénégal"
             ),
             tags$br(),
             ## Première carte - Évolution du PIB global
             tags$div(class = "card shadow-lg p-4 m-4 border-0 rounded", 
                      style = "background-color: #ffffff; box-shadow: 0 10px 20px rgba(0,0,0,0.19), 0 6px 6px rgba(0,0,0,0.23);",
                      
                      tags$h4("ÉVOLUTION DU PIB GLOBAL DU SÉNÉGAL", class = "text-success mb-4", 
                              style = "color: #007BFF; font-family: 'Times New Roman', Times, serif; text-transform: uppercase; text-shadow: 1px 1px 2px rgba(0,0,0,0.1); padding: 10px; background-color: #e8f4fc; border-left: 5px solid #3498DB; width: 100%; display: block;"),
                      
                      fluidRow(
                        # Colonne gauche : slider
                        column(
                          width = 4,
                          tags$div(
                            class = "bg-light p-4 rounded",
                            style = "box-shadow: inset 0 2px 5px rgba(0,0,0,0.1), 0 4px 8px rgba(0,0,0,0.15); border-left: 4px solid #3498DB;",
                            tags$h5("Période d'analyse", class = "text-primary mb-3", 
                                    style = "font-weight: 500; text-shadow: 0.5px 0.5px 1px rgba(0,0,0,0.1);"),
                            div(style = "padding: 10px 5px;",
                                sliderInput(
                                  inputId = "date_range",
                                  label = NULL,
                                  min = 2010,
                                  max = 2024,
                                  value = c(2016, 2022),
                                  sep = "",
                                  ticks = FALSE,
                                  animate = animationOptions(interval = 800, loop = TRUE),
                                  width = "100%"
                                )
                            ),
                            tags$div(
                              style = "text-align: center; font-size: 0.9em; color: #666; margin-top: 10px;",
                              "Déplacez les curseurs pour ajuster la période"
                            )
                          )
                        ),
                        
                        # Colonne droite : graphique
                        column(
                          width = 8,
                          tags$div(
                            class = "p-3",
                            style = "border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); background: linear-gradient(to bottom, #ffffff, #f9f9f9); border: 1px solid #e0e0e0;",
                            plotlyOutput("plot_pib_global", height = "450px")
                          )
                        )
                      )
             ),
             
             ## Deuxième carte - Contribution des composantes au PIB
             tags$div(class = "card shadow-lg p-4 m-4 border-0 rounded", 
                      style = "background-color: #ffffff; box-shadow: 0 10px 20px rgba(0,0,0,0.19), 0 6px 6px rgba(0,0,0,0.23);",
                      
                      tags$h4("CONTRIBUTION DES COMPOSANTES AU PIB", class = "text-success mb-4", 
                              style = "color: #007BFF; font-family: 'Times New Roman', Times, serif; text-transform: uppercase; text-shadow: 1px 1px 2px rgba(0,0,0,0.1); padding: 10px; background-color: #e8f4fc; border-left: 5px solid #3498DB; width: 100%; display: block;"),
                      
                      fluidRow(
                        # Colonne gauche : sélecteur d'année
                        column(
                          width = 3,
                          tags$div(
                            class = "bg-light p-4 rounded",
                            style = "box-shadow: inset 0 2px 5px rgba(0,0,0,0.1), 0 4px 8px rgba(0,0,0,0.15); border-left: 4px solid #3498DB;",
                            tags$h5("Sélection de l'année", class = "text-primary mb-3", 
                                    style = "font-weight: 500; text-shadow: 0.5px 0.5px 1px rgba(0,0,0,0.1);"),
                            div(style = "padding: 10px 5px;",
                                selectInput(
                                  inputId = "annee_select",
                                  label = NULL,
                                  choices = 2010:2024,
                                  selected = 2023,
                                  width = "100%"
                                )
                            ),
                            tags$div(
                              style = "text-align: center; font-size: 0.9em; color: #666; margin-top: 15px;",
                              "Sélectionnez une année pour voir la répartition"
                            ),
                            hr(),
                            tags$div(
                              style = "text-align: center; margin-top: 20px;",
                              tags$h6("PIB TOTAL", style = "font-family: 'Times New Roman', Times, serif; color: #2C3E50;"),
                              valueBoxOutput("pib_total_box", width = "100%")
                            )
                          )
                        ),
                        
                        # Colonne droite : graphique en camembert
                        column(
                          width = 9,
                          tags$div(
                            class = "p-3",
                            style = "border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); background: linear-gradient(to bottom, #ffffff, #f9f9f9); border: 1px solid #e0e0e0;",
                            fluidRow(
                              column(width = 7,
                                     plotlyOutput("plot_pib_components", height = "450px")  
                              ),
                              column(width = 5,
                                     div(
                                       style = "height: 350px; overflow-y: auto; padding: 10px; box-shadow: inset 0 0 5px rgba(0,0,0,0.1); border-radius: 5px; background-color: #fafafa;",
                                       h5("TOP COMPOSANTES", style = "text-align: center; font-family: 'Times New Roman', Times, serif; color: #3498DB; border-bottom: 2px dotted #3498DB; padding-bottom: 8px;"),
                                       tableOutput("top_components_table")
                                     )
                              )
                            )
                          )
                        )
                      )
             ),
             
   
    
    ## Troisième carte - Évolution d'une composante du PIB
    tags$div(class = "card shadow-lg p-4 m-4 border-0 rounded", 
             style = "background-color: #ffffff; box-shadow: 0 10px 20px rgba(0,0,0,0.19), 0 6px 6px rgba(0,0,0,0.23);",
             
             tags$h4("ÉVOLUTION DES COMPOSANTES DU PIB", class = "text-success mb-4", 
                     style = "color: #007BFF; font-family: 'Times New Roman', Times, serif; text-transform: uppercase; text-shadow: 1px 1px 2px rgba(0,0,0,0.1); padding: 10px; background-color: #e8f4fc; border-left: 5px solid #3498DB; width: 100%; display: block;"),
             
             fluidRow(
               # Colonne gauche : sélecteurs
               column(
                 width = 4,
                 tags$div(
                   class = "bg-light p-4 rounded",
                   style = "box-shadow: inset 0 2px 5px rgba(0,0,0,0.1), 0 4px 8px rgba(0,0,0,0.15); border-left: 4px solid #3498DB;",
                   
                   tags$h5("Choisissez une composante", class = "text-primary mb-3",
                           style = "font-weight: 500; text-shadow: 0.5px 0.5px 1px rgba(0,0,0,0.1);"),
                   
                   checkboxGroupInput(
                     inputId = "composantes_select",
                     label = "Choisissez les composantes :",
                     choices = NULL,
                     selected = NULL
                   ),
                   
                   tags$br(),
                   tags$h5("Période d'analyse", class = "text-primary mb-3",
                           style = "font-weight: 500; text-shadow: 0.5px 0.5px 1px rgba(0,0,0,0.1);"),
                   sliderInput(
                     inputId = "compo_date_range",
                     label = NULL,
                     min = 2010,
                     max = 2024,
                     value = c(2018, 2023),
                     sep = "",
                     ticks = FALSE,
                     animate = animationOptions(interval = 800, loop = TRUE),
                     width = "100%"
                   )
                 )
               ),
               
               # Colonne droite : graphique
               column(
                 width = 8,
                 plotlyOutput("plot_composante_evo", height = "500px")
               )
             )
             
    )
    
    ),
    
    # Onglet Régions
    tabPanel("Régions",
             tabsetPanel(
               tabPanel(
                 "Vue D'ensemble",  
                 h3("Carte du Sénégal en fonction des agrégats économiques de 2020 à 2022", align = "center"),
                 br(),
                 div(style = "display:vertical-align:center;center-align",
                     fluidRow(
                       column(4,
                              selectInput("agregats", "Choisis un agrégat",
                                          choices = c("PIB en valeur", "Production en valeur", "Taux de croissance"),
                                          selected = "PIB en valeur",
                                          width = 400)),
                       column(4,
                              selectInput("annee", "Choisis une Année",
                                          choices = unique(base_region$année),
                                          selected = "2020",
                                          width = 400)),
                       column(4,
                              tags$br(),
                              actionButton("filtre", "Filtre", class = "btn btn-warning btn-sm"))
                     )
                 ),
                 tags$br(),
                 tags$br(),
                 tags$hr(),
                 tags$br(),
                 fluidRow(
                   column(12, 
                          h5("Carte du Sénégal", align = "center"), 
                          plotOutput(outputId = "carte", height = "700px", width = "100%"))
                 )
               ),
               
               tabPanel("Tableau de bord",
                        tags$head(
                          tags$style(HTML("
                          .dashboard-title {
                            text-align: center; 
                            font-weight: bold; 
                            color: #2c3e50; 
                            margin-top: 20px;
                            margin-bottom: 20px;
                          }
                          .filter-section {
                            background-color: #f9f9f9;
                            padding: 15px;
                            border-radius: 5px;
                            margin-bottom: 20px;
                          }
                        "))
                        ),
                        
                        div(
                          h2("Tableau de bord : Apport en valeur ajoutée des régions du Sénégal (2020-2022)", 
                             class = "dashboard-title")
                        ),
                        
                        div(class = "filter-section",
                            fluidRow(
                              column(4,
                                     selectInput("agregats", "Choisis un agrégat",
                                                 choices = c("PIB en valeur", "Production en Valeur", "Taux de croissance"),
                                                 selected = "PIB en valeur")
                              ),
                              column(4,
                                     selectInput("annee", "Choisis une Année",
                                                 choices = c("2020", "2021", "2022"),
                                                 selected = "2020")
                              ),
                              column(4,
                                     div(style = "margin-top: 25px;",
                                         actionButton("filtre", "Filtrer", class = "btn btn-warning btn-block"))
                              )
                            )
                        ),
                        
                        fluidRow(
                          column(12,
                                 box(
                                   title = "Distribution circulaire des secteurs",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotlyOutput("circlePlot", height = "450px")
                                 )
                          )
                        ),
                        tags$br(),
                        fluidRow(
                          column(4,
                                 box(
                                   title = "Valeur ajoutée des régions par secteur",
                                   status = "info",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("graph1", height = "300px")
                                 )
                          ),
                          column(4,
                                 box(
                                   title = "Production par région",
                                   status = "success",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("graph2", height = "300px")
                                 )
                          ),
                          column(4,
                                 box(
                                   title = "Évolution de la valeur ajoutée",
                                   status = "warning",
                                   solidHeader = TRUE,
                                   width = 12,
                                   plotOutput("graph3", height = "300px")
                                 )
                          )
                        )
               )
             )
    ),
    
    tabPanel(title = tagList(icon("globe-africa"), "Secteurs"),
                             sidebarLayout(
                               sidebarPanel(
                                 fileInput("file", "Importer le fichier Excel", accept = c(".xlsx", ".xls")),
                                 conditionalPanel(
                                   condition = "output.dataLoaded",
                                   sliderInput("year_range", "Période d'analyse:", min = 2014, max = 2023, value = c(2014, 2023), step = 1),
                                   radioButtons("chart_type", "Type de graphique:",
                                                choices = c("Lignes" = "line", "Barres" = "bar", "Aires empilées" = "area"),
                                                selected = "line"),
                                   checkboxGroupInput("sectors_to_show", "Secteurs à afficher:",
                                                      choices = c("Primaire", "Secondaire", "Tertiaire"),
                                                      selected = c("Primaire", "Secondaire", "Tertiaire")),
                                   actionButton("reset", "Réinitialiser les filtres", icon = icon("refresh"), class = "btn-primary")
                                 )
                               ),
                               mainPanel(
                                 conditionalPanel(
                                   condition = "!output.dataLoaded",
                                   h4("Importez un fichier Excel")
                                 ),
                                 conditionalPanel(
                                   condition = "output.dataLoaded",
                                   tabsetPanel(
                                     tabPanel("Répartition par Secteur",
                                              fluidRow(column(12, h4("Évolution de la Valeur Ajoutée"), plotlyOutput("evolution_plot", height = "400px"))),
                                              fluidRow(column(10, h4("Comparaison par année"), plotlyOutput("yearly_comparison", height = "350px"))),
                                              fluidRow(column(12, h4("Taux de croissance annuel"), plotlyOutput("growth_plot", height = "350px")))),
                                     tabPanel("Base de donnée",
                                              fluidRow(
                                                column(12, h4("Détail par secteur")),
                                                column(12, DTOutput("sector_table"))
                                              )
                                     )
                                   )
                                 )
                               )
                             )
    ),
    
    
    # Onglet Prévisions
    # Onglet Prévisions
    tabPanel('Prevision',
             tabsetPanel(
               tabPanel("Vue d'ensemble",
                        fluidRow(
                          valueBoxOutput("gdp_box", width = 4),
                          valueBoxOutput("export_box", width = 4),
                          valueBoxOutput("import_box", width = 4)
                        ),
                        fluidRow(
                          box(
                            title = "Évolution des indicateurs économiques", 
                            status = "primary", solidHeader = TRUE, 
                            plotlyOutput("trend_plot"), width = 12
                          )
                        ),
                        fluidRow(
                          box(
                            title = "Balance Commerciale", 
                            status = "danger", solidHeader = TRUE, 
                            plotlyOutput("balance_plot"), width = 6
                          ),
                          box(
                            title = "Croissance du PIB", 
                            status = "success", solidHeader = TRUE, 
                            plotlyOutput("prevision_growth_plot"), width = 6
                          )
                        )
               ),
               tabPanel("📊 Prévisions",
                        fluidRow(
                          box(
                            width = 4,
                            selectInput("var_select", "Variable :", 
                                        choices = c("PIB" = "GDP", 
                                                    "Exportations" = "Exports", 
                                                    "Importations" = "Imports", 
                                                    "Balance commerciale" = "Balance")
                            ),
                            selectInput("model_select", " Modèle :", 
                                        choices = c("ARIMA " = "arima",
                                                    "Prophet " = "prophet",
                                                    "ETS " = "ets",
                                                    "Naïve ♂️" = "naive",
                                                    "Theta " = "theta")
                            ),
                            numericInput("horizon", "Horizon de prévision (années) :", value = 5, min = 1, max = 10),
                            actionButton("forecast_btn", " Générer prévisions", class = "btn btn-primary")
                          ),
                          box(
                            width = 8,
                            solidHeader = TRUE, status = "success", 
                            title = "📊 Prévisions", 
                            plotlyOutput("forecast_plot")
                          )
                        ),
                        fluidRow(
                          box(
                            width = 12, solidHeader = TRUE, status = "info",
                            title = " Métriques de prévision 📏", 
                            DTOutput("forecast_metrics")
                          )
                        )
               )
             ),
    
    
  
  # --- FOOTER COMPLET DYNAMIQUE ---
  tags$div(
    id = "footer",
    class = "footer-hidden",
    tags$div(
      style = "display: flex; justify-content: space-around; flex-wrap: wrap; align-items: center;",
      
      # Colonne 1 : Contacts
      tags$div(
        style = "text-align: left;",
        tags$h5("Contacts", style = "color: white;"),
        tags$ul(
          style = "list-style: none; padding-left: 0;",
          tags$li(HTML("<i class='fa fa-envelope'></i> fatima.bah@ensae.sn")),
          tags$li(HTML("<i class='fa fa-phone'></i> +221 77 123 45 67")),
          tags$li(HTML("<i class='fa fa-linkedin'></i> <a href='https://www.linkedin.com/in/fatima-bah' style='color: #dcdcdc;' target='_blank'>Fatima Bah</a>"))
        )
      ),
      
      # Colonne 2 : À propos
      tags$div(
        style = "text-align: center; max-width: 300px;",
        tags$h5("À propos du projet", style = "color: white;"),
        tags$p("Application développée dans le cadre d’un projet de fin d’année à l’ENSAE de Dakar. Objectif : visualiser les indicateurs économiques du Sénégal.",
               style = "color: #dcdcdc;")
      ),
      
      # Colonne 3 : Références
      tags$div(
        style = "text-align: right;",
        tags$h5("Références & Partenaires", style = "color: white;"),
        tags$ul(
          style = "list-style: none; padding-left: 0;",
          tags$li("ANSD - Agence Nationale de la Statistique"),
          tags$li("Ministère de l'Économie"),
          tags$li("ENSAE Dakar")
        )
      )
    ),
    tags$hr(style = "border-top: 1px solid white; margin-top: 10px;"),
    tags$p(HTML(
      "© 2024 - Richesse du Sénégal | Réalisé par <strong>Fatima Bah</strong> |
     <a href='mailto:fatima.bah@ensae.sn' style='color: #dcdcdc;'>Contact</a> |
     <a href='https://www.linkedin.com/in/fatima-bah' target='_blank' style='color: #dcdcdc;'>LinkedIn</a> |
     <a href='https://www.exemple.com' target='_blank' style='color: #dcdcdc;'>Site web</a>"
    ), style = "text-align: center; color: #dcdcdc; margin-bottom: 0;")
  ),
  
  # --- STYLE ---
  tags$style(HTML("
  #footer {
    position: fixed;
    bottom: 0;
    left: 0;
    width: 100%;
    background-color: #003366;
    color: white;
    text-align: center;
    padding: 15px 10px 5px 10px;
    font-size: 14px;
    transform: translateY(100%);
    transition: transform 0.3s ease-in-out;
    z-index: 1000;
  }

  #footer a {
    text-decoration: none;
    margin: 0 8px;
  }

  #footer a:hover {
    text-decoration: underline;
  }

  #footer.show {
    transform: translateY(0);
  }
")),
  
  # --- JS ---
  tags$script(HTML("
  document.addEventListener('mousemove', function(e) {
    var windowHeight = window.innerHeight;
    var y = e.clientY;
    var footer = document.getElementById('footer');

    if (windowHeight - y < 50) {
      footer.classList.add('show');
    } else {
      footer.classList.remove('show');
    }
  });
"))
)
)
)

# Chargement des bases
#* Lecture de la base (adapte le chemin si nécessaire)



# Serveur
server <- function(input, output, session) {
  # Données VA
  data_va <- reactive({
    if (!is.null(input$file)) {
      tryCatch({
        df <- read_excel(input$file$datapath, sheet = "VA")
        if (!"SECTEURS" %in% colnames(df)) stop("Colonne 'SECTEURS' manquante")
        df
      }, error = function(e) {
        showNotification(paste("Erreur:", e$message), type = "error")
        NULL
      })
    } else {
      tryCatch({
        df <- read_excel("Base/TRE_valeur_2014-2023.xlsx", sheet = "VA")
        if (!"SECTEURS" %in% colnames(df)) stop("Colonne 'SECTEURS' manquante dans le fichier par défaut")
        df
      }, error = function(e) {
        showNotification(paste("Erreur fichier par défaut:", e$message), type = "error")
        NULL
      })
    }
  })
  
  processed_data <- reactive({
    req(data_va())
    primaire <- c("AGRICULTURE", "ELEVAGE ET CHASSE", "SYLVICULTURE", "PECHE ET AQUACULTURE", "ACTIVITES EXTRACTIVES")
    secondaire <- c("PRODUITS AGRO-ALIMENTAIRES", "RAFFINAGE DU PETROLE", "PRODUITS CHIMIQUES", "MATERIAUX DE CONSTRUCTION", "AUTRES PRODUITS MANUFACTURES", "ELECTRICITE ET GAZ", "DISTRIBUTION D'EAU", "CONSTRUCTION")
    tertiaire <- c("COMMERCE", "TRANSPORTS", "HEBERGEMENT ET RESTAURATION", "INFORMATION ET COMMUNICATION", "ACTIVITES FINANCIERES", "ACTIVITES IMMOBILIERES", "ACTIVITES SPECIALISEES", "SERVICES DE SOUTIEN", "ADMINISTRATION PUBLIQUE", "ENSEIGNEMENT", "SANTE ET ACTION SOCIALE", "ACTIVITES ARTISTIQUES", "SERVICES DOMESTIQUES", "AUTRES SERVICES")
    
    df <- data_va()
    year_cols <- as.character(2014:2023)
    for (col in year_cols) if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
    
    df <- df %>% mutate(SECTEUR_ECO = case_when(
      SECTEURS %in% primaire ~ "Primaire",
      SECTEURS %in% secondaire ~ "Secondaire",
      SECTEURS %in% tertiaire ~ "Tertiaire",
      TRUE ~ "Autre"
    ))
    
    df_long <- df %>%
      pivot_longer(cols = year_cols, names_to = "ANNEE", values_to = "VALEUR_AJOUTEE") %>%
      mutate(ANNEE = as.numeric(ANNEE))
    
    list(df = df, df_long = df_long)
  })
  
  aggregated_data <- reactive({
    req(processed_data())
    df_long <- processed_data()$df_long %>%
      filter(ANNEE >= input$year_range[1], ANNEE <= input$year_range[2]) %>%
      filter(SECTEUR_ECO %in% input$sectors_to_show)
    
    df_agg <- df_long %>%
      group_by(SECTEUR_ECO, ANNEE) %>%
      summarise(VALEUR_AJOUTEE = sum(VALEUR_AJOUTEE, na.rm = TRUE), .groups = "drop")
    
    df_growth <- df_agg %>%
      arrange(SECTEUR_ECO, ANNEE) %>%
      group_by(SECTEUR_ECO) %>%
      mutate(CROISSANCE = (VALEUR_AJOUTEE / lag(VALEUR_AJOUTEE) - 1) * 100) %>%
      ungroup()
    
    list(aggregated = df_agg, growth = df_growth)
  })
  
  output$dataLoaded <- reactive({ !is.null(data_va()) })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  observeEvent(input$reset, {
    updateSliderInput(session, "year_range", value = c(2014, 2023))
    updateRadioButtons(session, "chart_type", selected = "line")
    updateCheckboxGroupInput(session, "sectors_to_show", selected = c("Primaire", "Secondaire", "Tertiaire"))
  })
  
  output$evolution_plot <- renderPlotly({
    req(aggregated_data())
    df <- aggregated_data()$aggregated
    
    p <- switch(input$chart_type,
                "line" = ggplot(df, aes(x = ANNEE, y = VALEUR_AJOUTEE, color = SECTEUR_ECO)) +
                  geom_line(size = 1.2) +
                  geom_point(size = 3),
                "bar"  = ggplot(df, aes(x = ANNEE, y = VALEUR_AJOUTEE, fill = SECTEUR_ECO)) +
                  geom_bar(stat = "identity", position = "dodge"),
                ggplot(df, aes(x = ANNEE, y = VALEUR_AJOUTEE, fill = SECTEUR_ECO)) +
                  geom_area(alpha = 0.8, position = "stack")
    )
    
    p <- p +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top")
    ggplotly(p)
  })
  
  output$yearly_comparison <- renderPlotly({
    req(aggregated_data())
    df <- aggregated_data()$aggregated %>% filter(ANNEE %in% range(ANNEE))
    p <- ggplot(df, aes(x = SECTEUR_ECO, y = VALEUR_AJOUTEE, fill = factor(ANNEE))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      theme_minimal() + theme(legend.position = "top")
    ggplotly(p)
  })
  
  output$growth_plot <- renderPlotly({
    req(aggregated_data())
    df <- aggregated_data()$growth %>% filter(!is.na(CROISSANCE))
    p <- ggplot(df, aes(x = ANNEE, y = CROISSANCE, color = SECTEUR_ECO)) +
      geom_line(size = 1.2) + geom_point(size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45), legend.position = "top")
    ggplotly(p)
  })
  
  output$sector_table <- renderDT({
    req(processed_data())
    df <- processed_data()$df %>%
      select(SECTEURS, SECTEUR_ECO, as.character(input$year_range[1]:input$year_range[2]))
    datatable(df, options = list(pageLength = 10, searchHighlight = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = as.character(input$year_range[1]:input$year_range[2]), digits = 0)
  })
  
  #*****************ACCEUIL**************************
  data <- read_excel("Base/PIB_DEPENSES.xlsx")
  
  # Premier graphique: évolution du PIB global
  output$plot_pib_global <- renderPlotly({
    # On filtre les données sur la plage d'années sélectionnée
    data_filtered <- data[data$Annee >= input$date_range[1] & data$Annee <= input$date_range[2], ]
    
    # On conserve uniquement une ligne par année avec le pib_global 
    pib_annee <- data_filtered |>
      dplyr::select(Annee, pib_global) |>
      dplyr::distinct()
    
    # Tracer avec plotly - version améliorée
    plot_ly(pib_annee, x = ~Annee, y = ~pib_global, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#18BC9C', width = 3),
            marker = list(color = '#2C3E50', size = 8),
            hoverinfo = 'text',
            text = ~paste('Année:', Annee, '<br>PIB:', format(pib_global, big.mark = ' ', 
                                                              decimal.mark = ',', scientific = FALSE), 'FCFA')) %>%
      layout(
        title = "",
        xaxis = list(title = "Années", tickfont = list(size = 11)),
        yaxis = list(title = "PIB Global (en milliards FCFA)", 
                     tickfont = list(size = 11)),
        hoverlabel = list(bgcolor = "#FFF", font = list(size = 12)),
        plot_bgcolor = '#fafafa',
        paper_bgcolor = '#fafafa',
        showlegend = FALSE
      )
  })
  
  
  # Affichage du PIB total pour l'année sélectionnée
  output$pib_total_box <- renderValueBox({
    # Filtrer pour l'année sélectionnée
    data_year <- data %>% 
      filter(Annee == input$annee_select) %>%
      select(pib_global) %>%
      distinct()
    
    # Format du PIB avec séparateurs de milliers
    pib_value <- format(data_year$pib_global[1], big.mark = " ", scientific = FALSE)
    
    valueBox(
      value = paste0(pib_value, " Milliards de FCFA"),
      subtitle = paste("Année", input$annee_select),
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  # Graphique des composantes du PIB pour l'année sélectionnée
  output$plot_pib_components <- renderPlotly({
    # Filtrer par année
    data_year <- data %>%
      filter(Annee == input$annee_select)
    
    # Calculer le pourcentage pour chaque composante
    data_year <- data_year %>%
      mutate(percentage = part_pib / unique(pib_global) * 100)
    
    # Graphique en camembert
    plot_ly(data_year, labels = ~Composante, values = ~part_pib, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(Composante, '<br>',
                          format(part_pib, big.mark = ' ', decimal.mark = ',', scientific = FALSE), ' FCFA<br>',
                          round(percentage, 1), '%'),
            marker = list(
              line = list(color = '#FFFFFF', width = 1),
              colors = colorRampPalette(c("#18BC9C", "#2C3E50", "#3498DB", "#E74C3C", "#F39C12"))(nrow(data_year))
            )) %>%
      layout(
        title = list(text = paste("Répartition du PIB -", input$annee_select), 
                     font = list(family = "Times New Roman", size = 16)),
        showlegend = FALSE, # Nous n'affichons pas la légende car on a déjà les labels dans le camembert
        margin = list(l = 20, r = 20, b = 20, t = 50),
        paper_bgcolor = '#fafafa',
        plot_bgcolor = '#fafafa'
      )
  })
  
  # Tableau des principales composantes
  output$top_components_table <- renderTable({
    # Filtrer par année
    data_year <- data %>%
      filter(Annee == input$annee_select)
    
    # Calculer le pourcentage pour chaque composante
    data_year <- data_year %>%
      mutate(percentage = part_pib / unique(pib_global) * 100) %>%
      arrange(desc(part_pib)) %>%
      select(Composante, part_pib, percentage) %>%
      head(10) # Top 10 composantes
    
    # Formater les valeurs pour l'affichage
    data_year$part_pib <- format(data_year$part_pib, big.mark = " ", scientific = FALSE) 
    data_year$percentage <- paste0(round(data_year$percentage, 2), "%")
    
    # Renommer les colonnes pour l'affichage
    names(data_year) <- c("Composante", "Valeur (Milliards de FCFA)", "% du PIB")
    
    data_year
  }, 
  striped = TRUE, 
  hover = TRUE,
  spacing = "m",
  align = "c",
  digits = 0)
  
  # Mise à jour des composantes disponibles (liste unique)
  observe({
    updateCheckboxGroupInput(
      session,
      inputId = "composantes_select",
      choices = unique(data$Composante),  
      selected = unique(data$Composante)[1:3]
    )
  })
  
  observe({
    updateCheckboxGroupInput(
      session,
      inputId = "composantes_select",
      choices = unique(data$Composante),
      selected = unique(data$Composante)[1:3]
    )
  })
  
  output$plot_composante_evo <- renderPlotly({
    req(input$composantes_select, input$compo_date_range)
    
    data_compo <- data %>%
      filter(
        Composante %in% input$composantes_select,
        Annee >= input$compo_date_range[1],
        Annee <= input$compo_date_range[2]
      )
    
    plot_ly(data_compo, x = ~Annee, y = ~part_pib, color = ~Composante, type = 'scatter', mode = 'lines+markers',
            line = list(width = 3),
            marker = list(size = 6),
            hoverinfo = 'text',
            text = ~paste("Composante :", Composante,
                          "<br>Année :", Annee,
                          "<br>Valeur :", format(part_pib, big.mark = " ", decimal.mark = ",", scientific = FALSE), " FCFA")) %>%
      layout(
        
        xaxis = list(title = "Années"),
        yaxis = list(title = "Part du PIB (en milliards FCFA)"),
        hoverlabel = list(bgcolor = "#FFF", font = list(size = 12)),
        plot_bgcolor = '#ffffff',
        paper_bgcolor = '#ffffff'
      )
  })
  
  observeEvent(input$filtre, {
    req(input$agregats, input$annee)
    
    data <- base_region %>%
      rename(region = region,
             annee = année,
             pib = `pib en valeur`,
             production = `production en valeur`,
             croissance = `taux de croissance`) %>%
      mutate(
        pib = as.numeric(gsub(",", "", pib)),
        production = as.numeric(gsub(",", "", production)),
        croissance = as.numeric(gsub(",", ".", gsub("%", "", croissance)))
      )
    
    data_filtered <- data %>% filter(annee == input$annee)
    
    var_agregat <- switch(input$agregats,
                          "PIB en valeur" = data_filtered$pib,
                          "Production en valeur" = data_filtered$production,
                          "Taux de croissance" = data_filtered$croissance)
    
    if (all(is.na(var_agregat))) {
      showNotification("Agrégat invalide ou données manquantes pour cette année", type = "error")
      return(NULL)
    }
    
    # Fusion avec shapefile
    map_data <- shapefile %>%
      st_make_valid() %>%
      left_join(data_filtered, by = c("ADM1_FR" = "region"))
    
    # Utiliser st_point_on_surface (plus sûr que st_centroid)
    points_surface <- st_point_on_surface(map_data$geometry)
    coords <- st_coordinates(points_surface)
    
    map_data$X <- coords[, 1]
    map_data$Y <- coords[, 2]
    map_data$agr_val <- var_agregat
    
    output$carte <- renderPlot({
      ggplot() +
        geom_sf(data = shapefile, fill = "gray90", color = "black") +
        
        # Cercles proportionnels
        geom_point(data = map_data,
                   aes(x = X, y = Y, size = agr_val),
                   fill = "darkred", color = "black", alpha = 0.6, shape = 21) +
        
        # Étiquettes
        geom_text(data = map_data,
                  aes(x = X, y = Y, 
                      label = paste0(ADM1_FR, "\n", round(agr_val, 1))),
                  size = 3.5, fontface = "bold", color = "black", vjust = -1) +
        
        scale_size_continuous(name = input$agregats, range = c(5, 30)) +
        
        labs(title = paste("Carte du Sénégal -", input$agregats, "en", input$annee)) +
        
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "right"
        )
    })
  })
  
  # Chargement de la base
  VA_region <- read_excel("Base/VA_region.xlsx")
  
  output$graph1 <- renderPlot({
    # Filtrage selon l'année choisie dans l'input utilisateur
    data <- VA_region %>% 
      filter(Année == input$annee) %>% 
      select(-Année)
    
    # Tri et transformation de la variable Région
    data <- data %>%
      mutate(Région = factor(Région, levels = unique(Région)))
    
    # Affichage du graphique
    ggplot(data, aes(x = Production, 
                     y = `Valeur ajoutée`, 
                     size = `Production`, 
                     color = Secteur)) +
      geom_point(alpha = 0.6) +
      scale_size(range = c(2, 15), name = "Valeur ajoutée") +
      theme_minimal() +
      labs(x = "Production",
           y = "Valeur ajoutée")
  })
  
  # Graphique 2 : Production totale par région
  output$graph2 <- renderPlot({
    df <- VA_region %>% 
      filter(Année == input$annee)
    
    ggplot(df, aes(x = Production, y = reorder(Région, Production))) +
      geom_col(fill = "#4E79A7") +
      theme_minimal() +
      labs(
        title = paste("Production totale par région en", input$annee),
        x = "Production",
        y = "Région"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)
      )
  })
  
  # Graphique 3 : Évolution de la valeur ajoutée par secteur (sur toutes les années)
  output$graph3 <- renderPlot({
    df <- VA_region %>%
      group_by(Année, Secteur) %>%
      summarise(valeur_totale = sum(`Valeur ajoutée`, na.rm = TRUE))
    
    ggplot(df, aes(x = Année, y = valeur_totale, color = Secteur, group = Secteur)) +
      geom_line(size = 1.2) +
      geom_point() +
      theme_minimal() +
      labs(title = "Évolution de la valeur ajoutée par secteur",
           x = "Année", y = "Valeur ajoutée")
  })
  
  # Graphique 4 : Part des secteurs dans la valeur ajoutée (camembert)
  output$circlePlot <- renderPlotly({
    data <- VA_region %>% 
      filter(Année == input$annee)
    
    # S'assurer qu'il y a des données à afficher
    if(is.null(data) || nrow(data) == 0) {
      return(plot_ly() %>% 
               layout(title = "Aucune donnée disponible pour l'affichage"))
    }
    
    # Obtenir la liste des régions
    regions <- unique(data$Région)
    
    # Créer des dataframes pour chaque secteur
    secteur_primaire <- data %>% 
      filter(Secteur == "Primaire") %>% 
      arrange(Région)
    
    secteur_secondaire <- data %>% 
      filter(Secteur == "Secondaire") %>% 
      arrange(Région)
    
    secteur_tertiaire <- data %>% 
      filter(Secteur == "Tertiaire") %>% 
      arrange(Région)
    
    # Créer un graphique circulaire
    fig <- plot_ly()
    
    # Calculer les positions angulaires pour chaque région
    n_regions <- length(regions)
    
    # Définir les secteurs et leurs caractéristiques (ordre corrigé et plus cohérent)
    secteurs <- list(
      A = list(
        nom = "Primaire",
        couleur = "#FF7F7F",
        données = secteur_primaire,
        angle_debut = 0,
        angle_fin = 90
      ),
      B = list(
        nom = "Secondaire",
        couleur = "#7FBF7F",
        données = secteur_secondaire,
        angle_debut = 90,
        angle_fin = 180
      ),
      C = list(
        nom = "Tertiaire",
        couleur = "#7F7FFF",
        données = secteur_tertiaire,
        angle_debut = 180,
        angle_fin = 270
      )
    )
    
    # Échelle de normalisation pour l'affichage uniforme
    max_value <- max(c(
      max(secteur_primaire$`Valeur ajoutée`, na.rm = TRUE),
      max(secteur_secondaire$`Valeur ajoutée`, na.rm = TRUE),
      max(secteur_tertiaire$`Valeur ajoutée`, na.rm = TRUE)
    ))
    
    # Pour chaque secteur avec des données réelles
    for (secteur_id in c("A", "B", "C")) {
      secteur <- secteurs[[secteur_id]]
      
      if (!is.null(secteur$données) && nrow(secteur$données) > 0) {
        # Données du secteur
        df <- secteur$données
        
        # Nombre de régions dans ce secteur
        n <- nrow(df)
        
        # Calculer l'angle pour chaque région dans ce secteur
        angles_secteur <- seq(
          secteur$angle_debut, 
          secteur$angle_fin, 
          length.out = n + 1
        )[1:n]
        
        # Ajouter chaque barre
        for (i in 1:n) {
          # Gérer les valeurs NA
          if (is.na(df$`Valeur ajoutée`[i])) next
          
          # Valeur normalisée (pour l'affichage)
          valeur_norm <- df$`Valeur ajoutée`[i] / max_value * 90
          
          # Minimum pour que les petites barres soient visibles
          valeur_norm <- max(valeur_norm, 5)
          
          # Points pour dessiner la barre
          angle <- angles_secteur[i]
          
          # Coordonnées x et y du début (rayon intérieur)
          rayon_interieur <- 0.4
          x0 <- 0.5 + cos(angle * pi / 180) * rayon_interieur
          y0 <- 0.5 + sin(angle * pi / 180) * rayon_interieur
          
          # Coordonnées x et y de la fin (rayon extérieur basé sur la valeur)
          x1 <- 0.5 + cos(angle * pi / 180) * (rayon_interieur + valeur_norm / 100)
          y1 <- 0.5 + sin(angle * pi / 180) * (rayon_interieur + valeur_norm / 100)
          
          # Ajouter la barre
          fig <- add_trace(
            fig,
            type = "scatter",
            mode = "lines",
            x = c(x0, x1),
            y = c(y0, y1),
            line = list(
              color = secteur$couleur,
              width = 10
            ),
            showlegend = FALSE,
            hoverinfo = "text",
            text = paste0(
              "Région: ", df$Région[i], "<br>",
              "Secteur: ", secteur$nom, "<br>",
              "Valeur ajoutée: ", format(df$`Valeur ajoutée`[i], big.mark = " ")
            )
          )
          
          # Ajouter étiquette de région pour les barres suffisamment grandes
          if (valeur_norm > 15) {
            x_label <- 0.5 + cos(angle * pi / 180) * (rayon_interieur + valeur_norm / 100 + 0.05)
            y_label <- 0.5 + sin(angle * pi / 180) * (rayon_interieur + valeur_norm / 100 + 0.05)
            
            fig <- add_annotations(
              fig,
              x = x_label,
              y = y_label,
              text = df$Région[i],
              showarrow = FALSE,
              font = list(
                size = 9,
                color = "black"
              ),
              align = "center"
            )
          }
        }
      }
    }
    
    # Ajouter annotations pour les secteurs (positions corrigées)
    secteur_annotations <- list(
      list(
        x = 0.75, y = 0.75,
        text = "A",
        showarrow = FALSE,
        font = list(size = 20, color = "#FF7F7F")
      ),
      list(
        x = 0.75, y = 0.25,
        text = "B",
        showarrow = FALSE,
        font = list(size = 20, color = "#7FBF7F")
      ),
      list(
        x = 0.25, y = 0.25,
        text = "C",
        showarrow = FALSE,
        font = list(size = 20, color = "#7F7FFF")
      )
    )
    
    # Configuration du graphique
    fig <- layout(
      fig,
      title = paste("Valeur ajoutée par région et secteur -", input$annee),
      showlegend = FALSE,
      annotations = secteur_annotations,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        range = c(0, 1)
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE, 
        showticklabels = FALSE,
        range = c(0, 1),
        scaleanchor = "x"
      ),
      plot_bgcolor = "rgba(240, 240, 240, 0.1)",
      shapes = list(
        # Cercle central
        list(
          type = "circle",
          x0 = 0.3, x1 = 0.7,
          y0 = 0.3, y1 = 0.7,
          fillcolor = "white",
          line = list(color = "gray", width = 0.5)
        )
      )
    )
    
    return(fig)
  })
  
  # Chargement des données
  data_wdi <- reactive({ load_data() })
  
  # Value boxes
  output$gdp_box <- renderValueBox({
    df <- data_wdi()
    valueBox(
      paste0(round(tail(df$GDP, 1) / 1e9, 1), " Mrd USD"),
      "PIB",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$export_box <- renderValueBox({
    df <- data()
    valueBox(
      paste0(round(tail(df$Exports, 1) / 1e9, 1), " Mrd USD"),
      "Exportations", 
      icon = icon("ship"),
      color = "blue"
    )
  })
  
  output$import_box <- renderValueBox({
    df <- data_wdi()
    valueBox(
      paste0(round(tail(df$Imports, 1) / 1e9, 1), " Mrd USD"),
      "Importations",
      icon = icon("truck"),
      color = "red"
    )
  })
  
  # Graphique des tendances
  output$trend_plot <- renderPlotly({
    df <- data_wdi()
    plot_ly(data = df) %>%
      add_lines(x = ~year, y = ~GDP / 1e9, name = "PIB", line = list(color = "green")) %>%
      add_lines(x = ~year, y = ~Exports / 1e9, name = "Exportations", line = list(color = "blue")) %>%
      add_lines(x = ~year, y = ~Imports / 1e9, name = "Importations", line = list(color = "red")) %>%
      layout(title = "Évolution des indicateurs économiques",
             xaxis = list(title = "Année"),
             yaxis = list(title = "Valeurs (Mrd USD)"))
  })
  
  # Graphique de la balance commerciale
  output$balance_plot <- renderPlotly({
    df <- data_wdi()
    if(!("Balance" %in% colnames(df))) df$Balance <- df$Exports - df$Imports
    plot_ly(data = df, x = ~year, y = ~Balance, type = "scatter", mode = "lines+markers",
            name = "Balance", line = list(color = "purple")) %>%
      layout(title = "Balance Commerciale (Exports - Imports)",
             xaxis = list(title = "Année"),
             yaxis = list(title = "Balance en USD"))
  })
  
  # Croissance approximative du PIB
  output$prevision_growth_plot <- renderPlotly({
    df <- data_wdi()
    growth <- c(NA, diff(df$GDP) / head(df$GDP, -1) * 100)
    plot_ly(x = df$year, y = growth, type = "scatter", mode = "lines+markers",
            name = "Croissance (%)", line = list(color = "orange")) %>%
      layout(title = "Croissance annuelle du PIB",
             xaxis = list(title = "Année"),
             yaxis = list(title = "Pourcentage"))
  })
  
  # Prévisions
  forecast_results <- eventReactive(input$forecast_btn, {
    req(input$var_select, input$model_select)
    df <- data_wdi()
    ts_data <- ts(df[[input$var_select]], start = as.numeric(format(min(df$year), "%Y")), frequency = 1)
    
    if(input$model_select == "prophet"){
      df_prophet <- data.frame(ds = df$year, y = df[[input$var_select]])
      m <- prophet(df_prophet, weekly.seasonality = FALSE, daily.seasonality = FALSE)
      future <- make_future_dataframe(m, periods = input$horizon, freq = "year")
      fcst <- predict(m, future)
      return(list(model = "prophet", forecast = fcst, ts_data = ts_data))
    } else if(input$model_select == "ets"){
      model <- ets(ts_data)
      fcst <- forecast(model, h = input$horizon)
      return(list(model = "other", forecast = fcst, ts_data = ts_data))
    } else if(input$model_select == "naive"){
      fcst <- naive(ts_data, h = input$horizon)
      return(list(model = "other", forecast = fcst, ts_data = ts_data))
    } else if(input$model_select == "theta"){
      fcst <- thetaf(ts_data, h = input$horizon)
      return(list(model = "other", forecast = fcst, ts_data = ts_data))
    } else {  # ARIMA par défaut
      model <- auto.arima(ts_data)
      fcst <- forecast(model, h = input$horizon)
      return(list(model = "other", forecast = fcst, ts_data = ts_data))
    }
  })
  
  output$forecast_plot <- renderPlotly({
    req(forecast_results())
    res <- forecast_results()
    if(res$model != "prophet"){
      fcst <- res$forecast
      plot_ly() %>%
        add_lines(x = ~time(fcst$x), y = ~fcst$x, name = "Historique", line = list(color = "black")) %>%
        add_lines(x = ~time(fcst$mean), y = ~fcst$mean, name = "Prévisions", line = list(color = "red")) %>%
        add_ribbons(x = ~time(fcst$mean),
                    ymin = fcst$lower[,"95%"],
                    ymax = fcst$upper[,"95%"],
                    name = "IC 95%",
                    opacity = 0.2) %>%
        layout(title = paste("Prévisions -", input$var_select),
               xaxis = list(title = "Année"),
               yaxis = list(title = "Valeur"))
    } else {
      fcst <- forecast_results()$forecast
      plot_ly() %>%
        add_lines(x = fcst$ds, y = fcst$yhat, name = "Prévisions", line = list(color = "red")) %>%
        layout(title = paste("Prévisions -", input$var_select),
               xaxis = list(title = "Date"),
               yaxis = list(title = "Valeur"))
    }
  })
  
  output$forecast_metrics <- renderDT({
    req(forecast_results())
    if(forecast_results()$model != "prophet"){
      acc <- accuracy(forecast_results()$forecast)
      datatable(round(acc, 4), options = list(pageLength = 5))
    } else {
      datatable(data.frame(Message = "Métriques d'accuracy indisponibles pour Prophet."),
                options = list(pageLength = 5))
    }
  })
}

shinyApp(ui = ui, server = server)