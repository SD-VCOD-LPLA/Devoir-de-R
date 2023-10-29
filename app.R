#installation des packages si nécessaire

if(!require(httr)){
  install.packages("httr") 
  library( httr )}
if(!require(jsonlite)){
  install.packages("jsonlite")
  library( jsonlite )
}
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
  
}
if(!require(plotly)){
  install.packages("plotly")
  library(plotly)
  
}
if(!require(cli)){
  install.packages("cli")
  library(cli)
}

if (!require(shinythemes)) {
  install.packages("shinythemes")
  library(shinythemes)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  library(leaflet)
}
#appel de l'API
contrat<-'Lyon'
apikey<-'b84356facb5f194cb1281e653610562177b15c29'
base<-'https://api.jcdecaux.com/vls/v3/stations?'

url<-paste0(base,"contract=", contrat, "&apiKey=", apikey)
reponse<-GET(url)
data<-fromJSON(rawToChar(reponse$content),flatten = T)

#ouverture du fichier csv contenant les numéros de stations+codes postaux

carte<-read.csv("carte.csv", sep = "", fileEncoding = "UTF-8")



# Define UI ----
ui <- navbarPage(
  #les onglets
  title = "Les Vélov à Lyon",
  theme=shinytheme("united"),#theme de l'appli
  tabPanel("Accueil",#première onglet
           sidebarLayout(sidebarPanel(actionButton("refresh","Rafraichir les données"),#bouton qui rfraichis les données
                                      h5(textOutput("clique")),#message qui s'affiche quand le bouton est clqiué
                                      img(src = "unnamed.png", height = "auto", width = "100%"), #logo Vélov
                                      selectInput("arrondissement","Sélectionner un arrondissement: ",choices=c("Tous",unique(carte$codepostale)))),
                         mainPanel(
                           
                           textOutput("text6"),
                           h1("Bienvenue sur la page d'accueil"),
                           h4("Cette application a pour but de vous montrer en temps réel quelques statistiques sur les Vélov à Lyon"),#intro 
                           h4("Au total la ville de Lyon compte ", length(unique(data$number)), "stations de Vélov"),#nb total de stations 
                           leafletOutput("carte")#ajout de la carte
                           
                         )
           )
  ), # Fin du premier onglet
  
  tabPanel("Vélos disponibles",#deuxième onglet sur les velos dispo
           sidebarLayout(
             sidebarPanel(checkboxGroupInput("station", h3("Sélectionnez une station :"), choices = unique(data$name))), # case à cocher pour filtrer sur le nom des stations 
             mainPanel(
               h2("Voici les vélos que vous pouvez emprunter !"),
               h4(textOutput('message1')),
               plotlyOutput("hist1")#graphque sur le nb de vélo dispo par stations
               
               
             )
           )
  ), # Fin du deuxième onglet
  
  tabPanel("Places disponibles",#troisième onglet sur les places dispo
           sidebarLayout(
             sidebarPanel(sliderInput("slider1", label = h3("Nombre de places:"), min = 0,max = 40, value = 0), #slider pour filter sur le nb de places           
                          checkboxGroupInput("station2", h3("Sélectionnez une station :"), choices = unique(data$name))), # case a cocher pour filtrer sur le nom des stations 
             mainPanel(
               h2(textOutput("titre1")),
               h4(textOutput("message2")),#message si aucune stations sélectionnées
               plotlyOutput("hist2"),#graphique sur les stations contenant le nb de places dispo selectionnées
               h2(textOutput("titre2")),
               h4(textOutput("message3")),#message si le nb de place vaut zero
               plotlyOutput("hist3"),#graphique sur le nombre de places dispo des stations sélectionnées
               
             )
           )
  ),
  tabPanel("Type de vélos",#quatrième onglet
           tabsetPanel(#onglets dans le quatriéme onglet
             tabPanel("Déscription",#première onglet qui décrit les type de vélos
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("type", "Les différents types:", choices = c("Mécaniques", "Eléctriques")),#menue déroulant avec les types de vélos
                          h4("Pour plus d'informations vous pouvez consulter notre site internet"),
                          tags$a("Cliquez ici", href = "https://velov.grandlyon.com/fr/home", target = "_blank")#lien pour accéder au site internet Vélov
                        ),
                        mainPanel(
                          h2("Quels types de vélos pouvez-vous emprunter?"),
                          h4("En 2022, Vélo'v est un service de location de vélos en libre-service à Lyon. Ils offrent différents modèles pour répondre aux besoins variés des utilisateurs. Voici quelques-uns des types de vélos que vous pourriez trouver dans leur flotte :"),
                          h4("Les vélos de types", textOutput("text1")),
                          # organiser l'image d'un coté et le texte de l'autre
                          div(style = "display: flex; align-items: center;",
                              div(imageOutput("velo", width = 400, height = 300), style = "flex: 1;"),  # Div pour l'image
                              div(textOutput("velo1"), textOutput("describ"), style = "flex: 1; margin-left: 10px;")  # Div pour le texte avec une marge à gauche pour avoir un espace entre les deux
                          )
                        )
                      )
             ),
             tabPanel("Répartition",#deuxième onglet sur la répartition des type de vélos par stations
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("station3", "Sélectionner une station:", choices = unique(data$name))#menue deroulant avec le nom des stations
                        ),
                        mainPanel(
                          textOutput("message4"),#message afficher si pas de station sélectionnées ou si pas de vélos sur la station sélectionnée
                          plotlyOutput("repartition")#graphique de la répartition des types
                        )
                      )
             )
           )
  )
) 
            # Fin du troisième onglet


# Define server logic ----
server <- function(input, output) {
  
  # premier onglet    
  
  #boutton refresh
  
  #si le boutton est cliqué on refait appel à l'API et les données sont misent à jour 
  observeEvent(input$refresh, {
    contrat<-'Lyon'
    apikey<-'b84356facb5f194cb1281e653610562177b15c29'
    base<-'https://api.jcdecaux.com/vls/v3/stations?'
    
    url<-paste0(base,"contract=", contrat, "&apiKey=", apikey)
    reponse<-GET(url)
    data<-fromJSON(rawToChar(reponse$content),flatten = T)
    
    output$clique <- renderText({
      "Les données ont bien été misennt à jour !"
    })
  })
  
#carte
  
  # jointure entre le csv avec les arrondissements et les données
  jointure <- reactive({
    merge(data, carte, by.x = "number", by.y = "number.", all.x = TRUE)
  })
  
  # on filtre sur le code postal sélectionné
  filtre_data5 <- reactive({
    if (input$arrondissement == "Tous") {
      return(jointure())
    } else {
      return(jointure()[jointure()$codepostale == input$arrondissement, ])
    }
  })
  
  # on créer une carte zoomée sur la zone de Lyon
  output$carte <- renderLeaflet({
    leaflet() %>%
      setView(lng = 4.8357, lat = 45.7640, zoom = 12) %>%
      addTiles() %>%
      # on ajoute des points pour les stations grâce à leurs coordonnées
      addMarkers(
        data = filtre_data5(),
        lng = ~position.longitude,
        lat = ~position.latitude,
        clusterOptions = markerClusterOptions(),# regroupe les points quand il y en a trop
        
        popup = ~paste("Station: ", name, "<br>Nombre de vélos disponibles: ", totalStands.availabilities.bikes)#etiquette des points avec le nom de la station et à la ligne le nb de vélos dispo
      )
  })
  # deuxième onglet
  
  #graphe 1
  
  # on filtre les données en fonction des stations sélectionnées  
  
  # Premier onglet
  filtre_data1 <- reactive({
    if (length(input$station) == 0) {#si pas de station sélectionnées retourne null
      return(NULL)
    }
    data[data$name %in% input$station, ]
  })
  
  # premier graphique sur le nb de vélos
  output$hist1 <- renderPlotly({
    if (is.null(filtre_data1())) {#si pas de stations retourne null
      return(NULL)
    }
    
    plot_ly(
      data = filtre_data1(),
      x = ~name,
      y = ~totalStands.availabilities.bikes,
      type = 'bar',
      text = ~totalStands.availabilities.bikes,
      marker = list(color = 'red')
    ) %>%
      layout(title = "Nombre de vélos par station", xaxis = list(title = "Stations"), yaxis = list(title = "Nombre de vélos disponibles"))
  })
  
  # message affiché si aucune station n'est sélectionnée ou si aucun vélo disponible
  output$message1 <- renderText({
    if (is.null(filtre_data1())) {#si pas de stations sélectionnées
      return("Aucune station sélectionnée. Veuillez choisir au moins une station.")#message retourné
    } else {
      #affcher message si pas de vélos dispo sur la station sélectionnée
      bikes_available <- unique(filtre_data1()$totalStands.availabilities.bikes)
      if (length(bikes_available) == 1 && bikes_available == 0) {#si pas de vélos dispo sur la stations sélectionnée
        return("Aucun vélo disponible pour la station sélectionnée.")#message retourné
      } else {
        return(NULL) 
      }
    }
  })
  
  #troisième onglet
  
  #graph 1
  
  output$titre1<-renderText(paste("Voici les stations où vous pouvez poser ", input$slider1, " velos"))
  output$titre2<-renderText("Voici le nombre de places disponibles dans les stations sélectionnées")
  
  
  #on filtre les données en fonction du nb de places sélectionnées  
  filtre_data2 <- reactive({
    if (input$slider1 == 0) {
      #si l'utilisateur sélectionne 0 places retourne NULL
      return(NULL)
    }
    filtre<-data[data$totalStands.availabilities.stands %in% input$slider1, ]
    if (nrow(filtre) == 0) {
      #si aucune station correspondante retourne NULL
      return(NULL)
    }
    return(filtre)
  })
  #deuxième graphique sur le nb de places
  output$hist2 <- renderPlotly({
    if (is.null(filtre_data2())) { #si aucune station selectionnée retourne null
      return(NULL)
    }
    plot_ly(
      data = filtre_data2(),
      x = ~name,
      y = ~totalStands.availabilities.stands,
      type = 'bar',
      marker = list(color = 'orange')
    ) %>%
      layout(title = paste("Station(s) avec ", input$slider1 ," places"), xaxis = list(title = "Stations"), yaxis = list(title = "Nombre de places disponibles"))
  })
  
  #on affiche un message si il y a aucune stations correspondnates ou si le nb de places sélectionnées vaut 0
  output$message2 <- renderText({
    if (is.null(filtre_data2())) {#si pas de stations sélectionnées 
      if (input$slider1 == 0) {#si le slider est sur zero
        return("Veuillez sélectionner un nombre de places supérieur à zéro.")#message retourné
      } else {
        return("Aucune station correspondante au nombre de places sélectionnées.")#message retourné
      }
    } else {
      return(NULL)
    }
  })
  
  #graph 2
  
  # on filtre les données en fonction des stations sélectionnées  
  
  filtre_data3 <- reactive({
    if (length(input$station2) == 0) {
      return(NULL) }
    
    data[data$name %in% input$station2, ] #on prends le nom des stations de notre dataframe qui ont été sélectionnées
  })
  
  output$hist3 <- renderPlotly({
    if (is.null(filtre_data3())) {
      return(NULL)
    }
    plot_ly(
      data = filtre_data3(),
      x = ~name,
      y = ~totalStands.availabilities.stands,
      type = 'bar',
      text = ~totalStands.availabilities.stands,
      marker = list(color = 'red')
    ) %>%
      layout(title = "Nombre de places disponibles par station", xaxis = list(title = "Stations"), yaxis = list(title = "Nombre de places disponibles"))
  })
  
  #message affiché si aucune stations n'est sélectionnées
  output$message3 <- renderText({
    if (is.null(filtre_data3())) {
      return("Aucune station sélectionnée. Veuillez choisir au moins une station.")
    } else {
      return(NULL)
    }
  })
  
  #quatrième onglet  
  
  # premier onglet
  
  output$text1<-renderText({input$type})
  
  #image affichée
  
  output$velo <- renderImage({
    if(input$type=="Eléctriques")# si le type selectionné est electrique
    {list(src = "www/evelov.jpg",#affiche image correspondante
          contentType = "image/jpg",
          width = 400, height = 300)}
    else if(input$type=="Mécaniques")# si le type selectionné est mecanique
    {list(src = "www/velovmec.jpg",#affiche image correspondante
          width = 400, height = 300)}})
  
  #description affichée
  
  output$describ<-renderText({#si le type sélectioné est electrique
    if(input$type=="Eléctriques"){
      "Les e-Vélo'v de Vélo'v sont des vélos électriques équipés d'une batterie rechargeable pour une assistance au pédalage. Ils intègrent un moteur électrique discret, facilitant ainsi les déplacements urbains. Les utilisateurs peuvent recharger la batterie aux stations dédiées pour maintenir la fonctionnalité électrique du vélo."
    }
    else if(input$type=="Mécaniques")# si le type selectionné est mecanique
    {
      "Les Vélo'v mécaniques de Vélo'v sont des vélos classiques conçus pour les déplacements urbains. Ils ne disposent pas d'assistance électrique et fonctionnent uniquement grâce à la force humaine. Ces vélos robustes sont adaptés à un usage quotidien et peuvent être verrouillés à des stations spécifiques lorsqu'ils ne sont pas utilisés"
    }
  })
    
  #deuxième onglet
  
  #on filtre les données en fonction de la station selectionnées
  
  filtre_data4 <- reactive({
    if (length(input$station3) == 0) {#si pas de station selectionné renvoie null
      return(NULL)
    }
    
    data[data$name %in% input$station3, ] # on prends le nom des stations de notre dataframe qui ont été sélectionnées
  })
  
  output$repartition <- renderPlotly({
    if (is.null(filtre_data4())) {#si pas de station selectionée renvoie null
      return(NULL)
    }
    #on créé un data frame avec les donnée donc on à besoin pour le diagramme circulaire
    pie_data <- data.frame(
      type = c("Électriques", "Mécaniques"),
      count = c(sum(filtre_data4()$totalStands.availabilities.electricalBikes),
                sum(filtre_data4()$totalStands.availabilities.mechanicalBikes))
    )
    
    fig <- plot_ly(
      pie_data,
      labels = ~type,
      values = ~count,
      type = "pie",
      marker = list(colors = c('orange', 'red')),#couleur des parties
      textinfo = "percent",#texte affiché
      textposition = "inside"#text à l'intérieur
    ) %>%
      layout(title = "Répartition du nombre de vélos par type",#titre du graphique
             legend = list(orientation = "h", x = 0.5, y = -0.1))#légende affcihée
    
    return(fig)
  })
  
  # Vérifier si des vélos sont disponibles dans la station
  output$message4 <- renderText({
    if (is.null(filtre_data4())) {
      return("Aucune station sélectionnée. Veuillez choisir au moins une station.")
    } else {
      total_bikes <- sum(filtre_data4()$totalStands.availabilities.electricalBikes) + sum(filtre_data4()$totalStands.availabilities.mechanicalBikes)
      
      if (total_bikes == 0) {
        return("Aucun vélo disponible dans la station sélectionnée.")
      } else {
        return(NULL)
      }
    }
  })  

  # Code du serveur, si nécessaire
}

# Run the app ----
shinyApp(ui = ui, server = server)