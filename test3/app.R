# Charger la librairie Shiny
library(shiny)

# Fonction pour générer une grille de Picross aléatoire
generer_grille_picross <- function(taille) {
  # Créer une matrice de taille 'taille' remplie de 0 et 1 avec une probabilité de 0.7 pour 0 et 0.3 pour 1
  matrix(
    sample(c(0, 1), taille^2, replace = TRUE, prob = c(0.7, 0.3)),
    nrow = taille
  )
}

# Fonction pour vérifier si la grille correspond aux indices
verifier_solution <- function(grille, indices_lignes, indices_colonnes) {
  taille <- nrow(grille)
  
  # Calculer les indices des lignes en parcourant chaque ligne de la grille
  indices_lignes_calcul <- sapply(1:taille, function(i) {
    longueurs_series <- rle(grille[i, ])$lengths[rle(grille[i, ])$values == 1]
    if (length(longueurs_series) == 0) {
      return(0)
    } else {
      return(length(longueurs_series))
    }
  })
  
  # Calculer les indices des colonnes en parcourant chaque colonne de la grille
  indices_colonnes_calcul <- sapply(1:taille, function(j) {
    longueurs_series <- rle(grille[, j])$lengths[rle(grille[, j])$values == 1]
    if (length(longueurs_series) == 0) {
      return(0)
    } else {
      return(length(longueurs_series))
    }
  })
  
  # Vérifier si les indices des lignes et des colonnes correspondent à ceux fournis
  if (all(indices_lignes_calcul == indices_lignes) && all(indices_colonnes_calcul == indices_colonnes)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Jeu de Picross"), # Titre de l'application
  
  # Panneau latéral avec des contrôles
  sidebarLayout(
    sidebarPanel(
      numericInput("taille_grille", "Taille de la Grille", value = 5, min = 3, max = 10), # Sélecteur de taille de grille
      actionButton("generer_grille", "Générer une Nouvelle Grille"), # Bouton pour générer une nouvelle grille
      actionButton("verifier_solution", "Vérifier la Solution") # Bouton pour vérifier la solution
    ),
    
    # Panneau principal pour afficher les indices et la grille
    mainPanel(
      h3("Indices des Lignes"), # Titre pour les indices des lignes
      verbatimTextOutput("indices_lignes"), # Sortie pour les indices des lignes
      h3("Indices des Colonnes"), # Titre pour les indices des colonnes
      verbatimTextOutput("indices_colonnes"), # Sortie pour les indices des colonnes
      h3("Grille de Picross"), # Titre pour la grille de Picross
      uiOutput("grille_picross") # Sortie pour afficher la grille de Picross
    )
  )
)

# Logique du serveur
server <- function(input, output, session) {
  # Réactivez l'objet pour stocker la grille de Picross générée
  grille_picross <- reactiveVal(NULL)
  indices_lignes <- reactiveVal(NULL)
  indices_colonnes <- reactiveVal(NULL)
  
  # Observer pour générer une nouvelle grille de Picross
  observeEvent(input$generer_grille, {
    taille <- input$taille_grille
    grille <- generer_grille_picross(taille)
    
    # Calculer les indices des lignes
    indices_lignes_val <- sapply(1:taille, function(i) {
      longueurs_series <- rle(grille[i, ])$lengths[rle(grille[i, ])$values == 1]
      if (length(longueurs_series) == 0) {
        return(0)
      } else {
        return(length(longueurs_series))
      }
    })
    
    # Calculer les indices des colonnes
    indices_colonnes_val <- sapply(1:taille, function(j) {
      longueurs_series <- rle(grille[, j])$lengths[rle(grille[, j])$values == 1]
      if (length(longueurs_series) == 0) {
        return(0)
      } else {
        return(length(longueurs_series))
      }
    })
    
    # Mettre à jour les valeurs des réactifs avec la nouvelle grille et les indices
    grille_picross(grille)
    indices_lignes(indices_lignes_val)
    indices_colonnes(indices_colonnes_val)
  })
  
  # Observer pour vérifier la solution
  observeEvent(input$verifier_solution, {
    grille <- grille_picross()
    if (!is.null(grille)) {
      if (verifier_solution(grille, indices_lignes(), indices_colonnes())) {
        showModal(modalDialog(
          title = "Bravo !", # Titre de la boîte de dialogue de réussite
          "Félicitations ! Vous avez résolu le Picross avec succès." # Message de réussite
        ))
      } else {
        showModal(modalDialog(
          title = "Erreur", # Titre de la boîte de dialogue d'erreur
          "Désolé, la solution n'est pas correcte. Veuillez réessayer." # Message d'erreur
        ))
      }
    }
  })
  
  # Afficher la grille de Picross
  output$grille_picross <- renderUI({
    grille <- grille_picross()
    if (!is.null(grille)) {
      grid_ui <- tagList(
        lapply(1:nrow(grille), function(i) {
          div(
            class = "row",
            lapply(1:ncol(grille), function(j) {
              actionButton(
                paste0("cell_", i, "_", j), "", # ID de la case
                class = ifelse(grille[i, j] == 1, "black-cell", "white-cell"), # Classe CSS pour le style de la case
                width = "30px", height = "30px"
              )
            })
          )
        })
      )
      grid_ui
    }
  })
  
  # Afficher les indices des lignes
  output$indices_lignes <- renderPrint({
    indices_lignes_val <- indices_lignes()
    if (!is.null(indices_lignes_val)) {
      indices_lignes_val
    }
  })
  
  # Afficher les indices des colonnes
  output$indices_colonnes <- renderPrint({
    indices_colonnes_val <- indices_colonnes()
    if (!is.null(indices_colonnes_val)) {
      indices_colonnes_val
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)










