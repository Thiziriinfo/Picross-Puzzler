library(shiny)
library(shinyjs)

# Fonction pour générer une grille aléatoire en fonction de la taille et de la probabilité
generer_grille_aleatoire <- function(taille, p) {
  matrix(rbinom(taille^2, 1, p), nrow = taille)
}

# Fonction pour obtenir les indices des lignes contiguës
obtenir_indices_ligne <- function(ligne) {
  consecutive_ones <- rle(ligne)$lengths[rle(ligne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}

# Fonction pour obtenir les indices des colonnes contiguës
obtenir_indices_colonne <- function(colonne) {
  consecutive_ones <- rle(colonne)$lengths[rle(colonne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}

# Définition de l'interface utilisateur (UI)
ui <- fluidPage(
  tags$style("
      body {
        background-image: url('https://example.com/background-image.jpg'); /* Remplacez l'URL par votre propre image */
        background-size: cover;
      }
      .square-button {
        width: 30px;
        height: 30px;
        margin: 0px;
        font-size: 12px; /* Taille de police ajustée pour une meilleure visibilité */
        background-color: blue; /* Couleur de fond des cellules de la grille */
      }
      .black-cell {
        background-color: black !important; /* Couleur de fond pour les cellules noires */
      }
      .cross-cell {
        color: red; /* Couleur du symbole de la cellule de croix */
        font-size: 18px;
        line-height: 30px;
      }
  "),
  titlePanel("Jeu Picross"),
  
  selectInput("gridSize", "Taille de la Grille",
              choices = c(5, 6, 7, 8, 9, 10),  # Options de taille de grille
              selected = 5),  # Taille de grille par défaut
  
  selectInput("difficultyLevel", "Niveau de difficulté",
              choices = c("Facile", "Moyen", "Difficile"),
              selected = "Facile"),
  
  actionButton("generateButton", "Générer une nouvelle grille"),
  actionButton("checkSolutionButton", "Vérifier la solution"),
  
  fluidRow(
    column(3, align = "center",
           uiOutput("rowIndicesTable")
    ),
    column(6, align = "center",
           uiOutput("picrossGrid")
    ),
    column(3, align = "center",
           uiOutput("columnIndicesTable")
    )
  )
)

# Fonction pour comparer deux matrices
compare_matrices <- function(mat1, mat2) {
  if (!identical(dim(mat1), dim(mat2))) {
    stop("Les dimensions des matrices ne correspondent pas.")
  }
  
  comparison <- mat1 == mat2
  
  return(comparison)
}

# Définition de la logique du serveur
server <- function(input, output) {
  
  picrossGridData <- reactiveVal(NULL)
  userGrid <- reactiveVal(matrix(0, nrow = 5, ncol = 5))  # Initialisation de userGrid
  
  observeEvent(input$generateButton, {
    taille_grille <- as.numeric(input$gridSize)
    niveau_difficulte <- input$difficultyLevel
    
    # Déterminez la probabilité en fonction du niveau de difficulté
    p <- switch(niveau_difficulte,
                "Facile" = 0.3,
                "Moyen" = 0.5,
                "Difficile" = 0.7)
    
    randomGrid <- generer_grille_aleatoire(taille_grille, p)
    indices_lignes <- apply(randomGrid, 1, obtenir_indices_ligne)
    indices_colonnes <- apply(randomGrid, 2, obtenir_indices_colonne)
    picrossGridData(list(
      picrossMatrix = randomGrid,
      indicesLignes = indices_lignes,
      indicesColonnes = indices_colonnes
    ))
    
    # Mettre à jour la taille de userGrid pour qu'elle corresponde à randomGrid
    userGrid(matrix(0, nrow = taille_grille, ncol = taille_grille))
  })
  
  observeEvent(input$checkSolutionButton, {
    picrossGridDataValue <- picrossGridData()
    if (is.null(picrossGridDataValue)) {
      showModal(modalDialog(
        title = "Erreur",
        "Veuillez générer une grille aléatoire avant de vérifier la solution."
      ))
      return()
    }
    
    randomGrid <- picrossGridDataValue$picrossMatrix
    comparison_result <- compare_matrices(userGrid(), randomGrid)
    if (all(comparison_result)) {
      showModal(modalDialog(
        title = "Bravo !",
        "Votre solution est correcte !"
      ))
    } else {
      showModal(modalDialog(
        title = "Réessayer",
        "Désolé, votre solution n'est pas correcte. Veuillez réessayer."
      ))
    }
  })
  
  output$rowIndicesTable <- renderTable({
    picrossGridDataValue <- picrossGridData()
    if (is.null(picrossGridDataValue)) return(NULL)
    t(sapply(picrossGridDataValue$indicesLignes, function(indices) {
      paste(indices, collapse = " ")
    }))
  })
  
  output$columnIndicesTable <- renderTable({
    picrossGridDataValue <- picrossGridData()
    if (is.null(picrossGridDataValue)) return(NULL)
    t(sapply(picrossGridDataValue$indicesColonnes, function(indices) {
      paste(indices, collapse = " ")
    }))
  })
  
  output$picrossGrid <- renderUI({
    picrossGridDataValue <- picrossGridData()
    if (is.null(picrossGridDataValue)) return(NULL)
    
    picrossGrid <- tagList(
      lapply(1:input$gridSize, function(i) {
        div(
          class = "cell-container",
          lapply(1:input$gridSize, function(j) {
            actionButton(
              inputId = paste0("cell", i, j),
              label = "",
              class = c("square-button", "cell-button"),
              value = picrossGridDataValue$picrossMatrix[i, j],
              onclick = paste("Shiny.setInputValue('selected_cell', {row: ", i, ", col: ", j, "});")
            )
          })
        )
      })
    )
    
    
    