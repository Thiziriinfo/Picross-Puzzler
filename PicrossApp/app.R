library(shiny)

# Fonction pour générer une grille de Picross aléatoire
generer_grille_picross <- function(taille) {
  matrix(sample(c(0, 1), taille^2, replace = TRUE, prob = c(0.7, 0.3)), nrow = taille)
}

# Fonction pour vérifier si la grille correspond aux indices
verifier_solution <- function(grille, indices_lignes, indices_colonnes) {
  taille <- nrow(grille)
  indices_lignes_calcul <- sapply(1:taille, function(i) {
    longueurs_series <- rle(grille[i, ])$lengths[rle(grille[i, ])$values == 1]
    if (length(longueurs_series) == 0) {
      return(0)
    } else {
      return(length(longueurs_series))
    }
  })
  indices_colonnes_calcul <- sapply(1:taille, function(j) {
    longueurs_series <- rle(grille[, j])$lengths[rle(grille[, j])$values == 1]
    if (length(longueurs_series) == 0) {
      return(0)
    } else {
      return(length(longueurs_series))
    }
  })
  if (all(indices_lignes_calcul == indices_lignes) && all(indices_colonnes_calcul == indices_colonnes)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Jeu de Picross"),
  sidebarLayout(
    sidebarPanel(
      numericInput("taille_grille", "Taille de la Grille", value = 5, min = 3, max = 10),
      actionButton("generer_grille", "Générer une Nouvelle Grille"),
      actionButton("verifier_solution", "Vérifier la Solution")
    ),
    mainPanel(
      h3("Indices des Lignes"),
      verbatimTextOutput("indices_lignes"),
      h3("Indices des Colonnes"),
      verbatimTextOutput("indices_colonnes"),
      h3("Grille de Picross"),
      uiOutput("grille_picross")
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
    indices_lignes_val <- sapply(1:taille, function(i) {
      longueurs_series <- rle(grille[i, ])$lengths[rle(grille[i, ])$values == 1]
      if (length(longueurs_series) == 0) {
        return(0)
      } else {
        return(length(longueurs_series))
      }
    })
    indices_colonnes_val <- sapply(1:taille, function(j) {
      longueurs_series <- rle(grille[, j])$lengths[rle(grille[, j])$values == 1]
      if (length(longueurs_series) == 0) {
        return(0)
      } else {
        return(length(longueurs_series))
      }
    })
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
          title = "Bravo !",
          "Félicitations ! Vous avez résolu le Picross avec succès."
        ))
      } else {
        showModal(modalDialog(
          title = "Erreur",
          "Désolé, la solution n'est pas correcte. Veuillez réessayer."
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
              actionButton(paste0("cell_", i, "_", j), "", class = ifelse(grille[i, j] == 1, "black-cell", "white-cell"), width = "30px", height = "30px")
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

    
    
    


