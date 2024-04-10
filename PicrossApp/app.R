library(shiny)

# Fonction pour générer une grille de Picross aléatoire
generer_grille_picross <- function(taille) {
  matrix(sample(c(0, 1), taille^2, replace = TRUE, prob = c(0.7, 0.3)), nrow = taille)
}

# Fonction pour générer des indices aléatoires sous forme de matrices
generer_indices <- function(taille) {
  indices <- matrix(0, nrow = taille, ncol = taille)
  
  # Générer des indices pour chaque ligne
  for (i in 1:taille) {
    somme <- 0
    for (j in 1:taille) {
      if (somme < taille) {
        indices[i, j] <- sample(0:(taille - somme - 1), 1)
        somme <- somme + indices[i, j] + 1
      }
    }
  }
  
  # Générer des indices pour chaque colonne
  for (j in 1:taille) {
    somme <- 0
    for (i in 1:taille) {
      if (somme < taille) {
        indices[j, i] <- sample(0:(taille - somme - 1), 1)
        somme <- somme + indices[j, i] + 1
      }
    }
  }
  
  return(indices)
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
  titlePanel("Bienvenue au jeu du Picross"),
  sidebarLayout(
    sidebarPanel(
      numericInput("taille_grille", "Taille de la Grille", value = 5, min = 5, max = 15),
      actionButton("generer_grille", "Générer une Nouvelle Grille", style = "color: #ffffff; background-color: #007bff; border-color: #007bff;"),
      actionButton("verifier_solution", "Vérifier la Solution", style = "color: #ffffff; background-color: #28a745; border-color: #28a745;")
    ),
    mainPanel(
      h3("Indices des Lignes"),
      uiOutput("indices_lignes"),
      h3("Indices des Colonnes"),
      uiOutput("indices_colonnes"),
      h3("Grille de Picross"),
      uiOutput("grille_picross")
    )
  ),
  tags$head(
    tags$style(
      HTML("
        .checkbox-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(30px, 1fr));
          gap: 1px;
        }
        .indices-matrix {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(20px, 1fr));
          gap: 1px;
          margin-bottom: 10px;
        }
        .indice-cell {
          background-color: lightgray;
          text-align: center;
          font-weight: bold;
          padding: 3px;
        }
      ")
    )
  )
)

# Logique du serveur
server <- function(input, output, session) {
  # Réactivez l'objet pour stocker la grille de Picross générée
  grille_picross <- reactiveVal(NULL)
  indices_lignes <- reactiveVal(NULL)
  indices_colonnes <- reactiveVal(NULL)
  grille_remplie <- reactiveVal(NULL)
  
  # Observer pour générer une nouvelle grille de Picross
  observeEvent(input$generer_grille, {
    taille <- input$taille_grille
    grille <- generer_grille_picross(taille)
    indices_lignes_val <- generer_indices(taille)
    indices_colonnes_val <- generer_indices(taille)
    grille_picross(grille)
    indices_lignes(indices_lignes_val)
    indices_colonnes(indices_colonnes_val)
    grille_remplie(matrix(FALSE, nrow = taille, ncol = taille))
  })
  
  # Observer pour vérifier la solution
  observeEvent(input$verifier_solution, {
    grille <- grille_picross()
    indices_lignes_val <- indices_lignes()
    indices_colonnes_val <- indices_colonnes()
    grille_remplie_val <- grille_remplie()
    
    if (!is.null(grille) && !is.null(indices_lignes_val) && !is.null(indices_colonnes_val) && !is.null(grille_remplie_val)) {
      solution_correcte <- verifier_solution(grille_remplie_val, indices_lignes_val, indices_colonnes_val)
      
      if (solution_correcte) {
        showModal(modalDialog(
          title = "Bravo !",
          "Félicitations ! Vous avez résolu le Picross avec succès."
        ))
      } else {
        showModal(modalDialog(
          title = "Oups",
          "Désolé, y'a des erreurs. Veuillez réessayer."
        ))
      }
    }
  })
  
  # Observer pour remplir ou effacer les cases de la grille
  observeEvent(input$grille, {
    grille <- grille_remplie()
    info <- strsplit(input$grille, "_")[[1]]
    i <- as.numeric(info[2])
    j <- as.numeric(info[3])
    grille[i, j] <- !grille[i, j]
    grille_remplie(grille)
  })
  
  # Afficher la grille de Picross
  output$grille_picross <- renderUI({
    grille <- grille_picross()
    if (!is.null(grille)) {
      grid_ui <- tagList(
        lapply(1:nrow(grille), function(i) {
          div(
            class = "checkbox-grid",
            lapply(1:ncol(grille), function(j) {
              checkboxInput(paste0("cell_", i, "_", j), "", value = grille_remplie()[i, j])
            })
          )
        })
      )
      grid_ui
    }
  })
  
  # Afficher les indices des lignes
  output$indices_lignes <- renderUI({
    indices_lignes_val <- indices_lignes()
    if (!is.null(indices_lignes_val)) {
      div(class = "indices-matrix",
          lapply(1:nrow(indices_lignes_val), function(i) {
            div(class = "indice-cell", 
                lapply(indices_lignes_val[i,], function(indice) {
                  if (indice == 0) {
                    ""
                  } else {
                    indice
                  }
                }))
          }))
    }
  })
  
  # Afficher les indices des colonnes
  output$indices_colonnes <- renderUI({
    indices_colonnes_val <- indices_colonnes()
    if (!is.null(indices_colonnes_val)) {
      div(class = "indices-matrix",
          lapply(1:nrow(indices_colonnes_val), function(i) {
            div(class = "indice-cell", 
                lapply(indices_colonnes_val[i,], function(indice) {
                  if (indice == 0) {
                    ""
                  } else {
                    indice
                  }
                }))
          }))
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)

    
    
    


