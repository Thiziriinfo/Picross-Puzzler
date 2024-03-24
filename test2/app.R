library(shiny)

# Définition des données de puzzle
puzzles <- list(
  list(
    size = 5,
    rows = c(2, 1, 1, 2, 1),
    cols = c(1, 2, 1, 1)
  ),
  list(
    size = 10,
    rows = c(3, 1, 2, 1, 1, 2, 1, 1, 2, 3),
    cols = c(2, 1, 1, 3, 1, 1, 1, 1, 1, 2)
  ),
  list(
    size = 15,
    rows = c(5, 1, 2, 1, 3, 1, 1, 2, 1, 1, 1, 2, 1, 3, 5),
    cols = c(3, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3)
  )
)

# Définition de l'interface utilisateur
ui <- fluidPage(
  titlePanel("Jeu Picross"),
  sidebarLayout(
    sidebarPanel(
      selectInput("puzzle_size", "Taille de la Grille:", choices = c(5, 10, 15)),
      selectInput("puzzle_level", "Niveau de Difficulté:", choices = 1:length(puzzles)),
      actionButton("new_game", "Nouvelle Partie"),
      actionButton("check", "Vérifier")
    ),
    mainPanel(
      tableOutput("picross_grid"),
      tableOutput("row_hints"),
      tableOutput("col_hints"),
      textOutput("result")
    )
  )
)

# Définition du serveur
server <- function(input, output, session) {
  
  # Sélectionner le puzzle en fonction du niveau choisi
  current_puzzle <- reactive({
    puzzles[[input$puzzle_level]]
  })
  
  # Afficher la grille de jeu
  output$picross_grid <- renderTable({
    req(current_puzzle())
    size <- as.numeric(input$puzzle_size)
    grid <- matrix(0, nrow = size, ncol = size)
    grid
  }, rownames = FALSE, colnames = FALSE)
  
  # Afficher les indices de lignes
  output$row_hints <- renderTable({
    req(current_puzzle())
    current_puzzle()$rows[1:as.numeric(input$puzzle_size)]
  }, rownames = FALSE, colnames = FALSE)
  
  # Afficher les indices de colonnes
  output$col_hints <- renderTable({
    req(current_puzzle())
    t(current_puzzle()$cols[1:as.numeric(input$puzzle_size)])
  }, rownames = FALSE, colnames = FALSE)
  
  # Permettre aux utilisateurs de remplir/effacer les cases
  observeEvent(input$picross_grid, {
    values <- input$picross_grid
    grid <- isolate(values)
    grid[!is.na(values)] <- ifelse(grid[!is.na(values)] == 0, 1, 0)
    updateTableInput(session, "picross_grid", value = grid)
  })
  
  # Vérifier la solution
  output$result <- renderText({
    req(current_puzzle())
    size <- as.numeric(input$puzzle_size)
    grid <- isolate(input$picross_grid)
    if (all(grid == as.matrix(size))) {
      "Bravo ! Vous avez résolu le puzzle."
    } else {
      "Continuez à remplir la grille."
    }
  })
  
  # Nouvelle partie
  observeEvent(input$new_game, {
    size <- as.numeric(input$puzzle_size)
    grid <- matrix(0, nrow = size, ncol = size)
    updateTableInput(session, "picross_grid", value = grid)
  })
}

shinyApp(ui, server)
