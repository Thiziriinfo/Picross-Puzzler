# Projet Picross en R avec Shiny


## Objectif du projet 
Créer une bibliothèque R pour soutenir le développement d'une application Shiny interactive en ligne.

## Description du Jeu Picross
Le Picross, également connu sous le nom de Nonogram, est un jeu de puzzle logique où le joueur doit remplir des cases sur une grille selon les indices donnés sur les côtés. Ces indices indiquent le nombre de cases consécutives à remplir sur chaque ligne et colonne, permettant au joueur de déduire quelle case doit être remplie ou laissée vide pour révéler une image cachée.

## Étapes pour réaliser le Jeu dans Shiny

### 1. Développement de la Bibliothèque R
- Créer une bibliothèque en R contenant les fonctions nécessaires pour générer et valider les puzzles Picross.
- Inclure des fonctions pour générer des grilles aléatoires, vérifier la validité des solutions et afficher les indices sur les côtés de la grille.

### 2. Création de l'Application Shiny
- Mettre en place l'infrastructure de base de l'application Shiny.
- Créer l'interface utilisateur permettant aux utilisateurs de sélectionner la taille de la grille et de remplir les cases.
- Intégrer les fonctionnalités pour afficher les indices sur les côtés de la grille.
- Implémenter un système de vérification pour valider les solutions proposées par les utilisateurs.
- Ajouter des options pour choisir différents niveaux de difficulté et générer des puzzles de taille variable.

### 3.Installation du package 
-install.packages("remotes")
-remotes::install_github("https://github.com/Thiziriinfo/Picross-Puzzler")

### 4.Auteurs 
thiziri.abchiche@etu.umontpelier.fr




