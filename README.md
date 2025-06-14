# VizDataWhatsApp

Un package R pour visualiser et analyser les conversations WhatsApp de manière interactive.

## Installation

```R
# Installation depuis GitHub
devtools::install_github("votre-username/VizDataWhatsApp")

# Ou installation depuis un fichier local
devtools::install_local("chemin/vers/VizDataWhatsApp")
```

## Utilisation

```R
library(VizDataWhatsApp)

# Lancer l'application
run_whatsapp_viz_app()
```

## Fonctionnalités

- Visualisation interactive des conversations WhatsApp
- Analyse de l'activité par jour et par heure
- Analyse de la diversité lexicale
- Support multilingue (Français/Anglais)
- Interface utilisateur moderne et intuitive

## Prérequis

- R >= 4.0.0
- Packages R requis :
  - shiny
  - dplyr
  - ggplot2
  - plotly
  - lubridate
  - tidytext
  - shinyjs
  - rwhatsapp

## Format des données

L'application accepte les fichiers d'export WhatsApp au format texte (.txt). Pour exporter vos conversations WhatsApp :

1. Ouvrez la conversation dans WhatsApp
2. Appuyez sur les trois points en haut à droite
3. Sélectionnez "Plus" > "Exporter la conversation"
4. Choisissez "Sans média"

## Contribution

Les contributions sont les bienvenues ! N'hésitez pas à :

1. Fork le projet
2. Créer une branche pour votre fonctionnalité
3. Commiter vos changements
4. Pousser vers la branche
5. Ouvrir une Pull Request

## Licence

Ce projet est sous licence MIT. Voir le fichier `LICENSE` pour plus de détails. 