# VizDataWhatsApp

<div align="center">
  <img src="inst/www/logo.svg" alt="VizDataWhatsApp Logo" width="200"/>
</div>

<!-- badges: start -->
[![R-CMD-check](https://github.com/PCS399/VizDataWhatsApp/workflows/R-CMD-check/badge.svg)](https://github.com/PCS399/VizDataWhatsApp/actions)
[![Codecov test coverage](https://codecov.io/gh/PCS399/VizDataWhatsApp/branch/main/graph/badge.svg)](https://codecov.io/gh/PCS399/VizDataWhatsApp?branch=main)
<!-- badges: end -->

VizDataWhatsApp est un package R qui fournit une application Shiny interactive pour visualiser et analyser les conversations WhatsApp.

## Installation

``` r
# Installation depuis GitHub
devtools::install_github("PCS399/VizDataWhatsApp")
```

## Utilisation

``` r
library(VizDataWhatsApp)

# Lancer l'application
run_whatsapp_viz_app()
```

## Fonctionnalités

- **Importation de données** : Importez facilement vos fichiers d'export WhatsApp ;
- **Visualisation temporelle** : Analysez l'activité par jour et par heure ;
- **Analyse lexicale** : Explorez la diversité du vocabulaire utilisé ;
- **Réactions** : Visualisez les réactions aux messages ;
- **Interface multilingue** : Disponible en français et en anglais (traduction de l'interface sans les graphiques).

## Documentation

Pour plus d'informations, consultez la [documentation complète](https://PCS399.github.io/VizDataWhatsApp/) (en cours de conception).

## Contribution

Les contributions sont les bienvenues ! N'hésitez pas à :
- Ouvrir une [issue](https://github.com/PCS399/VizDataWhatsApp/issues)
- Proposer une [pull request](https://github.com/PCS399/VizDataWhatsApp/pulls)

## Licence

Ce projet est sous licence MIT. Voir le fichier `LICENSE` pour plus de détails. 
