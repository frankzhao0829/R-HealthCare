# R-RTI-HealthCareDemoShiny
Actionable Analytics - Shiny App demo using Microsoft Band Data for healthcare analytics using R. Utilizes novelty scoring and rates, bayesian networks, Human Activity Recognition, stacked machine learning, randomForest, real-time data


## Screenshots

.gif of demo:

![Demo](https://raw.github.hpe.com/Analytics-DataScience/R-RTI-HealthCareDemoShiny/master/Screenshots/healthcareDemo2.gif)

Screenshots:

![Screenshot_1](https://raw.github.houston.entsvcs.net/Analytics-DataScience/R-RTI-HealthCareDemoShiny/master/Screenshots/screen1.JPG)
![Screenshot_2](https://raw.github.houston.entsvcs.net/Analytics-DataScience/R-RTI-HealthCareDemoShiny/master/Screenshots/screen2.JPG)

![Screenshot_3](https://raw.github.houston.entsvcs.net/Analytics-DataScience/R-RTI-HealthCareDemoShiny/master/Screenshots/screen3.JPG)


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. 

### Prerequisities

Install R and RStudio

Be sure you have all required packages installed

```
#Install packages to run app

install.packages("shiny", "MASS", "zoo", "reshape2", "plyr", "dplyr", "data.table", "caret", "caretEnsemble", "randomForest", "doParallel", "nnet", "bnlearn", "shinydashboard", "csvread", "arules")

```

### Installing

A step by step series of examples that tell you have to get a development env running

  - Save "shinyQTpro.RData" and "App.R" from repo to local drive on pc
  - Open RStudio and load in the workspace data you just saved (code snippet example below)
  - Open App.R files in RStudio
  - If shiny is installed properly in R, you should see "Run App" in top right of the codes window pane -- click "Run App" to run the app
  - If you do not see "Run App" button shiny may not be installed correctly. You can also try and select the entire code and "Run" it


```
#use load to import workspace

load("shinyQTpro.RData")

```

## Built With

* R and RStudio

## Authors

[Quinton Teter](mailto:quinton.s.teter@hpe.com)

[Greg MacDonald](mailto:greg.macdonald@hpe.com)

QTpro
