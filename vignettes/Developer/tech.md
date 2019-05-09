---
title: '<a name="top"></a><img src="https://raw.githubusercontent.com/samWieczorek/Prostar/master/inst/ProstarApp/www/images/LogoProstarComplet.png"    width="135"/> 
		**p r o s t a r - p r o t e o m i c s .org**'
output:
  html_document:
   toc: true
   toc_float: true
   theme: united
   toc_depth: 2  # upto three depths of headings (specified by #, ## and ###)
   highlight: tango  # specifies the syntax highlighting style
   #css: my.css   # you can add your custom css, should be in same folder 
---
<style>
body {
text-align: justify}
</style>



------

# Generalites

Prostar fonctionne à base de modules le plus possible
Plusieurs groupes de fonctionnalites existent: 


Ces fonctionnalites sont generalement représentees par des menus deroulants


# Pipelines


Les pipelines ne sont pas des modules mais une variable reactive composee d'un certain nombre de variable:
la variable dataset qui ets une liste contenant autant d'elements que contient le pipeline
La variable current.ind qui est l'indice dans 'dataset' de l'objet courant (celui sur lequel l'utilisateur travaille)

# Architecture d'un module de process

Un module de process doit avoir une entre et une sortie, instanciees par des variables reactives (dataIn et dataOut).
Chaque module a ses propres variables reactives, de ce fait il est tres indépendant du reste du logiciel.
La variable reactive porte le nom du module prefixé par 'rv.'. PAr exemple, pour le module Filtering, la variable s'appelle rv.Filtering. Dans cette liste, on trouve toutes les variables 'globales' nécessaires au fonctionnement du module.

## Paramètres d'entrée

## Nomenclature

Les modules de process sont stockés dans le répertoire modules/process. Le code d'un module (ui et server) est dans un seul fichier qui porte le nom du module préfixé par 'module'.
ceci est important car dans le fichier pipelineCore, les noms des menus et des modules sont crees dynamiquement.

## Squelette d'un module



```R
chooseCRANmirror()
install.packages("BiocManager")
```



# Module de navigation
Ce module est particulier dans le sens où il ne prend en parametre d'entree qu'une liste d'interfaces graphiques et n'a pas de valeur de sortie.
Il est appelé dans la partie UI des module de process qui en ont besoin


# Parametre des process
Ils sont concatenes sous forme de liste dans la partie de sauvegarde du dataset et sont enregistres dans le slot experimentData@other$l.params des MSnSet
