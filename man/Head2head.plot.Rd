% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ScriptFonctionsSourcesVpack.r
\name{Head2head.plot}
\alias{Head2head.plot}
\title{Plot en tete a tete}
\usage{
Head2head.plot(
  TABL,
  FACTEUR,
  col1 = 2,
  col2 = 3,
  cex_noms = 1,
  erreur = "sem",
  adj_meth = "BH"
)
}
\arguments{
\item{TABL}{un tableau de donnees avec les variables colonnes}

\item{FACTEUR}{un vecteur facteur, devant etre factorise, contenant 2 modalites exactement de la meme taille que le nombre de lignes de \code{TABL}}

\item{col1}{la couleur des barres de la modalite 1}

\item{col2}{la couleur des barres de la modalite 2}

\item{cex_noms}{taille de la police des noms des variables}

\item{erreur}{type d'erreur calcule pour la barre d'erreur (erreur standard : \code{"sem"}, intervalle de confiance : \code{"IC"}, variance : \code{var})}

\item{adj_meth}{methode d'ajustement du p calcule : \code{"bond"},\code{"BH"},\code{"hoch"}, etc. ou \code{"none"}}
}
\description{
Trace des histogrammes en vis en vis avec barres d'erreurs et calculs de Wilcoxon en fonction d'un facteur a 2 modalites
}
\details{
Attention, le premier graphe sera la premiere modalite de ton facteur, par defaut dans l'ordre
alphabetique...

Attention, il peut y avoir des messages d'erreur, c'est lie a des ex aequo dans les valeurs,
pour le calcul de wilcoxon, mais bon, ca ne change rien...
}
