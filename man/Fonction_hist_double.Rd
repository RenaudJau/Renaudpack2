% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ScriptFonctionsSourcesVpack.r
\name{Fonction_hist_double}
\alias{Fonction_hist_double}
\title{Double distribution}
\usage{
Fonction_hist_double(
  Distribution1,
  Distribution2,
  BREAKS,
  coul1 = 1,
  coul2 = 2,
  leg1 = "",
  leg2 = "",
  ...
)
}
\arguments{
\item{Distribution1}{Premiere distribution}

\item{Distribution2}{Deuxieme distribution}

\item{BREAKS}{correspond a l'argument \code{breaks} de la fonction \code{\link{hist}}, quelque chose du type \code{seq(50,350,10)} fonctionne bien}

\item{coul1}{la couleur de la distribution 1, afin d'avoir des transparences, c'est pratique d'utiliser la fonction \code{\link{hist}} de la maniere suivante : \code{rgb(139/255,62/255,47/255,0.5)}}

\item{coul2}{la couleur de la distribution 2}

\item{leg1}{nom de la distribution 1 (pour la legende)}

\item{leg2}{nom de la distribution 2 (pour la legende)}

\item{...}{d'autres argument de la fonction \code{\link{hist}}}
}
\description{
Fonction specifique dans le cadre du stage de Nadege et Anaelle pour superposer deux distributions sur un meme graphe et avec calcul du khi2 automatique
}
