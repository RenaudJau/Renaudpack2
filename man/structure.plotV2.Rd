% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ScriptFonctionsSourcesVpack.r
\name{structure.plotV2}
\alias{structure.plotV2}
\title{structure plot version 2}
\usage{
structure.plotV2(
  INDICE,
  FACTOR,
  MULTI = T,
  MTITLE = "",
  ABMAX = 5,
  col1 = "grey60",
  col2 = "white",
  col3 = "red",
  col4 = "orange",
  noms = "T",
  cex_noms = 1,
  erreur = "sem",
  w_err = 1,
  sp_star = 1,
  adj_meth = "BH",
  stars = "T",
  BASE = T,
  ...
)
}
\arguments{
\item{INDICE}{An object issued from \code{\link{ComStructIndices}} function}

\item{FACTOR}{A factor list, a barplot of species mean abundances will be performed for each factor level. If no factor is specified, MULTI=F should be specified.}

\item{MULTI}{If no factor is specified, MULTI=F should be specified}

\item{MTITLE}{Main title of the plot}

\item{ABMAX}{Numerical value of the maximum abundance}

\item{col1}{Colour information for the Reference mean abundances barplot}

\item{col2}{Colour information for the Reference mean abundances in assessed community barplot, i.e. "missing abundances"}

\item{col3}{Colour information for the abundances of target species in the assessed community}

\item{col4}{Colour information for the "higher abundances" in the assessed community}

\item{noms}{If other than "T", species names are not given}

\item{cex_noms}{expansion factor for species names}

\item{erreur}{(facultatif) type de calcul de la barre d'erreur \code{"sem"} (par defaut) calculera l'erreur standard (ecart type divise par n) \code{"IC"} calculera l'intervalle de confiance a 5 pourcent (quantile 97.5 a n-1 degre de liberte multiplie par l'ecart-type divise par la racine de n-1) \code{"var"} ou \code{"sd"} ou tout autre fonction de calcul connue de R Attention, ne supporte pas les \code{NA}... (j'ai deja essaye de modifier mais sans succes...)}

\item{w_err}{largeur de la barre d'erreur, 1 par defaut Visiblement ca peut encore etre optimise, la taille par defaut est bonne dans certains cas moins dans d'autres...a ajuster donc}

\item{sp_star}{ecartement des etoiles par rapport aux barres d'erreurs, 1 par defaut}

\item{adj_meth}{methode d'ajustement du p : \code{"bond"},\code{"BH"},\code{"hoch"}, etc. ou \code{"none"} (le p correspond a un test de wilcoxon entre la reference et la modalite evaluee)}

\item{stars}{"T" par defaut, dessine les etoiles, toute autre mention ne les dessinera pas}

\item{BASE}{T par defaut, dessine le graphe de refe pour chaque modalite, si F, pas de ligne de base}

\item{...}{all other arguments used by \code{\link{barplot}}}
}
\description{
idem que \code{\link{structure.plot}} mais avec quelques arguments en plus (notament barres d'erreur, calcul de difference a la reference, etc.)
}
\details{
Attention, le p est ajuste pour une modalite, en toute rigueur il faudrait sans doute l'ajuster sur l'ensemble des calculs
Attention, il peut y avoir des messages d'erreur, c'est lie a des ex aequo dans les valeurs, pour le calcul de wilcoxon, mais bon, ca ne change rien...'
}
\examples{
IndiC=ComStructIndices(releves[Type=="COUS",],releves,rar=5)
structure.plotV2(IndiC,Type,ABMAX=5,sp_star=2,w_err=0.1)

}
\seealso{
\code{\link{ComStructIndices}} et \code{\link{structure.plot}}
}
