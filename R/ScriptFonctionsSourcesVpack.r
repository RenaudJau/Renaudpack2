#################################################################################
#==============================Fonctions========================================#
#################################################################################

# Attention, ces fonctions sont pour certaines encore au stade de bricolage,
# ne pas diffuser sans le pr?ciser... Si jamais vous avez des suggestions d'optimisation,
# d'am?lioration ou des remarques concernant des bugs, n'h?sitez pasame pr?venir!
#
# Renaud.

#-----------------------Misesajour:
#M?J 31-10-2013 : Time.factor.plot() ajout d'un argument linktyp
#M?J 31-10-2013 : ajout de la fonction Head2head.plot
#M?J 15-04-2014 : ajout de la fonction barres.plot.beside
#M?J 15-05-2014 : ajout de l'argument ylim dans barres.plot et barres.plot.beside
#M?J 20-05-2014 : ajout de la sous-fonction label.corDCA
#M?J 20-05-2014 : ajout des fonctions anovLetters() et lmeLetters()
#M?J 22-05-2014 : ajout argument srt.let et cex.let dans barres.plot.beside
#M?J 22-05-2014 : modif --Letters pour retourner des "" si non significatif...
#M?J 22-01-2015 : ajout de la fonction multivar.polyg()
#M?J 15-06-2015 : ajout argument ?toilesaTime.factor.plot()
#M?J 16-06-2015 : ajout fonction better_arrows()
#M?J 16-06-2015 : ajout de l'argument "new"ala fonction multivar.polyg()
#M?J 23-06-2015 : ajout de la fonction Remplacer()
#M?J 27-08-2015 : ajout de la fonction radarchart2() (en cours)
#M?J 04-08-2015 : ajout de la fonction Classes_def
#M?J 10-01-2016 : ajout de la fonction occ
#M?J 20-04-2016 : ajout de la fonction Tableau_recap
#M?J 20-04-2016 : ajout de la fonction Couleur_continue
#M?J 22-04-2016 : ajout de la fonction Fonction_hist_double
#M?J 23-05-2016 : correction bug sous titre dans Fonction_hist_double
#MaJ 25-10-2016 : ajout de la fonctionSim_Clust_Ordre
#MaJ 02-03-2018 : ajout de la fonction modif_coul
#MaJ 06-03-2018 : ajout de la fonction point.plot

#--------------------------sem  ---------------------------------------------

#' sem
#'
#' @description Calcul de l'erreur standard
#'
#' @param x un vecteur
#'
#' @return une valeur d'erreur standard
#' @details formule ecart-type/racine du nombre de valeur. Attention, il faudrait rajouter un argument pour utiliser la formule n-1 quand neccessaire...
#' @export
#' @examples A<-rnorm(45,3,2)
#' sem(A)
#'
sem<-function(x)
{
  sqrt(var(x,na.rm=T))/sqrt(length(x))
}

#--------------------------IC  ---------------------------------------------

#' Intervalle de confiance
#'
#' @description Calcul d'un intervalle de confiance a 5 pour cent
#'
#' @param x un vecteur
#'
#' @return une valeur d'intervalle de confiance
#'
#' @export
#'
#' @examples A<-rnorm(45,3,2)
#' sem(A)
#'
IC<-function(x)
{
  qt(0.975,(length(x)-1))*sqrt(var(x,na.rm=T))/sqrt(length(x)-1)
}


#--------------------------rar.rm  ---------------------------------------------

#' Suppression d'especes rares
#'
#' @description Supprime les especes n'ayant pas un certain nombre d'occurences
#'
#' @param tableau.AD un tableau de contingence
#' @param n un nombre minimal d'occurence
#'
#' @export
#'
#' @return le meme taleau de contingence, mais sans les especes dont le nombre d'occurence est inferieur a n
#'
#' @examples #a construire...
#'
rar.rm<-function(tableau.AD,n)
{
  tableau.PA<-data.frame(apply(tableau.AD,c(1,2),function(x) if(x>0) 1 else 0))
  occur<-apply(tableau.PA,2,sum)
  tableau.AD.wr<-tableau.AD[,occur>=n]
}


#--------------------------barres.plot  ---------------------------------------------

#' Diagramme en barre
#'
#' @description Fait un diagramme en barre avec la moyenne et les barres d'erreurs, possibilite de changer le calcul des barres d'erreur, et d'ajouter les lettres des tests post hoc
#' @param variable un vecteur avec les valeurs des variables
#' @param Facteur un facteur donnant les differentes modalites
#' @param lettres (facultatif) liste de type c("text","text") qui figurera au dessus des barres
#' @param ecart (facultatif) type de calcul de la barre d'erreur (sem, IC, sd, var, etc.)
#' @param ylim comme pour la fonction plot(), permet de preciser les limites de l'axe des ordonnees
#' @param ... possibilite de rajouter des arguments comme col,main,ylab,etc. associees a barplot
#' @seealso \code{\link{barplot}} pour tout les arguments dans \code{...}
#'
#' @export
#'
#' @examples #Donnees
#' biomasse<-c(rnorm(35,20,5),rnorm(35,15,3),rnorm(35,5,3))
#' traitement<-factor(rep(c("T1","T2","T3"),each=35))
#' #Graphique de base
#' barres.plot(biomasse,traitement)
#' #En changeant les barres d'erreurs
#' barres.plot(biomasse,traitement,ecart=sd)
#' #En ajoutant les lettres de post-hoc
#' barres.plot(biomasse,traitement,lettres=c("a","ab","b"))
#' #En changeant des parametres classiques de barplot...
#' barres.plot(biomasse,traitement,lettres=c("a","ab","b"),
#' col=c("cadetblue","gold","orange"),
#' xlab="Traitements herbicides",ylab="Biomasse",font.lab=3,
#' main="Efficacite des traitement")
#' #Pour Julie.., si on veut changer l'ordre des traitements, il faut reprendre la fonction factor()
#' traitement<-factor(traitement,levels=c("T3","T1","T2"))
#' barres.plot(biomasse,traitement)
#'
barres.plot<-function(variable,Facteur,lettres=c(""),ecart="sem",ylim="NP",...)
{
  errors.bars<-function(yv,z,nn,lettres,YL)
  {
    xv<-
      barplot(yv,
              ylim=YL,
              names=nn,...)
    g<-(max(xv,na.rm=T)-min(xv,na.rm=T))/50
    for(i in 1:length(xv))
    {
      lines(c(xv[i],xv[i]),c(yv[i]+z[i],yv[i]-z[i]))
      lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i],yv[i]+z[i]))
      lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i],yv[i]-z[i]))
      text(xv[i],(yv[i]+z[i]+0.07*max(yv,na.rm=T)),lettres[i])
    }
  }
  meandon<-tapply(variable,Facteur,mean,na.rm=T)
  sem<-function(x)
  {
    sqrt(var(x,na.rm=T))/sqrt(length(x))
  }
  IC<-function(x)
  {
    qt(0.975,(length(x)-1))*sqrt(var(x,na.rm=T))/sqrt(length(x)-1)
  }
  ecartdon<-tapply(variable,Facteur,ecart)
  ybar<-as.vector(meandon)
  se<-as.vector(ecartdon)
  labels<-as.character(levels(Facteur))
  YL=if(length(ylim)==1) c(0,(1.25*max(ybar,na.rm=T)+max(se,na.rm=T))) else ylim
  errors.bars(ybar,se,labels,lettres,YL=YL)
}


#--------------------------ComStructIndices  --------------------------------------------

#' Community Structure Integrity Indices
#'
#' @description Calculates indices of community integrity compared to a reference community according to Jaunatre et al. (2013).
#'
#' @param REF is the reference community data matrix
#' @param ASSESS is the assessed community data matrix
#' @param rar (facultative) Minimum number of samples in which species have to be present to be taken into account in the calculation of indices. Default value is 1. It should not be used in the indices calculation, but it can be useful to reduce the number of species with the structure.plot() function.
#'
#' @return \item{Comb}{A combined community data matrix of reference and assessed communities}
#' @return \item{Nam_Tot}{A list of species names corresponding to the Comb matrix}
#' @return \item{Nam_Tar}{A list of the target species names}
#' @return \item{REF_Tab}{Reference community data matrix (with zero values for species which were absent in the reference community)}
#' @return \item{ASSESS_Tab}{Assessed community data matrix (with zero values for species which were absent in the assessed community)}
#' @return \item{SumMeanAbREF}{Sum of mean abundances}
#' @return \item{SumAbASSESS}{A list of sum of species abundance for each sample}
#' @return \item{Diff}{Matrix of differences, for each species, between mean abundance in the reference samples and abundance in each assessed community samples}
#' @return \item{SumNeg}{Sum of negative differences issued from Diff, i.e. sum of 'higher abundances' in each of assessed community sample}
#' @return \item{SumPos}{Sum of positive differences issued from Diff, i.e. sum of 'missing abundances' in each of assessed community sample}
#' @return \item{CSII}{A list of Community Integrity Index in each assessed community sample}
#' @return \item{HAI}{A list of Higher Abundance Index in each of assessed community sample}
#' @return \item{CSIInorm}{A list of Normalized Community Integrity Index in each assessed community sample}
#' @return \item{AbMeanREFOnly}{A list of mean abundances of target species in reference samples}
#' @return \item{ASSESSTarOnly_Tab}{An assessed community data matrix with target species only}
#' @return \item{HigherOnly_Tab}{An assessed community data matrix with non-target species only}
#'
#' @seealso \code{\link{structure.plot}} for graphical output
#'
#' @export
#'
ComStructIndices<-function(REF, ASSESS, rar=1){
  ##------Combination of the two tables function-----------------------#
  combin.tab<-function(table1,table2)
  {
    ##------Removing of doubles function-----------------#
    doubl.rm<-function(list1,list2)
    {
      comb<-c(list1,list2) ## combine
      sort.comb<-comb[order(comb)] ## order
      ## remove doubles
      code<-NULL
      code[1]<-1
      for(i in 2:length(sort.comb))
      {
        code[i]<-ifelse(sort.comb[i]==sort.comb[i-1],0,1)
      }
      comb.wt.db<-sort.comb[code==1]
    }
    #------------------------------------------------------#

    ## Table creation
    liste<-doubl.rm(names(table1),names(table2))
    tabcomb<-data.frame(matrix(0,ncol=length(liste),nrow=nrow(table1)+nrow(table2)))
    names(tabcomb)<-as.character(liste)
    ## Table filling
    for (i in seq(along=liste))
    {
      ## table1
      tabcomb[1:nrow(table1),i]<-
        if(is.numeric(table1[,names(table1)==names(tabcomb)[i]])=="TRUE")
          table1[,names(table1)==names(tabcomb)[i]] else
            rep(0,nrow(table1))
      ## table2
      tabcomb[(nrow(table1)+1):nrow(tabcomb),i]<-
        if(is.numeric(table2[,names(table2)==names(tabcomb)[i]])=="TRUE")
          table2[,names(table2)==names(tabcomb)[i]] else
            rep(0,nrow(table2))
    }
    return(tabcomb)
  }
  #----------------------------------------------------------------#

  ## Combine REF and ASSESS tables
  Comb1<-combin.tab(REF,ASSESS)

  ##------Removing rare species function------------#
  rar.rm<-function(table.AD,n)
  {
    table.PA<-data.frame(apply(table.AD,c(1,2),function(x) if(x>0) 1 else 0))
    occur<-apply(table.PA,2,sum)
    table.AD.wr<-table.AD[,occur>=n]
  }
  #------------------------------------------------------#

  Comb<-rar.rm(Comb1,rar) ## Removing species which do not occur in the Comb table
  Nam_Tot<-names(Comb) ## List of all the species
  ## Removing of species which do not occur in the reference
  REF2<-rar.rm(REF,1)
  REF1<-REF2[,names(REF2) %in% Nam_Tot=="TRUE"]
  Nam_Tar<-names(REF1) ## List of target species
  REF_Tab<-Comb[1:nrow(REF),] ## Reference community table
  ASSESS_Tab<-Comb[(nrow(REF)+1):nrow(Comb),] ## Assessed community table

  ## ------------------------------------------------------------------#
  AbMeanREF<-apply(REF_Tab,2,mean,na.rm=T) ## Mean abundances in the reference community
  SumMeanAbREF<-sum(AbMeanREF) ## Sum of mean abundances in the reference community
  SumAbASSESS<-apply(ASSESS_Tab,1,sum,na.rm=T) ## Sum of Abundances in the assessed community

  ## Calculation of differences:
  Diff<-as.data.frame(t(apply(ASSESS_Tab,1,function(x) AbMeanREF-x)))
  rownames(Diff)<-paste("ASSESS_",1:nrow(ASSESS_Tab),sep="")
  colnames(Diff)<-names(ASSESS_Tab)
  ## Sum of negative abundances:
  DiffNeg<-data.frame(apply(Diff,c(1,2),function(x) if(x<0) x else 0))
  SumNeg<-apply(DiffNeg,1,function(x) -sum(x))
  ## Sum of positive abudances:
  DiffPos<-data.frame(apply(Diff,c(1,2),function(x) if(x>0) x else 0))
  SumPos<-apply(DiffPos,1,sum)

  ## Calculation of differences for reference communities:
  DiffTar<-as.data.frame(t(apply(REF_Tab,1,function(x) AbMeanREF-x)))
  rownames(DiffTar)<-paste("REF_",1:nrow(REF_Tab),sep="")
  colnames(DiffTar)<-names(REF_Tab)
  ## Sum of negative abundances:
  DiffNegTar<-data.frame(apply(DiffTar,c(1,2),function(x) if(x<0) x else 0))
  SumNegTar<-apply(DiffNegTar,1,function(x) -sum(x))
  ## Sum of positive abudances:
  DiffPosTar<-data.frame(apply(DiffTar,c(1,2),function(x) if(x>0) x else 0))
  SumPosTar<-apply(DiffPosTar,1,sum)

  ## -----------------------------------------------#
  ## Indices:
  CSII<-(SumMeanAbREF-SumPos)/SumMeanAbREF ## Community Structure Integrity Index
  Ind_PourcREF<-(SumMeanAbREF-SumPosTar)/SumMeanAbREF ## Percentage of Community Structure Integrity in the references
  CSIInorm<-CSII/mean(Ind_PourcREF) ## Normalized Community Structure Integrity Index
  HAI<-SumNeg/SumAbASSESS ## Higher Abundance Index

  ## table with only target species in the references:
  AbMeanREFOnly<-AbMeanREF[names(REF_Tab) %in% Nam_Tar=="TRUE"]
  ## table with only target species in the assessed communities:
  ASSESSTarOnly_Tab<-ASSESS_Tab[,names(ASSESS_Tab) %in% Nam_Tar=="TRUE"]
  ## table with only non-target species:
  HigherOnly_Tab<-ASSESS_Tab[,names(ASSESS_Tab) %in% Nam_Tar=="FALSE"]

  ## Output variables:
  Output<-list(Comb,Nam_Tot,Nam_Tar,REF_Tab,ASSESS_Tab,SumMeanAbREF,SumAbASSESS,Diff,SumNeg,SumPos,
               CSII,HAI,CSIInorm,
               AbMeanREFOnly,ASSESSTarOnly_Tab,HigherOnly_Tab)
  names(Output)[[1]]<-"Comb"
  names(Output)[[2]]<-"Nam_Tot"
  names(Output)[[3]]<-"Nam_Tar"
  names(Output)[[4]]<-"REF_Tab"
  names(Output)[[5]]<-"ASSESS_Tab"
  names(Output)[[6]]<-"SumMeanAbREF"
  names(Output)[[7]]<-"SumAbASSESS"
  names(Output)[[8]]<-"Diff"
  names(Output)[[9]]<-"SumNeg"
  names(Output)[[10]]<-"SumPos"
  names(Output)[[11]]<-"CSII"
  names(Output)[[12]]<-"HAI"
  names(Output)[[13]]<-"CSIInorm"
  names(Output)[[14]]<-"AbMeanREFOnly"
  names(Output)[[15]]<-"ASSESSTarOnly_Tab"
  names(Output)[[16]]<-"HigherOnly_Tab"
  return(Output)
}


#--------------------------Fonction structure.plot--------------------------------------------

#' structure community plots
#'
#' @description Performs a barplot of abundances of species in assessed community compared to a REFerence community
#' @param INDICE An object issued from \code{\link{ComStructIndices}} function
#' @param FACTOR A factor list, a barplot of species mean abundances will be performed for each factor level. If no factor is specified, MULTI=F should be specified.
#' @param MULTI If no factor is specified, MULTI=F should be specified
#' @param MTITLE Main title of the plot
#' @param ABMAX Numerical value of the maximum abundance
#' @param col1 Colour information for the Reference mean abundances barplot
#' @param col2 Colour information for the Reference mean abundances in assessed community barplot, i.e. "missing abundances"
#' @param col3 Colour information for the abundances of target species in the assessed community
#' @param col4 Colour information for the "higher abundances" in the assessed community
#' @param noms If other than "T", species names are not given
#' @param cex_noms expansion factor for species names
#' @param ... other parameters from the \code{\link{barplot}} function
#'
#' @seealso \code{\link{ComStructIndices}}
#'
#' @export
#'
structure.plot<-function(INDICE, FACTOR, MULTI=T, MTITLE="", ABMAX=5, col1="grey60",
                         col2="white",col3="red",col4="orange",noms="T",cex_noms=1,...){

  ## If there is only one level, creation of the level:
  FACTOR1<-if(MULTI==T) FACTOR else factor(rep("",length(INDICE$HAI)))
  ## target and non-target species tables
  TabCombinASSESS<-cbind(INDICE$ASSESSTarOnly_Tab,INDICE$HigherOnly_Tab)
  TabCombinREF<-c(INDICE$AbMeanREFOnly,rep(0,ncol(INDICE$HigherOnly_Tab)))

  ## Calculation of means
  Means<-if(MULTI==T) as.data.frame(t(apply(TabCombinASSESS,2,function(x) tapply(x,FACTOR1,mean,na.rm=T)))) else as.data.frame(apply(TabCombinASSESS,2,function(x) tapply(x,FACTOR1,mean,na.rm=T)))
  MeansALL<-if(MULTI==T) apply(Means,1,function(x) mean(as.numeric(x),na.rm=T)) else Means

  ## Ordering the species
  Abundance<-data.frame(INDICE.Nam_Tot=names(TabCombinASSESS),TabCombinREF,MeansALL,Means)
  sort_Abundance1<-data.frame(Abundance[order(-Abundance[,3]),])
  sort_Abundance<-sort_Abundance1[order(-sort_Abundance1[,2]),]

  ## Graphical parameters
  par(mfrow=c(1,length(levels(FACTOR1))+1),mar=c(2.5,0.5,1.5,0.25),oma = c(0,0,3,0))
  ## The reference
  ycoo<-barplot(-sort_Abundance$TabCombinREF,xlim=c(-1.4*ABMAX,0),col=col1,horiz=T,main="Reference")
  species.names<-if(noms=="T") sort_Abundance$INDICE.Nam_Tot else "" ## names definition
  text(-0.95*ABMAX,ycoo,species.names,cex=cex_noms) ## names drawing
  ## adding the assessed community barplot
  for (i in 1:length(levels(FACTOR1)))
  {
    barplot(sort_Abundance$TabCombinREF,col=col2,xlim=c(0,ABMAX),
            main=levels(FACTOR1)[i],horiz=T) ## baseline barplot of reference means
    barplot(sort_Abundance[,3+i],col=col4,xaxt="n",horiz=T,add=T) ## barplot of higherabundances
    MIN<-NULL # minimum between REF and ASSESS
    for (j in 1:length(sort_Abundance[,3+i]))
    {
      MIN[j]<-min(c(sort_Abundance[j,3+i],sort_Abundance$TabCombinREF[j]))
    }
    barplot(MIN,col=col3,horiz=T,xaxt="n",add=T) ## barplot of minimum
  }
  mtext(MTITLE,side = 3, outer = TRUE,font = 2) ## Adding titles to levels of the factor
}


#--------------------------Fonction combin.tab---------------------------------------------

##------Fonction Combinaison des 2 tableaux-----------------------#

#' Combinaison de tableau
#'
#' @description Permet de combiner 2 tableaux (de contingence ou pas), en fusionnant les colonnes ayant le meme nom
#'
#' @param tableau1 le premier tableau, avec les variables en colonnes et les releves en ligne
#' @param tableau2 le deuxieme tableau, comme le tableau1
#'
#' @return un tableau combine
#' @export
#'
#' @details Si on veut combiner plus de 2 tableaux, il vaut mieux faire un \code{\link{write.table}} et \code{\link{read.table}} pour chaque tableau intermediaire (j'ai deja eu quelques bugs...)
#'
#' Par defaut, les variables seront classees par ordre alphabetique, si vous voulez garder les premieres colonnes (quadrat, date, traitement, hauteur de veg) au debut,
#' il vaut mieux utiliser l'ancienne version de la fonction \code{\link{combin.tabV0}}, qui demande un autre argument \code{Liste} une liste des noms des colonnes, pour la faire, sous excel, c'est simple, si vous copiez les noms d'especes a la suite (sur une meme colonnes), vous faites supprimer les doublons apres l'avoir mis par ordre alaphabetique, il suffit de rajouter les variables d'information au debut de la liste, et voila, vous avez votre liste en txt!
#'
combin.tab<-function(tableau1,tableau2)
  {
    ##------Fonction Restrait des doublons-----------------#
    doubl.rm<-function(list1,list2)
    {
      comb<-c(list1,list2) ## combiner
      sort.comb<-comb[order(comb)] ## classer
      ## supprimer les doublons
      code<-NULL
      code[1]<-1
      for(i in 2:length(sort.comb))
      {
        code[i]<-ifelse(sort.comb[i]==sort.comb[i-1],0,1)
      }
      comb.wt.db<-sort.comb[code==1]
    }
    #------------------------------------------------------ #

    ## Creation du tableau
    liste<-doubl.rm(names(tableau1),names(tableau2))
    tabcomb<-data.frame(matrix(0,ncol=length(liste),nrow=nrow(tableau1)+nrow(tableau2)))
    names(tabcomb)<-as.character(liste)
    ## Remplissage du tableau
    for (i in seq(along=liste))
    {
      ## tableau1
      tabcomb[1:nrow(tableau1),i]<-
      if(is.numeric(tableau1[,names(tableau1)==names(tabcomb)[i]])=="TRUE")
      tableau1[,names(tableau1)==names(tabcomb)[i]] else
      rep(0,nrow(tableau1))
      ## tableau2
      tabcomb[(nrow(tableau1)+1):nrow(tabcomb),i]<-
      if(is.numeric(tableau2[,names(tableau2)==names(tabcomb)[i]])=="TRUE")
      tableau2[,names(tableau2)==names(tabcomb)[i]] else
      rep(0,nrow(tableau2))
    }
  return(tabcomb)
  }

#' Version 0 de combin.tab
#' @description Permet de combiner 2 tableaux (de contingence ou pas), en fusionnant les colonnes ayant le meme nom
#'
#' @param tableau1 le premier tableau, avec les variables en colonnes et les releves en ligne
#' @param tableau2 le deuxieme tableau, comme le tableau1
#' @param Liste une liste des noms des colonnes du tableau final souhaite
#'
#' @return un tableau combine
#' @export
#' @details Si on veut combiner plus de 2 tableaux, il vaut mieux faire un \code{\link{write.table}} et \code{\link{read.table}} pour chaque tableau intermediaire (j'ai deja eu quelques bugs...)
#'
#' Par defaut, les variables seront classees par ordre alphabetique, si vous voulez garder les premieres colonnes (quadrat, date, traitement, hauteur de veg) au debut,
#' il vaut mieux utiliser l'ancienne version de la fonction \code{\link{combin.tabV0}}, qui demande un autre argument \code{Liste} une liste des noms des colonnes, pour la faire, sous excel, c'est simple, si vous copiez les noms d'especes a la suite (sur une meme colonnes), vous faites supprimer les doublons apres l'avoir mis par ordre alaphabetique, il suffit de rajouter les variables d'information au debut de la liste, et voila, vous avez votre liste en txt!
#'
combin.tabV0<-function(Tableau1,Tableau2,Liste)
{
  Tabcomb<-data.frame(matrix(0,nrow=length(Tableau1[,1])+length(Tableau2[,1]),
                             ncol=length(Liste)))
  ## Donner un nom aux colonnes:
  names(Tabcomb)<-sapply(Liste,as.character)
  ## Remplir chaque colonne:
  for (i in 1:length(Liste))
  {
    ## tableau1
    Tabcomb[1:nrow(Tableau1),i]<-
      if(is.numeric
         (Tableau1[,names(Tableau1)==names(Tabcomb)[i]])=="TRUE")
        Tableau1[,names(Tableau1)==names(Tabcomb)[i]] else
          if(is.factor
             (Tableau1[,names(Tableau1)==names(Tabcomb)[i]])=="TRUE")
            as.character(Tableau1[,names(Tableau1)==names(Tabcomb)[i]]) else
              rep(0,nrow(Tableau1))
    ## tableau2
    Tabcomb[(nrow(Tableau1)+1):nrow(Tabcomb),i]<-
      if(is.numeric
         (Tableau2[,names(Tableau2)==names(Tabcomb)[i]])=="TRUE")
        Tableau2[,names(Tableau2)==names(Tabcomb)[i]] else
          if(is.factor
             (Tableau2[,names(Tableau2)==names(Tabcomb)[i]])=="TRUE")
            as.character(Tableau2[,names(Tableau2)==names(Tabcomb)[i]]) else
              rep(0,nrow(Tableau2))
  }
  return(Tabcomb)
}


#--------------------------raup.calc  ---------------------------------------------

#' valeur de similarite/dissimilarite
#'
#' @description calcule un indice de similarite/dissimilarite entre 2 releves
#'
#' @param tableau tableau de releves especes en colonnes, releves en lignes
#' @param B numero du releves dont le numero de ligne est le plus eleve
#' @param A numero du releves dont le numero de ligne est le plus faible
#' @param method ethode a utiliser cf l'aide de \code{\link{vegdist}}
#' @param binary T ou F selon que l'on travaille en presence absence ou non (T par defaut)
#'
#' @return une valeur de similarite/dissimilarite
#' @seealso \code{\link{vegdist}}
#' @export
#' @import vegan
#'
raup.calc<-function(tableau,B,A,method,binary=T)
{
  method<-method
  matrice<-vegdist(tableau, method=method,binary=binary)
  n<- -(length(tableau[,1])-1)
  if(B>A) {b<-B
  a<-A} else {b<-A
  a<-B}
  for (i in 1:a)
  {
    n<-n+(length(tableau[,1]))-(i-1)
  }
  raup<-(n-1)+(b-a)
  result<-matrice[raup]
}

#--------------------------DissRef ---------------------------------------------

#' Dissimilarite a une reference
#'
#' @description Calcul de moyennes d'indices de similarite/dissimilarite entre des releves et plusieurs releves de references
#'
#' @param RELEVES tableau de releves especes en colonnes, releves en lignes
#' @param ETENDUEREL etendues des releves sur lesquels le calcul sera fait (exemple 1:9 OU 3:11)
#' @param ETENDUEREF etendues des releves de reference (exemple 1:3)
#' @param method methode a utiliser cf l'aide de \code{\link{vegdist}}
#' @param binary T ou F selon que l'on travaille en presence absence ou non (T par defaut)
#' @export
#' @import vegan
#'
#' @return Renvoie une liste de valeurs, une par releves chaque valeur correspond a la moyenne des indices qui ont ete calcules entre ce releves et tous les releves de references
#' @seealso \code{\link{vegdist}} \code{\link{raup.calc}}
DissRef=function(RELEVES,ETENDUEREL,ETENDUEREF,method,binary=T)
{
 Diss=NULL
 for (i in ETENDUEREL)
  {
    Dissn=NULL
    a<-ETENDUEREF
    b<-a[a!=i]
    binary<-binary
    for (j in b)
    {
      Dissn[j]<-raup.calc(RELEVES,max(c(i,j)),min(c(i,j)),method,binary)
    }
    Diss[i]<-mean(Dissn[b])
  }
  return(Diss)
}

#--------------------------label.cor ---------------------------------------------

#' Affichage d'especes correlees
#'
#' @description Calcul les correlations des especes avec les axes puis n'affiche que les plus correlees
#'
#' @param ANALYSE Objet resultant d'une analyse multivariee \code{\link{dudi.pca}}, \code{\link{dudi.coa}}, \code{\link{metaMDS}} ou \code{\link{decorana}}
#' @param RELEVES les releves ayant servi a faire l'analyse multivariee
#' @param p1 a p value minimale de correlation avec l'axe 1 pour etre affichee
#' @param p2 la p value minimale de correlation avec l'axe 2 pour etre affichee
#' @param coef un coef multiplicateur (pour exploser ou non la dispersion dans le plan)
#' @param add par defaut, creer un graphe seul, si add=T, les noms d'especes sont ajoute au graphe deja existant
#'
#' @details Attention, des erreurs souvent lorsqu'il y a des especes avec 0 occurences!!!! (hein Julie!)
#'
#' @export
#' @import ade4
#'
#' @seealso  \code{\link{label.cor}} apres  \code{\link{dudi.pca}} ou \code{\link{dudi.coa}} ;
#' \code{\link{label.corNMDS}} apres une NMDS avec  \code{\link{metaMDS}} ;
#'  \code{\link{label.corDCA}} apres une DCA avec  \code{\link{decorana}}
#'
label.cor<-function(ANALYSE,RELEVES,p1=0.05,p2=0.05,coef=1,add=F){
  pcorspear1<-apply(RELEVES,2,function (x)
      cor.test(rank(ANALYSE$li[1]),rank(x),method="spearman")$p.value)
  pcorspear2<-apply(RELEVES,2,function (x)
      cor.test(rank(ANALYSE$li[2]),rank(x),method="spearman")$p.value)
  if(add==F) plot(ANALYSE$li[,1:2],type="n")
  s.label(coef*ANALYSE$co[pcorspear1<p1,],boxes=F,add.plot=T)
  s.label(coef*ANALYSE$co[pcorspear2<p2,],boxes=F,add.plot=T)
  }

#' Affichage d'especes correlees
#'
#' @description Calcul les correlations des especes avec les axes puis n'affiche que les plus correlees
#'
#' @param ANALYSE Objet resultant d'une analyse multivariee \code{\link{dudi.pca}}, \code{\link{dudi.coa}}, \code{\link{metaMDS}} ou \code{\link{decorana}}
#' @param RELEVES les releves ayant servi a faire l'analyse multivariee
#' @param p1 a p value minimale de correlation avec l'axe 1 pour etre affichee
#' @param p2 la p value minimale de correlation avec l'axe 2 pour etre affichee
#' @param coef un coef multiplicateur (pour exploser ou non la dispersion dans le plan)
#' @param add par defaut, creer un graphe seul, si add=T, les noms d'especes sont ajoute au graphe deja existant
#'
#' @details Attention, des erreurs souvent lorsqu'il y a des especes avec 0 occurences!!!! (hein Julie!)
#'
#' @export
#' @import ade4
#'
#' @seealso  \code{\link{label.cor}} apres  \code{\link{dudi.pca}} ou \code{\link{dudi.coa}} ;
#' \code{\link{label.corNMDS}} apres une NMDS avec  \code{\link{metaMDS}} ;
#'  \code{\link{label.corDCA}} apres une DCA avec  \code{\link{decorana}}
#'
label.corNMDS<-function(ANALYSE,RELEVES,p1=0.05,p2=0.05,coef=1,add=F){
  pcorspear1<-apply(RELEVES,2,function (x)
    cor.test(rank(ANALYSE$points[,1]),rank(x),method="spearman")$p.value)
  pcorspear2<-apply(RELEVES,2,function (x)
    cor.test(rank(ANALYSE$points[,2]),rank(x),method="spearman")$p.value)
  if(add==F) plot(ANALYSE$points[,1:2],type="n")
  s.label(coef*ANALYSE$species[pcorspear1<p1,],boxes=F,add.plot=T)
  s.label(coef*ANALYSE$species[pcorspear2<p2,],boxes=F,add.plot=T)
}

#' Affichage d'especes correlees
#'
#' @description Calcul les correlations des especes avec les axes puis n'affiche que les plus correlees
#'
#' @param ANALYSE Objet resultant d'une analyse multivariee \code{\link{dudi.pca}}, \code{\link{dudi.coa}}, \code{\link{metaMDS}} ou \code{\link{decorana}}
#' @param RELEVES les releves ayant servi a faire l'analyse multivariee
#' @param p1 a p value minimale de correlation avec l'axe 1 pour etre affichee
#' @param p2 la p value minimale de correlation avec l'axe 2 pour etre affichee
#' @param coef un coef multiplicateur (pour exploser ou non la dispersion dans le plan)
#' @param add par defaut, creer un graphe seul, si add=T, les noms d'especes sont ajoute au graphe deja existant
#'
#' @export
#' @import ade4
#'
#' @details Attention, des erreurs souvent lorsqu'il y a des especes avec 0 occurences!!!! (hein Julie!)
#'
#' @seealso  \code{\link{label.cor}} apres  \code{\link{dudi.pca}} ou \code{\link{dudi.coa}} ;
#' \code{\link{label.corNMDS}} apres une NMDS avec  \code{\link{metaMDS}} ;
#'  \code{\link{label.corDCA}} apres une DCA avec  \code{\link{decorana}}
#'
label.corDCA=function(ANALYSE,RELEVES,p1=0.05,p2=0.05,coef=1,add=F){
  pcorspear1<-apply(RELEVES,2,function (x)
    cor.test(rank(ANALYSE$rproj[,1]),rank(x),method="spearman")$p.value)
  pcorspear2<-apply(RELEVES,2,function (x)
    cor.test(rank(ANALYSE$rproj[,2]),rank(x),method="spearman")$p.value)
  if(add==F) plot(ANALYSE$rproj[,1:2],type="n")
  s.label(coef*ANALYSE$cproj[pcorspear1<p1,],label=row.names(ANALYSE$cproj[pcorspear1<p1,]),boxes=F,add.plot=T)
  s.label(coef*ANALYSE$cproj[pcorspear2<p2,],label=row.names(ANALYSE$cproj[pcorspear2<p2,]),boxes=F,add.plot=T)
}

#--------------------------BBtransf---------------------------------------------

#' transformation de Braun-Blanquet
#'
#' @description Transforme les coef de Braun-Blanquet en pourcentage (moyenne des classes) Guinochet M. (1973). Phytosociologie. Masson, Paris
#'
#' @param comtab un tableau de releves de vegetation
#' @param plus la valeurs par lequel le + a ete transcrit... souvent c'est 0.1, 0.2 ou 0.5 (par defaut ici c'est 0.1)
#'
#' @export
#'
#' @return un tableau avec les valeurs transformees
#'
BBtransf<-function(comtab,plus=0.1){
  transf<-function(x){
  xtransf<-if(x==5) 87.5 else
            if(x==4) 62.5 else
            if(x==3) 37.5 else
            if(x==2) 17.5 else
            if(x==1) 5 else
            if(x==plus) 0.1 else 0
  }
  transftab<-data.frame(apply(comtab,c(1,2),transf))
}

#--------------------------Time.factor.plot ---------------------------------------------

#' Time.factor.plot
#'
#' @description Trace une courbe par modalite d'un facteur d'une variable en fonction du temps (moyenne + barres d'erreurs)
#'
#' @param TIME un vecteur comportant les donnees "temps" ou toute autre variable quantitative qui sera tracee en abscisse
#' @param FACTOR un vecteur comportant les donnees du facteur (il sera transformee en facteur qualitatif)
#' @param variable un vecteur comportant les donnees a tracer
#' @param xlab etiquette des abcsisses, par defaut: TIME
#' @param ylab tiquette des ordonnees, par defaut: variable
#' @param pch comme pour la fonction \code{\link{plot}}, permet d'assigner un symbole par modalite du facteur FACTOR
#' @param couleur permet d'assigner une couleur a chaque modalite du facteur FACTOR
#' @param lty permet d'assigner une forme de ligne par modalite du facteur FACTOR
#' @param lwd permet d'assigner une épaisseur de ligne par modalite du facteur FACTOR
#' @param xlim comme pour la fonction plot(), permet de preciser les limites de l'axe des abscisses
#' @param ylim comme pour la fonction plot(), permet de preciser les limites de l'axe des ordonnees
#' @param Posit_leg permet de preciser la position de la legende, soit c(x,y), soit "topleft", "bottomright", etc.
#' @param linktyp permet de pr?ciser quel type de lien entre les points... : \code{"p"} for points,\code{"l"} for lines, \code{"o"} for overplotted points and lines, \code{"b"}, \code{"c"})
#' @param etoiles permet de mettre du texte (ou des etoiles) au dessus de chacune des "TIME"
#' @param ... autres arguments associes a la fonction \code{\link{plot}}
#'
#' @details Ne pas faire attention aux messages d'erreur du type suivant, je n'ai pas encore reussi a les retirer...
#' Messages d'avis :
#' "1: In if (couleur == c(0)) rep(1, length(levels(factor(FACTOR)))) else couleur :
#'    la condition a une longueur > 1 et seul le premier element est utilise"
#'
#' @export
#'
#' @examples # Exemple d'application :
#' # Creation des variables / facteurs
#' tim<-rep(c(1:4),each=6)
#' fac<-factor(rep(rep(c("A","B"),each=3),4))
#' vari<-c(rnorm(3,5,2),rnorm(3,3,2),rnorm(3,3,2),rnorm(3,8,2),
#'         rnorm(3,8,2),rnorm(3,10,2),rnorm(3,16,2),rnorm(3,12,2))
#'
#' #Utilisation de la fonction :
#' Time.factor.plot(tim,fac,vari,etoiles=c("*","***","","***"))
#'
Time.factor.plot<-function(TIME,FACTOR,variable,xlab="TIME",ylab="variable"
                           ,pch=c(0),couleur=c(0),lty=c(0),lwd=c(0),xlim="NP",ylim="NP",
                           Posit_leg="topleft",linktyp="b",etoiles=c(""),...)
{
  #Regler les couleurs et symboles et lignes... :
  couleur=if(couleur==c(0)) rep(1,length(levels(factor(FACTOR)))) else couleur
  pch=if(pch==c(0)) rep(1,length(levels(factor(FACTOR)))) else pch
  linktyp=linktyp
  lty=if(lty==c(0)) rep(1,length(levels(factor(FACTOR)))) else lty
  lwd=if(lwd==c(0)) rep(1,length(levels(factor(FACTOR)))) else lwd
  #La fonction point.barres
  point.barres=function(moyenne,abscisse,erreur,couleur,pch,lty,lwd)
  {
    points(abscisse,moyenne,col=couleur,pch=pch,lty=lty,lwd=lwd,type=linktyp)
    g=(max(abscisse,na.rm=T)-min(abscisse,na.rm=T))/50
    for(i in 1:length(abscisse))
    {
      lines(c(abscisse[i],abscisse[i]),
            c(moyenne[i]+erreur[i],moyenne[i]-erreur[i]),col=couleur)
      lines(c(abscisse[i]-g,abscisse[i]+g),
            c(moyenne[i]+erreur[i],moyenne[i]+erreur[i]),col=couleur)
      lines(c(abscisse[i]-g,abscisse[i]+g),
            c(moyenne[i]-erreur[i],moyenne[i]-erreur[i]),col=couleur)
    }
  }

  #D?finition des limites
  XL=if(xlim=="NP") c(0,max(as.numeric(levels(factor(TIME))),na.rm=T)) else xlim
  YL=if(ylim=="NP") c(0,1.1*max(variable,na.rm=T)) else ylim

  #Fen?tre graphique
  plot(c(1,1),xlim=XL,
       ylim=YL,
       xlab=xlab,
       ylab=ylab,type="n",...)

  #Definition du facteur
  FACTOR=factor(FACTOR)

  #Tracer pour chaque modalit? du facteur, l'?volution en fonction du temps
  for(a in 1:length(levels(factor(FACTOR))))
  {
    #Calcul des moyennes,
    meandon=tapply(variable[FACTOR==levels(factor(FACTOR))[a]],
                   TIME[FACTOR==levels(factor(FACTOR))[a]],mean,na.rm=T)
    #Calcul des erreurs-type
    sem=function(x)
    {
      sqrt(var(x,na.rm=T))/sqrt(length(x))
    }
    semdon=tapply(variable[FACTOR==levels(factor(FACTOR))[a]],
                  factor(TIME[FACTOR==levels(factor(FACTOR))[a]]),sem)
    #Rendre utilisable les valeurs :
    absc1=as.numeric(levels(factor(TIME[FACTOR==levels(factor(FACTOR))[a]])))
    meandon=as.numeric(meandon)
    semdon=as.numeric(semdon)
    #mettre dans l'ordre...
    AA=data.frame(absc1,meandon,semdon)
    BB=AA[order(AA$absc1),]
    #Tracer les points des moyennes en fonction du temps
    #Avec les barres d'erreur :
    point.barres(BB$meandon,BB$absc,BB$semdon,couleur=couleur[a],
                 pch=pch[a],lty=lty[a],lwd=lwd[a])
  }

  #Placement ?toiles
  #calcul valeurs max :
  max_moy=NULL
  sem_max=NULL
  #pour chaque modalit? de TIME
  for (i in 1:length(unique(TIME)))
  {
    #calcul des moyennes pour chaque modalit? de FACTOR
    max_mo<-tapply(variable[TIME==unique(TIME)[i]],FACTOR[TIME==unique(TIME)[i]],mean,na.rm=T)
    max_moy[i]<-max(max_mo) #Moyenne max
    mod_max<-levels(FACTOR[TIME==unique(TIME)[i]])[max_mo==max_moy[i]] #modalit? ayant la valeur max
    #erreur standard pour la modalit? ayant la moyenne max
    sem_max[i]<-sem(variable[TIME==unique(TIME)[i]][FACTOR[TIME==unique(TIME)[i]]==mod_max])
  }
  max_tim<-0.05*max(variable,na.rm=T)+max_moy+sem_max
  text(x=unique(TIME),y=max_tim,labels=etoiles)

  #Ajout de la l?gende
  Posit_leg=Posit_leg
  legend(Posit_leg,as.character(levels(factor(FACTOR))),pch=pch,col=couleur)
}

#--------------------------Head2head.plot  -------------------------------
#Fonction pour graphes en tetes a tetes :

### Trace des histogrammes en vis en vis avec barres d'erreurs et calculs de Wilcoxon
# en fonction d'un facteura2 modalit?s

### Arguments:
## TABL -- un tableau de donn?es avec les variables colonnes
## FACTEUR -- un vecteur facteur, devant ?tre factoris?, contenant 2 modalit?s exactement
 # de la m?me taille que le nombre de lignes de TABL
## col1 -- la couleur des barres de la modalit? 1
## col2 -- la couleur des barres de la modalit? 2
## cex_noms -- taille de la police des noms des variables
## erreur -- type d'erreur calcul? pour la barre d'erreur
 #(erreur standar: "sem", intervalle de confiance "IC", variance: var
## adj_meth -- m?thode d'ajustement du p calcul? : "bond","BH","hoch", etc. ou "none"


#Attention, le premier graphe sera la premi?re modalit? de ton facteur, par defaut dans l'ordre
#alphab?tique...

#Attention, il peut y avoir des messages d'erreur, c'est li?ades ex aequo dans les valeurs,
#pour le calcul de wilcoxon, mais bon, ?a ne change rien...


#' Plot en tete a tete
#'
#' @description Trace des histogrammes en vis en vis avec barres d'erreurs et calculs de Wilcoxon en fonction d'un facteur a 2 modalites
#'
#' @param TABL un tableau de donnees avec les variables colonnes
#' @param FACTEUR un vecteur facteur, devant etre factorise, contenant 2 modalites exactement de la meme taille que le nombre de lignes de \code{TABL}
#' @param col1 la couleur des barres de la modalite 1
#' @param col2 la couleur des barres de la modalite 2
#' @param cex_noms taille de la police des noms des variables
#' @param erreur type d'erreur calcule pour la barre d'erreur (erreur standard : \code{"sem"}, intervalle de confiance : \code{"IC"}, variance : \code{var})
#' @param adj_meth methode d'ajustement du p calcule : \code{"bond"},\code{"BH"},\code{"hoch"}, etc. ou \code{"none"}
#'
#' @export
#'
#' @details Attention, le premier graphe sera la premiere modalite de ton facteur, par defaut dans l'ordre
#' alphabetique...
#'
#' Attention, il peut y avoir des messages d'erreur, c'est lie a des ex aequo dans les valeurs,
#' pour le calcul de wilcoxon, mais bon, ca ne change rien...
#'
Head2head.plot<-function(TABL,FACTEUR,col1=2,col2=3,cex_noms=1,erreur="sem",adj_meth="BH")
{

  moyennes<-as.data.frame(t(apply(TABL,2,function(x)
    tapply(x,FACTEUR,mean,na.rm=T)))) ## Calcul des moyennes

  ## Calcul des erreurs standards
  #fonction pour calcul erreur standard et ou intervalle de confiance
  sem<-function(x)  {sqrt(var(x,na.rm=T))/sqrt(length(x))}
  IC<-function(x)  {qt(0.975,(length(x)-1))*sqrt(var(x,na.rm=T))/sqrt(length(x)-1)}
  #Calcul de l'erreur
  ecart<-as.data.frame(t(apply(TABL,2,function(x) tapply(x,FACTEUR,erreur))))

  ## Cr?ation tableau et mise en ordre croissant (d'abord les especes cibles puis les autres)
  sort_moy1<-data.frame(moyennes[order(-moyennes[,2]),])
  sort_moy<-sort_moy1[order(-sort_moy1[,1]),]

  sort_eca1<-data.frame(ecart[order(-moyennes[,2]),])
  sort_eca<-sort_eca1[order(-sort_moy1[,1]),]

  ### L?gence avec nom espece et lettres tests :
  #Test de Wilcoxon
  WIL1=NULL
  for(i in 1:ncol(TABL))
  {
    wtest=wilcox.test(TABL[,i]~FACTEUR)
    WIL1[i]=wtest$p.value
  }
  #Ajustement du p
  WIL=p.adjust (WIL1, method = adj_meth)
  #Attribution des lettres
  lettre=NULL
  for(i in 1:ncol(TABL))
  {
    lettre[i]=if(WIL[i]>0.05) "NS" else if(WIL[i]>0.01) "*" else if(WIL[i]>0.001) "**" else "***"
  }
  #classement des noms:
  sort_noms1<-data.frame(colnames(TABL)[order(-moyennes[,2])])
  sort_noms<-sort_noms1[order(-sort_moy1[,1]),]
  #Fusion lettre et noms
  legende<-paste(lettre,sort_noms,sep=" ")

  ## D?finition des param?tres graphiques
  x11()
  par(mfrow=c(1,length(levels(FACTEUR))),mar=c(2.5,0.5,1.5,0.25),oma = c(0,0,0,0))
  ## R?cup?ration des coordonn?es et trac? de la r?f?rence
  ycoo1<-barplot(-sort_moy[,1],xlim=c(-1.4*max(sort_moy+sort_eca),0),col=col1,horiz=T,main=levels(FACTEUR)[1])
  g<-(max(ycoo1,na.rm=T)-min(ycoo1,na.rm=T))/50
  for(i in 1:length(ycoo1))
  {

    lines(c(-(sort_moy[i,1]+sort_eca[i,1]),-(sort_moy[i,1]-sort_eca[i,1])),c(ycoo1[i],ycoo1[i]))
    lines(c(-(sort_moy[i,1]+sort_eca[i,1]),-(sort_moy[i,1]+sort_eca[i,1])),c(ycoo1[i]+g,ycoo1[i]-g))
    lines(c(-(sort_moy[i,1]-sort_eca[i,1]),-(sort_moy[i,1]-sort_eca[i,1])),c(ycoo1[i]+g,ycoo1[i]-g))
  }

  #Tracer les noms...
  text(-0.95*max(sort_moy+sort_eca),ycoo1,legende,cex=cex_noms)

  #Tracer l'autre
  ycoo2<-barplot(sort_moy[,2],xlim=c(0,1.4*max(sort_moy+sort_eca)),col=col2,horiz=T,main=levels(FACTEUR)[2])
  for(i in 1:length(ycoo2))
  {

    lines(c((sort_moy[i,2]+sort_eca[i,2]),(sort_moy[i,2]-sort_eca[i,2])),c(ycoo2[i],ycoo2[i]))
    lines(c((sort_moy[i,2]+sort_eca[i,2]),(sort_moy[i,2]+sort_eca[i,2])),c(ycoo2[i]+g,ycoo2[i]-g))
    lines(c((sort_moy[i,2]-sort_eca[i,2]),(sort_moy[i,2]-sort_eca[i,2])),c(ycoo2[i]+g,ycoo2[i]-g))
  }
}

#--------------------------MultiDyn---------------------------------------------

#' Tracer la dynamique en multivariee
#' @description Trace les trajectoires de barycentres de modalites apres une analyse multivariee
#' @param MultCOO Les coordonnees de points sortant d'une analyse multivariee, par exemple si apres AFC sous ade4 (nommee \code{AFC_calc}) : \code{AFC_calc$li} par exemple si apres NMDS sous vegan (nommee \code{NMDS_calc}) : \code{NMDS$points}
#' @param FACTime le facteur temps, chaque modalit? corresponds un point de la ligne de trajectoire, il faut imperativement les avoir classe dans l'ordre
#' @param FACT2 le facteur qui permet de regrouper des points, pour chaque modalite sera  representee une trajectoire, le nom de la modalite est affiche au debut de chaque trajectoire
#' @param Axis (facultatif, defaut: c(1,2)) precise les numero des axes a tracer (correspondant au numero des colonnes de \code{MultCOO})
#' @param LTY (facultatif, defaut: 1) le format des lignes, cf \code{lty}
#' @param CEX (facultatif, defaut: 1) la taille du texte, cf \code{cex}
#' @param COL (facultatif, defaut: 1) la couleur des lignes et du texte, si une seule couleur est precisee, toutes les trajectoires sont de la meme couleur, si plusieurs couleurs sont precisees, une couleur par trajectoire
#' @param ARR (facultatif, defaut: F) trace ou non une fleche a la fin de la trajectoire si \code{ARR=T}
#' @param LARR (facultatif, defaut: 0.1) si \code{ARR=T}, determine la taille de la fleche
#' @param new si new="yes", trace les fleches sur une nouvelle fenetre (defaut), sinon, sur le graphe existant
#' @param ... d'autres arguments applicables a la fonction \code{\link{plot}}
#'
#' @export
MultiDyn<-function(MultCOO,FACTime,FACT2,Axis=c(1,2),
                   LTY=1,CEX=1,COL=1,ARR=F,LARR=0.1,new="yes",...)
{

  if(new=="yes") plot(MultCOO[,Axis],type="n",...)
  FACTime=factor(FACTime)
  NamMod<-levels(FACT2)
  for (i in 1:length(levels(FACT2)))
  {
    LTY2<-ifelse(length(LTY)==1,LTY,LTY[i])
    COL2<-ifelse(length(COL)==1,COL,COL[i])
    n<-length(levels(FACTime))
    CooX=tapply(MultCOO[FACT2==levels(FACT2)[i],1],FACTime[FACT2==levels(FACT2)[i]],function(x) mean(x,na.rm=T))
    CooY=tapply(MultCOO[FACT2==levels(FACT2)[i],2],FACTime[FACT2==levels(FACT2)[i]],function(x) mean(x,na.rm=T))
    text(CooX[1],CooY[1],NamMod[i],cex=CEX,col=COL2)
    lines(CooX,CooY,lty=LTY2,col=COL2,lend=3)
    if(ARR==T) arrows(CooX[n-1],CooY[n-1],CooX[n],
                      CooY[n],col=COL2,lty=LTY2,length=LARR)
  }
}
#--------------------------------------------------------------------------------------------------- -


#--------------------------------structure.plotV2---------------------------------

#' structure plot version 2
#'
#' @description idem que \code{\link{structure.plot}} mais avec quelques arguments en plus (notament barres d'erreur, calcul de difference a la reference, etc.)
#'
#' @param INDICE An object issued from \code{\link{ComStructIndices}} function
#' @param FACTOR A factor list, a barplot of species mean abundances will be performed for each factor level. If no factor is specified, MULTI=F should be specified.
#' @param MULTI If no factor is specified, MULTI=F should be specified
#' @param MTITLE Main title of the plot
#' @param ABMAX Numerical value of the maximum abundance
#' @param col1 Colour information for the Reference mean abundances barplot
#' @param col2 Colour information for the Reference mean abundances in assessed community barplot, i.e. "missing abundances"
#' @param col3 Colour information for the abundances of target species in the assessed community
#' @param col4 Colour information for the "higher abundances" in the assessed community
#' @param noms If other than "T", species names are not given
#' @param cex_noms expansion factor for species names
#' @param erreur (facultatif) type de calcul de la barre d'erreur \code{"sem"} (par defaut) calculera l'erreur standard (ecart type divise par n) \code{"IC"} calculera l'intervalle de confiance a 5 pourcent (quantile 97.5 a n-1 degre de liberte multiplie par l'ecart-type divise par la racine de n-1) \code{"var"} ou \code{"sd"} ou tout autre fonction de calcul connue de R Attention, ne supporte pas les \code{NA}... (j'ai deja essaye de modifier mais sans succes...)
#' @param w_err largeur de la barre d'erreur, 1 par defaut Visiblement ca peut encore etre optimise, la taille par defaut est bonne dans certains cas moins dans d'autres...a ajuster donc
#' @param sp_star ecartement des etoiles par rapport aux barres d'erreurs, 1 par defaut
#' @param adj_meth methode d'ajustement du p : \code{"bond"},\code{"BH"},\code{"hoch"}, etc. ou \code{"none"} (le p correspond a un test de wilcoxon entre la reference et la modalite evaluee)
#' @param stars "T" par defaut, dessine les etoiles, toute autre mention ne les dessinera pas
#' @param BASE T par defaut, dessine le graphe de refe pour chaque modalite, si F, pas de ligne de base
#' @param ... all other arguments used by \code{\link{barplot}}
#' @seealso \code{\link{ComStructIndices}} et \code{\link{structure.plot}}
#' @details Attention, le p est ajuste pour une modalite, en toute rigueur il faudrait sans doute l'ajuster sur l'ensemble des calculs
#' Attention, il peut y avoir des messages d'erreur, c'est lie a des ex aequo dans les valeurs, pour le calcul de wilcoxon, mais bon, ca ne change rien...'
#'
#' @export
#'
#' @examples IndiC=ComStructIndices(releves[Type=="COUS",],releves,rar=5)
#' structure.plotV2(IndiC,Type,ABMAX=5,sp_star=2,w_err=0.1)
#'
structure.plotV2<-function(INDICE, FACTOR, MULTI=T, MTITLE="", ABMAX=5, col1="grey60",
                           col2="white",col3="red",col4="orange",noms="T",cex_noms=1,
                           erreur="sem",w_err=1,sp_star=1,adj_meth="BH",stars="T",
                           BASE=T,...)#--V2
{
  ## If there is only one level, creation of the level:
  FACTOR1<-if(MULTI==T) FACTOR else factor(rep("",length(INDICE$HAI)))
  ## target and non-target species tables
  TabCombinASSESS<-cbind(INDICE$ASSESSTarOnly_Tab,INDICE$HigherOnly_Tab)
  TabCombinREF<-c(INDICE$AbMeanREFOnly,rep(0,ncol(INDICE$HigherOnly_Tab)))

  ## Calculation of means
  Means<-if(MULTI==T) as.data.frame(t(apply(TabCombinASSESS,2,function(x) tapply(x,FACTOR1,mean,na.rm=T)))) else as.data.frame(apply(TabCombinASSESS,2,function(x) tapply(x,FACTOR1,mean,na.rm=T)))
  MeansALL<-if(MULTI==T) apply(Means,1,function(x) mean(as.numeric(x),na.rm=T)) else Means

  ## Calculation of errors
  #fonction pour calcul erreur standard et ou intervalle de confiance
  sem<-function(x)  {sqrt(var(x,na.rm=T))/sqrt(length(x))} #--V2
  IC<-function(x)  {qt(0.975,(length(x)-1))*sqrt(var(x,na.rm=T))/sqrt(length(x)-1)}#--V2

  #Calcul des erreurs en fonction des modalit?s
  Errors<-if(MULTI==T) as.data.frame(t(apply(TabCombinASSESS,2,function(x) tapply(x,FACTOR1,erreur)))) else as.data.frame(apply(TabCombinASSESS,2,function(x) tapply(x,FACTOR1,erreur)))#--V2

  #R?cup?ration des donn?es de la r?f?rences, en mettant les especes cibles puis les especes en plus
  REFTarOnly_Tab<-INDICE$REF_Tab[,names(INDICE$REF_Tab) %in% INDICE$Nam_Tar=="TRUE"]#--V2
  REFHighOnly_Tab<-INDICE$REF_Tab[,names(INDICE$REF_Tab) %in% INDICE$Nam_Tar=="FALSE"]#--V2
  Tab_val_REF<-cbind(REFTarOnly_Tab,REFHighOnly_Tab)#--V2
  #Calcul des erreurs dans la ref
  ErrorREF<-apply(Tab_val_REF,2,erreur)#--V2

  #Mise en tableau comme pour les autres releves
  ErrorTab<-data.frame(INDICE.Nam_Tot=names(TabCombinASSESS),TabCombinREF,MeansALL,ErrorREF,Errors)#--V2

  ## Ordering the species
  sort_Error1<-data.frame(ErrorTab[order(-ErrorTab[,3]),])#--V2
  sort_Error<-sort_Error1[order(-sort_Error1[,2]),]  #--V2

  ## Ordering the species
  Abundance<-data.frame(INDICE.Nam_Tot=names(TabCombinASSESS),TabCombinREF,MeansALL,Means)
  sort_Abundance1<-data.frame(Abundance[order(-Abundance[,3]),])
  sort_Abundance<-sort_Abundance1[order(-sort_Abundance1[,2]),]

  ## Graphical parameters
  par(mfrow=c(1,length(levels(FACTOR1))+2),mar=c(2.5,0.5,1.5,0.25),oma = c(0,0,3,0))
  species.names<-if(noms=="T") sort_Abundance$INDICE.Nam_Tot else "" ## names definition
  #Les noms des especes
  sp_space<-barplot(-sort_Abundance$TabCombinREF,xlim=c(-2,0),col="white",horiz=T,main="Species",axes=F, beside=F, border = NA) #blank plot to write species names
  text(-2,sp_space,species.names,cex=cex_noms,font=3,pos=4) ## names drawing

  ## The reference
  errors.bars2<-function(yv,z,XLIM,etoiles,col1,MAIN,sp_star=1,w_err=1,REF=F,sp_nm,...) #--V2
  {
    yv<-ifelse(yv==0,NA,yv) #--V2
    xv<-barplot(yv,xlim=XLIM,col=col1,horiz=T,main=MAIN,...)#--V2
    g<-(max(xv,na.rm=T)-min(xv,na.rm=T))/20*w_err#--V2
    for(i in 1:length(xv))#--V2
    {
      if(z[i]!=0) lines(c(yv[i]+z[i],yv[i]-z[i]),c(xv[i],xv[i]))#--V2
      if(z[i]!=0) lines(c(yv[i]+z[i],yv[i]+z[i]),c(xv[i]+g,xv[i]-g))#--V2
      if(z[i]!=0) lines(c(yv[i]-z[i],yv[i]-z[i]),c(xv[i]+g,xv[i]-g))#--V2
      text((yv[i]+z[i]+0.2*sp_star*max(yv,na.rm=T)),xv[i],etoiles[i])#--V2
    }
  }

  species.names<-if(noms=="T") sort_Abundance$INDICE.Nam_Tot else "" ## names definition
  errors.bars2(sort_Abundance$TabCombinREF,sort_Error$ErrorREF, #--V2
               XLIM=c(ABMAX,0),col=col1,MAIN="Reference",w_err=w_err,#--V2
               sp_star=sp_star,etoiles=c(""),sp_nm=species.names)#--V2

  ## adding the assessed community barplot
  for (i in 1:length(levels(FACTOR1)))
  {
    AbRef<-ifelse(sort_Abundance$TabCombinREF==0,NA,sort_Abundance$TabCombinREF)#--V2
    XV<-barplot(AbRef,col=col2,xlim=c(0,ABMAX),#--V2
                main=levels(FACTOR1)[i],horiz=T,plot=BASE)#--V2 ## baseline barplot of reference means

    Ab_Assess<-ifelse(sort_Abundance[,3+i]==0,NA,sort_Abundance[,3+i])#--V2
    barplot(Ab_Assess,col=col4,xaxt="n",horiz=T,add=BASE)#--V2 ## barplot of higherabundances




    ##Tests stats R?f?rence/comm assessed
    #Test de Wilcoxon
    WIL1=NULL
    for(j in 1:length(Ab_Assess))
    {
      ref_val=Tab_val_REF[,j]
      ass_val=TabCombinASSESS[FACTOR1==levels(FACTOR1)[i],j]
      wtest=wilcox.test(ref_val,ass_val)
      WIL1[j]=wtest$p.value
    }
    #Ajustement du p
    WIL<-p.adjust (WIL1, method = adj_meth)
    WIL<-ifelse(is.na(WIL)==TRUE,1,WIL)
    #Attribution des signes

    sign=NULL
    for(j in 1:length(Ab_Assess))
    {
      sign[j]=if(WIL[j]>0.05) "" else if(WIL[j]>0.01) "*" else if(WIL[j]>0.001) "**" else "***"
    }

    sign1<-sign[order(-Abundance[,3])]
    sign2<-sign1[order(-sort_Abundance1[,2])]



    MIN<-NULL # minimum between REF and ASSESS
    for (j in 1:length(sort_Abundance[,3+i]))
    {
      MIN[j]<-min(c(sort_Abundance[j,3+i],sort_Abundance$TabCombinREF[j]))
    }
    MIN<-ifelse(MIN==0,NA,MIN)
    barplot(MIN,col=col3,horiz=T,xaxt="n",add=T) ## barplot of minimum
    #Fonction avec juste les barres d'erreurs:
    errors.bars.only<-function(yv,z,xv,etoiles,etoiles_pos,sp_star=1,w_err=1)#--V2
    {
      yv<-ifelse(yv==0,NA,yv) #--V2
      g<-(max(xv,na.rm=T)-min(xv,na.rm=T))/20*w_err#--V2
      for(i in 1:length(xv))#--V2
      {
        if(z[i]!=0) lines(c(yv[i]+z[i],yv[i]-z[i]),c(xv[i],xv[i]))#--V2
        if(z[i]!=0) lines(c(yv[i]+z[i],yv[i]+z[i]),c(xv[i]+g,xv[i]-g))#--V2
        if(z[i]!=0) lines(c(yv[i]-z[i],yv[i]-z[i]),c(xv[i]+g,xv[i]-g))#--V2
        text((max(c(etoiles_pos[i],yv[i]+z[i]),na.rm=T)+0.1*sp_star*max(c(yv,etoiles_pos),na.rm=T)),xv[i],etoiles[i])#--V2
      }
    }
    etoiles=if(stars!="T") c("") else sign2
    errors.bars.only(sort_Abundance[,3+i],sort_Error[,4+i],xv=XV,#--V2
                     etoiles=etoiles,etoiles_pos=sort_Abundance$TabCombinREF,
                     sp_star=sp_star,w_err=w_err)#--V2
  }
  mtext(MTITLE,side = 3, outer = TRUE,font = 2) ## Adding titles to levels of the factor
}

#--------------------------barres.plot.beside---------------------------------------------

#' barres.plot, mais en parallele
#' @description Fait un diagramme en barre avec la moyenne et les barres d'erreurs, les barres sont regroupees par modalites d'un premier facteur comme barres.plot avec avec possibilite de changer le calcul des barres d'erreur, et d'ajouter les lettres des tests post hoc, ou des etoiles.
#' @param VARI une liste de valeurs de la variable (vecteur)
#' @param FAC1 une liste des modalites du premier facteur (factor) de meme longueur que \code{variable}
#' @param FAC2 une liste des modalites du deuxieme facteur (factor) de meme longueur que \code{variable}
#' @param lettres (facultatif) liste de type \code{c("text","text")} qui figurera au dessus d'une barre
#' @param etoiles (facultatif) liste de type \code{c("text","text")} qui figurera au dessus d'un groupe de barres
#' @param ecart (facultatif) type de calcul de la barre d'erreur \code{"sem"} (par defaut) calculera l'erreur standard (ecart type divise par n) \code{"IC"} calculera l'intervalle de confiance a 5 pourcent (quantile 97.5 a n-1 degre de liberte multiplie par l'ecart-type divise par la racine de n-1) \code{"var"} ou \code{"sd"} ou tout autre fonction de calcul connue de R Attention, ne supporte pas les \code{NA}... (j'ai deja essaye de modifier mais sans succes...)
#' @param POSI position des labels pour le 2eme facteur, soit en haut \code{"top"}, soit en bas, \code{"bottom"}, soit absent \code{"none"}
#' @param ylim comme pour la fonction \code{\link{plot}}, permet de preciser les limites de l'axe des ordonnees
#' @param cex.let facultatif, taille des lettres
#' @param srt.let facultatif, angle des lettres
#' @param ... possibilite de rajouter des arguments comme \code{col},\code{main},\code{ylab},etc. associees a \code{\link{barplot}}
#'
#' @export
#'
#' @examples # Creation des donnees, avec 2 facteurs, l'age et le sexe, et une variable: taille :
#' Age=factor(rep(c(2,10,20,30),each=10))
#' Sexe=factor(rep(rep(c("Homme","Femme"),5),4))
#' Taille=c(rnorm(5,60,7),rnorm(5,55,7),rnorm(5,145,15),rnorm(5,129,15),rnorm(5,175,15),
#'          rnorm(5,165,15),rnorm(5,175,15),rnorm(5,165,15))
#'
#' # Un premier graphe le plus basique qui soit:
#' barres.plot.beside(Taille,Sexe,Age)
#' # et si on inverse les facteurs, avec des couleurs :
#' barres.plot.beside(Taille,Age,Sexe,col=c("burlywood1","darkseagreen3"))
#'
#' # avec les annotations qui vont bien (attention, les lettres et etoiles
#' # sontad?finir soit meme, C-a-d apres tests statistiques, ici pour l'exemple,
#' # c'est de l'aleatoire...), et avec des couleurs simples (mais mocheS...)
#' barres.plot.beside(Taille,Sexe,Age,POSI="bottom",
#' lettres=c("a","b","c","c","a","b","c","c"),
#'                    etoiles=c("*","***"),ylab="Taille (cm)",col=2:5)
#'
#' #  avec des couleurs choisies avec colorRampPalette
#' coul<-c(colorRampPalette(colors=c("olivedrab1", "grey22"))(4))
#' barres.plot.beside(Taille,Sexe,Age,POSI="bottom",
#'                    lettres=c("a","b","c","c","a","b","c","c"),
#'                    etoiles=c("*","***"),col=coul,ylab="Taille (cm)")
#' # Voici comment mettre une legende
#' legend("topleft",levels(Age),fill=coul,bty="n")
#'
#' # Si on defini des couleurs pour chaque combinaison de modalite, il va falloir
#' # adapter la legende:
#' coul2<-c(colorRampPalette(colors=c("lightgoldenrod1", "gold4"))(4),
#'          colorRampPalette(colors=c("salmon", "firebrick4"))(4))
#' barres.plot.beside(Taille,Sexe,Age,POSI="bottom",
#'                    lettres=c("a","b","c","c","a","b","c","c"),
#'                    etoiles=c("*","***"),col=coul2,ylab="Taille (cm)")
#' legend("topleft",paste(rep(levels(Sexe),each=length(levels(Age))),
#'                        rep(levels(Age),2),sep=" "),fill=coul2,bty="n")
#'
barres.plot.beside<-function(VARI,FAC1,FAC2,lettres=c(""),
                             etoiles=c(""),ecart="sem",POSI="none",ylim="NP",cex.let=1,srt.let=0,...)
{
  sem<-function(x)
  {
    sqrt(var(x,na.rm=T))/sqrt(length(x))
  }
  IC<-function(x)
  {
    qt(0.975,(length(x)-1))*sqrt(var(x,na.rm=T))/sqrt(length(x)-1)
  }
  MEAN<-ERRORS<-data.frame(matrix(NA,ncol=length(levels(FAC1)),nrow=length(levels(FAC2))))
  colnames(MEAN)<-colnames(ERRORS)<-as.character(levels(FAC1))
  rownames(MEAN)<-rownames(ERRORS)<-as.character(levels(FAC2))
  for (i in 1:length(levels(FAC1)))
  {
    MEAN[,i]=tapply(VARI[FAC1==levels(FAC1)[i]],FAC2[FAC1==levels(FAC1)[i]],mean,na.rm=T)
    ERRORS[,i]=tapply(VARI[FAC1==levels(FAC1)[i]],FAC2[FAC1==levels(FAC1)[i]],ecart)
  }
  YL=if(length(ylim)==1) c(0,(1.25*max(MEAN,na.rm=T)+max(ERRORS,na.rm=T))) else ylim
  xv<-barplot(as.matrix(MEAN),
              ylim=YL,
              names=as.character(levels(FAC1)),beside=T,...)
  g<-(max(xv,na.rm=T)-min(xv,na.rm=T))/100
  a=0
  for(j in 1:dim(xv)[2])
  {
    for(i in 1:dim(xv)[1])
    {

      lines(c(xv[i,j],xv[i,j]),c(MEAN[i,j]+ERRORS[i,j],MEAN[i,j]-ERRORS[i,j]))
      lines(c(xv[i,j]-g,xv[i,j]+g),c(MEAN[i,j]+ERRORS[i,j],MEAN[i,j]+ERRORS[i,j]))
      lines(c(xv[i,j]-g,xv[i,j]+g),c(MEAN[i,j]-ERRORS[i,j],MEAN[i,j]-ERRORS[i,j]))
      text(xv[i,j],(MEAN[i,j]+ERRORS[i,j]+0.07*max(MEAN,na.rm=T)),lettres[a+i],cex=cex.let,srt=srt.let)
      posi=ifelse(POSI=="top",(max(MEAN+ERRORS,na.rm=T)+0.2*max(MEAN,na.rm=T)),
                  ifelse(POSI=="none",-max(MEAN,na.rm=T),0))
      text(xv[i,j],posi,as.character(levels(FAC2)[i]),pos=3)
    }
    a=a+dim(xv)[1]
    text(mean(xv[,j]),
         (max(MEAN[,j]+ERRORS[,j])+0.15*max(MEAN,na.rm=T)),etoiles[j])
  }
}

# #-------------------------- Exemples d'utilisation ------------------------------------ --

# ----------------------------------------------------------------------------------------

#--------------------------...Letters (en d?velopement)------------------------------------

# # #Avec un exemple bidon :
# # biomasse<-c(rnorm(35,20,5),rnorm(35,15,3),rnorm(35,5,3))
# # traitement<-factor(rep(c("T1","T2","T3"),each=35))
#
# #Fonctionne pour une ANOVA suivie d'un Tukey
#
# # #usage :
# # anovLetters(VAR,FAC,ALPHA)
#
# #VAR : la variable
# #FAC : le facteur
# #ALPHA : (facultatif) donne le seuil de significativit?, 0.05 par defaut...
#
# anovLetters<-function(VAR,FAC,ALPHA=0.05)
# {
#   FAC<-factor(FAC)
#   anov<-lm(VAR~FAC)
#   MOD<-lsmeans(anov,pairwise~FAC)
#   PH<-cld(MOD$lsmeans,alpha=ALPHA,Letters=letters)
#   letters<-NULL
#   for (i in 1:length(PH$.group))
#   {
#     letters[i]<-PH$.group[PH$FAC==levels(FAC)[i]]
#   }
#   letters
# }
#

#' AnovLetters
#'
#' @description Donne les lettres associees a un test post hocc de Tukey effectue apres une anova (implique donc de respecter les conditions d'utilisation des tests parametriques...)
#'
#' @param VAR variable
#' @param FAC facteur
#' @param ALPHA (facultatif) donne le seuil de significativit?, 0.05 par defaut...
#'
#' @return une liste de lettres qui correspondent aux modalites d'un facteur
#' @export
#' @import lsmeans
#'
#' @examples biomasse<-c(rnorm(35,20,5),rnorm(35,15,3),rnorm(35,5,3))
#' traitement<-factor(rep(c("T1","T2","T3"),each=35))
#' anov<-aov(biomasse~traitement)
#' summary(anov)
#' # OK, il y a un effet significatif, du coup anovLetters renvoie la liste des lettres...
#' lettresPH<-anovLetters(VAR = biomasse,FAC = traitement)
#' lettresPH
#' # ...que l'on peut utiliser directement sur barres.plot :
#' barres.plot(biomasse,traitement,lettres = lettresPH)
anovLetters<-function(VAR,FAC,ALPHA=0.05)
{
  FAC<-factor(FAC)
  anov<-lm(VAR~FAC)
  MOD<-lsmeans(anov,pairwise~FAC)
  PH<-cld(MOD$lsmeans,alpha=ALPHA,Letters=letters)
  letters<-NULL
  for (i in 1:length(PH$.group))
  {
    letters[i]<-PH$.group[PH$FAC==levels(FAC)[i]]
  }
  letters<-if(length(unique(letters))==1) rep(c(""),length(levels(FAC))) else letters
}
#
#
# # let<-anovLetters(biomasse,traitement)
# # barres.plot(biomasse,traitement,lettres=let)
#
#
# #Essai avec lme :
#
# #Fonctionne pour un LME suivie d'un Tukey
#
# # #usage :
# # lmeLetters(VAR,FAC,FACAlea,ALPHA)
#
# #VAR : la variable
# #FAC : le facteur
# #FACAlea : le facteur al?atoire
# #ALPHA : (facultatif) donne le seuil de significativit?, 0.05 par defaut...
#
#
# lmeLetters<-function(VAR,FAC,FACalea,ALPHA=0.05)
# {
#   FAC<-factor(FAC)
#   mod<-lme(VAR~FAC, random=~1|FACalea)
#   MOD<-lsmeans(mod,pairwise~FAC, adjust= "tukey")
#   PH<-cld(MOD$lsmeans,alpha=ALPHA,Letters=letters)
#   letters<-NULL
#   for (i in 1:length(PH$.group))
#   {
#     letters[i]<-PH$.group[PH$FAC==levels(FAC)[i]]
#   }
#   letters<-if(length(unique(letters))==1) rep(c(""),length(levels(FAC))) else letters
# }
# ----------------------------------------------------------------------------------------

#--------------------------multivar.polyg---------------------------------------------

#' multivar.polyg
#'
#' @description Dessine des polygones autour des points en fonction des modalites d'un facteur
#'
#' @param ANAcoo coordonnees des points d'une analyse multivariees (__$points pour une NMDS, __$li pour une AFC, etc.)
#' @param FAC facteur qui permettra de separer les polygones
#' @param pch symbole des points (noir par defaut)
#' @param col_dot couleur des points (noir par defaut)
#' @param col_fill couleur du remplissage des polygone (transparent par defaut)
#' @param col_text couleur des lignes du texte (noir par defaut)
#' @param col_bord couleur des lignes du polygone (noir par defaut)
#' @param cex_lab taille des noms de modalites (si lab="yes")
#' @param dot si dot="yes", trace les points de chaque individu, "yes" par defaut
#' @param lab si lab="yes", affiche le nom de la modalite au niveau du barycentre de ses points, "yes" par defaut
#' @param new si new="yes", trace les polygones sur une nouvelle fenetre (defaut), sinon, sur le graphe existant
#' @param sep si sep="yes", trace chaque polygone sur une fenetre graphique differente, "no" par defaut
#' @param ... possibilite de rajouter des arguments comme main,ylab,etc. associees a plot
#'
#' @export
#'
#' @examples library(vegan)
#' data(varespec)
#' data(varechem)
#' facteur_Alu<-factor(ifelse(varechem$Al<50,"Al50",
#'                            ifelse(varechem$Al<110,"Al110",
#'                                   ifelse(varechem$Al<230,"Al230","Al++"))))
#' ord <- metaMDS(varespec)
#' multivar.polyg(ord$points,facteur_Alu)
multivar.polyg<-function(ANAcoo,FAC,pch=1,col_dot=1,col_fill=NA,col_text=1,col_bord=1,
                         cex_lab=1,dot="yes",lab="yes",new="yes",sep="no",...)
{
  if(sep=="yes") x11()
  if(new=="yes") plot(ANAcoo[,1:2],type="n",...)
  for (i in 1:length(levels(FAC)))
  {
    pch1<-ifelse(length(pch)==1,pch,pch[i])
    col_dot1<-ifelse(length(col_dot)==1,col_dot,col_dot[i])
    col_fill1<-ifelse(length(col_fill)==1,col_fill,col_fill[i])
    col_bord1<-ifelse(length(col_bord)==1,col_bord,col_bord[i])
    col_text1<-ifelse(length(col_text)==1,col_text,col_text[i])
    if(dot=="yes") points(ANAcoo[FAC==levels(FAC)[i],1:2],pch=pch1,col=col_dot1)
    Xcoo<-ANAcoo[FAC==levels(FAC)[i],1]
    Ycoo<-ANAcoo[FAC==levels(FAC)[i],2]
    hpts <- chull(x = Xcoo, y = Ycoo)
    bnd=cbind(Xcoo[hpts], Ycoo[hpts])
    polygon(bnd,col=col_fill1,border=col_bord1)
    text(mean(Xcoo,na.rm=T),mean(Ycoo,na.rm=T),
         labels=ifelse(lab=="yes",levels(FAC)[i],c("")),cex=cex_lab,col=col_text1)
    if(sep=="yes") x11()
    if(sep=="yes") plot(ANAcoo[,1:2],type="n")
  }
}


#--------------------------better_arrows---------------------------------------------

#' Trace des fleches suite a une ACP
#'
#' @param ANALYSE objets issues d'una ACP avec dudi.pca (ade4)
#' @param coef un coef multiplicateur (pour exploser ou non la dispersion dans le plan)
#' @param length longueur de l'encoche des fleches, 0.1 = 10pourcent par defaut
#' @param angle angle des encoches des fleches, 15 = 15deg par defaut
#' @param col_text couleur du texte
#' @param ... n'importe quel argument de la fonction arrows
#'
#' @export
#'
#' @examples # Creation donnees :
#' sol<-c(rnorm(10,5,2),rnorm(10,15,3))
#' veg<-c(rnorm(20,21,5))
#' amphi<-c(rnorm(5,5,2),rnorm(5,5,3),rnorm(5,5,2),rnorm(5,5,3))
#' tablo<-data.frame(sol,veg,amphi)
#' # Faire l'ACP
#' library(ade4)
#' ACP<-dudi.pca(tab,scannf = F, nf = 2)
#' plot(ACP$li)
#' better_arrows(ACP,coef=2)
better_arrows<-function(ANALYSE,coef=1,length=0.1,angle=15,col_text=1,...)
{
  for(i in 1:ncol(ANALYSE$tab))
  {
    arrows(0,0,coef*ANALYSE$co[i,1],coef*ANALYSE$co[i,2],length=length,angle=angle,...)
    adj1<-ifelse(ANALYSE$co[i,1]<0,1,0)
    adj2<-ifelse(ANALYSE$co[i,2]<0,1,0)
    text(coef*ANALYSE$co[i,],names(ANALYSE$tab)[i],adj=c(adj1,adj2),col=col_text)
  }
}

#-------------------------- Remplacer ---------------------------------------------

#' Permet, pour chaque valeur possible d'un vecteur, de les remplacer par des valeurs predefinies
#'
#' @param LISTE un vecteur
#' @param VALEURS les valeurs possibles de LISTE (possibilite d'utiliser unique(LISTE) ou levels(LISTE))
#' @param VALEURS_NEW les valeurs de remplacements, a donner dans le meme ordre que VALEURS
#'
#' @export
#'
#' @return un vecteur ayant la meme longueur que LISTE
#' @examples chiffres<-c(1,1,1,2,4,3,2,1,2,3,3,1,3,2)
#' valeurs_avant<-c(1,2,3,4)
#' valeurs_apres<-c("A","B","C","D")
#'
#' Remplacer(chiffres,valeurs_avant,valeurs_apres)
Remplacer<-function(LISTE,VALEURS,VALEURS_NEW)
{
  if(length(VALEURS)<length(VALEURS_NEW)) warning("Attention, c'est peut-?tre sans importance mais,
  VALEURS et VALEURS_NEW ont des tailles diff?rentes",call.=F)
  if(length(VALEURS)>length(VALEURS_NEW)) warning("Attention, c'est peut-?tre sans importance mais,
  VALEURS et VALEURS_NEW ont des tailles diff?rentes,
  les VALEURS_NEW manquantes ont ?t? remplac?es par des NA",call.=F)
  liste_new<-NULL
  for(i in 1:length(LISTE))
  {
    liste_new[i]<-VALEURS_NEW[VALEURS==LISTE[i]]
  }
  return(liste_new)
}






#----------------------- radarchart2 (en developement) -------------------------------------

#' Diagramme radar avec possibilite de ponderer...
#' @details Fonction en cours de production... pas d'aide pour le moment... Modif de la fonction radarchart du package (fmsb) avec ajout de l'argument coef  pour ponderer les variables...
#'
#' Minato Nakazawa (2014). fmsb: Functions for medical statistics book with some demographic data. R package version 0.5.1.
#'
#' http://CRAN.R-project.org/package=fmsb
#'
#' neccessite de charger la library fmsb
#'
#' @export
#' @import fmsb
#'
radarchart2<-function (df, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6,
                       plwd = 1, pdensity = NULL, pangle = 45, pfcol = NA, cglty = 3,
                       cglwd = 1, cglcol = "navy", axislabcol = "blue", title = "",
                       maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL,
                       vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL,
                       palcex = NULL, coef=1, ...)
{
  if (!is.data.frame(df)) {
    cat("The data must be given as dataframe.\n")
    return()
  }
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }


  plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE,
       axes = FALSE, xlab = "", ylab = "", main = title, asp = 1)
  coef<-if(length(coef)==1) rep(1,n) else coef

  nb<-sum(coef)
  theta <- seq(90, 450, length = nb + 1) * pi/180

  rank_t=rep.int(1,times=coef[1])
  for (i in 2:n) {rank_t<-c(rank_t,rep.int(i,times=coef[i]))}

  thetab=NULL
  for (i in 1:n)
  {
    thetab[i]<-mean(theta[1:nb][rank_t==i])
  }
  length(thetab)
  theta <- thetab[1:n]
  xx <- cos(theta)
  yy <- sin(theta)


  CGap <- ifelse(centerzero, 0, 1)
  for (i in 0:seg) {
    polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg +
                                                             CGap), lty = cglty, lwd = cglwd, border = cglcol)
    if (axistype == 1 | axistype == 3)
      CAXISLABELS <- paste(i/seg * 100, "(%)")
    if (axistype == 4 | axistype == 5)
      CAXISLABELS <- sprintf("%3.2f", i/seg)
    if (!is.null(caxislabels) & (i < length(caxislabels)))
      CAXISLABELS <- caxislabels[i + 1]
    if (axistype == 1 | axistype == 3 | axistype == 4 | axistype ==
          5) {
      if (is.null(calcex))
        text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS,
             col = axislabcol)
      else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS,
                col = axislabcol, cex = calcex)
    }
  }
  if (centerzero) {
    arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty,
           length = 0, col = cglcol)
  }
  else {
    arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy *
             1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
  }
  PAXISLABELS <- df[1, 1:n]
  if (!is.null(paxislabels))
    PAXISLABELS <- paxislabels
  if (axistype == 2 | axistype == 3 | axistype == 5) {
    if (is.null(palcex))
      text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
    else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol,
              cex = palcex)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels))
    VLABELS <- vlabels
  if (is.null(vlcex))
    text(xx * 1.2, yy * 1.2, VLABELS)
  else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex)
  series <- length(df[[1]])
  SX <- series - 2
  if (length(pty) < SX) {
    ptys <- rep(pty, SX)
  }
  else {
    ptys <- pty
  }
  if (length(pcol) < SX) {
    pcols <- rep(pcol, SX)
  }
  else {
    pcols <- pcol
  }
  if (length(plty) < SX) {
    pltys <- rep(plty, SX)
  }
  else {
    pltys <- plty
  }
  if (length(plwd) < SX) {
    plwds <- rep(plwd, SX)
  }
  else {
    plwds <- plwd
  }
  if (length(pdensity) < SX) {
    pdensities <- rep(pdensity, SX)
  }
  else {
    pdensities <- pdensity
  }
  if (length(pangle) < SX) {
    pangles <- rep(pangle, SX)
  }
  else {
    pangles <- pangle
  }
  if (length(pfcol) < SX) {
    pfcols <- rep(pfcol, SX)
  }
  else {
    pfcols <- pfcol
  }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1,
                                                         ] - df[2, ]) * seg/(seg + CGap)
    if (sum(!is.na(df[i, ])) < 3) {
      cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, df[i,
                                                         ]))
    }
    else {
      for (j in 1:n) {
        if (is.na(df[i, j])) {
          if (na.itp) {
            left <- ifelse(j > 1, j - 1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left > 1, left - 1, n)
            }
            right <- ifelse(j < n, j + 1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right < n, right + 1, 1)
            }
            xxleft <- xx[left] * CGap/(seg + CGap) +
              xx[left] * (df[i, left] - df[2, left])/(df[1,
                                                         left] - df[2, left]) * seg/(seg + CGap)
            yyleft <- yy[left] * CGap/(seg + CGap) +
              yy[left] * (df[i, left] - df[2, left])/(df[1,
                                                         left] - df[2, left]) * seg/(seg + CGap)
            xxright <- xx[right] * CGap/(seg + CGap) +
              xx[right] * (df[i, right] - df[2, right])/(df[1,
                                                            right] - df[2, right]) * seg/(seg + CGap)
            yyright <- yy[right] * CGap/(seg + CGap) +
              yy[right] * (df[i, right] - df[2, right])/(df[1,
                                                            right] - df[2, right]) * seg/(seg + CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft
              yytmp <- yyleft
              xxleft <- xxright
              yyleft <- yyright
              xxright <- xxtmp
              yyright <- yytmp
            }
            xxs[j] <- xx[j] * (yyleft * xxright - yyright *
                                 xxleft)/(yy[j] * (xxright - xxleft) - xx[j] *
                                            (yyright - yyleft))
            yys[j] <- (yy[j]/xx[j]) * xxs[j]
          }
          else {
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] *
            (df[i, j] - df[2, j])/(df[1, j] - df[2, j]) *
            seg/(seg + CGap)
          yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] *
            (df[i, j] - df[2, j])/(df[1, j] - df[2, j]) *
            seg/(seg + CGap)
        }
      }
      if (is.null(pdensities)) {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i -
                                                            2], border = pcols[i - 2], col = pfcols[i -
                                                                                                      2])
      }
      else {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i -
                                                            2], border = pcols[i - 2], density = pdensities[i -
                                                                                                              2], angle = pangles[i - 2], col = pfcols[i -
                                                                                                                                                         2])
      }
      points(xx * scale, yy * scale, pch = ptys[i - 2],
             col = pcols[i - 2])
    }
  }
}

#----------------- Classes_def ---------------------------

#' Definition de classes
#' @description Permet de transformer un vecteur d'une variable numerique en classes
#'
#' @param variab un vecteur a transformer
#' @param bornes les bornes definissant les classes, la borne inferieure de la classe la plus basse ainsi que la borne superieure de la classe la plus haute ne doivent pas etre renseignees
#' @param noms_classes les noms des classes par lequel sera transforme le vecteur variab. Doit avoir une dimensions de length(bornes)+1
#'
#' @export
#'
#' @examples variable<-c(0.5, #classe 1
#' 1.2,1.3, #classe 2
#' 2.1, #classe 3
# aucun en classe 4
#' 7.2,8.4,9, #classe 5
#' 10.5, #classe 6
#' 22) #et classe 7
#'
#' boundaries<-c(1,2,3,7,10,15)
#' classnames<-paste("classe",c(1:(length(boundaries)+1)))
#' Classes<-Classes_def(variable,boundaries,classnames)
#' data.frame(Classes,variable)
Classes_def<-function(variab,bornes,noms_classes)
{
  if(length(noms_classes)!=(length(bornes)+1)) print("--- !!! --- Les tailles de 'bornes' et 'noms_classes' ne correspondent pas --- !!! ---")
  classes<-rep(noms_classes[length(noms_classes)],length(variab))
  for(i in (length(bornes)):1)
  {
    classes<-ifelse(variab<bornes[i],noms_classes[i],classes)
  }
  return(classes)
}

#-----------------Occ--------------------------
#' Graphe d'occ
#'
#' @description Fonction specifique dans le cadre de l'analyse des resumes de REVER... permet de faire
#' un graphe d'occurence des mots en donnant le vecteur
#'
#' @param VAR le vecteur avec les mots
#' @param CEX expansion factor des noms des occurences
#' @param postext la position du nombre de (cf axe des y)
#' @param ec ecart avec la marge \code{\link{par}} et son parametre \code{oma}
#' @param reor si \code{T}, dans l'ordre croissant, sinon, dans l'ordre donne
#' @param x11 si \code{T}, une nouvelle fenetre est ouverte, sinon, non
#' @param ... d'autres arguments de la fonction \code{\link{barplot}}
#' @export
#' @examples Texte_exemple=c("Banana","Fraise","Chocolat","Chocolat","Chocolat","Fraise","Banana","Banana","Banana","Banana")
#' occ(Texte_exemple,main="Exemple parfum de glaces...")
occ<-function(VAR,CEX=0.5,postext=1,ec=5,reor=T,x11=F,...)
{
  if(x11==T) x11()
  VAR<-if(reor==T) reorder(VAR,VAR,length) else VAR
  occuVAR<-tapply(VAR,VAR,length)
  par(mar=c(3,ec,1,2))
  m<-barplot(occuVAR,horiz=T,names.arg=levels(VAR),las=1,cex.names=CEX,...)
  text(rep(postext,length(m)),m,occuVAR,cex=CEX)
}

#-----------------Tableau_recap--------------------------
#' Tableau recapitulatif de donnees
#'
#' @description Permet de creer automatiquement un tableau de moyenne, erreur
#'
#' @param VAR le vecteur avec les mots
#' @param FAC un facteur donnant les differentes modalites
#' @param ROUND un nombre de decimal pour l'arrondi des moyennes et erreurs
#' @param LETTRES (facultatif) liste de type c("text","text") qui figurera au dessus des barres
#' @param ERROR (facultatif) type de calcul de la barre d'erreur (sem, IC, sd, var, etc.)
#' @export
#' @examples #Donnees
#' biomasse<-c(rnorm(35,20,5),rnorm(35,15,3),rnorm(35,5,3))
#' traitement<-factor(rep(c("T1","T2","T3"),each=35))
#' Tableau_recap(biomasse,traitement,2)
#' #Si on a fait un test post-hoc :
#' anov<-aov(biomasse~traitement)
#' shapiro.test(anov$residuals)
#' summary(anov)
#' TukeyHSD(anov)
#' #les lettres sont donc a, b et c :
#' Tableau_recap(biomasse,traitement,2,c("a","b","c"))
Tableau_recap<-function(VAR,FAC,ROUND,LETTRES=c(""),ERROR=sem){
  Modalites<-levels(FAC)
  Moyennes<-round(tapply(VAR,FAC,mean,na.rm=T),ROUND)
  Erreur<-round(tapply(VAR,FAC,ERROR),ROUND)
  Nombre<-tapply(VAR,FAC,length)
  if(length(LETTRES)==1) {
    Table_all<-data.frame(Modalites,Nombre,Moyennes,Erreur) } else {
      Lettres<-LETTRES
      Table_all<-data.frame(Modalites,Nombre,Moyennes,Erreur,Lettres)
    }
  return(Table_all)
}

#--------------------------Couleur_continue  ---------------------------------------------

#' Couleur continue
#'
#' @description Transforme une variable en palette de couleur
#'
#' @param VAR un vecteur
#' @param COLORS une liste de couleur cf l'argument colors de la fonction \code{\link{colorRampPalette}}
#' @param minVAR valeur minimale theorique (le minimum de VAR par defaut)
#' @param maxVAR valeur maximale theorique (le maximum de VAR par defaut)
#' @param nombre precision de la couleur, c-a-d le nombre d'elements composant la palette de couleur (100 par defaut)
#'
#' @return un vecteur de couleur de la meme longueur que VAR
#' @export
#' @examples xx<-c(1:11)
#' yy<-c(1,3,3.5,4,8,9,3,2.5,7,7.2,NA)
#' plot(xx,xx,cex=5,col=Couleur_continue(yy),pch=15)
#' plot(xx,xx,cex=5,col=Couleur_continue(yy,COLORS=c("Orange","Green")),pch=15)
#' plot(xx,xx,cex=5,col=Couleur_continue(yy,COLORS=c("Orange","Green"),maxVAR=100),pch=15)

Couleur_continue<-function(VAR,COLORS=c("Red","Blue"),minVAR="def",maxVAR="def",nombre=100){
  palette_Func<-colorRampPalette(colors=COLORS)
  minVAR<-ifelse(minVAR=="def",min(VAR,na.rm=T),minVAR)
  maxVAR<-ifelse(maxVAR=="def",max(VAR,na.rm=T),maxVAR)
  diffmm<-maxVAR-minVAR
  nombre_coul<-nombre+1
  palette_New<-palette_Func(nombre_coul)[1:nombre_coul]
  rapport_coul<-1+round((nombre)*(VAR-minVAR)/diffmm,0)
  coul_New<-ifelse(is.na(VAR)==FALSE,palette_New[c(rapport_coul)], NA)
  return(coul_New)
}

#-------------------------- Fonction_hist_double ---------------------------------------------

#' Double distribution
#'
#' @description Fonction specifique dans le cadre du stage de Nadege et Anaelle pour superposer deux distributions sur un meme graphe et avec calcul du khi2 automatique
#'
#' @param Distribution1 Premiere distribution
#' @param Distribution2 Deuxieme distribution
#' @param BREAKS correspond a l'argument \code{breaks} de la fonction \code{\link{hist}}, quelque chose du type \code{seq(50,350,10)} fonctionne bien
#' @param coul1 la couleur de la distribution 1, afin d'avoir des transparences, c'est pratique d'utiliser la fonction \code{\link{hist}} de la maniere suivante : \code{rgb(139/255,62/255,47/255,0.5)}
#' @param coul2 la couleur de la distribution 2
#' @param leg1  nom de la distribution 1 (pour la legende)
#' @param leg2 nom de la distribution 2 (pour la legende)
#' @export
#' @param ... d'autres argument de la fonction \code{\link{hist}}

Fonction_hist_double<-function(Distribution1,Distribution2,BREAKS,coul1=1,coul2=2,leg1="",leg2="",...){
  H1<-hist(Distribution1,col=coul1,breaks=BREAKS,freq=FALSE,...) # hist pour Isere

  H2<-hist(Distribution2,col=coul2,breaks=BREAKS,freq=FALSE,add=T) #hist pour Typha
  legend("topright",col=c(coul1,coul2),legend=c(leg1,leg2),pch=15,bty="n") #legende

  Count_1<-H1$counts #recuperation des valeurs des effectifs pour l'Isere
  Count_2<-H2$counts # pour Typha

  Dist1<-H1$mids[Count_1!=0] #recup des Vet_.ms.s moyens pour Isere (juste ceux avec effectifs Isere >0)
  effectifs<-Count_2[Count_1!=0] #recup des effectifs pour Typha (juste ceux avec effectifs Isere >0)
  effectifs.theo<-Count_1[Count_1!=0] #recup des effectifs pour Isere (juste ceux avec effectifs Isere >0)
  prop.theo <- effectifs.theo / sum(effectifs.theo) #Calcul proportion theorique
  ctest<-chisq.test(effectifs,p=prop.theo) #Test du Khi2

  #Pour afficher les resultats du khi 2
  title(sub=paste0("Homogeneite des distributions : Chi2=",round(ctest$statistic,1),
                   " df=",ctest$parameter," p=",round(ctest$p.value,3)),cex.sub=1)
}

#-------------------------- Sim_Clust_Ordre ---------------------------------------------

#' Similarite releves-syntaxon (cf these Julie)
#' @description Calcul la similiarite des releves entre un tableau de releves (releves regroupes dans un cluster) et un groupement vegetal de reference (de type alliance vegetale)
#' @param TAB_FREQ_CLU tableau de releves des especes en colonnes et des frequences des especes de chaque cluster en ligne (2 colonnes de variables et colonnes especes). Les valeurs correspondent a la frequence des especes pour chaque cluster
#' @param NOM_CLU Nom du cluster choisit
#' @param TAB_ORDRE tableau des releves de references qui definit les differentes alliances vegetales (les especes en colonne et les alliances en ligne) (4 colonnes de variables et des colonnes especes). Les valeurs correspondent a la frequence des especes pour chaque alliance.
#' @param METH methode de similarite a utiliser (par defaut = bray)
#' @param BINA binarite T ou F selon que l'on travaille en presence/absence ou non (par default = FALSE)
#'
#' @export
#' @import vegan
#'
#' @return renvoie un tableau avec  une valeur de similarite (1-indice de Bray Curtis) et un rang (l'alliance la plus similaire) pour chaque alliance en comparaison avec le cluster choisit
#' @examples # Exemple d'application
#' # chargement du package vegan
#' library(vegan)
#' # Creation de tableaux de variables / facteurs
#' tord<-data.frame(list(nom_ordre = c("Sisymbrietea", "Sisymbrietea", "Sisymbrietea"),
#'                       Ordres = c("O.1.", "O.1.", "O.1."),
#'                       alliances = c("A.1.2.", "A.1.3.", "A.1.4."),
#'                       Nb_syntaxons = c(22,28,32),
#'                       Sp1=c(1,0,0),
#'                       Sp2=c(1,1,0),
#'                       Sp3=c(0,1,1),
#'                       Sp4=c(0,1,0)))
#' tord
#' tclu<-data.frame(list(Num_releve = c("3", "4", "6"),All_rel = c("Carriere", "Carriere", "Carriere"),
#'                       Sp1=c(0,0,0.5),
#'                       Sp2=c(1,0,1),
#'                       Sp3=c(1,0,0.1),
#'                       Sp4=c(0.5,0,0),
#'                       Sp5=c(0,1,0),
#'                       Sp6=c(0,1,0)))
#' tclu
#' # Utilisation de la fonction pour le cluster 6 (=Num_releve) par exemple
#' Sim_Clust_Ordre(tclu,NOM_CLU="6",tord,METH="bray",BINA=F)
Sim_Clust_Ordre<-function(TAB_FREQ_CLU,NOM_CLU,TAB_ORDRE,METH = "bray",BINA = F){
  tab_freq_clu_i<-TAB_FREQ_CLU[TAB_FREQ_CLU$Num_releve==NOM_CLU,]
  tab_freq_clu_i_esp<-tab_freq_clu_i[,3:ncol(tab_freq_clu_i)]
  tab_ordre_esp<-TAB_ORDRE[,5:ncol(TAB_ORDRE)]
  tab_ordre_V2<-data.frame(Num_releve=factor(TAB_ORDRE$alliances),All_Rel=factor(TAB_ORDRE$nom_ordre),tab_ordre_esp)
  liste_esp<-c("Num_releve","All_Rel",unique(c(names(tab_freq_clu_i_esp),names(tab_ordre_esp))))
  clui_O<-combin.tabV0(tab_freq_clu_i,tab_ordre_V2,liste_esp)
  clui_O_esp<-clui_O[,3:ncol(clui_O)]
  bray_ordre<-1
  for(i in 2:length(clui_O$Num_releve))
  {
    bray_ordre[i]<-1-(raup.calc(clui_O_esp,i,1,method=METH,binary=BINA))
  }
  return(data.frame(Alliance=clui_O$Num_releve,Similarite=bray_ordre,rang=(rank(-bray_ordre)-1)))
}

#-------------------------- modif_coul ---------------------------------------------

#' modif_coul
#'
#' @description  Permet de modifier des couleurs
#'
#' @param COULEUR une couleur, soit en nom de couleur R ("cadetblue"), soit en code hexadécimal
#' @param mods coefficient modérateur de la saturation, sous 0.5 ça désature, au dessus, ça sature
#' @param modv coefficient modérateur de la brillance, sous 0.5 ça fonce, au dessus, ça éclaircit
#' @param modh coefficient modérateur de la teinte, ça tourne en rond..
#' @param alpha transparence, sous 1, ça devient transparent
#'
#' @return un code hexadécimal de couleur
#' @export
#'
#' @examples #Sans modification de couleur :
#' hist(rnorm(30),col="cadetblue")
#' #En modifiant la saturation :
#' hist(rnorm(30),col=modif_coul("cadetblue",mods=0.1))
#' hist(rnorm(30),col=modif_coul("cadetblue",mods=0.9))
#' #En modifiant la clarté :
#' hist(rnorm(30),col=modif_coul("cadetblue",modv=0.1))
#' hist(rnorm(30),col=modif_coul("cadetblue",modv=0.9))
#' #En modifiant la transparence :
#' hist(rnorm(30),col=modif_coul("cadetblue",alpha=0.1))
#' hist(rnorm(30),col=modif_coul("cadetblue",alpha=0.9))
#'
modif_coul<-function(COULEUR,mods=0.5,modv=0.5,modh=0.5,alpha =1){
  RGB<-rgb2hsv(r=matrix(data = c(col2rgb(COULEUR)[1]/255,
                                 col2rgb(COULEUR)[2]/255,
                                 col2rgb(COULEUR)[3]/255),nrow = 3))
  h<-RGB[1]
  s<-RGB[2]
  v<-RGB[3]
  h2<-if(modh<=0.5) h*modh*2 else (modh*(2-2*h)+2*h-1)
  s2<-if(mods<=0.5) s*mods*2 else (mods*(2-2*s)+2*s-1)
  v2<-modv
  newc<-hsv(h2,s2,v2,alpha = alpha)
  return(newc)
}

#-------------------------- point.plot ---------------------------------------------

#' point.plot
#'
#' @description Fait un diagramme en points avec la moyenne et les barres d'erreurs
#'
#' @param variable un vecteur avec les valeurs des variables
#' @param Facteur un facteur donnant les differentes modalites
#' @param lettres	(facultatif) liste de type c("text","text") qui figurera au dessus des barres
#' @param type Type de liaison entre les points ("b": points reliés - esp points-ligne;"o": points reliés - pas d'espace; "l": que les lignes;"p" : que les points;"s": en marches
#' @param ecart (facultatif) type de calcul de la barre d'erreur (sem, IC, sd, var, etc.)
#' @param ylim	comme pour la fonction plot(), permet de preciser les limites de l'axe des ordonnees
#' @param xlim	comme pour la fonction plot(), permet de preciser les limites de l'axe des abscisses
#' @param add si jamais on veut que ça soit rajouté sur un graphique existant, il faut mettre "TRUE"
#' @param ...	possibilite de rajouter des arguments comme col,main,ylab,etc. associees a barplot
#'
#' @export
#'
#' @details Co-écrite avec Julie Chenot!
#'
#' @examples #Donnees
#' biomasse<-c(rnorm(35,20,5),rnorm(35,15,3),rnorm(35,5,3))
#' traitement<-factor(rep(c("T1","T2","T3"),each=35))
#' #Graphique de base
#' point.plot(variable = biomasse,Facteur = traitement)
#' #En changeant les barres d'erreurs
#' point.plot(biomasse,traitement,ecart=sd)
#' #En ajoutant les lettres de post-hoc
#' point.plot(biomasse,traitement,lettres=c("a","ab","b"))
#' #En changeant des parametres classiques de barplot...
#' point.plot(biomasse,traitement,lettres=c("a","ab","b"),
#'            col=c("cadetblue"),
#'            xlab="Traitements herbicides",ylab="Biomasse",font.lab=3,
#'            main="Efficacite des traitement",pch=16,type="b",lty=3,lwd=4,cex=3)
#' #Si on veut ajouter à un graphe existant..
#' point.plot(biomasse/2,traitement,lettres=c("a","ab","b"),
#'            col=c("darkolivegreen"),pch=16,add="TRUE")
#'
point.plot<-function(variable,Facteur,lettres=c(""),type="p",ecart="sem",ylim="NP",xlim="NP",add="FALSE",...)
{
  errors.bars<-function(yv,z,nn,lettres,YL)
  {
    xv<-c(1:length(yv))
    if(add=="TRUE") {points(xv,yv,ylim=YL,type=type,...)} else {
      plot(yv,type=type,xlim=XL,ylim=YL,xaxt='n',...)}
    g<-(max(xv,na.rm=T)-min(xv,na.rm=T))/50
    for(i in 1:length(xv))
    {
      lines(c(xv[i],xv[i]),c(yv[i]+z[i],yv[i]-z[i]))
      lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i],yv[i]+z[i]))
      lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i],yv[i]-z[i]))
      text(xv[i],(yv[i]+z[i]+0.07*max(yv,na.rm=T)),lettres[i])
    }
    axis(side = 1,at = xv,labels = labels)

  }
  meandon<-tapply(variable,Facteur,mean,na.rm=T)
  sem<-function(x)
  {
    sqrt(var(x,na.rm=T))/sqrt(length(x))
  }
  IC<-function(x)
  {
    qt(0.975,(length(x)-1))*sqrt(var(x,na.rm=T))/sqrt(length(x)-1)
  }
  ecartdon<-tapply(variable,Facteur,ecart)
  ybar<-as.vector(meandon)
  se<-as.vector(ecartdon)
  labels<-as.character(levels(Facteur))
  XL=if(length(xlim)==1) c(0.5,length(ybar)+0.5) else xlim
  YL=if(length(ylim)==1) c(0,(1.25*max(ybar,na.rm=T)+max(se,na.rm=T))) else ylim
  errors.bars(ybar,se,labels,lettres,YL=YL)
}

#' Similarities between references and restoration
#' 
#' @description Calculate similarity between plots and a group of reference plot
#'
#' @param RELEVES Variables of the restoration sites data matrix
#' @param REF Variables surveys of reference sites data matrix
#' @param METHOD Dissimilarity index, partial match to "manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao" or "mahalanobis". Cf \code{\link{vegan::vegdist}} function 
#' @param BINARY (FALSE by default) If TRUE, data is converted to binary data
#' @param DUPLICATES (TRUE by default) Are REF plots also included in RELEVES data?
#' 
#' @return \item{Diss_Mean}{average dissimilarity by RELEVES}
#' @return \item{Diss_Min}{minimal dissimilarity by RELEVES}
#' @return \item{RelRef_order}{list of REF names in ascending order of dissimilarity for each RELEVES}
#' @return \item{DistRef_order}{list of REF distance values in ascending order of dissimilarity for each RELEVES}
#' 
#' @examples # ------------  Creating the data needed for the example --------------------
#' library(vegan)
#' data("dune") #downloading of dune data (cf vegan)
#' data("dune.env") #downloading of dune.env data (cf vegan)
#' # keeping only the numeric variables :
#' dune.env <- data.frame(A1 = dune.env$A1, 
#'                        Moisture =  as.numeric(as.vector(dune.env$Moisture)),
#'                        Manure = as.numeric(as.vector(dune.env$Manure)))
#'                                           
#' # Creating a vector indicating which plots are the potential references and which ones are the restored sites
#' sites <- factor(c(rep("Rest",5),rep("Ref",15)))
#' # Creating a vector with the plot names
#' sites_names <- paste(sites,c(1:5,1:15))
#'
#' #Poviding names to rows (useful for the outputs)
#' row.names(dune.env) <- sites_names
#' row.names(dune) <- sites_names
#' dune.envRest <- dune.env[sites=="Rest",]
#' dune.envRef <- dune.env[sites=="Ref",]
#' 
#' # --------------  Calculating reference dissimilarities ---------------------
#' Distances <- DissRef3(RELEVES = dune.envRest, REF = dune.envRef, METHOD = "euclidean", DUPLICATES = FALSE)
#' Distances
#' 
#' # ----------------  Plotting reference dissimilarities ----------------------
#' Diss_Ref_Plot(RELEVES = dune.envRest, REF = dune.envRef, DISTANCES = Distances, LINK_NUMBER = "N_REF", N_REF = 3)
DissRef3 <- function (RELEVES, REF, METHOD = "bray", BINARY = FALSE, DUPLICATES = TRUE)
{
  # Fusion of tables:
  if(all.equal(names(RELEVES),names(REF))!=TRUE){warning("The two tables do not have the same variables")}
  Tableaux <- rbind(RELEVES,REF)
  
  # Dissimilarity matrix:
  matrice <- as.matrix(vegdist(Tableaux, method = METHOD, binary = BINARY))
  
  # Creation of variables:
  Diss_Mean <- NULL # average dissimilarity of RELEVES
  Diss_Min <- NULL # minimal dissimilarity of RELEVES
  
  # Rel_order will be, for each RELEVES, the list of the names of REF in ascending order of dissimilarity
  Rel_order <- data.frame(matrix(data = 0, nrow = nrow(RELEVES), ncol = nrow(REF)))
  row.names(Rel_order) <- row.names(RELEVES) # line names = RELEVES names
  
  # ‘Dist_order’: for each RELEVES, the list of distance values to REF in ascending order of dissimilarity
  # The object is identical to ‘Rel_order’
  Dist_order <- Rel_order
  
  # loop to calculate for each relevé
  for(i in 1:nrow(RELEVES))
  {
    # Ref distances (from the distance matrix)
    diss_qn <- matrice[i,c((nrow(RELEVES)+1):(nrow(RELEVES)+nrow(REF)))]
    
    # ‘if’ & ‘else’: to delete a zero in the distance matrix when there is a duplicate of data in RELEVES and in REF (in this case set DUPLICATES = TRUE) and we have a value 0 between the RELEVES i and at  least one of REF
    
    if(DUPLICATES==TRUE & min(diss_qn)==0)
    {
      diss_qnw0_2 <- diss_qn[-c(1:length(diss_qn))[diss_qn==0][1]]
    } else {
      diss_qnw0_2 <- diss_qn
    }
    
    # Placing the REF names in ascending order according to the distance to the RELEVES i
    Rel_order[i,] <- factor(row.names(REF)[order(diss_qn)])
    
    # Placing the distance values in ascending order according to the distance to RELEVES i
    Dist_order[i,] <- diss_qn[order(diss_qn)]
    
    # Average distance value
    Diss_Mean[i] <- mean(diss_qnw0_2)
    
    # Minimum distance value
    Diss_Min[i] <- min(diss_qnw0_2)
    
  }
  
  Output <- list(Diss_Mean,Diss_Min,Rel_order,Dist_order)
  names(Output) <- c("Diss_Mean","Diss_Min","RelRef_order","DistRef_order")
  return(Output)
}



#' Plot of references and restoration site dissimilarities
#' @description Display similarity link (previously calculated with \code{\link{ChosseRef::DissRef3}} function) between plots and a group of reference plot
#' 
#' @param RELEVES  Variables of the restoration sites data matrix
#' @param REF Variables of reference sites data matrix
#' @param DISTANCES Object resulting from the analysis of the \code{\link{ChosseRef::DissRef3}} function.
#' @param METHOD Dissimilarity index, partial match to "manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao" or "mahalanobis". Cf \code{\link{vegan::vegdist}} function 
#' @param COUL_RELEVES Color of RELEVES names
#' @param COUL_Rel_variable If TRUE: color change for each RELEVES
#' @param COUL_REF Color of REF names
#' @param COUL_Seg Color of links
#' @param COUL_Seg_variable If TRUE: colors change for each link
#' @param LINK_NUMBER Number of link drawn. If "N_REF" then for each RELEVES, the number of link drawn is the number given in \code{N_REF}. If "DIST_MIN"
#' only reference that have distance to restoration sites lower than the value given in \code{DIST_MIN} are drawn.
#' @param N_REF Number of REF to display a link between REF and RELEVES
#' @param DIST_MIN Minimum distance value to draw a link between REF and RELEVES
#' @param VAL_DIST Providing or not (TRUE by default) the distance value display
#' @param DECAL Distance between the printing of the distance and the link
#' 
#' @examples # ------------  Creating the data needed for the example --------------------
#' library(vegan)
#' data("dune") #downloading of dune data (cf vegan)
#' data("dune.env") #downloading of dune.env data (cf vegan)
#' # keeping only the numeric variables :
#' dune.env <- data.frame(A1 = dune.env$A1, 
#'                        Moisture =  as.numeric(as.vector(dune.env$Moisture)),
#'                        Manure = as.numeric(as.vector(dune.env$Manure)))
#'                                           
#' # Creating a vector indicating which plots are the potential references and which ones are the restored sites
#' sites <- factor(c(rep("Rest",5),rep("Ref",15)))
#' # Creating a vector with the plot names
#' sites_names <- paste(sites,c(1:5,1:15))
#'
#' #Poviding names to rows (useful for the outputs)
#' row.names(dune.env) <- sites_names
#' row.names(dune) <- sites_names
#' dune.envRest <- dune.env[sites=="Rest",]
#' dune.envRef <- dune.env[sites=="Ref",]
#' 
#' # --------------  Calculating reference dissimilarities ---------------------
#' Distances <- DissRef3(RELEVES = dune.envRest, REF = dune.envRef, METHOD = "euclidean", DUPLICATES = FALSE)
#' Distances
#' 
#' # ----------------  Plotting reference dissimilarities ----------------------
#' Diss_Ref_Plot(RELEVES = dune.envRest, REF = dune.envRef, DISTANCES = Distances, LINK_NUMBER = "N_REF", N_REF = 3)
Diss_Ref_Plot <- function(RELEVES, REF, DISTANCES,
                          METHOD = "euclidean",
                          COUL_RELEVES = 2, COUL_Rel_variable = TRUE,
                          COUL_REF = 1,
                          COUL_Seg = "#9C8809", COUL_Seg_variable = TRUE,
                          LINK_NUMBER="absent", N_REF="absent", 				  	   DIST_MIN="absent",
                          VAL_DIST = TRUE, DECAL = 0 )
  
{
  
  # Check that the names of RELEVES and REF are the same:
  if(all.equal(names(RELEVES),
               names(REF))!=TRUE){warning("REF and RELEVES don't have the same variables")}
  
  # Verification that the information of CHOICE NUMBER, N_REF and DIST_MIN coincide well:
  if(LINK_NUMBER!="N_REF" & LINK_NUMBER!="DIST_MIN"){warning("LINK_NUMBER is not correctly assigned")}
  if(LINK_NUMBER=="N_REF" & N_REF=="absent"){warning("N_REF is not correctly assigned")}
  if(LINK_NUMBER=="DIST_MIN" & DIST_MIN=="absent"){warning("DIST_MIN is not correctly assigned")}
  
  # Verification that DIST_MIN is greater than the minimum distance for each RELEVES:
  if(max(DISTANCES$DistRef_order[,1])>DIST_MIN){warning("DIST_MIN is too low and there is no reference close enough for each RELEVES")}
  
  # Fusion of tables
  Tableaux <- rbind(RELEVES,REF)
  
  # NMDS analysis:
  NMDS <- metaMDS(Tableaux, distance = METHOD)
  
  # Display samples of RELEVES and REF
  plot(NMDS$points, type="n", main="Choices of reference
       with environmental conditions")
  text(NMDS$points, labels = row.names(Tableaux),
       col = c(rep(COUL_RELEVES, nrow(RELEVES)), rep(COUL_REF, nrow(REF))))
  # Creation of the color palette if COUL_Seg_variable = TRUE:
  if(COUL_Seg_variable==TRUE){
    COUL_Seg <- colorRampPalette(colors=c("#0789e0","#9C093F","#9C0972","#91099C","#57099C","#26099C","#09199C","#09459C","#09819C","#099C8C","#099C5C","#099C26","#199C09","#4A9C09","#7B9C09","#9C9309","#9C6209","#9C3109","#9C0909"))(nrow(DISTANCES$RelRef_order))}else{COUL_Seg <- rep(COUL_Seg,nrow(DISTANCES$RelRef_order))
    }
  
  # Loop for each RELEVES:
  for (j in 1:nrow(RELEVES))
  {
    # Coordinates of relevé:
    Coo_REL <- NMDS$points[j,c(1:2)]
    
    # If we choose to present a number of references:
    if(LINK_NUMBER=="N_REF"){
      for(i in 1:N_REF)
      {
        Num_ref_proche <- c(1:nrow(REF))[row.names(REF)==DISTANCES$RelRef_order[j,i]]
        Coo_REF <- NMDS$points[nrow(RELEVES)+Num_ref_proche,c(1:2)]
        segments(x0 = Coo_REL[1], y0 = Coo_REL[2], x1 = Coo_REF[1],
                 y1 = Coo_REF[2], col = COUL_Seg[j])
        Dist_REF <- round(DISTANCES$DistRef_order[j,i],2)
        if(VAL_DIST==TRUE){text(x = mean(c(Coo_REF[1],Coo_REL[1]))+DECAL,
                                y = mean(c(Coo_REF[2],Coo_REL[2]))+DECAL,
                                Dist_REF, col="Grey40", cex = 0.7)}
      }
    }
    
    # If you choose a distance value for which to display the links:
    if(LINK_NUMBER=="DIST_MIN"){
      
      # Number of REF with distance less than DIST_MIN:
      N_REF <- length(which(DISTANCES$DistRef_order[1,]<DIST_MIN))
      for(i in 1:N_REF)
      {
        Num_ref_proche <- c(1:nrow(REF))[row.names(REF)==DISTANCES$RelRef_order[j,i]]
        Coo_REF <- NMDS$points[nrow(RELEVES)+Num_ref_proche,c(1:2)]
        segments(x0 = Coo_REL[1], y0 = Coo_REL[2], x1 = Coo_REF[1],
                 y1 = Coo_REF[2], col = COUL_Seg[j])
        Dist_REF <- round(DISTANCES$DistRef_order[j,i],2)
        if(VAL_DIST==TRUE){text(x = mean(c(Coo_REF[1],Coo_REL[1]))+DECAL,
                                y = mean(c(Coo_REF[2],Coo_REL[2]))+DECAL,
                                Dist_REF, col="Grey40", cex = 0.7)}
      }
    }
    
  }
}

