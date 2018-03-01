#---------------------------------------------------------------------#
##----------- Elements pour la construction du package --------------##
#---------------------------------------------------------------------#

#-------- package dependencies -------------

devtools::use_package("ade4","imports")
devtools::use_package("vegan","imports")
devtools::use_package("fmsb","imports")
devtools::use_package("lsmeans","imports")

#------- datasets -------------------------

#Exemple 1
biomasse<-c(rnorm(35,20,5),rnorm(35,15,3),rnorm(35,5,3))
traitement<-factor(rep(c("T1","T2","T3"),each=35))
Exemple1<-data.frame(biomasse,traitement)

devtools::use_data(Exemple1,overwrite=TRUE)

#Exemple 2
Age=factor(rep(c(2,10,20,30),each=10))
Sexe=factor(rep(rep(c("Homme","Femme"),5),4))
Taille=c(rnorm(5,60,7),rnorm(5,55,7),rnorm(5,145,15),rnorm(5,129,15),rnorm(5,175,15),
         rnorm(5,165,15),rnorm(5,175,15),rnorm(5,165,15))
Exemple2<-data.frame(Age,Sexe,Taille)

devtools::use_data(Exemple2,overwrite=TRUE)


sol<-c(rnorm(10,5,2),rnorm(10,15,3))
veg<-c(rnorm(20,21,5))
amphi<-c(rnorm(5,5,2),rnorm(5,5,3),rnorm(5,5,2),rnorm(5,5,3))
tablo<-data.frame(sol,veg,amphi)

#------------- Manuel pdf ----------------
pack <- "Renaudpack2"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(path)))

