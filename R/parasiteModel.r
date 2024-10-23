#' Calculates regression from count data
#'
#' Fits generalized linear model with negative binomial distribution suitable for 
#' analysing count data such as number of ectoparasites. Allows the user to choose
#' working directory and the data file, choose the dependent and explanatory variables, 
#' fits the model and plots variable predictions as specified by the user.
#' @param farebna.paleta character. Color palette in \code{hcl.pals()} or 
#'   \code{palette.pals()}.
#' @details Function communicates with the user in Slovak to distinguish what dialogs
#'   represent intended functionality. Pay attention to dialog boxes (2) at the beginning 
#'   and prompts on the R console afterwards. 
#' @returns Called for side effects. Stores summary of the model fit in a text file and
#'   plots predicted values in pdf files.
#' @export

parasiteModel <- function(farebna.paleta = "Accent"){

for(i in c("utils", "tcltk", "vioplot", "MASS")){
  if(!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
  }
}


paleta = farebna.paleta
farby = { if(paleta %in% palette.pals())
			palette.colors(11, palette = paleta, recycle = TRUE)
		 else 
		 	hcl.colors(11, paleta) }


### vybrat adresar pre pracu

choose_directory = function(caption = 'Vyber adresar, kde sa budu ukladat vysledky') {
  if (exists('utils::choose.dir')) {
    choose.dir(caption = caption) 
  } else {
    tcltk::tk_choose.dir(caption = caption)
  }
}


setwd(doma <- choose_directory())

if(!dir.exists("Vysledky")) dir.create("Vysledky")

message("Vyber tabulku s udajmi - musi byt tab-delimited format")
dat = read.table(file.choose(), header = TRUE, sep = "\t", stringsAsFactors = FALSE)
dat$animal = paste(dat$cohort, dat$toe.clip.tatoo, sep = "-")
dat$date = as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
dat$season = as.numeric(format(dat$date, "%j"))   # dni od zaciatku roka

dat$age = sub("egg", NA, dat$age)
dat$age[grepl("juv|sub", dat$age)] = "young"
dat$age[grepl("adult", dat$age)] = "adult"

dat$sex = sub("maleV", "male", dat$sex)


dat[] = lapply(dat, FUN = function(x) if(is.character(x)) as.factor(x) else {x})



dat$season = as.numeric(format(as.Date(paste(dat$year, dat$month, dat$day, sep="-")), "%j")) 

zavisla = "parasites"
stlpce = c("colour.form", "sex", "site")

### kontrola stlpcov pre analyzu

suhlas = "a"

while(suhlas == "a"){
	cat("Pre vypocet budem predikovat premennu:", zavisla, "a vysvetlovat ju budu tieto stlpce:", stlpce,
		sep = "\n")
	suhlas = readline("Chces zmenit niektore premenne [a/n]? ")
	if(suhlas == "a"){
		print(colnames(dat))
		zavisla = colnames(dat)[as.numeric(readline("Napis cislo stlpca premennej, ktoru chces modelovat: "))]
		ktore = as.numeric(unlist(strsplit(readline("Napis cisla stlpcov oddelene ciarkou pre premenne, ktore ju maju vysvetlovat: "), ",")))
		stlpce = colnames(dat)[ktore]
	}
}

dat2 = droplevels(dat[complete.cases(dat[,c(stlpce, zavisla, "animal")]),])

message("Datova sada pre analyzu ma ", nrow(dat2), " riadkov. Pocitam regresny model.")

fit = glm.nb(formula = as.formula(paste(zavisla, paste(stlpce, collapse = "+"), sep = "~")), data = dat2)


k = sum(grepl("GLM", dir(doma, recursive = TRUE))) +1
cat(paste(zavisla, paste(stlpce, collapse = " + "), sep = " ~ "), "\n\n", file = paste0("Vysledky/GLM",k,".txt"), append =FALSE)
capture.output(summary(fit), file = paste0("Vysledky/GLM",k,".txt"), append = TRUE)

print(summary(fit))

suhlas = "a"

while(suhlas == "a"){
  cat("\n",paste(1:length(stlpce), stlpce), sep = "\n")
  ktore = as.numeric(readline("Ktoru premennu zobrazit do grafu? Poradie v zozname: "))
  
  pred = predict(fit, type = "response")

  pdf(paste0("Vysledky/", stlpce[ktore], ".pdf"), width= 8, height = 4.5)
  layout(matrix(c(1,1,2), ncol=3))
  par(mar = c(4.1,4.1,.5,.5))
  if(is.factor(dat2[,stlpce[ktore]])){
    vioplot(pred ~ dat2[,stlpce[ktore]], col = farby, wex = .6, las =1, xlab = stlpce[ktore], ylab = "Predicted parasite load")
    box()
    axis(1, at = 1:nlevels(dat2[,stlpce[ktore]]), labels = levels(dat2[,stlpce[ktore]]))
    axis(2, las = 1)
  } else {
    vioplot(pred ~ as.factor(dat2[,stlpce[ktore]]), col = farby, wex = .6, las =1, xlab = stlpce[ktore], ylab = "Predicted parasite load")
    box()
    axis(1, at = 1:nlevels(dat2[,stlpce[ktore]]), labels = levels(dat2[,stlpce[ktore]]))
    axis(2, las = 1)
    
  }
  plot.new()
  legend("topleft", legend = levels(as.factor(dat2[,stlpce[ktore]])), fill = farby[1:nlevels(as.factor(dat2[,stlpce[ktore]]))])
  dev.off()
  suhlas = readline("Chces nakreslit predikovane hodnoty pre dalsiu premennu z tohto modelu? [a/n] ")
}

}