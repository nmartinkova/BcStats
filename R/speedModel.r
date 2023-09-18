#' Calculates mixed regression model from continuous data
#'
#' Fits generalized linear mixed model for 
#' analysing continuous data, where more than one measurement was taken per animal on 
#' each day, such as running speed. Allows the user to choose
#' working directory and the data file, choose the dependent and explanatory variables, 
#' fits the model and plots variable predictions as specified by the user.
#' @param farebna.paleta character. Color palette in \code{hcl.pals()} or 
#'   \code{palette.pals()}.
#' @details Function communicates with the user in Slovak to distinguish what dialogs
#'   represent intended functionality. Pay attention to dialog boxes (3) at the beginning 
#'   and prompts on the R console afterwards. 
#' @returns Called for side effects. Stores summary of the model fit in a text file and
#'   plots predicted values in pdf files.
#' @export


speedModel <- function(farebna.paleta = "Accent"){

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


# nakreslit LMM model

nakresli_lmm = function(model, dat, nezavisla, zavisla, ...){
	plot(dat[,nezavisla], dat[,zavisla], type = "n", las=1, ...)
	# https://cosanlab.com/static/papers/AdvPlot_HO.pdf
	fixed.p = fixef(model)
	random.p = ranef(model)
	coefs = switch(ncol(random.p), cbind(fixed.p[1] + random.p[,1], fixed.p[2]), cbind(fixed.p[1] + random.p[,1], fixed.p[2] + random.p[,2]))
	for(i in 1:nrow(random.p)){
		abline(a = coefs[i,1], b = coefs[i,2], lty=2, col="grey")
	}
	abline(fixed.p[c("(Intercept)", nezavisla)], lwd=2)
}


setwd(doma <- choose_directory())

if(!dir.exists("Vysledky")) dir.create("Vysledky")

message("Vyber tabulku s rychlostou behu - idealne vysledok z funkcie calculateSpeed()")

beh = read.table(file.choose(), header = TRUE, sep = "\t", stringsAsFactors = TRUE)
beh$date = as.Date(paste(beh$year, beh$month, beh$day, sep="-"))





message("Vyber tabulku s udajmi Lagilis - musi byt tab-delimited format")

dat = read.table(file.choose(), header = TRUE, sep = "\t", stringsAsFactors = FALSE)
dat$animal = paste(dat$cohort, dat$toe.clip.tatoo, sep = "-")
dat$date = as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
dat$season = as.numeric(format(dat$date, "%j"))   # dni od zaciatku roka

dat[] = lapply(dat, FUN = function(x) if(is.character(x)) as.factor(x) else {x})



dat$season = as.numeric(format(as.Date(paste(dat$year, dat$month, dat$day, sep="-")), "%j")) 



dat = merge(beh, dat, by.y = c("toe.clip.tatoo", "date"), by.x = c("id.animal", "date"), all = TRUE)

zavisla = "speed"
stlpce = c("temperature", "humidity", "air.temp")

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

dat2 = droplevels(dat[complete.cases(dat[,c(stlpce, zavisla, "id.animal")]),])

message("Datova sada pre analyzu ma ", nrow(dat2), " riadkov. Pocitam regresny model.")

fit = lme(as.formula(paste(zavisla, paste(stlpce, collapse = "+"), sep = "~")), 
  random = ~ 1|id.animal, data = dat2)


k = sum(grepl("LMM", dir(doma, recursive = TRUE))) +1
cat(paste(zavisla, paste(stlpce, collapse = " + "), sep = " ~ "), "\n\n", file = paste0("Vysledky/LMM",k,".txt"), append =FALSE)
capture.output(summary(fit), file = paste0("Vysledky/LMM",k,".txt"), append = TRUE)

print(summary(fit))

suhlas = "a"

while(suhlas == "a"){
  cat("\n",paste(1:length(stlpce), stlpce), sep = "\n")
  ktore = as.numeric(readline("Ktoru premennu zobrazit do grafu? Poradie v zozname: "))
  
  pred = predict(fit, type = "response", se.fit = TRUE)

  pdf(paste0("Vysledky/", stlpce[ktore], ".pdf"), width= 8, height = 4.5)
  par(mar = c(4.1,4.1,.5,.5))
  popisok = switch(stlpce[ktore], temperature = expression("Temperature ("^"o" * "C)"),
       sex = "Sex",
       humidity = "Humidity (%)",
       air.temp = expression("Air temperature ("^"o" * "C)"))
  popisok.y = switch(zavisla, speed = "Predicted running speed (m/s)")
  if(is.factor(dat2[,stlpce[ktore]])){   
  layout(matrix(c(1,1,2), ncol=3))
  par(mar = c(4.1,4.1,.5,.5))
    vioplot(pred ~ dat2[,stlpce[ktore]], col = farby, wex = .6, las =1, xlab = popisok, ylab = popisok.y)
    box()
    axis(1, at = 1:nlevels(dat2[,stlpce[ktore]]), labels = levels(dat2[,stlpce[ktore]]))
    axis(2, las = 1)
  plot.new()
  legend("topleft", legend = levels(as.factor(dat2[,stlpce[ktore]])), fill = farby[1:nlevels(as.factor(dat2[,stlpce[ktore]]))])
  } else {
	nakresli_lmm(model = fit, dat = dat2, zavisla = zavisla, nezavisla = stlpce[ktore],
		xlab = popisok, ylab = popisok.y)	
  }
  dev.off()
  suhlas = readline("Chces nakreslit predikovane hodnoty pre dalsiu premennu z tohto modelu? [a/n] ")
}

}