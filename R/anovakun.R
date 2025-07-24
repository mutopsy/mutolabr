#' Perform ANOVA using the anovakun procedure (version 4.8.9)
#'
#' This function performs analysis of variance (ANOVA) based on the `anovakun` version 4.8.9,
#' originally developed by Prof. Ryuta Iseki (Taisho University, Japan).
#'
#' In this implementation, the code of `anovakun` version 4.8.9 has been included as-is with no modification
#' to its computational logic or output. However, for the purpose of maintainability and modularity,
#' internal helper functions have been separated into a different file.
#'
#' Additionally, all original Japanese-language comments have been removed from the source code to maintain
#' stylistic consistency with this package and to simplify documentation.
#'
#' Redistribution and minor modifications of the original function are permitted under the terms indicated
#' by the original author, provided that such modifications are clearly stated. This implementation
#' complies with that policy.
#'
#' For methodological details and rationale, please refer to the original documentation of `anovakun` version 4.8.9.
#'
#' @param dataset A data frame containing the input data.
#' @param design A character string specifying the experimental design (e.g., "A", "AB", "ABB", etc.).
#' @param ... Additional arguments passed to internal functions.
#' @param long Logical. Whether the data is in long format.
#' @param type2 Logical. Whether to use Type II sums of squares (default is FALSE for Type III).
#' @param nopost Logical. If TRUE, skips post hoc tests.
#' @param tech Logical. If TRUE, returns raw lists of results instead of printing.
#' @param data.frame Logical. If TRUE, returns the reformatted data frame.
#' @param copy Logical. If TRUE, copies the output to the clipboard.
#' @param holm,...,bgraph Additional flags controlling specific analyses (see documentation).
#'
#' @return Depending on the arguments, either printed output or a list of ANOVA results.
#'
#' @keywords anova internal replication
#' @examples
#'
#'  data_snakemr %>%
#'    anovakun("sABC", 2, 2, 5, long = TRUE)
#'
#' @seealso
#' The original documentation for `anovakun` by Prof. Ryuta Iseki is available at:
#' \url{https://riseki.cloudfree.jp/?ANOVA%E5%90%9B}
#'
#' @import dplyr
#' @import tidyr
#' @export

anovakun <- function(dataset, design, ..., long = FALSE, type2 = FALSE, nopost = FALSE, tech = FALSE, data.frame = FALSE, copy = FALSE,
	holm = FALSE, hc = FALSE, s2r = FALSE, s2d = FALSE, fs1 = FALSE, fs2r = FALSE, fs2d = FALSE, welch = FALSE, criteria = FALSE,
	lb = FALSE, gg = FALSE, hf = FALSE, cm = FALSE, auto = FALSE, mau = FALSE, har = FALSE, iga = FALSE, ciga = FALSE,
	eta = FALSE, peta = FALSE, geta = NA, eps = FALSE, peps = FALSE, geps = NA, omega = FALSE, omegana = FALSE, pomega = FALSE,
	gomega = NA, gomegana = NA, prep = FALSE, nesci = FALSE, besci = FALSE, cilmd = FALSE, cilm = FALSE, cind = FALSE, cin = FALSE,
	ciml = FALSE, cipaird = FALSE, cipair = FALSE, bgraph = c(NA, NA)){
	maxfact <- nchar(design) - 1
	datform <- uni.long(dataset = dataset, design = design, ... = ..., long = long)
	dat <- datform$dat
	factnames <- datform$factnames
	flev <- datform$flev
	miscase <- datform$miscase
	if(sum(is.na(bgraph)) < 2){
		eval(parse(text = paste0(bgraph, " <- TRUE")))
	}
	baseresults <- ci.calc(dat = dat, design = design, factnames = factnames,
		cilmd = cilmd, cilm = cilm, cind = cind, cin = cin, ciml = ciml, cipaird = cipaird, cipair = cipair)
	mainresults <- anova.modeler(dat = dat, design = design, factnames = factnames, type2 = type2, lb = lb, gg = gg, hf = hf,
		cm = cm, auto = auto, mau = mau, har = har, iga = iga, ciga = ciga, eta = eta, peta = peta, geta = geta,
		eps = eps, peps = peps, geps = geps, omega = omega, omegana = omegana, pomega = pomega, gomega = gomega,
		gomegana = gomegana, prep = prep, nesci = nesci)
	postresults <- post.analyses(dat = dat, design = design, factnames = factnames, mainresults = mainresults, type2 = type2,
		nopost = nopost, holm = holm, hc = hc, s2r = s2r, s2d = s2d, fs1 = fs1, fs2r = fs2r, fs2d = fs2d, welch = welch,
		criteria = criteria, lb = lb, gg = gg, hf = hf, cm = cm, auto = auto, mau = mau, har = har, iga = iga, ciga = ciga,
		eta = eta, peta = peta, geta = geta, eps = eps, peps = peps, geps = geps, omega = omega, omegana = omegana, pomega = pomega,
		gomega = gomega, gomegana = gomegana, prep = prep, nesci = nesci)
	if(besci){
		bootes <- boot.esci(dat, design, factnames = factnames, type2 = type2, nopost = nopost, mainresults = mainresults,
			postresults = postresults, lb = lb, gg = gg, hf = hf, cm = cm, auto = auto, mau = mau, har = har, iga = iga, ciga = ciga,
			eta = eta, peta = peta, geta = geta, eps = eps, peps = peps, geps = geps, omega = omega, omegana = omegana,
			pomega = pomega, gomega = gomega, gomegana = gomegana, prep = prep, B = 2000, conf.level = 0.95)
		mainresults$besci.info <- bootes$besci.info
		mainresults$bescitab <- bootes$bescitab[[1]]
		if(length(bootes$bescitab) > 1){
			postnames <- names(postresults)
			intnames <- postnames[nchar(postnames) == 3]
			for(i in 1:length(intnames)){
				postresults[[intnames[i]]]$bescitab <- bootes$bescitab[[i + 1]]
			}
		}
	}
	info1 <- paste0("[ ", design, "-Type Design ]")
	info2 <- paste0("This output was generated by anovakun 4.8.9 under ", strsplit(R.version$version.string, " \\(")[[1]][1], ".")
	info3 <- paste0("It was executed on ", date(), ".")
	exe.info <- c(info1, info2, info3)
	if(length(unique(baseresults$bstatist$n)) != 1){
		if(type2 == TRUE) mainresults$ano.info1 <- append(mainresults$ano.info1, c("== This data is UNBALANCED!! ==", "== Type II SS is applied. =="))
		else mainresults$ano.info1 <- append(mainresults$ano.info1, c("== This data is UNBALANCED!! ==", "== Type III SS is applied. =="))
	}
	if(miscase != 0){
		baseresults$bstat.info1 <- append(baseresults$bstat.info1, paste0("== The number of removed case is ", miscase, ". =="))
	}
	if(copy){
		plat.info <- .Platform
		if(sum(grep("windows", plat.info)) != 0){
			sink("clipboard", split = TRUE)
		}else if(sum(grep("mac", plat.info)) != 0){
			tclip <- pipe("pbcopy", "w")
			sink(tclip, split = TRUE)
		}else if(sum(grep("linux", R.version$system)) != 0){
			tclip <- pipe("xclip -selection clipboard")
			sink(tclip, split = TRUE)
		}
	}
	if(tech){
		postnames <- names(postresults)
		intnames <- postnames[nchar(postnames) == 3]
		if(length(intnames) > 0){
			for(i in intnames){
				postresults[[i]] <- postresults[[i]][-(7:9)]
			}
		}
		if(is.null(mainresults$besci.info)){
			retlist <-list("INFORMATION" = rbind(info1, info2, info3),
				"DESCRIPTIVE STATISTICS" = baseresults,
				"SPHERICITY INDICES" = list(mainresults$epsi.info1, mainresults$epsitab),
				"ANOVA TABLE" = list(mainresults$ano.info1, mainresults$anovatab, mainresults$nescitab),
				"POST ANALYSES" = postresults)
		}else{
			retlist <-list("INFORMATION" = rbind(info1, info2, info3),
				"DESCRIPTIVE STATISTICS" = baseresults,
				"SPHERICITY INDICES" = list(mainresults$epsi.info1, mainresults$epsitab),
				"ANOVA TABLE" = list(mainresults$ano.info1, mainresults$anovatab, mainresults$nescitab),
				"EFFECT SIZE INFORMATION" = list(mainresults$besci.info, mainresults$bescitab),
				"POST ANALYSES" = postresults)
		}
		if(data.frame == TRUE){
			names(dat) <- c("s", factnames, "y")
			retlist <- c(retlist, list("DATA.FRAME" = dat))
		}
		return(retlist)
	}else{
		if(data.frame == TRUE){
			names(dat) <- c("s", factnames, "y")
			anova.output(maxfact = maxfact, exe.info = exe.info, baseresults = baseresults,
				mainresults = mainresults, postresults = postresults)
			return(list("DATA.FRAME" = dat))
		}else{
			anova.output(maxfact = maxfact, exe.info = exe.info, baseresults = baseresults,
				mainresults = mainresults, postresults = postresults)
		}
	}
	if(copy){
		sink()
		if(plat.info$OS.type != "windows"){
			close(tclip)
		}
	}
	if(sum(is.na(bgraph)) < 2 & maxfact <= 3){
		ci.bars(dat, design, factnames = factnames, inn.tier = bgraph[1], out.tier = bgraph[2])
	}
}
