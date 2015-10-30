
getS3Classes  <-  function(x){
	(temp = x[grepl("^(as|print|plot|<-|\\[|\\[\\[|\\$)\\.",x)])
	bkup  <-  character(0L)
	while(!identical(bkup,temp)){
		bkup <- temp
		for(name in temp)
			temp  <-  temp[!grepl(paste0('^',gsub('([[$])','\\\\\\1',name)),temp) | name==temp]
	}
	sort(unique(gsub("^(as|print|plot|<-|\\[|\\[\\[|\\$)\\.","",temp)))
}

dropS3Methods <- function(x,classes){
	(x  <-  x[!grepl("[-[`~!@#$^&*(=+}|;:'?,<>^\\{\\\"]",x)])
	for(cls in classes)
		x  <-  x[(!grepl(paste0('\\.',cls,'$'),x) &
				  !grepl(paste0('^[ai]s\\.',cls,'$'),x) )]
	union(x)
}

dropSpecials  <-  function(x) x[!grepl("[-[`~!@#$^&*(=+}|;:'?,<>^\\{\\\"]",x)]

basePackages  <-  c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")


#' Generate a custom R.vim syntax file
#'
#' Generate a custom R.vim syntax file
#' @export
#' @param packages packages from which function names should be included in your syntax file
#' @param syntax.vim a compatible Syntax file from which to build your custom syntax file
#' @param S3classes a vector of classes to be included when matching S3 methods
#' @param outfile Optional; a file name either a character string naming a file or a connection open for writing. otherwise, the r.vim file is returned to the console.
#' @note  
#' vimSyntax() searches for the names of S3 classes using a few heuristic rules, but may overlook S3 
#' classes whose name contains a dot (e.g. "data.table"), which affects the exported keywords.
#' to include classes like "data.table", include them in the S3classes argument.
#' @examples
#' \dontrun{
#' BUNDLE <-  "~/../vimfiles/bundle" # Your path will most likely be different!
#' dir.create(file.path(BUNDLE,"vim-R-keywords/syntax"),FALSE,TRUE)
#' 
#' packages  <-  c("assertthat", "data.table", "DEoptim", "devtools", "dplyr",
#' 				  "DT", "dygraphs", "foreach", "ggplot2", "htmltools",
#' 				  "htmlwidgets", "iterators", "jsonlite", "knitr", "lubridate",
#' 				  "metricsgraphics", "mpgUtils", "plyr", "Rcpp", "reshape",
#' 				  "reshape2", "RPostgreSQL", "shiny", "shinydashboard", "tidyr",
#' 				  "TTR", "xlsx", "xml2", "yaml", "zoo",
#' 				  "nlme", "parallel", "splines", "survival")
#' 
#' vimSyntax(myPackages, 
#'           S3classes = "data.table", 
#'           outfile = file.path(BUNDLE,"vim-R-keywords/syntax/r.vim"))
#' 
#' }
vimSyntax  <-  function(packages=union(basePackages,row.names(installed.packages())),
						syntax.vim=readLines("~/../temp/defaultsyntax.txt"),
						S3classes = "data.table",
						outfile=NULL){

	# READ IN THE EXISTING KEY WORDS
	keyWords  <- list() 
	cur_line=0
	keyword_line=0
	for(ll in  syntax.vim){
		cur_line <- cur_line +1
		if(length(ll) && grepl("^\\s*syn\\s*keyword\\s*(\\w)",ll)){
			(key= gsub("\\s*syn\\s*keyword\\s*(\\w+).*","\\1",ll))
			(words= base::strsplit(gsub("\\s*syn\\s*keyword\\s*\\w+\\s(.*)$","\\1",ll),'\\s')[[1]])
			keyWords[[key]]  <-  c(keyWords[[key]],words)
		}
		if(ll == 'if exists("r_package_other") && r_package_other != 0')
			r_package_other_line  <-  cur_line
	}

	# GET NEW KEY WORDS
	pkgNames <- list()
	for(pkg in union(basePackages,packages)){
		if(require(pkg,character.only=TRUE))
			pkgNames[[pkg]]  <-  ls(envir=as.environment(paste0('package:',pkg)))
		else
			cat("LOADING OF PACKAGE '",pkg,"' FAILED\n",sep = '')
	}

	allNames  <- unname(unlist(pkgNames))

	# DROP SPECIALS
	pkgNames  <-  lapply(pkgNames,dropSpecials)
	keyWords  <-  lapply(keyWords,dropSpecials)

	# DROP S3 METHODS 
	allClasses  <-  union(getS3Classes(unlist(pkgNames)),S3classes)
	pkgNames  <-  lapply(pkgNames,dropS3Methods,classes=allClasses)
	keyWords  <-  lapply(keyWords,dropS3Methods,classes=allClasses)

	# DROP EXISTING KEY WORDS
	pkgNames  <-  lapply(pkgNames,setdiff,y=unlist(keyWords))

	NewKeyWords  <-  union(unname(unlist(pkgNames[packages])),
						   intersect(c(paste0('is.',allClasses),
									   paste0('as.',allClasses),
									   allClasses),
									 allNames))

	out  <-  paste(c(syntax.vim[1:r_package_other_line],
		   	paste("  syn keyword rPackageFuns", base::strwrap(do.call(paste,as.list(NewKeyWords)),70)),
		   	syntax.vim[-(1:r_package_other_line)]),
		  collapse = '\n')

	if(!is.null(outfile))
		cat(out,file=outfile)
	else 
		out
}

