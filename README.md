
## The problem

You love syntax highlighting with VIM because it helps you rapidly identify 
mis-spelled function names, but none of the existing VIM R syntax files
include the function names from *your* favorite packages.  

This little R script helps you out by creating you own personal R syntax file,
with the key words from all your favorite R packages.

## The solution 

Start by loading or installing `vim_R_keywords`:

	if(!require("devtools"))
		install.packages("devtools")
	if(!require("vim.R.syntax")){
		devtools::install_github("jdthorpe/vim-R-syntax")
		library(vim.R.syntax)
	}

The easiest way to use the syntax file is with a package 
manager like [pathogen](https://github.com/tpope/vim-pathogen) 
or [vundle](https://github.com/VundleVim/Vundle.vim), 
in which case, you will want to 
store the script within your bundle directory like so:

	# Create the `vim-R-keywords/syntax` directory
	BUNDLE <-  "~/../vimfiles/bundle" # Your path will most likely be different!
	dir.create(file.path(BUNDLE,"vim-R-keywords/syntax"),FALSE,TRUE)

Your new syntax file may not work if there is an existing 
`syntax/r.vim` file elsewhere in your bundle directory, 
so you will want to find any such files already in existence and delete them:

	# Search for existing syntax/r.vim files
	BUNDLE_DIRS  <-  list.dirs(BUNDLE,full.names = TRUE)
	for(d in BUNDLE_DIRS[basename(BUNDLE_DIRS)=='syntax']) 
		if("r.vim" %in% list.files(d))
			cat(d,'\n')

The default list of packages (all currently installed packages!) can produce 
a huge number of function names (mine produces >10,000!), so you may want to 
limit the package to your favorite few dozen packages.

	packages  <-  c("assertthat", "data.table", "DEoptim", "devtools", "dplyr",
					  "DT", "dygraphs", "foreach", "ggplot2", "htmltools",
					  "htmlwidgets", "iterators", "jsonlite", "knitr", "lubridate",
					  "metricsgraphics", "mpgUtils", "plyr", "Rcpp", "reshape",
					  "reshape2", "RPostgreSQL", "shiny", "shinydashboard", "tidyr",
					  "TTR", "xlsx", "xml2", "yaml", "zoo",
					  "nlme", "parallel", "splines", "survival")

	vimSyntax(packages, 
              S3classes = "data.table",
			  outfile = file.path(BUNDLE,"vim-R-keywords/syntax/r.vim"))


