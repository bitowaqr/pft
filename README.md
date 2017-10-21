WIKIPEDIA FLU TREND.alpha [DEPRECATED] – For some background visit: https://goo.gl/NT2DfQ 


__Quick Start__: Copy-Paste and run these three lines of code in your up-to-date version of R.  Everything will be downloaded automatically and the platform will launch in your browser (First installation may take a few minutes):

if(eval(require(RCurl))==0){install.packages("RCurl")}
url = "https://raw.githubusercontent.com/projectflutrend/pft/master/pft_installation.R"
eval(parse(text = RCurl::getURL(url)), envir= .GlobalEnv)
