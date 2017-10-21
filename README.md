WIKIPEDIA FLU TREND.alpha [DEPRECATED] – For some background visit:

Copy-Paste and run these three lines of code in your up-to-date version of R.  Everything will be downloaded automatically and the platform will – hopefully – launch in your browser (First installation may take a few minutes):

# WIKIPEDIA FLU TREND.alpha - Quick Start
if(eval(require(RCurl))==0){install.packages("RCurl")}
url = "https://raw.githubusercontent.com/projectflutrend/pft/master/pft_installation.R"
eval(parse(text = RCurl::getURL(url)), envir= .GlobalEnv)
