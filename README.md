__WIKIPEDIA FLU TREND.alpha [DEPRECATED]__  

__Quick Start__: Copy-Paste and run these three lines of code in your up-to-date version of R.  Everything will be downloaded automatically and the platform should launch in your browser (First installation may take a few minutes):

```ruby
if(eval(require(RCurl))==0){install.packages("RCurl")}
url = "https://raw.githubusercontent.com/projectflutrend/pft/master/pft_installation.R"
eval(parse(text = RCurl::getURL(url)), envir= .GlobalEnv)
```

For more info see https://goo.gl/NT2DfQ 
