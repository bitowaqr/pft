# __WIKIPEDIA FLU TREND.alpha [DEPRECATED]__  

‘Wikipedia Flu Trend.alpha’ is an (early stage) R-based platform, intended to investigate the utility of Wikipedia page view data for real-time surveillance of influenza in different countries. It is build to automatically identify, download and organize page view data from Wikipedia in different languages and to support model building for predicting influenza activity  i.e. predicting future surveillance reports about the present influenza activity. The platform has an easy-to-use interface and comes with some pre-installed Wikipedia and influenza data. It features bivariate linear regression, lasso regression + cross validation and arima forecasting techniques, and provides informative figures to assess and compare predictive models.

<mark>Please note: The platform and its functions are in a very early stage of development and may cause fatal errors and crashes. </mark>

If you have any questions or comments, or if you want to contribute, please contact 
schneider.paulpeter@gmail.com 


For more info see:
[Presentation slides](https://docs.google.com/presentation/d/16tvTO1sNVNIHuwBpFuxBwfpbSZkow7aO3pBnC9QDJiE/edit?usp=sharing) 
[Essential literature and references](https://docs.google.com/document/d/1Wpocdt58o2gXja8BRurwVe6JddnX2mdCckA58Ukt478/edit?usp=sharing)
[Documentation]() (In Preparation)

## __Quick Start Wikipedia Flu Trend.alpha__
Copy-Paste and run these three lines of code in your up-to-date version of R.  Everything will be downloaded automatically and the platform should launch in your browser (First installation may take a few minutes):

```ruby
if(eval(require(RCurl))==0){install.packages("RCurl")}
url = "https://raw.githubusercontent.com/projectflutrend/pft/master/pft_installation.R"
eval(parse(text = RCurl::getURL(url)), envir= .GlobalEnv)
```

While a proper documentation is in preparation, you can find some guidance  on how to use Wikipedia Flu Trend.alpha here: https://github.com/projectflutrend/pft/blob/master/pft_installation.R 
