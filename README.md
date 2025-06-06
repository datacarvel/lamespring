# Lame spring in Montreal, Ottawa and Quebec City : A Calendar Heatmap
Calendar heatmap showing how terrible this spring was in Montreal, Ottawa and Quebec City, showing max temperatures, cloudiness and precipitations for April and May.

Using R with packages {ggcal} {weathercan} {tidyverse} {patchwork}

![Montreal](https://github.com/datacarvel/lamespring/blob/main/MTL.png "Montreal")

![Ottawa](https://github.com/datacarvel/lamespring/blob/main/OTT.png "Ottawa")

![Quebec City](https://github.com/datacarvel/lamespring/blob/main/QC.png "Quebec City")

To see the code, start with scriptMTL-main.R, which contains all the code from start to finish. The two other scripts simply are slightly edited versions for the two other city.

## Notes on methodology and visualization :

Black squares are NAs (Not Available) in ECCC's data. 

Precipitations include rain and snow (all in millimeters, snow occasionally fall in April but rarely in May). 

Cloudiness (_Nuageux_) is anytime the weather is not described as "Clear" or "Mostly Clear". So it includes Cloudy, Mostly Cloudy, Fog, Rain, Thunderstorm, Snow, Drizzle, etc. 

Also, and more importantly, regarding the hourly cloudiness data, when the weather is either clear/mostly clear/mostly cloudy/cloudy (that is, when nothing is happening, weather-wise), the station data is not updated hourly, but every 3 hours, or whenever conditions change to something more noteworthy like precipitations or reduced visibility. **So in these cases, gaps in the hourly data were filled with their closest neighbor with available data**. Imperfect solution, but unlikely to significantly change the overall picture. This is only for the empty observations between clear/mostly clear/cloudy/mostly cloudy values. 

You can read more on the data collection and conditions/phenomenas definitions here : https://climate.weather.gc.ca/glossary_e.html
