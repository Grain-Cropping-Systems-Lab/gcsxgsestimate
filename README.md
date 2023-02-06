# The Crop Growth Stage and Forecasting Web-Tool

The growth stage of a crop is a critical piece of information for growers and agronomists. For small grain crops like wheat and triticale, many field management decisions are sensitive to the stage of crop development. These include irrigation, fertilizer additions, herbicide and pesticide applications, and harvest timing. In addition, the planting date, the amount and timing of rainfall and/or irrigation, and cumulative seasonal temperatures all impact how rapidly a crop emerges, develops and matures. Because of the extreme variability in winter rainfall in the state, rates of development for small grain crops in California are not always consistent from year-to-year and farm-to-farm. This can make precision management and planning more difficult. This tool provides estimates of crop growth stage, and seasonal patterns of nitrogen (N) and water use. Users can compare the current season to historical averages and see how forecasted conditions may impact near-term crop growth and development.

The tool is available now at: https://smallgrains.ucanr.edu/General_Production_Guidelines/Crop_Growth_Stage_and_Forecasting_Tool/

This repository contains the code for the R Shiny App that is The Crop Growth Stage and Forecasting Web-Tool. While currently only available for California, the underlying data is available for much of the US and the app could easily be adapted to provide information for other regions.

## Application set-up / contributions 

This application uses the framework laid out by [ThinkR's golem] (https://github.com/ThinkR-open/golem). To run the package locally, you can install like a R package. The application is run via the run_app() function, which requires the link to a [database with weather data](https://github.com/Grain-Cropping-Systems-Lab/grain-variety-db/blob/main/schema/tables/prism.sql) and a Google Maps API key. 

```r
devtools::install_github("Grain-Cropping-Systems-Lab/gcsxgsestimate")
gcsxgsestimate::run_app()
```

Please open an issue if you would like to contribute. 

## Citation

Please cite as

> Nelsen, T., Merz, J., Rosa, G., & Lundy, M. (2023). The Crop Growth Stage and Forecasting Web-Tool [Computer software]. https://github.com/Grain-Cropping-Systems-Lab/gcsxgsestimate


