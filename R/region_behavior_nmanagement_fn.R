region_behavior_nmanagement <- function(shapefile, region_data, current_markers, testing_markers){
		current_markers$lat <- testing_markers$lat
		current_markers$lon <- testing_markers$lon
		current_markers$region <- region_data$region
		current_markers$nuptakemod <- region_data$nuptakemod
		return(current_markers)
}