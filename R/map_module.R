#' @import dplyr

gen_nested_id <- function(id, widget_id, scope_id = '') {

	if(scope_id == '')
	  return(paste(id, widget_id, sep = "-"))

  return(paste(scope_id, id, widget_id, sep = "-"))
}

render_map <- function(shapefile, markers, api_key){

	googleway::renderGoogle_map({
		googleway::google_map(key = api_key,
							 location = c(markers$lat, markers$lon),
							 zoom = 8,
							 search_box = TRUE,
							 geolocation = TRUE,
							 width = '100%',
							 height = '100%'
		) %>%
			googleway::add_polygons(data = shapefile,
									 fill_colour = "fill_color",
									 stroke_colour = "#030303",
									 update_map_view = FALSE) %>%
			googleway::add_markers(data = data.frame(lat = markers$lat, lon = markers$lon),
									draggable = TRUE,
									update_map_view = FALSE)
	})

}

update_map <- function(markers, map_id, type){

	if (type == "geolocation"){
	 
		googleway::google_map_update(map_id = map_id) %>%
			googleway::clear_markers() %>%
			googleway::add_markers(data = data.frame(lat = markers$lat, lon = markers$lon),
									draggable = TRUE,
									update_map_view = FALSE) %>%
			googleway::google_map_view(location = c(markers$lat, markers$lon), zoom = 8)
	} else {
		googleway::google_map_update(map_id = map_id) %>%
			googleway::clear_markers() %>%
			googleway::add_markers(data = data.frame(lat = markers$lat, lon = markers$lon),
									draggable = TRUE,
									update_map_view = FALSE)
	}

}

region_data <- function(shapefile, markers) {

	removeNotification(id = "region_error", session = getDefaultReactiveDomain())

	dat <- data.frame(Longitude = markers$lon,
										Latitude = markers$lat,
										names = c("Point"))

	dat <- sf::st_as_sf(dat,
											coords = c("Longitude",
																 "Latitude"))

	sf::st_crs(dat) <- sf::st_crs(shapefile)

	return(as.data.frame(shapefile)[which(sapply(sf::st_intersects(shapefile,dat), function(z) if (length(z)==0) NA_integer_ else z[1]) == 1), ])
}

map_mod_ui <- function(id){
	ns <- NS(id)
	googleway::google_mapOutput(ns("map")) %>% shinycssloaders::withSpinner(type = 6, color="#005fae")
}

map_mod_server <- function(id, api_key, shapefile_path, region_behavior, default_lat, default_lon, scope_id = "") {
	moduleServer(
		id,
		function(input, output, session){

			shapefile <- sf::st_read(shapefile_path)

			shapefile <- sf::st_as_sf(shapefile)  %>%
				mutate(fill_color = if_else(!is.na(region), "#1C00ff00", "#A9A9A9"))


			output$map <- render_map(api_key = api_key, 
			                         shapefile = shapefile,
															 markers = data.frame(lat = default_lat, lon = default_lon))

			current_markers <- reactiveValues(
				lat=default_lat, lon=default_lon)
			
			rd <- region_data(shapefile = shapefile,
			                  markers = data.frame(lat = default_lat,
			                                       lon = default_lon))
			
			current_markers <- region_behavior(shapefile = shapefile,
			                                   region_data = rd,
			                                   current_markers = current_markers,
			                                   testing_markers = data.frame(lat = default_lat,
			                                                                lon = default_lon))

			observeEvent(input$map_marker_drag, {


				rd <- region_data(shapefile = shapefile,
													markers = data.frame(lat = input$map_marker_drag$lat,
																							 lon = input$map_marker_drag$lon))

				if(nrow(rd) == 0){
					showNotification("Error: no data for this location - moving point to default location!", id = "region_error")
					update_map(markers = current_markers,
										 map_id = gen_nested_id(id, "map", scope_id),
										 type = "drag")
				} else {
					current_markers <- region_behavior(shapefile = shapefile,
																						 region_data = rd,
																						 current_markers = current_markers,
																						 testing_markers = data.frame(lat = input$map_marker_drag$lat,
																						 														 lon = input$map_marker_drag$lon))

					if(!(current_markers$lat == input$map_marker_drag$lat &
							 current_markers$lon == input$map_marker_drag$lon)){
						update_map(markers = current_markers,
											 map_id = paste0(scope_id, id, "-map"),
											 type = "drag")
					}
				}


			})

			observeEvent(input$map_polygon_click, {

				rd <- region_data(shapefile = shapefile,
													markers = data.frame(lat = input$map_polygon_click$lat,
																							 lon = input$map_polygon_click$lon))

				current_markers <- region_behavior(shapefile = shapefile,
																					 region_data = rd,
																					 current_markers = current_markers,
																					 testing_markers = data.frame(lat = input$map_polygon_click$lat,
																					 														 lon = input$map_polygon_click$lon))

				update_map(markers = current_markers,
									 map_id = gen_nested_id(id, "map", scope_id),
									 type = "click")

			})

			observeEvent(input$map_geolocation,{
			  

				rd <- region_data(shapefile = shapefile,
													markers = data.frame(lat = current_markers$lat,
																							 lon = current_markers$lon))

				current_markers <- region_behavior(shapefile = shapefile,
																					 region_data = rd,
																					 current_markers = current_markers,
																					 testing_markers = data.frame(lat = current_markers$lat,
																					 														 lon = current_markers$lon))

				if(is.null(input$map_geolocation$lat) | is.null(input$map_geolocation$lon)){


	#				rd <- region_data(shapefile = shapefile,
	#													markers = data.frame(lat = current_markers$lat,
	#																							 lon = current_markers$lon))

	#				current_markers <- region_behavior(shapefile = shapefile,
	#																					 region_data = rd,
	#																					 current_markers = current_markers,
	#																					 testing_markers = data.frame(lat = current_markers$lat,
	#																					 														 lon = current_markers$lon))

	#				update_map(markers = current_markers,
	#									 map_id = gen_nested_id(id, "map", scope_id),
	#									 type = "geolocation")

				} else {

					rd <- region_data(shapefile = shapefile,
														markers = data.frame(lat = as.numeric(input$map_geolocation$lat),
																								 lon = as.numeric(input$map_geolocation$lon)))

					current_markers <- region_behavior(shapefile = shapefile,
																						 region_data = rd,
																						 current_markers = current_markers,
																						 testing_markers = data.frame(lat = as.numeric(input$map_geolocation$lat),
																						 														 lon = as.numeric(input$map_geolocation$lon)))

					if(length(current_markers$region) == 1){
					update_map(markers = current_markers,
										 map_id = gen_nested_id(id, "map", scope_id),
										 type = "geolocation")
					} else {
						showNotification("Error: no data for this location - moving point to default location!", id = "region_error")
					}


				}

			}, ignoreNULL = FALSE)

			observeEvent(input$map_place_search, {

				rd <- region_data(shapefile = shapefile,
													markers = data.frame(lat = input$map_place_search$lat,
																							 lon = input$map_place_search$lon))
				

				if(nrow(rd) == 0){
					showNotification("Error: no data for this location - moving point to default location!", id = "region_error")
					update_map(markers = current_markers,
										 map_id = gen_nested_id(id, "map", scope_id),
										 type = "search")
				} else {
					current_markers <- region_behavior(shapefile = shapefile,
																						 region_data = rd,
																						 current_markers = current_markers,
																						 testing_markers = data.frame(lat = input$map_place_search$lat,
																						 														 lon = input$map_place_search$lon))

					update_map(markers = current_markers,
										 map_id = gen_nested_id(id, "map", scope_id),
										 type = "search")
				}
			})

			return(current_markers)

		})
}
