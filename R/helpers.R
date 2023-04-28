update_tab <- function(id, triggerId, dashboardId, tab, parent
) {
	moduleServer(id,
							 function(input, output, session){
							 	observeEvent(triggerId(), {
							 		updateTabItems(parent, dashboardId, selected = tab)
							 	})
							 })
}

reverseValue <- function(value) {
  if(value <= 10) { return(value) }
  else if(value > 10.0 & value <= 10.1) { return( 10 + ((value - 10) * 10)) }
  else if(value > 10.1 & value <= 10.5) { return( 11 + ((value - 10.1) * 2.5)) }
  else if(value > 10.5 & value <= 11) { return( 12 + ((value - 10.5) * 2)) }
  else if(value > 11 & value <= 11.4) { return( 13 + ((value - 11) * 2.5) ) }
}


hline <- function(y = 0, color = "black") {
  if(!is.na(y)){
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(color = color)
    )
  } 
}
