#gs_correction(crop_type = "COMMON", cum_gdd = 1100, gd_rel_gdds_input = gd_rel_gdds)

gs_correction <- function(crop_type, cum_gdd, gd_rel_gdds_input){
  if(cum_gdd <1125){
    (gd_rel_gdds_input %>% 
       filter(group == 1000) %>% 
       filter(crop_sub_type == crop_type))$relative_gs
  } else if (cum_gdd <1375) {
    
    range_pts <- gd_rel_gdds_input %>% 
      filter(group == 1000 | 
               group == 1250) %>% 
      filter(crop_sub_type == crop_type)
    
    y1 <- range_pts[range_pts$group == 1000, ]$relative_gs
    y2 <- range_pts[range_pts$group == 1250, ]$relative_gs
    
    x1 <- range_pts[range_pts$group == 1000, ]$group
    x2 <- range_pts[range_pts$group == 1250, ]$group
    
    m <- ((y2-y1)/(x2-x1))
    
    b <- y1 - (x1*m)
    
    m*cum_gdd + b
    
  } else if (cum_gdd < 1625){
    
    range_pts <- gd_rel_gdds_input %>% 
      filter(group == 1250 | 
               group == 1500) %>% 
      filter(crop_sub_type == crop_type)
    
    y1 <- range_pts[range_pts$group == 1250, ]$relative_gs
    y2 <- range_pts[range_pts$group == 1500, ]$relative_gs
    
    x1 <- range_pts[range_pts$group == 1250, ]$group
    x2 <- range_pts[range_pts$group == 1500, ]$group
    
    m <- ((y2-y1)/(x2-x1))
    
    b <- y1 - (x1*m)
    
    m*cum_gdd + b
    
  } else {
    
    range_pts <- gd_rel_gdds_input %>% 
      filter(group == 1500 | 
               group == 1750) %>% 
      filter(crop_sub_type == crop_type)
    
    y1 <- range_pts[range_pts$group == 1500, ]$relative_gs
    y2 <- range_pts[range_pts$group == 1750, ]$relative_gs
    
    x1 <- range_pts[range_pts$group == 1500, ]$group
    x2 <- range_pts[range_pts$group == 1750, ]$group
    
    m <- ((y2-y1)/(x2-x1))
    
    b <- y1 - (x1*m)
    
    m*cum_gdd + b
    
  }
}