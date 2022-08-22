gdd_to_feekes <- function(x){
	
	df <- data.frame(growth_stage = c("emergence", "early-tillering", "mid-tillering", "late-tillering", 
																		"1-node visible", "2-nodes visible", "last leaf visible", "last leaf fully formed", 
																		"early boot", "late-boot/early-heading", "heading", "flowering", "milk", "dough", "kernel hard", "maturity"),
									 gdd = c(0, 350, 425, 500, 515, 530, 620, 680, 740, 800, 893, 986, 1100, 1300, 1550, 1650),
									 feekes = c(0, 3,4,5,6,7,8, 9,10, 10.1, 10.3, 10.5, 11.1, 11.2, 11.3, 11.4))
	
	if (x >= df[1, "gdd"] & x < df[2, "gdd"]){
		# lm(feekes ~ gdd, df[1:2, ])$coefficients
		x*0.008571429
	} else if (x >= df[2, "gdd"] & x < df[3, "gdd"]) {
		# lm(feekes ~ gdd, df[2:3, ])$coefficients
		x*0.01333333 - 1.66666667
	} else if (x >= df[3, "gdd"] & x < df[4, "gdd"]) {
		#lm(feekes ~ gdd, df[3:4, ])$coefficients
		x*0.01333333 - 1.66666667
	} else if (x >= df[4, "gdd"] & x < df[5, "gdd"]) {
		#lm(feekes ~ gdd, df[4:5, ])$coefficients
		x*0.06666667 - 28.33333333
	} else if (x >= df[5, "gdd"] & x < df[6, "gdd"]) {
		#lm(feekes ~ gdd, df[5:6, ])$coefficients
		x*0.06666667 - 28.33333333
	} else if (x >= df[6, "gdd"] & x < df[7, "gdd"]) {
		#lm(feekes ~ gdd, df[6:7, ])$coefficients
		x*0.01111111 + 1.11111111
	} else if (x >= df[7, "gdd"] & x < df[8, "gdd"]) {
		#lm(feekes ~ gdd, df[7:8, ])$coefficients
		x*0.01666667 - 2.33333333
	} else if (x >= df[8, "gdd"] & x < df[9, "gdd"]) {
		lm(feekes ~ gdd, df[8:9, ])$coefficients
		x*0.01666667 - 2.33333333
	} else if (x >= df[9, "gdd"] & x < df[10, "gdd"]) {
		#lm(feekes ~ gdd, df[9:10, ])$coefficients
		x*0.001666667 + 8.766666667
	} else if (x >= df[10, "gdd"] & x < df[11, "gdd"]) {
		#lm(feekes ~ gdd, df[10:11, ])$coefficients
		x*0.002150538 + 8.379569892
	} else if (x >= df[11, "gdd"] & x < df[12, "gdd"]) {
		#lm(feekes ~ gdd, df[11:12, ])$coefficients
		x*0.002150538 + 8.379569892
	} else if (x >= df[12, "gdd"] & x < df[13, "gdd"]) {
		#lm(feekes ~ gdd, df[12:13, ])$coefficients
		x*0.005263158 + 5.310526316
	} else if (x >= df[13, "gdd"] & x < df[14, "gdd"]) {
		#lm(feekes ~ gdd, df[13:14, ])$coefficients
		x*0.0005 + 10.5500
	} else if (x >= df[14, "gdd"] & x < df[15, "gdd"]) {
		#lm(feekes ~ gdd, df[14:15, ])$coefficients
		x*0.0004 + 10.6800
	} else if (x >= df[15, "gdd"] & x < df[16, "gdd"]) {
		#lm(feekes ~ gdd, df[15:16, ])$coefficients
		x*0.001 + 9.750
	} else {
		11.4
	}
}

growth_stage_estimate <- function(x){
	df <- data.frame(growth_stage = c("pre-emergence", "emergence", "1 leaf", "2 leaf", "3 leaf", 
																		"early-tillering", "mid-tillering", "late-tillering", 
																		"1-node visible", "2-nodes visible", "last leaf visible", "last leaf fully formed", 
																		"early boot", "late-boot/early-heading", "heading", "flowering", "milk", "dough", "kernel hard", "maturity"),
									 gdd = c(0, 98, 134, 206, 279, 350, 425, 500, 515, 530, 620, 680, 740, 800, 893, 986, 1100, 1300, 1550, 1650),
									 feekes = c(0, 0.86, 1.15, 1.77, 2.39, 3, 4, 5, 6, 7, 8, 9, 10, 10.1, 10.3, 10.5, 11.1, 11.2, 11.3, 11.4))
	
	df[which.max(df[x >= df$feekes, "feekes"]), "growth_stage"]
}

feekes_to_gdd <-  function(x){
	
	df <- data.frame(growth_stage = c("emergence", "early-tillering", "mid-tillering", "late-tillering", 
																		"1-node visible", "2-nodes visible", "last leaf visible", "last leaf fully formed", 
																		"early boot", "late-boot/early-heading", "heading", "flowering", "milk", "dough", "kernel hard", "maturity"),
									 gdd = c(0, 350, 425, 500, 515, 530, 620, 680, 740, 800, 893, 986, 1100, 1300, 1550, 1650),
									 feekes = c(0, 3,4,5,6,7,8, 9,10, 10.1, 10.3, 10.5, 11.1, 11.2, 11.3, 11.4))
	
	if (x >= df[1, "feekes"] & x < df[2, "feekes"]){
		#lm(gdd ~ feekes, df[1:2, ])$coefficients
		x*1.166667e+02
	} else if (x >= df[2, "feekes"] & x < df[3, "feekes"]) {
		# lm(gdd ~ feekes, df[2:3, ])$coefficients
		x*75 + 125
	} else if (x >= df[3, "feekes"] & x < df[4, "feekes"]) {
		#lm(gdd ~ feekes, df[3:4, ])$coefficients
		x*75 + 125
	} else if (x >= df[4, "feekes"] & x < df[5, "feekes"]) {
		#lm(gdd ~ feekes, df[4:5, ])$coefficients
		x*15 + 425
	} else if (x >= df[5, "feekes"] & x < df[6, "feekes"]) {
		#lm(gdd ~ feekes, df[5:6, ])$coefficients
		x*15 + 425
	} else if (x >= df[6, "feekes"] & x < df[7, "feekes"]) {
		#lm(gdd ~ feekes, df[6:7, ])$coefficients
		x*90 - 100
	} else if (x >= df[7, "feekes"] & x < df[8, "feekes"]) {
		#lm(gdd ~ feekes, df[7:8, ])$coefficients
		x*60 + 140
	} else if (x >= df[8, "feekes"] & x < df[9, "feekes"]) {
		#lm(gdd ~ feekes, df[8:9, ])$coefficients
		x*60 + 140
	} else if (x >= df[9, "feekes"] & x < df[10, "feekes"]) {
		#lm(gdd ~ feekes, df[9:10, ])$coefficients
		x*600 - 5260
	} else if (x >= df[10, "feekes"] & x < df[11, "feekes"]) {
		#lm(gdd ~ feekes, df[10:11, ])$coefficients
		x*465 - 3896.5
	} else if (x >= df[11, "feekes"] & x < df[12, "feekes"]) {
		#lm(gdd ~ feekes, df[11:12, ])$coefficients
		x*465 - 3896.5
	} else if (x >= df[12, "feekes"] & x < df[13, "feekes"]) {
		#lm(gdd ~ feekes, df[12:13, ])$coefficients
		x*190 - 1009
	} else if (x >= df[13, "feekes"] & x < df[14, "feekes"]) {
		#lm(gdd ~ feekes, df[13:14, ])$coefficients
		x*190 - 1009
	} else if (x >= df[14, "feekes"] & x < df[15, "feekes"]) {
		#lm(gdd ~ feekes, df[14:15, ])$coefficients
		x*2500 - 26700
	} else if (x >= df[15, "feekes"] & x < df[16, "feekes"]) {
		lm(gdd ~ feekes, df[15:16, ])$coefficients
		x*1000 - 9750
	} else {
		1650
	}
}


