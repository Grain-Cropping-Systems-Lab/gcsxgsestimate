slider_js <-
	"
$('.slider-container .sw-no-ui-slider').get().forEach(slider_div => {
let slider = slider_div.noUiSlider;
slider.updateOptions({
format: wNumb({
encoder: function(x){
if(x <= 10.0) {return parseFloat(x.toPrecision(5));}
else if (x > 10 && x < 11) {return parseFloat((10 + ((x-10)*.1)).toPrecision(5));}
else if (x >= 11 && x < 12) {return parseFloat((10.1 + ((x-11)*.4)).toPrecision(5));}
else if (x >= 12 && x <= 13) {return parseFloat((10.5 + ((x-12)*.5)).toPrecision(5));}
else {return parseFloat((11 + ((x - 13)*.4)).toPrecision(5));}}
})
});
slider.pips({
mode: 'values',
values: [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14],
density: 13,
format: {
to: function(x){
if(x <= 10) {return x}
else if(x == 11) {return 10.1;}
else if(x == 12) {return 10.5;}
else if(x == 13) {return 11;}
else {return 11.4;}}
}
});
});
"

color_slider_js <-
	"

let color_map = {0.79: 'red', 0.8: 'orange', 0.92:'orange', 0.93: 'yellow', 0.96:'yellow', 0.97:'green' };
$('.color-slider-container .sw-no-ui-slider').get().forEach(slider_div => {
	let slider = slider_div.noUiSlider;
	slider.on('update', function () {
		let goal = slider.get();
		let closest = Object.keys(color_map).reduce(function(prev, curr) {
		  return (Math.abs(curr - goal) < Math.abs(prev - goal) ? curr : prev);
		});
		let element = document.getElementById(this.target.id);
		for (const [key, value] of Object.entries(color_map)) {
			element.classList.remove(value);
		}
		element.classList.add(color_map[closest]);
	});
});
"

color_slider_lite_js <-
	"

let color_map = {1: 'red', 2: 'orange', 3: 'yellow', 4:'green' };
$('.color-slider-lite-container .sw-no-ui-slider').get().forEach(slider_div => {
	let slider = slider_div.noUiSlider;
	slider.on('update', function () {
		let goal = slider.get();
		let closest = Object.keys(color_map).reduce(function(prev, curr) {
		  return (Math.abs(curr - goal) < Math.abs(prev - goal) ? curr : prev);
		});
		let element = document.getElementById(this.target.id);
		for (const [key, value] of Object.entries(color_map)) {
			element.classList.remove(value);
		}
		element.classList.add(color_map[closest]);
	});
});
"
