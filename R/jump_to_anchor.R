autoscroll_to_anchor <- "
console.log('auto scrolling to top');
$('html, body').stop().animate({
				scrollTop: $('#anchor').offset().top
}, 100, 'linear');
"
