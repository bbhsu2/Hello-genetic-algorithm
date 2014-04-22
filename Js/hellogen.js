var GA_POPSIZE = 2048,
    GA_ELITRATE = 0.10,
    GA_MUTATIONRATE = 0.25,
    GA_TARGET = "Hello world!",
    population = [],
    buffer = [];
function generateCitizen(){
	var text = "",
		possible = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&'()*+,-./:;<=>?@[\]^_`{|}~";
	for(var i = 0; i < GA_TARGET.length; i++){
		text += possible.charAt(Math.floor(Math.random() * possible.length));
	}
	return text;
}
function init(){
	for(var i = 0; i < GA_POPSIZE; i++){
		population[i] = generateCitizen();
		buffer[i] = generateCitizen();
	}
}
function getFitness(value){
	var a = 0;
	for (var i = 0; i < value.length; i++){
		a += Math.abs(value.charCodeAt(i) - GA_TARGET.charCodeAt(i));
	}
	return a;
}
function mate(){
	var esize = (GA_POPSIZE * GA_ELITRATE)|0;
	for(var i=0; i < esize; i++){
		buffer[i] = population[i];
	}
	for(var i = esize; i < GA_POPSIZE; i++){
		var i1 = (Math.random() * (GA_POPSIZE/2) )|0,
			i2 = (Math.random() * (GA_POPSIZE/2) )|0,
			spos = (Math.random() * (GA_TARGET.length))|0;
		buffer[i] = population[i1].substring(0,spos) + population[i2].substring(spos, population[i2].length);
		if(Math.random() < GA_MUTATIONRATE){
			var pos = (Math.random() * GA_TARGET.length)|0;
			buffer[i] = buffer[i].substring(0, pos) + String.fromCharCode((Math.random() * 125)) + buffer[i].substring(pos+1, buffer[i].Length);
		}
	}
}
function swap(){
	var temp = population;
	population = buffer;
	buffer = temp;
}

function run(){
	init();
	var i = 0;
	$("#displayList").empty();
	while(true){
		population.sort( function(a,b){return getFitness(a) - getFitness(b)});
		$("#displayList").append('<li>' + 'Best(' + getFitness(population[0]) + '):' + population[0]+'</li>').hide().slideDown("slow");
		if(getFitness(population[0]) === 0)
			break;
		mate();
		swap();
		i++;
	}
}
run()