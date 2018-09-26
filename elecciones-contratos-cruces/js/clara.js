$(document).on('click', '.butAcc', function () {
  Shiny.onInputChange('last_accion', this.id+Math.random());
});


$(document).on('click', '.butHallazgos', function (evn) {
  Shiny.onInputChange('last_hllz',this.id);
  var button =  evn.target;
  var bisabuelo = this.parentNode;
  var button = bisabuelo.querySelector('button');
  var isActive = document.querySelector('.butHallazgos.active');
  if (isActive) {
    isActive.classList.remove('active');
  }
  button.classList.add('active');
});


$(document).on('click', '.butVerMasCan', function () {
  Shiny.onInputChange('last_CandMenu',this.id+Math.random());
});


$(document).on('click', '.butVerMas', function () {
  Shiny.onInputChange('last_ver',this.id);
});


$(document).on('click', '.butVerMasFinRec', function () {
  Shiny.onInputChange('last_FindMenu',this.id);
});

$(document).on('click', '.butVerMasFin', function () {
  Shiny.onInputChange('last_verFin',this.id);
});




$(document).on('click', '.butCandt', function (evn) {
  Shiny.onInputChange('last_cand',this.id+Math.random());
  var button =  evn.target;
  var bisabuelo = this.parentNode;
  console.log(this.parentNode)
  var button = bisabuelo.querySelector('button');
  var isActive = document.querySelector('.butCandt.activeTem');
  if (isActive) {
    isActive.classList.remove('activeTem');
  }
  button.classList.add('activeTem');
});




$(document).on('click', '.butFindr', function (evn) {
  Shiny.onInputChange('last_frd',this.id+Math.random());
  var button =  evn.target;
  var bisabuelo = this.parentNode;
  console.log(this.parentNode)
  var button = bisabuelo.querySelector('button');
  var isActive = document.querySelector('.butFindr.activeFin');
  if (isActive) {
    isActive.classList.remove('activeFin');
  }
  button.classList.add('activeFin');
});

$(document).on('click', '.butFin', function (evn) {
  Shiny.onInputChange('last_Fin',this.id);
  var button =  evn.target;
  var bisabuelo = this.parentNode;
  console.log(this.parentNode)
  var button = bisabuelo.querySelector('button');
  var isActive = document.querySelector('.activeFin');
  if (isActive) {
    isActive.classList.remove('activeFin');
  }
  button.classList.add('activeFin');
});


$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});