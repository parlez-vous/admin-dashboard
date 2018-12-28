'use strict';

require('./css/normalize.css')
require('./css/skeleton.css')
require('./css/custom.css')

var Elm = require('./elm/Main.elm').Elm;

// .embed() can take an optional second argument.
// This would be an object describing the data we need to start a program
// i.e. a userID or some token
var app = Elm.Main.init();
