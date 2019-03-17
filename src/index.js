'use strict';

require('./css/normalize.css')
require('./css/skeleton.css')
require('./css/custom.css')

var Elm = require('./elm/Main.elm').Elm;

var app = Elm.Main.init();
