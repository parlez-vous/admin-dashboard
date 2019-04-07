'use strict';

require('./css/normalize.css')
require('./css/skeleton.css')
require('./css/custom.css')

const Elm = require('./elm/Main.elm').Elm;

const token = localStorage.getItem('@pv/token')

Elm.Main.init({
  flags: token
});
