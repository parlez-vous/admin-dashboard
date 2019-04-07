'use strict';

require('./css/normalize.css')
require('./css/skeleton.css')
require('./css/custom.css')

const Elm = require('./elm/Main.elm').Elm;

const storagekey = '@pv/token'

const token = localStorage.getItem(storagekey)

const app = Elm.Main.init({
  flags: token
});

app.ports.setToken.subscribe((t) => {
  localStorage.setItem(storagekey, t)
})
