'use strict';

require('./css/normalize.css')
require('./css/skeleton.css')
require('./css/custom.css')

const Elm = require('./elm/Main.elm').Elm;

const storagekey = '@pv/token'

const api = process.env.API || 'http://staging.api.parlez-vous.io/admins'

const token = localStorage.getItem(storagekey)

const app = Elm.Main.init({
  flags: { 
    token, 
    api
  },
});

app.ports.setToken.subscribe((t) => {
  localStorage.setItem(storagekey, t)
})
