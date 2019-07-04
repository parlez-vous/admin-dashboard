'use strict';

require('./css/custom.sass')

const Elm = require('./elm/Main.elm').Elm;

const storagekey = '@pv/token'

const api = process.env.API || 'https://parlez-vous-web.appspot.com'

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

app.ports.removeToken.subscribe(() => {
  localStorage.removeItem(storagekey)
})
