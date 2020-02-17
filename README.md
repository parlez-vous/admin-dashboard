# Parlez-Vous Landing Page

> Work In Progress

`production` maps to https://parlez-vous.io

`master` maps to https://master--adoring-banach-15797c.netlify.com/ ... consider this the staging branch


Need to connect this with the [server](https://github.com/parlez-vous/server)



## Architectural Inspiration

- [elm-shared-state](https://github.com/ohanhi/elm-shared-state)
  - This, I would say, is the app from which I took the most inspiration from. I didn't even change the type names around shared state.

- [elm-shared-login](https://github.com/jxxcarlson/elm-shared-login)
  - Same idea

- [elm-spa-example](https://github.com/rtfeldman/elm-spa-example)


## Contributing

### Adding a new page

- Add a new `.elm` file to the `Routes` folder. This file will represent the page.
- Connect the `Model` and the `Msg` within the `Router.elm` file
  - Update the `parser` which will take care of url handling
  - Update the `update` function so that it's aware of events that will occur within the new page
  - Update the `view` function so that it renders your page!
