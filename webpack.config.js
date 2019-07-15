const webpack = require('webpack')
const path = require("path");
const HtmlWebpackPlugin = require('html-webpack-plugin');

const SOURCE_DIR = path.join(__dirname, 'src')

const mode = process.env.NODE_ENV === 'production'
  ? 'production'
  : 'development'


// Copy the specified environment variables into an object we can pass to
// webpack's DefinePlugin
const copyArgs = (args) =>
  args.reduce(
    (acc, key) => ({
      // Create an object with the specified key
      ...acc,
      [`process.env.${key}`]: JSON.stringify(process.env[key]),
    }),
    {}
  )
  
const commonCssLoaders = [
  'style-loader',
  'css-loader',
]
  
module.exports = {
  mode,
  
  entry: {
    app: [
      './src/index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js',
    publicPath: '/',
  },

  module: {
    rules: [
      {
        test: /\.(css|scss)$/,
        use: commonCssLoaders,
      },
      {
        test: /\.sass$/,
        use: [
          ...commonCssLoaders,
          'sass-loader'
        ]
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack-loader',
      },
    ],

    noParse: /\.elm$/,
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: path.join(SOURCE_DIR, 'index.html'),
      minify: {
        collapseWhitespace: true,
        removeComments: true
      }
      // favicon: path.resolve('./static/favicon.png')
    }),
    new webpack.DefinePlugin({
      ...copyArgs([
        'NODE_ENV',
        'API',
      ]),
    })
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
    historyApiFallback: true
  },
};
