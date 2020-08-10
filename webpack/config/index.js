import MiniCssExtractPlugin from 'mini-css-extract-plugin'
import * as path from 'path'
import * as webpack from 'webpack'

import root from '../lib/root'

console.log(path.resolve(root, 'dist'))
export default async function ({
  watch,
  production,
  serverPort,
}) {
  return {
    watch: watch,

    target: 'web',

    mode: production ? 'production' : 'development',
    // mode: 'development',

    output: {
      path: path.resolve(root, 'dist'),
      filename: 'index.js',
      publicPath: '/',
    },

    entry: { main: path.resolve(root, "app", "indexs") },

    bail: true,
    profile: false,
    stats: 'errors-only',

    context: root,

    devtool: production ? false : 'eval',

    module: {
      rules: require('./rules')({ production })
    },

    resolve: {
      modules: [ 'node_modules' ],
      extensions: [ '.purs', '.js']
    },

    plugins: [
      new MiniCssExtractPlugin({
        filename: 'index.css',
      }),

      new webpack.NoEmitOnErrorsPlugin(),

      new (require('clean-webpack-plugin').CleanWebpackPlugin)(),

      new (require('html-webpack-plugin'))({
        minify: false,
      })
    ],
  }
}
