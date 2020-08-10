import webpack from 'webpack'

import createConfig from './config'
import webpackGetError from './lib/webpackGetError'

require('webpack-spago-loader/watcher-job')({
  additionalWatchGlobs: ['app/**/*.css', 'src/**/*.css'],
  options: require('./lib/spago-options'),
  onStart: () => {},
  onError: () => {},
  onSuccess: async () => {
    const config = createConfig({ production: false })

    const compiler = webpack(config)

    console.log('[webpack] Compiling...')

    compiler.run((err, stats) => {
      const error = webpackGetError(err, stats)

      if (error) {
        console.error(error)
        return
      }
    })
  }
})
