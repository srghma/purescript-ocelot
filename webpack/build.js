import webpack from 'webpack'

import createConfig from './config'
import webpackGetError from './lib/webpackGetError'

;(async function() {
  const config = createConfig({ production: true })

  await require('webpack-spago-loader/build-job')(require('./lib/spago-options'))

  const compiler = webpack(config)

  console.log('[webpack] Compiling...')

  compiler.run((err, stats) => {
    const error = webpackGetError(err, stats)

    if(error) {
      throw new Error(error)
    }
  })
})()
