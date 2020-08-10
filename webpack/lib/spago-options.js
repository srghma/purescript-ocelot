module.exports = {
  compiler:  'psa',
  output:    require('webpack-spago-loader/lib/getAbsoluteOutputDirFromSpago')('./app/spago.dhall'),
  pursFiles: require('webpack-spago-loader/lib/getSourcesFromSpago')('./app/spago.dhall'),

  // note that warnings are shown only when file is recompiled, delete output folder to show all warnigns
  compilerOptions: {
    censorCodes: ['ImplicitQualifiedImport', 'UnusedImport', 'ImplicitImport'].join(','),

    // strict: true
  }
}
