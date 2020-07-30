module.exports = scoped => {
  if (scoped) {
    return [
      require('tailwindcss')('./tailwind.config.js'),
      require('postcss-prefix-selector')({
        prefix: '.ocelot-scoped',
        transform(prefix, selector, prefixedSelector) {
          if (selector === 'html') {
            return `html, ${prefix}`
          }
          if (selector === 'body') {
            return `body ${prefix}`
          }
          return prefixedSelector
        }
      }),
    ]
  }

  return [
    require('tailwindcss')('./tailwind.config.js'),
  ]
}
