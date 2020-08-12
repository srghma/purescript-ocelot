// This file modifies the base Tailwind configuration. We can freely re-use the same names or add new options to existing variables.

const config = require('tailwindcss/defaultConfig')

module.exports = {
  theme: {
    extend: {
      zIndex: {
        '60': '60'
      },

      borderWidth: {
        '3': '3px'
      },

      maxWidth: {
        '6xl': '110rem',
      },

      colors: {
        'black-10':        '#00091A',
        'black-20':        '#242A33',
        // 'black-modal-a90': 'rgba(0,9,26,0.9)',
        'gray-50':         '#5C6573',
        // 'gray-50-a20':     'rgba(102,113,128,0.2)',
        // 'gray-50-a30':     'rgba(102,113,128,0.3)',
        // 'gray-50-a80':     'rgba(102,113,128,0.8)',
        // 'gray-50-a90':     'rgba(102,113,128,0.9)',
        'gray-70':         '#8f9eb3',
        // 'gray-70-a30':     'rgba(143,158,179,0.3)',
        // 'gray-70-a40':     'rgba(143,158,179,0.4)',
        'gray-80':         '#C2C6CC',
        'gray-90':         '#E1E3E6',
        'gray-95':         '#F0F1F2',
        'gray-97':         '#F7F7F7',
        'blue-65':         '#008AA6',
        'blue-75':         '#009FBF',
        'blue-82':         '#00ABD1',
        'blue-88':         '#00BBE0',
        // 'red':             '#FF5471',
        // 'yellow':          '#FFC859',
        // 'green':           '#66C7AF',
        'steel-75':        '#A8B2BF',
        'steel-85':        '#BFCAD9',
        'steel-100':       '#E6F0FF',
      },

      spacing: {
        '7': '1.75rem',
        '9': '2.25rem',
        '14': '3.5rem',
        '30': '7.5rem',
        '50': '12.5rem',
        '60': '15rem',
        '80': '20rem',
        '90': '22.5rem',
        '120': '30rem',
        '160': '40rem',
      }
    },

    borderColor: (theme) => ({
      ...theme('colors'),
      default: theme('colors.gray-80', 'curentColor'),
    }),

    // minWidth: Object.assign(config.theme.minWidth, spacing),
    // maxHeight: Object.assign(config.theme.maxHeight, spacing),
    // minHeight: Object.assign(config.theme.minHeight, spacing),
  },

  variants: {
    borderWidth:     config.variants.borderWidth.concat(['hover']),
    visibility:      config.variants.visibility.concat(['hover', 'focus', 'group-hover']),
    borderColor:     config.variants.borderColor.concat(['group-hover']),
    textColor:       config.variants.textColor.concat(['group-hover']),
    backgroundColor: config.variants.backgroundColor.concat(['active', 'group-hover']),
  },
}
