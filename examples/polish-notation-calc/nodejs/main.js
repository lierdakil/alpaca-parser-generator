'use strict'
const {tokToStr, TokenType, lex} = require('./lexer.js')
const {Parser} = require('./parser.js')
const {createInterface} = require('readline')

const rl = createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
})

const debug = process.argv.length > 2 && process.argv[2] == '-d'
rl.on('line', line => {
  try {
    const tokens = lex(line, debug)
    const parser = new Parser(debug)
    const expr = parser.parse(tokens)
    console.log(`Result: ${expr}`)
  } catch (e) {
    console.log(`Error while parsing: ${e}`)
  }
})
