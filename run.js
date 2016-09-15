#!/usr/bin/env node
const fs = require('fs')
const readStdin = require('./stdin.js')
const ohm = require('ohm-js')
const grammar = ohm.grammar(fs.readFileSync('./jslike.grammar').toString('utf8'))

readStdin(text => {
  const result = grammar.match(text)
  if (result.succeeded()) {
    console.log('Worked')
  } else {
    console.log('Failed to parse')
  }
})

