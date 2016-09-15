#!/usr/bin/env node

const fs = require('fs')
const ohm = require('ohm-js')
const grammar = ohm.grammar(fs.readFileSync('./jsre.grammar').toString('utf8'))

const example = fs.readFileSync('./ex.txt').toString('utf8')

const semantics = grammar.createSemantics()
semantics.addAttribute('asRe', require('./stringify'))

const match = grammar.match(example)
const node = semantics(match)
const json = node.asRe
if (process.argv[2] !== 'quiet') {
  console.log(JSON.stringify(json, null, 2))
}

