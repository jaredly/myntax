#!/usr/bin/env node
'use strict'

var readStdin = done => {
  var text = ''
  process.stdin.setEncoding('utf8');

  process.stdin.on('readable', () => {
    var chunk = process.stdin.read();
    if (chunk !== null) {
      text = chunk.toString('utf8')
    }
  });

  process.stdin.on('end', () => {
    done(text)
  });
}

const walk = (data, fn) => {
  if (!data) return data
  if (typeof data !== 'object') return data
  for (let name in data) {
    data[name] = fn(walk(data[name], fn), name)
  }
  return data
}

const noloc = data => {
  walk(data, (obj, name) => {
    if (name === 'loc' ||
        name === 'pexp_loc' ||
        name === 'pvb_loc' ||
        name === 'ppat_loc' ||
        name === 'pstr_loc'
       ) return null
    return obj
  })
}

readStdin(text => {
  const data = JSON.parse(text)
  if (process.argv[2] === '--noloc') noloc(data)
  console.log(JSON.stringify(data, null, 2))
})

