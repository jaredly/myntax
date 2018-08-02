
const watch = require('watch')
const fs = require('fs')
const c = require('child_process')
const path = require('path')

const [_, __, target] = process.argv;
if (!target) {
  console.log('Usage: node watch.js some/directory')
  process.exit(10)
}
console.log('Watching', target)

const bin = path.join(path.dirname(__filename), 'lib', 'bs', 'native', 'lisp.native')

watch.watchTree(target, {
  filter: f => f.slice(-4) === '.rel',
  interval: .2,
}, function (f, curr, prev) {
  if (curr) {
    let name = f.slice(0, -3) + 'ml'
    if (curr.nlink === 0) {
      fs.unlink(name)
    } else {
      console.log('changed', f)
      c.exec(bin + ' bin ' + f + ' > ' + name, (e, o, er) => {
        // console.log(o)
        // console.log(er)
      })
    }
  } else if (!prev) {
    Object.keys(f).forEach(f => {
      if (f.slice(-4) === '.rel') {
        let name = f.slice(0, -3) + 'ml';
        console.log('initial compile ' + f)
        c.exec(bin + ' bin ' + f + ' > ' + name);
      }
    })
  }
})
