
module.exports = {
  devtool: 'cheap-module-eval',
  entry: {
    bundle: './bs_build/browser.js',
    worker: './bs_build/worker.js',
  },
  output: {
    path: './docs/',
    filename: '[name].js',
  }
}

