module.exports = {
  env: {
    browser: true,
    commonjs: true,
    es2021: true
  },
  parserOptions: {
    ecmaVersion: 'latest'
  },
  extends: [
    'standard'
  ],
  globals: {
    describe: true,
    it: true
  }
}
