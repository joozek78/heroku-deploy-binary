[![Build Status](https://travis-ci.org/joozek78/heroku-deploy-binary.svg?branch=master)](https://travis-ci.org/joozek78/heroku-deploy-binary)
# heroku-deploy-binary
This tool enables you to deploy a custom binary completely overriding heroku's git deployment.

# build

Should build under [stack](https://github.com/commercialhaskell/stack/) with simple `stack build`.
Right now it won't build with cabal if you have Haskell Platform installed due to conflict in `process` package version.

# usage
`heroku-deploy-binary <heroku app name> <binary>`

*Remember to compile the binary for linux x64 (as required by heroku)*
