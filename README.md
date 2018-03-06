# platypuslol

Clone of bunny1 (or facebooks bunnylol) in Haskell. Currently no ground breaking features, just simple playground project. Use at your own risk (for now).

## Notes

```
stack build --haddock
stack hoogle -- generate --local
stack build hlint hscope haskell-tools-daemon haskell-tools-cli --copy-compiler-tool
# this is sad:-(
# How to get ghc-mod working: 
# https://github.com/DanielG/ghc-mod/pull/922#issuecomment-353896120
# TODO this is not scalable for more than 1 project
stack hoogle -- server --local --port=8080
```
