## TODO

- [x] Split into separate crates: nfa parser, redirect query, command line tool, wasm module
- [x] Create mock extension (with faked suggestor)
- [x] Bundle WASM into extension
- [x] Plug in the parser (with hardcoded config or something like that).
- [x] Fix suggestion behavior -- return multiple suggestions for substitutions
- [x] Fix suggestion behavior -- return multipls suggestions in middle of substitution
- [x] Figure out how to handle state
- [ ] Figure out how to handle configuration
- [ ] Extension pages: conflict resolution
- [ ] Extension pages: list of all commands
- [ ] Extension pages: configuration
- [ ] Fix suggestions behavior (return longer items)
- [ ] Add more tests to all parser parts to document expectations / behavior of modules
- [ ] Add better parser error handling -- more useful errors
- [ ] Consider removing regex node for compile time weight (or not bundle it into wasm).
