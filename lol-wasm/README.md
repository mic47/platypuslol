## TODO

- [ ] Split into separate crates: nfa parser, redirect query, command line tool, wasm module
- [ ] Create mock extension (with faked suggestor)
- [ ] Bundle WASM into extension
- [ ] Plug in the parser (with hardcoded config or something like that).
- [ ] Figure out how to handle state, & configuration
- [ ] Extension pages: conflict resolution
- [ ] Extension pages: list of all commands
- [ ] Extension pages: configuration
- [ ] Add better parser error handling -- more useful errors
- [ ] Consider removing regex node for compile time weight (or not bundle it into wasm).
