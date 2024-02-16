# platypuslol

Clone of bunny1 (or facebooks bunnylol) in Rust. Works as local server, or browser extension (currently chrome only, possibly broken). Use at your own risk (for now).

## Instructions

Assuming you have rust installed, simply run

```bash
make install
```

It will install everything for you, and run the service using systemd on linux. See Makefile for more details.
If you are on mac, you are on your own, but please share how you did it, I am happy to include it here.

After that, visit [https://localhost:3047/install](https://localhost:3047/install) in your browser, follow instructions and profit.
You can edit then `~/.config/platypus-lol/commands.conf` to your liking.

## Extension

To make extension, run `make wasm`, and then load it unpacked into chrome. Currently it's not in chrome store (work in progress).

## TODO

- [x] Split into separate crates: nfa parser, redirect query, command line tool, wasm module
- [x] Create mock extension (with faked suggestor)
- [x] Bundle WASM into extension
- [x] Plug in the parser (with hardcoded config or something like that).
- [x] Fix suggestion behavior -- return multiple suggestions for substitutions
- [x] Fix suggestion behavior -- return multipls suggestions in middle of substitution
- [x] Figure out how to handle state
- [x] Figure out how to handle configuration
- [-] ~Extension pages: conflict resolution~ -- canceled, no conflict resolution, just everything
- [x] Extension pages: list of all commands
- [x] Extension pages: configuration
- [x] Options validation
- [-] ~Make it possible to load parser from external configured url.~ -- canceled, this is not easy, because of chrome security model
- [x] Fix suggestions behavior (return longer items)
- [x] Consider removing regex node for compile time weight (or not bundle it into wasm).
- [ ] Removal of duplicit suggestions in API, not in client
- [x] Make it as suggestion api server.
- [x] Make list of commands page.
- [-] ~Make configuration page.~ Not necessary
- [x] Make hot reloading of config file.
- [x] Rename binary
- [x] Check equivalency with haskell and delete haskell
- [x] Limit size for returning amount of suggestions to some reasonable number (20)
- [x] Docker / systemd setup with cron-like update of config repo
- [x] When doing something wrong, substitute query for anything? -- parser do not allow it I think
- [x] Figure out why reloading of config does not work.
- [x] Default to google or other configurable search engine
- [x] Require space between suggestions, but not before / after if it's fixed word.
- [x] Test for 2 different substitutions of same type
- [x] Make default and extra config (multiple config files possible)
- [x] Add possible to have local files
- [x] keyboard shortcut for list page if possible to fill
- [ ] Check for html escapes and so on from links redirects
- [ ] Remove unwraps, handle errors properly everywhere
- [ ] Better validation errors and documentation
- [ ] Add more tests to all parser parts to document expectations / behavior of modules
- [ ] Add better parser error handling -- more useful errors
- [ ] Foldable list of command for same type of suggestion (substitutions)
- [ ] Go over code, remove unnecessary things / dependencies (some libraries are heavy)
- [ ] Remove browser extension
- [x] shortcuts [e]xtend all
- [x] shortcuts fi[r]st, defa[u]lt
- [ ] just fold by tokens
- [x] Work with mouse 
- [ ] back with mouse
- [ ] Help message for list
- [ ] Make it pretty
