# platypuslol

Smart URL redirector, inspired by bunny1 (or facebooks bunnylol), but with more advanced query matching.
Writtern in Rust.
Works as local server, it's possible to deploy it as cloudflare worker (with fixed config).
Use at your own risk (for now).

## Instructions (local server)

Assuming you have rust installed, simply run

```bash
make install
```

It will install everything for you, and run the service using systemd on linux. See Makefile for more details.
If you are on mac, you are on your own, but please share how you did it, I am happy to include it here.

After that, visit [http://localhost:3047/install](http://localhost:3047/install) in your browser, follow instructions and profit.
You can edit then `~/.config/platypus-lol/commands.json` to your liking.
To get documentation on the language, run config command.

## Running it as cloudflare worker (experimental)

This is work in progress, for now, try to make following commands pass.

```bash
make cf-dev # development server
make cf-deploy # deploy to your cf dev account
```
