

wasm:
	wasm-pack build --target web wasm_lol 
	
ci:
	cargo fmt && cargo test && cargo clippy --all-targets -- -D clippy::all -D warnings

run:
	cargo run --bin tool -- --link-config commands.json --query "create google rawg"
suggest:
	cargo run --bin tool -- --link-config commands.json --query "cr"
