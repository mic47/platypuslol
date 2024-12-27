ci: FORCE
	cargo fmt && cargo test && cargo clippy --all-targets -- -D clippy::all -D warnings

run: FORCE
	cargo run --bin tool -- --link-config config/commands.json --query "create google rawg"

run-suggest: FORCE
	cargo run --bin tool -- --link-config config/commands.json --query "cr"

run-server: FORCE
	 cargo run --bin platypus-lol -- --link-config config/commands.json

release: FORCE
	cargo build --release --bin platypus-lol


LOCAL_BIN=${HOME}/.local/bin
LOCAL_BINARY=${LOCAL_BIN}/platypus-lol
LOCAL_CONFIG=${HOME}/.config/platypus-lol
LOCAL_COMMANDS=${HOME}/.config/platypus-lol/commands.json
LOCAL_SYSTEMD=${HOME}/.config/systemd/user

install: FORCE release
	mkdir -p ${LOCAL_BIN}
	if [ -f ${LOCAL_BINARY} ] ; then mv ${LOCAL_BINARY} $$(mktemp) ; fi
	cp target/release/platypus-lol ${LOCAL_BINARY}
	mkdir -p ${LOCAL_CONFIG}
	if [ ! -f "${LOCAL_COMMANDS}" ] ; then cp config/commands.json ${LOCAL_COMMANDS} ; fi
	cat scripts/platypus-lol.service | envsubst > ${LOCAL_SYSTEMD}/platypus-lol.service
	systemctl --user daemon-reload
	systemctl --user enable platypus-lol
	systemctl --user restart platypus-lol
	systemctl --user status platypus-lol

status: FORCE
	systemctl --user status platypus-lol

start: FORCE
	systemctl --user start platypus-lol

restart: FORCE
	systemctl --user restart platypus-lol

logs: FORCE
	 journalctl --user -u platypus-lol.service

cf-dev: FORCE
	bash -c 'pushd cf-http-worker && npx wrangler dev ; popd'
         
cf-deploy: FORCE
	bash -c 'pushd cf-http-worker && npx wrangler deploy ; popd'

FORCE:
