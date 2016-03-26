TARGET ?= pieth

upload:
	rsync -Ccruv ale $(TARGET):
	ssh $(TARGET) -t 'cd ale; tmux new "rm -rf _build/default/lib/erlang_ale/ebin/ && ../bin/rebar3 shell --apps erlang_ale"'
