ERL          ?= erl
EBIN_DIRS    := lib/mochiweb/ebin
APP          := imageherd

all: erl

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

dist:
	git archive --format=tar --prefix=fetcherl/ HEAD | \
            gzip > fetcherl.tar.gz