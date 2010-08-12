#!/bin/bash

EBIN="ebin lib/mochiweb/ebin"

erl -noshell -pa $EBIN -s fetcherl main $@ -s init stop
