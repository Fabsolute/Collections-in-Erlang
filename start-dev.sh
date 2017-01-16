#!/bin/sh
gmake
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname collections
