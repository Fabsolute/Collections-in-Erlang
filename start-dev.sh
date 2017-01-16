#!/bin/sh
gmake
exec erl \
    -pa ebin deps/*/ebin \
    -sname collections \
    -s reloader
