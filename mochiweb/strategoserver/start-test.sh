#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s strategoserver start 9092 test.pid -detached -sname test@localhost
