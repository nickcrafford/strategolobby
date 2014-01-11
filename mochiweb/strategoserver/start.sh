#!/bin/sh
cd `dirname $0`
exec erl -smp enable -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s strategoserver start 9093 prod.pid -detached -sname prod@localhost
