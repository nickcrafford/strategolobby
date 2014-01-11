all: ebin/
	(cd src;$(MAKE) all)
	(cd src/mnesia; erlc *.erl; mv *.beam ../../ebin/mnesia/)
	(cd src/mysql;  erlc *.erl; mv *.beam ../../ebin/mysql/)
	(cd src/mongo;  erlc *.erl; mv *.beam ../../ebin/mongo/)
	(cd deps/erlang-mysql-driver/src; erlc *.erl; cp *.beam ../../../ebin/; cp *.hrl ../../../ebin/)
	(cd deps/erlydtl/; make all; cd ebin/; cp *.beam ../../../ebin/; cp *.app ../../../ebin/)
	(cd ../ebin/; cp *.beam ../strategoserver/ebin/)

edoc:
	(cd src;$(MAKE) edoc)

test:
	(cd src;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)

clean_plt:
	(cd src;$(MAKE) clean_plt)

dialyzer:
	(cd src;$(MAKE) dialyzer)

ebin/:
	@mkdir -p ebin
