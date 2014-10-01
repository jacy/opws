#dialyzer --build_plt --apps erts kernel stdlib
dialyzer -DTEST --src src/**/*.erl --src test/*.erl -I include -I include/texas -I include/test
