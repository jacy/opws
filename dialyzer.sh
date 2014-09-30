#dialyzer --build_plt --apps erts kernel stdlib
dialyzer --src src/**/*.erl --src test/*.erl -I include -I include/texas -I include/test
