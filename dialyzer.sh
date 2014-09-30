#dialyzer --build_plt --apps erts kernel stdlib
dialyzer --src src/**/*.erl -I include -I include/texas -I include/test
