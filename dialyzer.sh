#dialyzer --build_plt --apps erts kernel stdlib
dialyzer --src src/**/*.erl
