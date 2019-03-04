#!/usr/bin/swipl -f -q

:- initialization main.

main :-
  current_prolog_flag(argv, Argv),
  format('Hello World, argv:~w\n', [Argv]),
  ['AlpBPM'],
  test_opt_incompleteness(fracture_treatment).
