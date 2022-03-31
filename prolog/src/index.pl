:- use_module('controllers/deckController.pl').
:- use_module('controllers/cardController.pl').
:- use_module('controllers/CLIController.pl').


:- working_directory(CWD, CWD),
	atom_concat(CWD, '/database', Path),
	absolute_file_name(Path, AbsPath),
	atom_concat(AbsPath, '/database.json', Out),
  nb_setval(dbPath, Out).

:- set_prolog_flag('encoding', 'utf8').

main():-
	gummyReminderLogo(),
	initialMenu(),
	mainMenu().