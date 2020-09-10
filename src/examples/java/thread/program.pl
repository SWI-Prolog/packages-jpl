:- use_module(library(backcomp)).
:- use_module(library(debug)).

:- thread_local
    p/1.

welcome :-
    thread_self(Me),
    thread_at_exit(done),
    debug(start, 'I\'m Prolog thread ~w~n', [Me]),
    sleep(1000).

done :-
    thread_self(Me),
    format('Thread ~p done~n', [Me]).

:- format('Program loaded~n', []).
