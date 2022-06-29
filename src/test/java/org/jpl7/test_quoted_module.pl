:- module(moduleTest, [quoted_name/2]).

quoted_name(Atom, Quoted) :-
    format(atom(Quoted), '~q', [Atom]).
