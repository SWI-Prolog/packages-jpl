:- module(jpl).


quoted_name(Atom, Quoted) :-
    write("Here is the ATOM in Prolog the quoted name: "), writeln(Atom),
    format(atom(Quoted), '~q', [Atom]),
    write("Here is in Prolog the QUOTED ATOM: "), writeln(Quoted).


test :- writeln("THIS IS A TEST FROM PROLOG").