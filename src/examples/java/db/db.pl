person(john, 20, melbourne).
person(maria, 31, city(sydney)).
person(maria, 11, city(perth)).
person(adam, 18, geelong).
person(michelle, 14, lorne).

t :-
    throw( 'this is an error message').


display( X) :-
    write( X).

