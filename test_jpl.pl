/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2018, Paul Singleton
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_jpl,
          [ test_jpl/0,
            run_tests/0,
            run_tests/1
          ]).
% ensure we get the local copies

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(jpl_examples, 'examples/prolog')).
:- asserta(user:file_search_path(jar, '.')).
:- asserta(user:file_search_path(jar, foreign('src/main/java'))).
:- asserta(user:file_search_path(jar, foreign('.'))).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

% paths for Prolog embedded in Java under CTest
:- asserta(user:file_search_path(library, '../../../packages/jpl')).
:- asserta(user:file_search_path(library, '../../../packages/plunit')).

:- use_module(library(jpl)).
:- use_module(library(plunit)).

% Using cmake location
:- jpl:add_search_path('CLASSPATH', 'src/test/java/jpltest.jar').
% For before cmake
:- jpl:add_search_path('CLASSPATH', 'jpltest.jar').

test_jpl :-
    run_tests(
    [
      jpl
     ,identifier_chars
     ,java_id
     ,java_type_id
     ,messy_dollar_split
     ,jpl_classname_without_dollar
     ,jpl_classname_with_dollar
     ,jpl_entityname
     ,jpl_findclass_descriptor
     ,jpl_method_descriptor
     ,compare_old_and_new_entityname
     ,compare_old_and_new_findclass_descriptor
     ,compare_both_field_descriptors
     ,jpl_type_to_classname
     ,jpl_classname_to_type
     ,safe_type_to_classname
    ]).


% ---
% Switch these on for some output in the logfile (build/Testing/Temporary/LastTest.log)
% ---

% :- debug(identifier_chars).
% :- debug(java_id).
% :- debug(run_both).
% :- debug(dcg_mangle).

:- begin_tests(jpl).

test(
        exception_jref_1,
        [       true((
                        E = error(java_exception(JRef), 'java.lang.IllegalArgumentException'),
                        blob(JRef, jref)
                ))
        ]
) :-
    catch(jpl_new('java.util.Date', [never], _), E, true).

test(
        array_to_from_terms_1,
        [       true(
                        Terms1 == Terms2
                )
        ]
) :-
    Terms1 = [x,[1,a,7,[y,z]],k,[]],
    jpl_terms_to_array(Terms1, JRef),
    jpl_array_to_terms(JRef, Terms2).

test(
        ancestor_types_1,
        [       true(
                        Ts == [class([org,jpl7],['Term']),class([java,lang],['Object'])]
                )
        ]
) :-
    jpl:jpl_type_to_ancestor_types(class([org,jpl7],['Atom']), Ts).

test(
        call_array_equals_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A1),
                        jpl_new(array(byte), [4,5,6], A2)
                ))
        ]
) :-
    jpl_call(A1, equals, [A2], @(false)).

test(
        call_array_equals_2,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A1)
                ))
        ]
) :-
    jpl_call(A1, equals, [A1], @(true)).

test(
        call_array_hashcode_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A)
                )),
                true((
                        integer(H)
                ))
        ]
) :-
    jpl_call(A, hashCode, [], H).

test(
        call_array_hashcode_2,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A1),
                        jpl_new(array(byte), [4,5,6], A2)
                )),
                true((
                        H1 \== H2
                ))
        ]
) :-
    jpl_call(A1, hashCode, [], H1),
    jpl_call(A2, hashCode, [], H2).

test(
        call_array_to_string_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A)
                )),
                true((
                        atom_codes(S, [0'[, 0'B | _])
                ))
        ]
) :-
    jpl_call(A, toString, [], S).

test(
        call_instance_param_cyclic_term_1,
        [       setup((
                        T = f(T),
                        jpl_new('org.jpl7.Test', [], Test)
                )),
                throws(
                        error(type_error(acyclic,T),context(jpl_call/4,_))
                )
        ]
) :-
    jpl_call(Test, methodInstanceTerm, [{T}], @(true)).

testX(
        call_instance_param_cyclic_term_2,
        [       setup((
                        T = f(T),
                        jpl_new('org.jpl7.Test', [], Test)
                )),
                throws(
                        error(type_error(acyclic,_),context(jpl_call/4,_))
                )
        ]
) :-
    jpl_call(Test, methodInstanceTerm, [{T}], @(true)).

test(
        call_method_static_array_1,
        [       setup((
                        jpl_new(array(int), [3,4,5], IntArray)
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticArray, [IntArray], 'int[]').

test(
        call_method_static_array_2,
        [       setup((
                        jpl_new(array(byte), [3,4,5], ByteArray)
                )),
                throws(
                        error(
                                type_error(method_params,[ByteArray]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticArray, [ByteArray], _).

test(
        call_static_param_cyclic_term_1,
        [       setup((
                        T = f(T)
                )),
                throws(
                        error(type_error(acyclic,T),context(jpl_call/4,_))
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticTerm, [{T}], @(true)).

test(
        call_class_get_name_1,
        [       setup((
                        ClassName = 'java.lang.Integer',
                        jpl_classname_to_class(ClassName, ClassObject)
                )),
                true((
                        ClassName == ClassName2
                ))
        ]
) :-
    jpl_call(ClassObject, getName, [], ClassName2).

test(
        call_get_array_bad_field_name_1,
        [       setup((
                        jpl_new(array(byte), 5, A),
                        FieldName = colour
                )),
                throws(
                        error(domain_error(array_field_name,FieldName),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, FieldName, _).

test(
        call_get_array_bad_fspec_1,
        [       setup((
                        jpl_new(array(byte), 5, A),
                        Fspec = poo(77)
                )),
                throws(
                        error(type_error(array_lookup_spec,Fspec),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, Fspec, _).

test(
        call_get_array_bad_index_range_1,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(domain_error(array_index_range,(-1)-2),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, (-1)-2, _).

test(
        call_get_array_bad_index_range_2,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(domain_error(array_index_range,10-12),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, 10-12, _).

test(
        call_get_array_bad_index_range_3,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(domain_error(array_index_range,3-33),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, 3-33, _).

test(
        call_get_array_bad_index_range_4,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(type_error(array_index_range,this-that),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, this-that, _).

test(
        get_array_element_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6,7,8], A)
                )),
                true((
                        7 == V
                ))
        ]
) :-
    jpl_get(A, 3, V). % should bind V = 7 i.e. a[3] i.e. the fourth array element counting from zero

test(
        get_array_elements_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6,7,8], A)
                )),
                true((
                        [5,6] == V
                ))
        ]
) :-
    jpl_get(A, 1-2, V). % should bind V = [5,6] i.e. a[1-2] i.e. the 2nd to 3rd array elements counting from zero

test(
        get_array_length_1,
        [       setup((
                        Len1 is 5,
                        jpl_new(array(byte), Len1, A)
                )),
                true((
                        Len1 == Len2
                ))
        ]
) :-
    jpl_get(A, length, Len2). % should bind Len2 to the (integer) value of Len1

test(
        get_array_negative_index_1,
        [       setup((
                        BadIndex is -1,
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(domain_error(array_index,BadIndex), context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, BadIndex, _).

test(
        get_array_unbound_fspec_1,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(instantiation_error,context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, _, _).

test(
        get_field_static_boolean_1,
        [       true((
                        V == @(false)
                ))
        ]
) :-
    jpl_get('org.jpl7.Test', fieldStaticBoolean1, V).

test(
        get_field_static_boolean_2,
        [       true((
                        V == @(true)
                ))
        ]
) :-
    jpl_get('org.jpl7.Test', fieldStaticBoolean2, V).

test(
        get_field_static_char_1,
        [       true((
                        V == 0
                ))
        ]
) :-
    jpl_get('org.jpl7.Test', fieldStaticChar1, V).

test(
        get_field_static_char_2,
        [       true((
                        V == 65535
                ))
        ]
) :-
    jpl_get('org.jpl7.Test', fieldStaticChar2, V).

test(
        get_field_instance_byte_2,
        [       setup((
                        jpl_new('org.jpl7.Test', [], Test)
                )),
                true((
                        V == -1
                ))
        ]
) :-
    jpl_get(Test, fieldInstanceByte2, V).

test(
        list_to_array_1,
        [       true((
                        Type == array(byte)
                ))
        ]
) :-
    jpl_list_to_array([1,2,3], A),
    jpl_object_to_type(A, Type).

test(
        method_static_byte_1,
        [       throws(
                        error(
                                type_error(method_params,[-129]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoByte, [-129], _).

test(
        method_static_echo_boolean_1,
        [       setup((
                        jpl_false(V1)
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoBoolean, [V1], V2).

test(
        method_static_echo_boolean_2,
        [       setup((
                        jpl_true(V1)
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoBoolean, [V1], V2).

test(
        method_static_echo_char_1,
        [       setup((
                        V1 = 0
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], V2).

test(
        method_static_echo_char_2,
        [       setup((
                        V1 = 65535
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], V2).

test(
        method_static_char_3,
        [       setup((
                        V1 = -1
                )),
                throws(
                        error(
                                type_error(method_params,[V1]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], _).

test(
        method_static_char_4,
        [       setup((
                        V1 = 1.0
                )),
                throws(
                        error(
                                type_error(method_params,[V1]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], _).

test(
        method_static_char_5,
        [       setup((
                        V1 = a
                )),
                throws(
                        error(
                                type_error(method_params,[V1]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], _).

test(
        method_static_echo_double_1,
        [       setup((
                        V1 = 1.5
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoDouble, [V1], V2).

test(
        method_static_echo_double_2,
        [       setup((
                        V1 = 2
                )),
                true((
                        V2 =:= float(V1)
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoDouble, [V1], V2).

test(
        method_static_echo_double_3,
        [       setup((
                        (   current_prolog_flag(bounded, true)
                    ->  current_prolog_flag(max_integer, V1)
                    ;   V1 is 2**63-1
                    ),
                        V2b is float(V1)
                )),
                true((
                        V2 == V2b
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoDouble, [V1], V2).

test(
        method_static_echo_float_1,
        [       setup((
                        V1 = 1.5
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoFloat, [V1], V2).

test(
        method_static_echo_float_2,
        [       setup((
                        V1 is 2,
                        V2b is float(V1)
                )),
                true((
                        V2 == V2b
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoFloat, [V1], V2).

test(
        method_static_echo_float_3,
        [       setup((
                        (       current_prolog_flag(bounded, true)
                    ->  current_prolog_flag(max_integer, V1)
                    ;   V1 is 2**63-1 % was 2**99
                    ),
                        V2b is float(V1)
                )),
                true((
                        V2 == V2b
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoFloat, [V1], V2).

test(
        method_static_echo_float_4,
        [       blocked('we do not yet widen unbounded integers to floats or doubles'),
                setup((
                        (   current_prolog_flag(bounded, true)
                    ->  current_prolog_flag(max_integer, V1)
                    ;   V1 is 2**99             % an unbounded integer
                    ),
                        V2b is float(V1)
                )),
                true((
                        V2 == V2b
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoFloat, [V1], V2).

test(
        new_abstract_class_1,
        [       setup((
                        Classname = 'java.util.Dictionary'
                )),
                throws(
                        error(
                                type_error(concrete_class,Classname),
                                context(jpl_new/3,_)
                        )
                )
        ]
) :-
    jpl_new(Classname, [], _).

test(
        new_array_boolean_from_val_1,
        [       setup((
                        jpl_false(V)
                )),
                true((
                        V == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', newArrayBooleanFromValue, [V], A),
    jpl_get(A, 0, V2).

test(
        new_array_double_from_val_1,
        [       setup((
                        V is 1.5
                )),
                true((
                        V == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', newArrayDoubleFromValue, [V], A),
    jpl_get(A, 0, V2).

test(
        new_array_float_from_val_1,
        [       setup((
                        V is 1.5
                )),
                true((
                        V == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', newArrayFloatFromValue, [V], A),
    jpl_get(A, 0, V2).

test(
        new_interface_1,
        [       setup((
                        Classname = 'java.util.Enumeration'
                )),
                throws(
                        error(
                                type_error(concrete_class,Classname),
                                context(jpl_new/3,_)
                        )
                )
        ]
) :-
    jpl_new(Classname, [], _).

test(
        new_param_cyclic_term_1,
        [       setup((
                        T = f(T)
                )),
                throws(
                        error(
                                type_error(acyclic,T),
                                context(jpl_new/3,_)
                        )
                )
        ]
) :-
    jpl_new('org.jpl7.Test', [{T}], _).

test(
        prolog_calls_java_calls_prolog_1,
        [       true((
                        V == @(true)
                ))
        ]
) :-
    jpl_new('org.jpl7.Query', ['4 is 2+2'], Q),
    jpl_call(Q, hasSolution, [], V).

test(
        set_array_element_cyclic_term_1,
        [       setup((
                        T = f(T),
                        jpl_new(array(class([org,jpl7],['Test'])), 5, A)
                )),
                throws(
                        error(
                                type_error(acyclic,T),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 0, {T}).

test(
        set_array_elements_bad_type_1,
        [       setup((
                        jpl_new(array(byte), 3, A)
                )),
                throws(
                        error(
                                type_error(array(byte),[128]),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 0, 128).

test(
        set_array_length_1,
        [       setup((
                        jpl_new(array(byte), 6, A)
                )),
                throws(
                        error(
                                permission_error(modify,final_field,length),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, length, 13).

test(
        set_field_bad_field_spec_1,
        [       setup((
                        BadFieldName = 3.7
                )),
                throws(
                        error(
                                type_error(field_name,BadFieldName),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', BadFieldName, a).

test(
        set_field_instance_cyclic_term_1,
        [       setup((
                        T = f(T),
                        jpl_new('org.jpl7.Test', [], Test)
                )),
                throws(
                        error(
                                type_error(acyclic,T),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(Test, instanceTerm, {T}).

test(
        set_field_long_array_1,
        [       setup((
                        jpl_new(array(long), [1,2,3], LongArray)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticLongArray, LongArray).

test(
        set_field_long_array_2,
        [       setup((
                        jpl_new(array(int), [1,2,3], IntArray)
                )),
                throws(
                        error(
                                type_error('[J',IntArray),      % NB '[J' is *not* how the type was specified in the failing goal
                                context(
                                        jpl_set/3,
                                        'the value is not assignable to the named field of the class'
                                )
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticLongArray, IntArray).

test(
        set_field_object_array_1,
        [       setup((
                        jpl_new('java.util.Date', [], Date),
                        jpl_new(array(class([java,lang],['Object'])), [Date,Date], ObjArray)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticObjectArray, ObjArray).

test(
        set_field_static_bad_type_1,
        [       setup((
                        BadVal = 27
                )),
                throws(
                        error(
                                type_error(boolean,BadVal),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticBoolean, BadVal).

test(
        set_field_static_boolean_1,
        [       setup((
                        jpl_true(V)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticBoolean, V).

test(
        set_field_static_boolean_2,
        [       setup((
                        jpl_false(V)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticBoolean, V).

test(
        set_field_static_boolean_bad_1,
        [       setup((
                        BadVal = foo(bar)
                )),
                throws(
                        error(
                                type_error(field_value,BadVal),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticBoolean, BadVal).

test(
        set_field_static_cyclic_term_1,
        [       setup((
                        T = f(T)
                )),
                throws(
                        error(
                                type_error(acyclic,T),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', staticTerm, {T}).

test(
        set_field_static_final_int_1,
        [       setup((
                        FieldName = fieldStaticFinalInt,
                        Value = 6
                )),
                throws(
                        error(
                                permission_error(modify,final_field,FieldName),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', FieldName, Value).

test(
        set_field_static_shadow_1,
        [       blocked('we do not yet resolve same-named shadowed fields')
        ]
) :-
    jpl_set('org.jpl7.ShadowB', fieldStaticInt, 3).

test(
        set_field_static_term_1,
        [       setup((
                        T1 = foo(bar,33),
                        T2 = bar(77,bing)
                )),
                true((
                        T1 == T1a,
                        T2 == T2a
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticTerm, {T1}),
    jpl_get('org.jpl7.Test', fieldStaticTerm, {T1a}),
    jpl_set('org.jpl7.Test', fieldStaticTerm, {T2}),
    jpl_get('org.jpl7.Test', fieldStaticTerm, {T2a}).

test(
        set_field_static_term_2,
        [       setup((
                        T1 = foo(bar,33),
                        T2 = bar(77,bing)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticTerm, {T1}),
    jpl_get('org.jpl7.Test', fieldStaticTerm, {T1}),
    jpl_set('org.jpl7.Test', fieldStaticTerm, {T2}),
    jpl_get('org.jpl7.Test', fieldStaticTerm, {T2}).

test(
        set_get_array_element_boolean_1,
        [       setup((
                        jpl_new(array(boolean), 3, A),
                        V = @(false)
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_boolean_2,
        [       setup((
                        jpl_new(array(boolean), 3, A),
                        V = @(true)
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_boolean_3,
        [       setup((
                        jpl_new(array(boolean), 3, A),
                        V = bogus
                )),
                throws(
                        error(
                                type_error(array(boolean),[V]),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 2, V).

test(
        set_get_array_element_byte_1,
        [       setup((
                        jpl_new(array(byte), 3, A),
                        V = 33
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_byte_2,
        [       setup((
                        jpl_new(array(byte), 3, A),
                        V = 128
                )),
                throws(
                        error(
                                type_error(array(byte),[V]),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 2, V).

test(
        set_get_array_element_char_1,
        [       setup((
                        jpl_new(array(char), 3, A),
                        V = 65535
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_double_1,
        [       setup((
                        jpl_new(array(double), 3, A),
                        V = 2.5
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_float_1,
        [       setup((
                        jpl_new(array(float), 3, A),
                        V = 7.5
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_float_2,
        [       setup((
                        jpl_new(array(float), 3, A),
                        V is 2,
                        VrX is float(V)
                )),
                true((
                        VrX == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_float_3,
        [       setup((
                        jpl_new(array(float), 3, A),
                        (       current_prolog_flag(bounded, true)
                        ->      current_prolog_flag(max_integer, Imax)
                        ;       Imax is 2**63-1
                        ),
                        VrX is float(Imax)
                )),
                true((
                        VrX == Vr
                ))
        ]
) :-
    jpl_set(A, 2, Imax),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_long_1,
        [       setup((
                        jpl_new(array(long), 3, A),
                        (       current_prolog_flag(bounded, true)
                        ->      current_prolog_flag(max_integer, V)
                        ;       V is 2**63-1
                        )
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_long_2,
        [       setup((
                        jpl_new(array(long), 3, A),
                        (       current_prolog_flag(bounded, true)
                        ->      current_prolog_flag(max_integer, V)
                        ;       V is 2**63
                        )
                )),
                throws(
                        error(
                                type_error(array(long),[V]),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 2, V).

test(
        set_get_array_elements_boolean_1,
        [       setup((
                        jpl_new(array(boolean), 3, A),
                        Vf = @(false),
                        Vt = @(true)
                )),
                true((
                        Vf+Vt+Vf == Vr0+Vr1+Vr2
                ))
        ]
) :-
    jpl_set(A, 0, Vf),
    jpl_set(A, 1, Vt),
    jpl_set(A, 2, Vf),
    jpl_get(A, 0, Vr0),
    jpl_get(A, 1, Vr1),
    jpl_get(A, 2, Vr2).

test(
        set_get_field_static_long_1,
        [       setup((
                        (   current_prolog_flag(bounded, true)
                    ->  current_prolog_flag(max_integer, V)
                    ;   V is 2**63-1
                    )
                )),
                true((
                        V == V2
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticLong, V),
    jpl_get('org.jpl7.Test', fieldStaticLong, V2).

test(
        set_non_accessible_field_1,
        [       throws(
                        error(
                                existence_error(field,gagaga),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', gagaga, 4).

test(
        terms_to_array_1,
        []
) :-
    jpl_terms_to_array([foo(bar)], A),
    jpl_object_to_type(A, array(class([org,jpl7],['Term']))),
    jpl_get(A, length, 1),
    jpl_get(A, 0, T),
    jpl_call(T, toString, [], 'foo(bar)').

test(
        throw_java_exception_1,
        [       blocked('part of the error term is nondeterministic: we need to match with _'),
                throws(
                        error(
                                java_exception(@(_)),
                                'java.lang.NumberFormatException'
                        )
                )
        ]
) :-
    jpl_call('java.lang.Integer', decode, [q], _).

test(
        versions_1,
        [       true((
                        Vpl == Vc,
                        Vc == Vjava
                ))
        ]
) :-
    jpl_pl_lib_version(Vpl),
    jpl_c_lib_version(Vc),
    jpl_call('org.jpl7.JPL', version_string, [], Vjava).

%       JW: Mutual recursion check.  Moved from jpl.pl to here.  As the
%       callback is in module user, we define it there.

user:jpl_test_fac(N, F) :-
    (       N == 1
    ->      F = 1
    ;       N > 1
    ->      N2 is N-1,
            jpl_call('org.jpl7.Test', fac, [N2], F2),  % call its Java counterpart, which does vice versa
            F is N*F2
    ;       F = 0
    ).

test(fac10,
        [       true(N==3628800)
        ]
) :-
    user:jpl_test_fac(10, N).

test(threads1,
        [       true((
                        thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
                        thread_join(ThreadId, true)
                ))
        ]
) :-
    jpl_call('java.lang.System', currentTimeMillis, [], _).

test(threads2,
        [       true(X==true)
        ]
) :-
    jpl_call('java.lang.System', currentTimeMillis, [], _),
    thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
    thread_join(ThreadId, X).

test(threads3,
        [       true((
                        length(Ss, N),
                        sort(Ss, [true])
                ))
        ]
) :-
    N is 100,  % was 1000 (ok in V6); in V7 traditional, fails with 200
    jpl_call('java.lang.System', currentTimeMillis, [], _),
    findall(
            Status,
            (       between(1, N, _),
                    thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
                    thread_join(ThreadId, Status)
            ),
            Ss
    ).

test(jref1,
        [       true((
                        Term1 \== Term2
                ))
        ]
) :-
    length(Term1, 5),
    jpl:jni_term_to_jref(Term1, JRef),
    jpl:jni_jref_to_term(JRef, Term2).

test(jref2,
        [       true((
                        Term1 =@= Term2
                ))
        ]
) :-
    length(Term1, 5),
    jpl:jni_term_to_jref(Term1, JRef),
    jpl:jni_jref_to_term(JRef, Term2).

:- end_tests(jpl).



         /*******************************
         * Testing recognizing/parsing  *
         *******************************/

% ===========================================================================
% Some helper functions
% ===========================================================================

andify_rightwards(true,true,true).
andify_rightwards(true,false,false).
andify_rightwards(false,true,false).
andify_rightwards(false,false,false).

all_true(List) :-
   foldl([E,FromLeft,ToRight]>>once(andify_rightwards(FromLeft,E,ToRight)),List,true,Out),
   Out == true.

% ---
% An implementation of ->/2. Pass three goals.
% (How do I transform this into a proper metacall?)
% ---

if_then_else(Condition,Then,Else) :-
   call(Condition) -> call(Then) ; call(Else).

% ---
% Reification of truth value
% (How do I transform this into a proper metacall?)
% ---

reify(Goal,Truth) :-
   if_then_else(call(Goal),(Truth=true),(Truth=false)).

% ---
% An implementation of ->/2 with an "else" that's true. Pass two goals.
% (How do I transform this into a proper metacall?)
% ---

if_then(Condition,Then) :-
   call(Condition) -> call(Then) ; true.

% ---
% Add/Remove the module from an atomic goal "M:G" (a compound), giving separate "M" and "G",
% if the goal is qualified. If not, nothing happens, "M" remains a freshvar.
% Runs in "both directions"
% ---

module_name_on_off(:(ModuleName,UnqualifiedGoal),ModuleName,UnqualifiedGoal) :- atomic(ModuleName),!,nonvar(UnqualifiedGoal).
module_name_on_off(Goal,UnusedModuleName,Goal) :- var(UnusedModuleName),nonvar(Goal).

% ---
% Dynamically call a DCG, "DcgGoal", parsing "In" into (a structure) "Out",
% with "Rest" remaining. We are parsing some Java class names or descriptors
% and "Mode" indicates what we really expect: "slashy", "dotty" as input.
% Note that "DcgGoal" is augmented with the two arguments "Out"
% (generally a freshvar that captures the result of recognition) and "Mode"
% (one of slashy or dotty).
% ---

dcg_parse_moded(In,Rest,DcgGoal,Out,Mode) :-
   assertion(nonvar(In)),
   assertion(memberchk(Mode,[slashy,dotty])),
   module_name_on_off(DcgGoal,ModuleName,UnqualifiedGoal),
   compound_name_arguments(
      UnqualifiedGoalExtended,
      UnqualifiedGoal,
      [Out,Mode]),
   module_name_on_off(DcgGoalNew,ModuleName,UnqualifiedGoalExtended),
   atom_codes(In,InCodes),
   phrase(DcgGoalNew,InCodes,RestCodes),
   atom_codes(Rest,RestCodes).

% ---
% Dynamically call a DCG, "DcgGoal", parsing "In" into (a structure) "Out",
% with "Rest" remaining (or the converse). We are parsing some Java class names
%  or descriptors. There is no mode, as "DcgGoal" is assumed to not be mode-dependent.
% ---

dcg_mangle(JavaSide,Rest,DcgGoal,JplSide) :-
   assertion(nonvar(JavaSide);nonvar(JplSide)),
   assertion(nonvar(DcgGoal)),
   % extend DcgGoal with the "JplSide" argument
   module_name_on_off(DcgGoal,ModuleName,UnqualifiedGoal),
   compound_name_arguments(
      UnqualifiedGoalExtended,
      UnqualifiedGoal,
      [JplSide]),
   module_name_on_off(DcgGoalNew,ModuleName,UnqualifiedGoalExtended),
   % Branch on whether it's Java->Jpl or Jpl->Java
   (nonvar(JavaSide)
       ->
       (dcg_parse_from_java(JavaSide,Rest,DcgGoalNew), debug(dcg_mangle,"~q => ~q",[JavaSide,JplSide]))
       ;
       (dcg_generate_from_jpl(JavaSide,DcgGoalNew), debug(dcg_mangle,"~q => ~q",[JplSide,JavaSide]))).

dcg_parse_from_java(JavaSide,Rest,DcgGoal) :- % JplSide has been buried in DcgGoal
   atom_codes(JavaSide,JavaSideCodes),
   phrase(DcgGoal,JavaSideCodes,RestCodes),
   atom_codes(Rest,RestCodes).

dcg_generate_from_jpl(JavaSide,DcgGoal) :- % JplSide has been buried in DcgGoal
   phrase(DcgGoal,JavaSideCodes),
   atom_codes(JavaSide,JavaSideCodes).

% ===========================================================================
% Calling jpl_field_descriptor//2 in dotty or slashy mode
% ===========================================================================

jpl_field_descriptor_dotty(T)  --> jpl:jpl_field_descriptor(T,dotty).
jpl_field_descriptor_slashy(T) --> jpl:jpl_field_descriptor(T,slashy).

% ===========================================================================
% Code used to compare the results of the old calls and the new calls
% ===========================================================================

% ---
% Construct the returnable term
% ---

outcome(true ,X,success(X)).
outcome(false,_,failure).

% ---
% Run the old call and the new call with input "JavaSide".
% The NewDcg and OldDcg are names of 1-arg DCGs.a
% ReifNew : output from new 1-arg DCG: success(JplType) on success, otherwise the atom 'failure'
% ReifOld : output from old 1-arg DCG: success(JplType) on success, otherwise the atom 'failure'
% ---

run_both(JavaSide,NewDcg,OldDcg,ReifNew,ReifOld) :-
   reify(
      dcg_mangle(JavaSide,'',NewDcg,JplSideNew),
      SuccessNew),
   reify(
      dcg_mangle(JavaSide,'',OldDcg,JplSideOld),
      SuccessOld),
   outcome(SuccessNew,JplSideNew,ReifNew),
   outcome(SuccessOld,JplSideOld,ReifOld),
   if_then_else(
      call(SuccessNew),
      debug(run_both,"~q : New   : ~q",[JavaSide,JplSideNew]),
      debug(run_both,"~q : New failed",[JavaSide])),
   if_then_else(
      call(SuccessOld),
      debug(run_both,"~q : Old   : ~q",[JavaSide,JplSideOld]),
      debug(run_both,"~q : Old failed",[JavaSide])),
   call(once(SuccessNew;SuccessOld)). % at least one must have succeeded!

% ===========================================================================
% Testing characters of Java identifiers
% ===========================================================================

% ---
% Helper
% Create a list of true/false atoms, one for each position of the input list
% of character codes, as the results of applying the predicates under test
% ---

maplist_java_id_start_char(ListIn,ListOut) :-
   maplist([C,T]>>
      reify(jpl:jpl_java_id_start_char(C),T),
      ListIn,ListOut).

maplist_java_id_part_char(ListIn,ListOut) :-
   maplist([C,T]>>
      reify(jpl:jpl_java_id_part_char(C),T),
      ListIn,ListOut).

maplist_java_id_disallowed_char(ListIn,ListOut) :-
   maplist([C,T]>>
      reify(
         ((\+ jpl:jpl_java_id_part_char(C)),
          (\+ jpl:jpl_java_id_start_char(C))),T),
      ListIn,ListOut).

maplist_java_id_part_char_but_not_start_char(ListIn,ListOut) :-
   maplist([C,T]>>
      reify(
         ((   jpl:jpl_java_id_part_char(C)),
          (\+ jpl:jpl_java_id_start_char(C))),T),
      ListIn,ListOut).


:- begin_tests(identifier_chars).

test("characters allowed at start of an identifier") :-
   maplist_java_id_start_char(`abcdefghijklöüä`,R),
   debug(identifier_chars,"Result for 'characters allowed at start of an identifier': ~q",[R]),
   all_true(R).

test("more characters allowed at start of an identifier") :-
   maplist_java_id_start_char(`$\u3047_`,R), % \u3047 Hiragana letter small e
   debug(identifier_chars,"Result for 'more characters allowed at the start of an identifier': ~q",[R]),
   all_true(R).

test("characters disallowed in identifiers") :-
   maplist_java_id_disallowed_char(`-.`,R),
   debug(identifier_chars,"Result for 'characters disallowed in identifiers': ~q",[R]),
   all_true(R).

test("characters allowed as part but not as start of identifiers") :-
   maplist_java_id_part_char_but_not_start_char(`0123456789`,R),
   debug(identifier_chars,"Result for 'characters allowed as part but not as start of identifiers': ~q",[R]),
   all_true(R).

:- end_tests(identifier_chars).

% ===========================================================================
% Testing Java identifiers via "jpl:jpl_java_id//1"
% ===========================================================================

:- begin_tests(java_id).

the_dcg(jpl:jpl_java_id).

test("recognize Java identifier (unconstrained Out), no rest", true([Out,Rest] == [my_identifier,''])) :-
   the_dcg(DCG),
   dcg_mangle('my_identifier',Rest,DCG,Out),
   debug(java_id,"Recognized: ~q with rest: ~q",[Out,Rest]).

test("recognize Java identifier (unconstrained Out), with rest", true([Out,Rest] == [my_identifier,'.dodododo'])) :-
   the_dcg(DCG),
   dcg_mangle('my_identifier.dodododo',Rest,DCG,Out),
   debug(java_id,"Recognized: ~q with rest: ~q",[Out,Rest]).

test("recognize Java identifier of length 1, no rest", true([Out,Rest] == [m,''])) :-
   the_dcg(DCG),
   dcg_mangle('m',Rest,DCG,Out).

test("recognize Java identifier (Out already set to result), no rest", true(Rest == '')) :-
   the_dcg(DCG),
   dcg_mangle('my_identifier',Rest,DCG,'my_identifier').

test("recognize Java identifier (Out already set to result), with rest", true(Rest == '.dodododo')) :-
   the_dcg(DCG),
   dcg_mangle('my_identifier.dodododo',Rest,DCG,'my_identifier').

test("starts with dash: not a Java identifier", fail) :-
   the_dcg(DCG),
   dcg_mangle('-my',_,DCG,_).

test("contains dash and thus is broken up", true([Out,Rest] == ['my','-my'])) :-
   the_dcg(DCG),
   dcg_mangle('my-my',Rest,DCG,Out).

test("empty atom is not a Java identifier", fail) :-
   the_dcg(DCG),
   dcg_mangle('',_,DCG,_).

test("valid identifier with differing Out", fail) :-
   the_dcg(DCG),
   dcg_mangle('my',_,DCG,'notmy').

:- end_tests(java_id).

% ===========================================================================
% Testing Java type identifiers via "jpl_java_type_id//1"
% This is practically the same as testing "jpl_java_id";
% here only the keywords "var" and "yield" are additionally disallowed.
% ===========================================================================

:- begin_tests(java_type_id).

the_dcg(jpl:jpl_java_type_id).

test("recognize Java type identifier",true([Out,Rest] == [my_identifier,''])) :-
   the_dcg(DCG),
   dcg_mangle('my_identifier',Rest,DCG,Out).

test("reject bad Java type identifier 'var'",fail) :-
   the_dcg(DCG),
   dcg_mangle('var',_,DCG,_).

test("java type identifier DOES NOT stop at '$'",true([Out,Rest] == ['foo$bar',''])) :-
   the_dcg(DCG),
   dcg_mangle('foo$bar',Rest,DCG,Out).

:- end_tests(java_type_id).

% ===========================================================================
% Testing the "messy dollar split" which is used to split Java classnames
% but is actually of dubious value
% ===========================================================================

:- begin_tests(messy_dollar_split).

test(1,true(Runs == [alfa])) :-
   jpl:messy_dollar_split(alfa,Runs).

test(2,true(Runs == [a])) :-
   jpl:messy_dollar_split(a,Runs).

test(3,true(Runs == ['$'])) :-
   jpl:messy_dollar_split('$',Runs).

test(4,true(Runs == ['alfa$'])) :-
   jpl:messy_dollar_split('alfa$',Runs).

test(5,true(Runs == [alfa,bravo])) :-
   jpl:messy_dollar_split('alfa$bravo',Runs).

test(6,true(Runs == ['$alfa'])) :-
   jpl:messy_dollar_split('$alfa',Runs).

test(7,true(Runs == ['alfa','$bravo'])) :-
   jpl:messy_dollar_split('alfa$$bravo',Runs).

test(8,true(Runs == ['$alfa','bravo','charlie$'])) :-
   jpl:messy_dollar_split('$alfa$bravo$charlie$',Runs).

test(9,true(Runs == ['$$alfa','$bravo','$$charlie','$$$'])) :-
   jpl:messy_dollar_split('$$alfa$$bravo$$$charlie$$$$',Runs).

:- end_tests(messy_dollar_split).

% ===========================================================================
% Testing recognition of the "binary classname", i.e. the classname
% as it appears in binaries (in its 'dotty' form)
% This calls the actual DCG parsing the binary classname, not the predicate
% eventually calling it.
% ===========================================================================

:- begin_tests(jpl_classname_without_dollar).

the_dcg(jpl:jpl_classname).

test("simple classname",true(Out == class([],[foo]))) :-
   the_dcg(DCG),
   dcg_parse_moded('foo','',DCG,Out,dotty).

test("qualified classname",true(Out == class([alfa,bravo,charlie],[foo]))) :-
   the_dcg(DCG),
   dcg_parse_moded('alfa.bravo.charlie.foo','',DCG,Out,dotty).

:- end_tests(jpl_classname_without_dollar).

% ===========================================================================
% Testing recognition of the "binary classname" with "$" inside.
% Note that "splitting at a dollar is ill-defined and should eventually disappear.
% ===========================================================================

:- begin_tests(jpl_classname_with_dollar).

the_dcg(jpl:jpl_classname).

test("qualified inner member type",true(Out == class([alfa,bravo,charlie],[foo,bar]))) :-
   the_dcg(DCG),
   dcg_parse_moded('alfa.bravo.charlie.foo$bar','',DCG,Out,dotty).

test("qualified inner anonymous type",true(Out == class([alfa,bravo,charlie],[foo,'01234']))) :-
   the_dcg(DCG),
   dcg_parse_moded('alfa.bravo.charlie.foo$01234','',DCG,Out,dotty).

test("qualified inner local class",true(Out == class([alfa,bravo,charlie],[foo,'01234bar']))) :-
   the_dcg(DCG),
   dcg_parse_moded('alfa.bravo.charlie.foo$01234bar','',DCG,Out,dotty).

test("qualified inner member type, deep",true(Out == class([alfa,bravo,charlie],[foo,bar,baz,quux]))) :-
   the_dcg(DCG),
   dcg_parse_moded('alfa.bravo.charlie.foo$bar$baz$quux','',DCG,Out,dotty).

:- end_tests(jpl_classname_with_dollar).

% ===========================================================================
% Testing Java entityname <-> JPL type (success)
% Use assertion magic: on assertion failure, the unit test does not fail
% immediately.
% ===========================================================================

:- begin_tests(jpl_entityname).

the_dcg(jpl:jpl_entityname).

the_tests([
   q( int                       , int ),
   q( integer                   , class([],[integer]) ),
   q( void                      , void ),
   q( char                      , char ),
   q( double                    , double ),
   q( '[D'                      , array(double) ),
   q( '[[I'                     , array(array(int)) ),
   q( 'java.lang.String'        , class([java,lang],['String'])),
   q( '[Ljava.lang.String;'     , array(class([java,lang],['String']))),
   q( '[[Ljava.lang.String;'    , array(array(class([java, lang], ['String']))) ),
   q( '[[[Ljava.util.Calendar;' , array(array(array(class([java,util],['Calendar'])))))
]).

test("Java entity name to JPL type") :-
   the_tests(L),
   the_dcg(DCG),
   maplist({DCG}/[q(EN,T)]>>assertion((dcg_mangle(EN,'',DCG,Out),Out == T)),L).

test("JPL type to Java entity name") :-
   the_tests(L),
   the_dcg(DCG),
   maplist({DCG}/[q(EN,T)]>>assertion((dcg_mangle(Out,'',DCG,T),Out == EN)),L).

test("array of void",fail) :-
   the_dcg(DCG),
   dcg_mangle('[[V','',DCG,_).

test("yield",fail) :-
   the_dcg(DCG),
   dcg_mangle('yield','',DCG,_).

test("var",fail) :-
   the_dcg(DCG),
   dcg_mangle('var','',DCG,_).

test("bad id with dash", fail) :-
   the_dcg(DCG),
   dcg_mangle(_,'',DCG,array(array(array(class([foo,bared],['-hello']))))).

:- end_tests(jpl_entityname).

% ===========================================================================
% Testing Java findclass descriptor <-> JPL type
% Use assertion magic: on assertion failure, the unit test does not fail
% immediately.
% ===========================================================================

:- begin_tests(jpl_findclass_descriptor).

the_dcg(jpl:jpl_findclass_descriptor).

the_tests([
   q( '[D'                      , array(double) ),
   q( '[[I'                     , array(array(int)) ),
   q( 'java/lang/String'        , class([java,lang],['String'])),
   q( '[Ljava/lang/String;'     , array(class([java,lang],['String']))),
   q( '[[Ljava/lang/String;'    , array(array(class([java, lang], ['String']))) ),
   q( '[[[Ljava/util/Calendar;' , array(array(array(class([java,util],['Calendar'])))) )
]).

test("Java findclass descriptor to JPL type") :-
   the_tests(L),
   the_dcg(DCG),
   maplist({DCG}/[q(FCD,T)]>>assertion((dcg_mangle(FCD,'',DCG,Out),Out == T)),L).

test("JPL type to Java findclass descriptor") :-
   the_tests(L),
   the_dcg(DCG),
   maplist({DCG}/[q(FCD,T)]>>assertion((dcg_mangle(Out,'',DCG,T),Out == FCD)),L).

test("no primitive in Java",fail) :-
   the_dcg(DCG),
   dcg_mangle(int,'',DCG,_).

test("no primitive in JPL",fail) :-
   the_dcg(DCG),
   dcg_mangle(_,'',DCG,int).

test("no void in Java",fail) :-
   the_dcg(DCG),
   dcg_mangle(void,'',DCG,_).

test("no void in JPL",fail) :-
   the_dcg(DCG),
   dcg_mangle(_,'',DCG,void).

test("no primitive descriptor (like I) in Java; in fact, it's a class name",true(T==class([],['I']))) :-
   the_dcg(DCG),
   dcg_mangle('I','',DCG,T).

:- end_tests(jpl_findclass_descriptor).

% ===========================================================================
% Testing Java field descriptor <-> JPL type
% Use assertion magic: on assertion failure, the unit test does not fail
% immediately.
% ===========================================================================

:- begin_tests(jpl_field_descriptor).

the_dcg(jpl_field_descriptor_dotty).

the_tests([
   q( 'D'                       , double ),
   q( 'I'                       , int ),
   q( '[D'                      , array(double) ),
   q( '[[I'                     , array(array(int)) ),
   q( 'Ljava.lang.String;'      , class([java,lang],['String'])),
   q( '[Ljava.lang.String;'     , array(class([java,lang],['String']))),
   q( '[[Ljava.lang.String;'    , array(array(class([java, lang], ['String']))) ),
   q( '[[[Ljava.util.Calendar;' , array(array(array(class([java,util],['Calendar'])))) )
]).

test("Java field descriptor to JPL type") :-
   the_tests(L),
   the_dcg(DCG),
   maplist({DCG}/[q(FD,T)]>>assertion((dcg_mangle(FD,'',DCG,Out),Out == T)),L).

test("JPL type to Java field descriptor") :-
   the_tests(L),
   the_dcg(DCG),
   maplist({DCG}/[q(FD,T)]>>assertion((dcg_mangle(Out,'',DCG,T),Out == FD)),L).

test("no void on the left") :-
   the_dcg(DCG),
   dcg_mangle(void,_,DCG,_).

test("no void on the right") :-
   the_dcg(DCG),
   dcg_mangle(v_,_,DCG,void).

:- end_tests(jpl_field_descriptor).


% ===========================================================================
% Testing Java method descriptor <-> JPL type
% Use assertion magic: on assertion failure, the unit test does not fail
% immediately.
% ===========================================================================

:- begin_tests(jpl_method_descriptor).

the_dcg(jpl:jpl_method_descriptor).

the_tests([
   q( '()V', method([],void) ),
   q( '(Ljava/lang/String;DD[Ljava/util/Calendar;)[[F',
      method([class([java,lang],['String']),double,double,array(class([java,util],['Calendar']))],array(array(float)))  )

]).

test("Java method descriptor to JPL type") :-
   the_tests(L),
   the_dcg(DCG),
   maplist({DCG}/[q(MD,T)]>>assertion((dcg_mangle(MD,'',DCG,Out),Out == T)),L).

test("JPL type to Java method descriptor") :-
   the_tests(L),
   the_dcg(DCG),
   maplist({DCG}/[q(MD,T)]>>assertion((dcg_mangle(Out,'',DCG,T),Out == MD)),L).

test("not a method descriptor",fail) :-
   the_dcg(DCG),
   dcg_mangle('(void)void',_,DCG,_).

:- end_tests(jpl_method_descriptor).

% ===========================================================================
% Directly comparing old and new Java entityname <-> JPL type mapping
% ===========================================================================

:- begin_tests(compare_old_and_new_entityname).

help_run_both(JavaSide,ReifNew,ReifOld) :-
   run_both(
      JavaSide,                % input
      jpl:jpl_entityname,     % name of new 1-arg DCG (in module jpl)
      jpl_type_classname_1,    % name of old 1-arg DCG (included further below)
      ReifNew,                 % output from new 1-arg DCG: success(JplType) on success, otherwise the atom 'failure'
      ReifOld).                % output from old 1-arg DCG: success(JplType) on success, otherwise the atom 'failure'

test("comparing #1" ,[blocked("Old response bad"),true(ReifNew == ReifOld)]) :-
   help_run_both('int',ReifNew,ReifOld).    % Old: class([],[int])   ???

test("comparing #2" ,[blocked("Old response bad"),true(ReifNew == ReifOld)]) :-
   help_run_both('float',ReifNew,ReifOld).  % Old: class([],[float]) ???

test("comparing #3" ,[blocked("Old response bad"),true(ReifNew == ReifOld)]) :-
   help_run_both('void',ReifNew,ReifOld).   % Old: class([],[void])  ???

test("old call gives wrong result #1") :-
   help_run_both('foo.bar.baz.Foo$',success(OutNew),success(OutOld)),
   OutNew == class([foo,bar,baz],['Foo$']),
   OutOld == class([foo,bar,baz],['Foo','']). % OLD IS WRONG

test("failure on old call #2") :-
   help_run_both('foo.bar.baz.$Foo',success(OutNew),failure), % OLD FAILS
   OutNew == class([foo,bar,baz],['$Foo']).

the_tests([
   'java.lang.Integer',
   'integer',
   '[D',
   '[[[[[I',
   '[[J',
   '[[Ljava.lang.String;',
   'java.lang.String',
   'Foo',
   'foo.bar.baz.Foo',
   'foo.bar.baz.Foo$Quux' ]).

test("comparing several examples") :-
   the_tests(L),
   maplist([EN]>>assertion((help_run_both(EN,success(OutNew),success(OutOld)),OutNew == OutOld)),L).

:- end_tests(compare_old_and_new_entityname).

% ===========================================================================
% Directly comparing old and new Java findclass descriptor <-> JPL type mapping
% ===========================================================================

:- begin_tests(compare_old_and_new_findclass_descriptor).

help_run_both(JavaSide,ReifNew,ReifOld) :-
   run_both(
      JavaSide,                       % input
      jpl:jpl_findclass_descriptor,   % name of new 1-arg DCG (in module jpl)
      jpl_type_findclassname,         % name of old 1-arg DCG (included further below)
      ReifNew,                        % output from new 1-arg DCG: success(JplType) on success, otherwise the atom 'failure'
      ReifOld).                       % output from old 1-arg DCG: success(JplType) on success, otherwise the atom 'failure'

the_tests([
   'java/lang/Integer',
   'integer',
   '[D',
   '[[[[[I',
   '[[J',
   '[[Ljava/lang/String;',
   'java/lang/String',
   'Foo',
   'foo/bar/baz/Foo',
   'foo/bar/baz/Foo$Quux' ]).

test("comparing several examples") :-
   the_tests(L),
   maplist([FCD]>>assertion((help_run_both(FCD,success(OutNew),success(OutOld)),OutNew == OutOld)),L).

:- end_tests(compare_old_and_new_findclass_descriptor).

% ===========================================================================
% Directly comparing old and new
% ===========================================================================

:- begin_tests(compare_both_field_descriptors).

help_run_both(JavaSide,ReifNew,ReifOld) :-
   run_both(
      JavaSide,                       % input
      jpl_field_descriptor_slashy,    % name of new 1-arg DCG (an indirection to one in module JPL)
      jpl_type_descriptor_1,          % name of old 1-arg DCG (included further below)
      ReifNew,                        % output from new 1-arg DCG: success(JplType) on success, otherwise the atom 'failure'
      ReifOld).                       % output from old 1-arg DCG: success(JplType) on success, otherwise the atom 'failure'

the_tests([
   '[D',
   '[[[[[I',
   '[[J',
   '[[Ljava/lang/String;',
   'Ljava/lang/String;',
   'LFoo;',
   'Lfoo/bar/baz/Foo;',
   'Lfoo/bar/baz/Foo$Quux;'
]).

test("comparing several examples") :-
   the_tests(L),
   maplist([FD]>>assertion((help_run_both(FD,success(OutNew),success(OutOld)),OutNew == OutOld)),L).

:- end_tests(compare_both_field_descriptors).


% ===========================================================================
% Testing jpl_type_to_classname/2, the exported predicate.
% This internally actually calls jpl_entityname//2
% ===========================================================================

:- begin_tests(jpl_type_to_classname).

test("jpl_type_to_classname: class", CN == 'java.util.String') :-
   jpl_type_to_classname(class([java,util],['String']),CN).

test("jpl_type_to_classname: class in default package", CN == 'String') :-
   jpl_type_to_classname(class([],['String']),CN).

test("jpl_type_to_classname: class name separated along '$'", CN == 'foo.bar.Bling$Blong') :-
   jpl_type_to_classname(class([foo,bar],['Bling','Blong']),CN).

test("jpl_type_to_classname: array of class", CN == '[Ljava.util.String;') :-
   jpl_type_to_classname(array(class([java,util],['String'])),CN).

test("jpl_type_to_classname: array of primtive", CN == '[B') :-
   jpl_type_to_classname(array(byte),CN).

test("jpl_type_to_classname: unboxed primitive", CN == byte) :-
   jpl_type_to_classname(byte,CN).

test("jpl_type_to_classname: void", CN == void) :-
   jpl_type_to_classname(void,CN).

test("jpl_type_to_classname: unrecognized primitive", fail) :-
   jpl_type_to_classname(bar,_).

:- end_tests(jpl_type_to_classname).

% ===========================================================================
% Testing jpl_classname_to_type/2, the exported predicate.
% This internally actually calls jpl_entityname//2 and performs caching
% ===========================================================================

:- begin_tests(jpl_classname_to_type).

test("jpl_classname_to_type: class", T == class([java,util],['String'])) :-
   jpl_classname_to_type('java.util.String',T).

test("jpl_classname_to_type: class in default package", T == class([],['String'])) :-
   jpl_classname_to_type('String',T).

test("jpl_classname_to_type: class name separated along '$'", T == class([foo,bar],['Bling','Blong'])) :-
   jpl_classname_to_type('foo.bar.Bling$Blong',T).

test("jpl_classname_to_type: array of class", T == array(class([java,util],['String']))) :-
   jpl_classname_to_type('[Ljava.util.String;',T).

test("jpl_classname_to_type: array of primtive", T == array(byte)) :-
   jpl_classname_to_type('[B',T).

test("jpl_classname_to_type: unboxed primitive 1", T == byte) :-
   jpl_classname_to_type(byte,T).

test("jpl_classname_to_type: unboxed primitive 2", T == int) :-
   jpl_classname_to_type(int,T).

test("jpl_classname_to_type: not an integer", T == class([],[integer])) :-
   jpl_classname_to_type(integer,T).

test("jpl_classname_to_type: void", T == void) :-
   jpl_classname_to_type(void,T).

:- end_tests(jpl_classname_to_type).

% ===========================================================================
% Testing safe_type_to_classname, used in exceptions
% ===========================================================================

:- begin_tests(safe_type_to_classname).

test("safe_type_to_classname: class", CN == 'java.util.String') :-
   jpl:safe_type_to_classname(class([java,util],['String']),CN).

test("safe_type_to_classname: class in default package", CN == 'String') :-
   jpl:safe_type_to_classname(class([],['String']),CN).

test("safe_type_to_classname: class name separated along '$'", CN == 'foo.bar.Bling$Blong') :-
   jpl:safe_type_to_classname(class([foo,bar],['Bling','Blong']),CN).

test("safe_type_to_classname: array of class", CN == '[Ljava.util.String;') :-
   jpl:safe_type_to_classname(array(class([java,util],['String'])),CN).

test("safe_type_to_classname: array of primtive", CN == '[B') :-
   jpl:safe_type_to_classname(array(byte),CN).

test("safe_type_to_classname: unboxed primitive", CN == byte) :-
   jpl:safe_type_to_classname(byte,CN).

test("safe_type_to_classname: void", CN == void) :-
   jpl:safe_type_to_classname(void,CN).

test("safe_type_to_classname: unrecognized primitive", CN == 'bar') :-
   jpl:safe_type_to_classname(bar,CN).

test("safe_type_to_classname: unrecognized structure", CN == 'foo(bar(baz))') :-
   jpl:safe_type_to_classname(foo(bar(baz)),CN).

:- end_tests(safe_type_to_classname).

% ===========================================================================
% The original jpl code for recognizing entity names etc.
% Added here because we want to run direct comparisons
% ===========================================================================

% jpl_type_alfa(0'$) -->        % presumably not allowed
%   "$".                        % given the "inner class" syntax?

jpl_type_alfa(0'_) -->
    "_",
    !.
jpl_type_alfa(C) -->
    [C], { C>=0'a, C=<0'z },
    !.
jpl_type_alfa(C) -->
    [C], { C>=0'A, C=<0'Z }.


jpl_type_alfa_num(C) -->
    jpl_type_alfa(C),
    !.
jpl_type_alfa_num(C) -->
    [C], { C>=0'0, C=<0'9 }.


jpl_type_array_classname(array(T)) -->
    "[", jpl_type_classname_2(T).


jpl_type_array_descriptor(array(T)) -->
    "[", jpl_type_descriptor_1(T).


jpl_type_bare_class_descriptor(class(Ps,Cs)) -->
    jpl_type_slashed_package_parts(Ps), jpl_type_class_parts(Cs).


jpl_type_bare_classname(class(Ps,Cs)) -->
    jpl_type_dotted_package_parts(Ps), jpl_type_class_parts(Cs).


jpl_type_class_descriptor(class(Ps,Cs)) -->
    "L", jpl_type_bare_class_descriptor(class(Ps,Cs)), ";".


jpl_type_class_part(N) -->
    jpl_type_id(N).


jpl_type_class_parts([C|Cs]) -->
    jpl_type_class_part(C), jpl_type_inner_class_parts(Cs).


jpl_type_classname_1(T) -->
    jpl_type_bare_classname(T),
    !.
jpl_type_classname_1(T) -->
    jpl_type_array_classname(T),
    !.
jpl_type_classname_1(T) -->
    jpl_type_primitive(T).


jpl_type_classname_2(T) -->
    jpl_type_delimited_classname(T).
jpl_type_classname_2(T) -->
    jpl_type_array_classname(T).
jpl_type_classname_2(T) -->
    jpl_type_primitive(T).



jpl_type_delimited_classname(Class) -->
    "L", jpl_type_bare_classname(Class), ";".



jpl_type_descriptor_1(T) -->
    jpl_type_primitive(T),
    !.
jpl_type_descriptor_1(T) -->
    jpl_type_class_descriptor(T),
    !.
jpl_type_descriptor_1(T) -->
    jpl_type_array_descriptor(T),
    !.
jpl_type_descriptor_1(T) -->
    jpl_type_method_descriptor(T).



jpl_type_dotted_package_parts([P|Ps]) -->
    jpl_type_package_part(P), ".", !, jpl_type_dotted_package_parts(Ps).
jpl_type_dotted_package_parts([]) -->
    [].



jpl_type_findclassname(T) -->
    jpl_type_bare_class_descriptor(T).
jpl_type_findclassname(T) -->
    jpl_type_array_descriptor(T).



jpl_type_id(A) -->
    { nonvar(A) -> atom_codes(A,[C|Cs]) ; true },
    jpl_type_alfa(C), jpl_type_id_rest(Cs),
    { atom_codes(A, [C|Cs]) }.



jpl_type_id_rest([C|Cs]) -->
    jpl_type_alfa_num(C), !, jpl_type_id_rest(Cs).
jpl_type_id_rest([]) -->
    [].



jpl_type_id_v2(A) -->                   % inner class name parts (empirically)
    { nonvar(A) -> atom_codes(A,Cs) ; true },
    jpl_type_id_rest(Cs),
    { atom_codes(A, Cs) }.



jpl_type_inner_class_part(N) -->
    jpl_type_id_v2(N).



jpl_type_inner_class_parts([C|Cs]) -->
    "$", jpl_type_inner_class_part(C), !, jpl_type_inner_class_parts(Cs).
jpl_type_inner_class_parts([]) -->
    [].



jpl_type_method_descriptor(method(Ts,T)) -->
    "(", jpl_type_method_descriptor_args(Ts), ")", jpl_type_method_descriptor_return(T).



jpl_type_method_descriptor_args([T|Ts]) -->
    jpl_type_descriptor_1(T), !, jpl_type_method_descriptor_args(Ts).
jpl_type_method_descriptor_args([]) -->
    [].



jpl_type_method_descriptor_return(T) -->
    jpl_type_void(T).
jpl_type_method_descriptor_return(T) -->
    jpl_type_descriptor_1(T).



jpl_type_package_part(N) -->
    jpl_type_id(N).



jpl_type_primitive(boolean) -->
    "Z",
    !.
jpl_type_primitive(byte) -->
    "B",
    !.
jpl_type_primitive(char) -->
    "C",
    !.
jpl_type_primitive(short) -->
    "S",
    !.
jpl_type_primitive(int) -->
    "I",
    !.
jpl_type_primitive(long) -->
    "J",
    !.
jpl_type_primitive(float) -->
    "F",
    !.
jpl_type_primitive(double) -->
    "D".



jpl_type_slashed_package_parts([P|Ps]) -->
    jpl_type_package_part(P), "/", !, jpl_type_slashed_package_parts(Ps).
jpl_type_slashed_package_parts([]) -->
    [].



jpl_type_void(void) -->
    "V".


