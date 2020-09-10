# Attach a Prolog engine per Java thread

This demo illustrates Prolog.create_engine() and Prolog.destroy_engine()
to realise a one-to-one mapping between Java threads and Prolog engines.

In the normal scenario JPL uses a pool   of Prolog engines, one of which
is temporary attached to the Java thread  wishing to run a Prolog query.
Using Prolog.create_engine() we can attach  a Prolog engine permanently.
This notably allows for using   thread-local  dynamic predicates, global
variables, etc. to store thread specific data  on the Prolog side as the
Java thread is guaranteed to use the  same Prolog engine. Using the pool
model distinct Java to Prolog queries  may   end  up in different Prolog
engines.

## Memory usage (sleeping threads)

Below is a test for the memory and  time needed to create N Java threads
with sleeping Prolog engines. Measured on   a 64-bit Linux machine using
SWI-Prolog 8.3.8 and OpenJDK 11.0.8:

    Threads   Time   VIRT     RES
    1            0  19.1g   56640
    10           1  19.7g   57568
    100	        20  26.8g   58452
    1000        98  34.9g   75648
    10000     1100  45.0g  236860
