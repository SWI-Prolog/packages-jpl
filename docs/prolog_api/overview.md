<!DOCTYPE doctype PUBLIC "-//w3c//dtd html 4.0 transitional//en">
<html>
<head>
  <meta http-equiv="Content-Type"
 content="text/html; charset=iso-8859-1">
  <meta name="Author" content="Paul Singleton">
  <meta name="GENERATOR"
 content="Mozilla/4.74 [en] (WinNT; U) [Netscape]">
  <title>A SWI-Prolog to Java interface</title>
</head>
<body>
<h1><span style="font-style: italic;"> JPL 3.x</span> Prolog API
overview</h1>
<hr style="width: 100%; height: 2px;">
<ul>
  <li><a href="#Introduction">Introduction</a></li>
  <li><a href="#JPL_types_Java_types_as_seen_by">JPL types (Java types,
as seen by Prolog)</a><a href="#Java_types_as_seen_by_Prolog"></a></li>
  <li><a href="#representation_of_Java_values...">representation of
Java values and references within Prolog</a></li>
  <li><a href="#repn_of_Java_types_1_structured">representation of Java
types within Prolog (1): <i>structured</i> notation</a></li>
  <li><a href="#repn_of_Java_types_2_descriptor">representation of Java
types within Prolog (2): <i>descriptor</i> notation</a></li>
  <li><a href="#repn_of_Java_types_3_classname">representation of Java
types within Prolog (3): classname notation</a></li>
  <li><a href="#creating_instances_of_Java_classes">creating instances
of Java classes</a></li>
  <li><a href="#calling_methods_of_Java_objects_...">calling methods of
Java objects or classes</a></li>
  <li><a href="#fetching_field_values_of_Java_objects...">fetching
field values of Java objects or classes</a></li>
  <li><a href="#setting_field_values_of_Java_objects...">setting field
values of Java objects or classes</a></li>
  <li><a href="#a_slightly_longer_example">a slightly longer example</a></li>
  <li><a href="#jpl_new3">jpl_new/3</a></li>
  <li><a href="#jpl_call4">jpl_call/4</a></li>
  <li><a href="#jpl_set3">jpl_set/3</a></li>
  <li><a href="#jpl_get3">jpl_get/3</a></li>
  <li><a href="#exceptions">exceptions thrown by Java</a></li>
  <li><a href="#testing">testing</a></li>
  <li><a href="#to_do">to do</a><br>
  </li>
</ul>
<hr width="100%">
<h2><font face="Arial,Helvetica"><a name="Introduction"></a>Introduction</font></h2>
<font face="Arial,Helvetica">This is an overview of
an interface which allows SWI-Prolog programs to dynamically create and
manipulate Java objects.</font>
<p><font face="Arial,Helvetica">Here are some significant features of
the interface and its implementation:</font> </p>
<ul>
</ul>
<ul>
  <li><font face="Arial,Helvetica">it is completely dynamic: no
precompilation is required to manipulate any Java classes which can be
found at run time, and any objects which can be instantiated from them<br>
    </font></li>
</ul>
<font face="Arial,Helvetica"></font>
<ul>
  <li><font face="Arial,Helvetica">it is interoperable with <span
 style="font-weight: bold; font-style: italic;">JPL</span>'s Java API
(which has evolved from Fred Dushin's <span style="font-style: italic;">JPL
1.0.1</span>)</font></li>
</ul>
<font face="Arial,Helvetica"></font>
<ul>
  <li><font face="Arial,Helvetica">it requires a Java 2 JVM and class
libraries (although it doesn't depend on any Java 2-specific
facilities, and originally was developed for use with both 1.1 and 1.2
JVMs, I haven't tested it with 1.1 recently, and don't support this)</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">it exploits the <i>Invocation API</i>
of the <i>Java Native Interface</i>: this is a mandatory feature of
any compliant
JVM (even the now defunct "Microsoft Virtual Machine" supported JNI,
although they seemed to want to keep that a secret :-)</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">it is implemented with a fair
amount of
Prolog code in one module (<span style="font-style: italic;">jpl.pl</span>)&nbsp;
(which I believe to be ISO Standard Prolog compliant
and portable) and a SWI-Prolog-specific foreign library (<span
 style="font-style: italic;">jpl.dll</span> for Windows), implemented
in ANSI C but making a lot of use of the SWI-Prolog <i>Foreign
Language Interface</i></font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">the foreign-language part has so
far been tested only under Windows NT4, but is believed to be readily
portable to SWI-Prolog
on other platforms</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">as far as is feasible, Java data
values and
object references are represented within Prolog canonically and without
loss
of information (minor exceptions: Java <span
 style="font-style: italic;">float </span>and <span
 style="font-style: italic;">double</span> values are both converted to
Prolog <span style="font-style: italic;">float </span>values; Java <span
 style="font-style: italic;">byte</span>, <span
 style="font-style: italic;">char</span>, <span
 style="font-style: italic;">short</span>, <span
 style="font-style: italic;">int</span> and <span
 style="font-style: italic;">long</span> values are all converted to
Prolog <span style="font-style: italic;">integer</span> values; the
type distinctions which are lost are normally of no significance)<br>
    </font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">references within Prolog to Java
objects:</font></li>
  <ul>
    <li> <font face="Arial,Helvetica">should be treated as opaque
handles</font></li>
    <li> <font face="Arial,Helvetica">are canonical (two references
are ==/2
equal if-and-only-if they refer to the <span
 style="font-style: italic;">same
object</span> within the JVM)</font></li>
    <li> <font face="Arial,Helvetica">are represented as structures
containing
a distinctive atom so as to exploit SWI-Prolog's atom garbage
collection: when an object reference is garbage-collected in Prolog,
the JVM garbage collector
is informed, so there is sound and complete overall garbage collection
of
Java objects within the combined Prolog+Java system</font></li>
  </ul>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">Java class methods can be called
by name: <i style="font-weight: bold;">JPL</i> invisibly fetches (and
caches) essential
details of method invocation, exploiting <i>Java Reflection</i>
facilities</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">the API is similar to that of
XPCE: the
four main interface calls are <i>jsp_new</i>, </font><font
 face="Arial,Helvetica"><i>jsp_call, </i></font><font
 face="Arial,Helvetica"><i>jsp_set</i> and </font><font
 face="Arial,Helvetica"><i>jsp_get</i> (there is no <i>jsp_free</i>,
since Java's garbage collection
is extended transparently into Prolog)</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica"><i>jsp_call</i> resolves
overloaded methods automatically and dynamically, inferring the types
of the call's actual parameters,
and identifying the most specific of the applicable method
implementations
(similarly, <i>jsp_new</i> resolves overloaded constructors)</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">Prolog code which uses the API
calls is
responsible for passing suitably-typed values and references, since the
JNI
doesn't perform complete dynamic type-checking, and nor currently does <span
 style="font-weight: bold; font-style: italic;">JPL</span> (although
the <i>overloaded method resolution</i> mechanism could probably be
adapted to do this)</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">Prolog code can reason about the
types
of Java data values, object references, fields and methods: <span
 style="font-weight: bold; font-style: italic;">JPL</span> supports a
canonical
representation of all Java types as structured terms (e.g. </font><b><tt>array(array(byte))</tt></b><font
 face="Arial,Helvetica">) and also as atomic JVM signatures</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">the Prolog-calls-Java (mine) and
Java-calls-Prolog (Fred's) parts of <span
 style="font-weight: bold; font-style: italic;">JPL</span>
are largely independent; mine concentrates on representing all Java
data
values and objects within Prolog, and supporting manipulation of
objects;
Fred's concentrates on representing any Prolog term within Java, and
supporting
the calling of goals within Prolog and the retrieving of results back
into
Java</font></li>
</ul>
<ul style="font-family: helvetica,arial,sans-serif;">
  <li>when called from Prolog, <span style="font-style: italic;">void</span>
methods return a <span style="font-style: italic;">void</span> value
(which is distinct from all other <span
 style="font-weight: bold; font-style: italic;">JPL</span> values and
references)</li>
</ul>
<ul>
  <li><font face="Arial,Helvetica">it uses </font><b><tt><font
 size="+1">@/1</font></tt></b><font face="Arial,Helvetica"> to
construct representations of certain Java values; if&nbsp; </font><b><tt><font
 size="+1">@/1</font></tt></b><font face="Arial,Helvetica"> is defined
as a
prefix operator (as used by XPCE), then you can write </font><b><tt><font
 size="+1">@false</font></tt></b><font face="Arial,Helvetica">, </font><b><tt><font
 size="+1">@true</font></tt></b><font face="Arial,Helvetica">, </font><b><tt><font
 size="+1">@null</font></tt></b><font face="Arial,Helvetica"> etc. in
your
source code; </font><font face="Arial,Helvetica">otherwise (and for
portability) you'll have to write e.g. </font><b><tt><font size="+1">@(true)</font></tt></b><font
 face="Arial,Helvetica"> etc.</font></li>
</ul>
<hr width="100%">
<h2><a name="JPL_types_Java_types_as_seen_by"></a>JPL types (Java
types, as seen by Prolog)</h2>
<blockquote><font face="Arial,Helvetica">All Java values and object
references which are passed between Prolog engines and Java VMs via <span
 style="font-weight: bold; font-style: italic;">JPL</span>'s Prolog API
are seen as instances of types within this simplified <span
 style="font-weight: bold; font-style: italic;">JPL</span> type system:<br>
  <br>
a <b><i>datum</i></b>&nbsp;&nbsp; (this term is introduced, out of
necessity, to refer jointly to <span style="font-style: italic;">values
  </span>and <span style="font-style: italic;">refs</span>)</font>
  <blockquote><font face="Arial,Helvetica">is a <b><i>value</i></b>&nbsp;&nbsp;&nbsp;
(values are copied between Prolog and the JVM)</font></blockquote>
</blockquote>
<blockquote>
  <blockquote>
    <blockquote><font face="Arial,Helvetica">is a <b><i>boolean</i></b></font></blockquote>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote>
    <blockquote><font face="Arial,Helvetica">or a <b><i>char</i></b></font></blockquote>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote>
    <blockquote><font face="Arial,Helvetica">or a <b><i>long</i></b>, <b><i>int</i></b>,
      <b><i>short</i></b> or <b><i>byte</i></b></font></blockquote>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote>
    <blockquote><font face="Arial,Helvetica">or a <b><i>double</i></b>
or <b><i>float</i></b></font></blockquote>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote>
    <blockquote><font face="Arial,Helvetica">or a <b><i>string</i></b>&nbsp;&nbsp;
(an instance of <span style="font-style: italic;">java.lang.String</span>)</font></blockquote>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote>
    <blockquote><font face="Arial,Helvetica">or a <b><i>void</i></b>&nbsp;&nbsp;&nbsp;&nbsp;
(an artificial value returned by calls to Java void methods)</font></blockquote>
    <font face="Arial,Helvetica">or a <b><i>ref</i></b></font>
    <blockquote><font face="Arial,Helvetica">is <b><i>null</i></b></font></blockquote>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote>
    <blockquote><font face="Arial,Helvetica">or an <b><i>object</i></b>&nbsp;&nbsp;&nbsp;
(held within the JVM, and represented in Prolog by a canonical
reference)</font>
      <blockquote><font face="Arial,Helvetica">is an <b><i>array</i></b></font></blockquote>
    </blockquote>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote>
    <blockquote>
      <blockquote><font face="Arial,Helvetica">or a <b><i>class
instance</i></b> (other than of <span style="font-style: italic;">java.lang.String</span>)<b><i><br>
        </i></b></font></blockquote>
    </blockquote>
  </blockquote>
</blockquote>
<hr width="100%">
<h2><a name="representation_of_Java_values..."></a> representation of
Java values and references within Prolog</h2>
<span style="font-family: helvetica,arial,sans-serif;">Instances of <span
 style="font-weight: bold; font-style: italic;">JPL</span> types are
represented within Prolog as follows:</span><br
 style="font-family: helvetica,arial,sans-serif;">
<blockquote><font face="Arial,Helvetica"><b><i>boolean</i></b> has two
values,
represented by </font><b><tt><font size="+1">@(true)</font></tt></b><font
 face="Arial,Helvetica"> and </font><b><tt><font size="+1">@(false)</font></tt></b>
  <p><font face="Arial,Helvetica"><b><i>char</i></b> values are
represented by corresponding Prolog <i>integers</i></font> </p>
  <p><font face="Arial,Helvetica"><b><i>int</i></b>, <b><i>short</i></b>
and <b><i>byte</i></b> values are represented by corresponding Prolog <i>integers</i></font>
  </p>
  <p><font face="Arial,Helvetica"><b><i>long</i></b> values are
represented as Prolog <i>integers</i> if possible (32-bit in current
SWI-Prolog), else as </font><b><tt><font size="+1">jlong(Hi,Lo)</font></tt></b><font
 face="Arial,Helvetica"> where </font><b><tt><font size="+1">Hi</font></tt></b><font
 face="Arial,Helvetica"> is an <i>integer</i> corresponding to the
top32
bits of the long, and </font><b><tt><font size="+1">Lo</font></tt></b><font
 face="Arial,Helvetica"> similarly represents the lower 32 bits</font> </p>
  <p><font face="Arial,Helvetica"><b><i>double</i></b> and <b><i>float</i></b>
values are represented as Prolog floats (which are equivalent to Java
doubles) (there may be minor rounding, normalisation or
loss-of-precision issues when
a Java float is widened to a Prolog float then narrowed back again, but
what
the heck)</font> </p>
  <p><font face="Arial,Helvetica"><b><i>string</i></b> values
(immutable instances
of <span style="font-style: italic;">java.lang.Stri</span>ng) are
represented as Prolog <i>atoms</i> (in UTF-8 encoding)</font> </p>
  <p><font face="Arial,Helvetica"><b><i>null</i></b> has only one
value, represented
as </font><b><tt><font size="+1">@(null)</font></tt></b> </p>
  <p><font face="Arial,Helvetica"><b><i>void</i></b> has only one
value, represented
as </font><b><tt><font size="+1">@(void)</font></tt></b> </p>
  <p><font face="Arial,Helvetica"><b><i>array</i></b> and <b><i>class
instance</i></b> references are currently represented as </font><b><tt><font
 size="+1">@(Tag)</font></tt></b><font face="Arial,Helvetica">, where
Tag ia an <i>atom</i> whose name encodes
a JNI global reference value; this may change, but won't affect Prolog
programs
which respect the opacity of references</font></p>
</blockquote>
<hr width="100%">
<h2><a name="repn_of_Java_types_1_structured"></a> representation of
Java types within Prolog (1): <i>structured</i> notation</h2>
<font face="Arial,Helvetica">The <span
 style="font-weight: bold; font-style: italic;">JPL Prolog API</span>
allows Prolog
applications to inspect, manipulate, and reason about the types of Java
values, references,
methods etc., and this section describes how these types themselves (as
opposed to instances thereof) are represented.&nbsp; Predicates which
pass these type representations include </font><b><a
 href="api.md#jpl_class_to_type/2">jpl_class_to_type/2</a>, </b><b><a
 href="api.md#jpl_classname_to_type/2">jpl_classname_to_type/2</a>,
</b><b><a href="api.md#jpl_datum_to_type/2">jpl_datum_to_type/2</a>,
</b><b><a href="api.md#jpl_is_object_type/1">jpl_is_object_type/1</a>,
</b><b><a href="api.md#jpl_is_type/1">jpl_is_type/1</a>, </b><a
        href="api.md#jpl_object_to_type/2" style="font-weight: bold;">jpl_object_to_type/2</a><b>,
</b><b><a href="api.md#jpl_primitive_type/1">jpl_primitive_type/1</a>,
</b><b><a href="api.md#jpl_ref_to_type/2">jpl_ref_to_type/2</a>, </b><b><a
 href="api.md#jpl_type_to_class/2">jpl_type_to_class/2</a>. </b><b><a
 href="api.md#jpl_type_to_classname/2">jpl_type_to_classname/2</a>.</b>
<blockquote><font face="Arial,Helvetica"><b><i>void</i></b> is
represented as </font><b><tt><font size="+1">void</font></tt></b></blockquote>
<blockquote><font face="Arial,Helvetica"><b><i>null</i></b> is
represented as </font><b><tt><font size="+1">null</font></tt></b></blockquote>
<blockquote><font face="Arial,Helvetica">the primitive types are
represented as </font><b><tt><font size="+1">boolean</font></tt></b><font
 face="Arial,Helvetica">, </font><b><tt><font size="+1">char</font></tt></b><font
 face="Arial,Helvetica">, </font><b><tt><font size="+1">byte</font></tt></b><font
 face="Arial,Helvetica">, </font><b><tt><font size="+1">short</font></tt></b><font
 face="Arial,Helvetica">, </font><b><tt><font size="+1">int</font></tt></b><font
 face="Arial,Helvetica">, </font><b><tt><font size="+1">long</font></tt></b><font
 face="Arial,Helvetica">, </font><b><tt><font size="+1">float</font></tt></b><font
 face="Arial,Helvetica">, </font><b><tt><font size="+1">double</font></tt></b></blockquote>
<blockquote><font face="Arial,Helvetica"><b><i>classes</i></b> are
represented as </font><b><tt><font size="+1">class(</font></tt></b><i><font
 face="Arial,Helvetica">package_parts</font></i><b><tt><font size="+1">,</font></tt></b><i><font
 face="Arial,Helvetica">classname_parts</font></i><b><tt><font size="+1">)</font></tt></b>
  <blockquote><font face="Arial,Helvetica">e.g.&nbsp; </font><b><tt><font
 size="+1">class([java,util],['Date'])</font></tt></b></blockquote>
  <font face="Arial,Helvetica"><b><i>array</i></b> types are
represented as </font><b><tt><font size="+1">array(</font></tt></b><i><font
 face="Arial,Helvetica">type</font></i><b><tt><font size="+1">)</font></tt></b>
  <blockquote>e.g.&nbsp; <b><tt><font size="+1">array(boolean)</font></tt></b></blockquote>
</blockquote>
<blockquote>
  <blockquote>e.g.&nbsp; <b><tt><font size="+1">array(class([java,lang],['String'])<br>
    </font></tt></b></blockquote>
</blockquote>
<span style="font-family: helvetica,arial,sans-serif;">This <span
 style="font-style: italic;">structured </span>notation for Java types
is designed to be convenient for composition and decomposition by
matching (unification).</span><br>
<hr width="100%" style="font-family: helvetica,arial,sans-serif;">
<h2><a name="repn_of_Java_types_2_descriptor"></a> representation of
Java types within Prolog (2): <i>descriptor</i> notation</h2>
<font face="Arial,Helvetica">The <i>descriptor</i> notation for Java
types is one of two textual notations employed by the JVM and the Java
class libraries; <span style="font-weight: bold; font-style: italic;">JPL</span>
(necessarily)
supports both (and supports conversion between all three
representations).</font>
<p><font face="Arial,Helvetica">Examples:</font> </p>
<blockquote><b><tt><font size="+1">'Z'</font></tt></b><font
 face="Arial,Helvetica"> denotes <b><i>boolean</i></b></font>
  <p><b><tt><font size="+1">'B'</font></tt></b><font
 face="Arial,Helvetica"> denotes <b><i>byte</i></b></font> </p>
  <p><b><tt><font size="+1">'C'</font></tt></b><font
 face="Arial,Helvetica"> denotes <b><i>char</i></b></font> </p>
  <p><b><tt><font size="+1">'S'</font></tt></b><font
 face="Arial,Helvetica"> denotes <b><i>short</i></b></font> </p>
  <p><b><tt><font size="+1">'I'</font></tt></b><font
 face="Arial,Helvetica"> denotes <b><i>int</i></b></font> </p>
  <p><b><tt><font size="+1">'J'</font></tt></b><font
 face="Arial,Helvetica"> denotes <b><i>long</i></b></font> </p>
  <p><b><tt><font size="+1">'F'</font></tt></b><font
 face="Arial,Helvetica"> denotes <b><i>float</i></b></font> </p>
  <p><b><tt><font size="+1">'D'</font></tt></b><font
 face="Arial,Helvetica"> denotes <b><i>double</i></b></font> </p>
  <p><b><tt><font size="+1">'Ljava/util/Date;'</font></tt></b><font
 face="Arial,Helvetica"> (for example) deno<span
 style="font-family: helvetica,arial,sans-serif;">tes the Jav</span>a
clas<span style="font-family: helvetica,arial,sans-serif;">s </span></font><span
 style="font-family: helvetica,arial,sans-serif; font-style: italic;">java.util.Date</span>
  </p>
  <p><b><tt><font size="+1">'[</font></tt></b><i><font
 face="Arial,Helvetica">type</font></i><b><tt><font size="+1">'</font></tt></b><font
 face="Arial,Helvetica"> denotes an <b><i>array</i></b> of <i>type</i></font>
  </p>
  <p><b><tt><font size="+1">'(</font></tt></b><i><font
 face="Arial,Helvetica">argument_types</font></i><b><tt><font size="+1">)</font></tt></b><i><font
 face="Arial,Helvetica">return_type</font></i><b><tt><font size="+1">'</font></tt></b><font
 face="Arial,Helvetica"> denotes the type of a method</font></p>
</blockquote>
<hr width="100%">
<h2><a name="repn_of_Java_types_3_classname"></a> representation of
Java types within Prolog (3): <i>classname</i> notation</h2>
<font face="Arial,Helvetica">The <i>classname</i> notation for Java
types is the other textual notation employed by the JVM and the Java
class libraries.&nbsp; It is a (seemingly unnecessary) variation on the
<i>descriptor</i> notation, used by a few JNI routines.&nbsp; It has
the slight advantage that, in the
case of simple class types only, it resembles the Java source text
notation for classes.&nbsp; This representation is supported only
because certain JNI functions use it; it is used within <span
 style="font-weight: bold; font-style: italic;">JPL</span>'s
implementation of <span style="font-weight: bold;">jpl_call/4</span>
etc.&nbsp; You may encounter this notation when tracing <span
 style="font-weight: bold; font-style: italic;">JPL</span> activity,
but otherwise you need not know about it.</font>
<p><font face="Arial,Helvetica">Examples:</font> </p>
<blockquote><b><tt><font size="+1">'java.util.Vector'</font></tt></b><font
 face="Arial,Helvetica"> denotes the Java class </font><tt><font
 size="+1">java.util.Vector</font></tt></blockquote>
<blockquote><b><tt><font size="+1">'[B'</font></tt></b><font
 face="Arial,Helvetica"> denotes an <b><i>array</i></b> of <b><i>boolean</i></b></font></blockquote>
<blockquote><b><tt><font size="+1">'[Ljava.lang.String;'</font></tt></b><font
 face="Arial,Helvetica"> denotes an <b><i>array</i></b> of <b><i>string</i></b></font></blockquote>
<hr width="100%">
<h1>Using the <span style="font-style: italic;">JPL 3.x</span> Prolog
API<br>
</h1>
<h2><a name="creating_instances_of_Java_classes"></a> creating
instances of Java classes</h2>
<font face="Arial,Helvetica">To create an instance of a Java class from
within Prolog,
call <b>jpl_new(+Class,+Params,-Ref)</b> with a classname, a list of
actual parameters for the
constructor, and a variable to be bound to the new reference, e.g.</font>
<blockquote><b><tt><font size="+1">jpl_new( 'javax.swing.JFrame',
['frame with dialog'], F)</font></tt></b></blockquote>
<font face="Arial,Helvetica">which binds <b>F</b> to a new object
reference, e.g.</font>
<blockquote><font face="Arial,Helvetica">&nbsp;</font><b><tt><font
 size="+1">@('J#0008272420')</font></tt></b></blockquote>
<font face="Arial,Helvetica">(not that the details of this structure
are of any necessary concern to the Prolog programmer or to the
applications she
writes).<br>
NB for convenience, this predicate is overloaded: <span
 style="font-weight: bold;">Class</span> can also be a class type in <span
 style="font-style: italic;">structured </span>notation, e.g. <code>array(boolean)</code>.<br>
</font>
<p> </p>
<hr width="100%">
<h2><a name="calling_methods_of_Java_objects_..."></a> calling methods
of Java objects or classes</h2>
<font face="Arial,Helvetica">The object reference generated by the <b>jpl_new/3</b>
call (above) can be passed to other <span
 style="font-weight: bold; font-style: italic;">JPL</span> API
predicates such as <br>
</font>
<blockquote><b><tt><font size="+1">jpl_call( +Ref, +Method, +Params,
-Result)</font></tt></b></blockquote>
&nbsp;<font face="Arial,Helvetica">e.g.</font>
<blockquote><b><tt><font size="+1">jpl_call( F, setVisible, [@(true)],
_)</font></tt></b></blockquote>
<font face="Arial,Helvetica">which calls the <b>setVisible</b> method
of the object to which <b>F</b> refers, effectively passing it the
Java value <span style="font-style: italic;">true.</span></font>
<p><font face="Arial,Helvetica">(This call should display the new <b>JFrame</b>
in the top left corner of the desktop.)</font> </p>
<p><font face="Arial,Helvetica">Note the anonymous variable passed as
the fourth argument to <b>jsp_call/4.&nbsp;</b> A variable in this
position receives
the result of the method call: either a value or a reference.&nbsp;
Since
</font><b><tt><font size="+1">SetVisible()</font></tt></b><font
 face="Arial,Helvetica"> is a void method, the call returns the
(artificial)
reference </font><b><tt><font size="+1">@(void)</font></tt></b><font
 face="Arial,Helvetica">.</font> </p>
<p><font face="Arial,Helvetica">Some may prefer to code this call thus:</font>
</p>
<blockquote><b><tt><font size="+1">jpl_call( F, setVisible, [@true],
@void)</font></tt></b></blockquote>
<font face="Arial,Helvetica">which documents the programmer's
understanding that this is a <span style="font-style: italic;">void </span>method
(and fails if it isn't :-).</font><font face="Arial,Helvetica"><br>
</font>&nbsp;<br>
<font face="Arial,Helvetica">If the <span
 style="font-family: helvetica,arial,sans-serif; font-weight: bold;">+Ref</span>
argument represents a class, then the named static method of that
class&nbsp; is called.</font>
<p> </p>
<hr width="100%">
<h2><a name="fetching_field_values_of_Java_objects..."></a> fetching
field values of Java objects or classes</h2>
<font face="Arial,Helvetica">The <b>jpl_get/3</b> API predicate can
retrieve the value of an instance field or a static field, e.g.</font>
<blockquote><b><tt><font size="+1">jpl_get( 'java.awt.Color', pink,
Pink)</font></tt></b></blockquote>
<font face="Arial,Helvetica">which binds the Prolog variable </font><b><tt><font
 size="+1">Pink</font></tt></b><font face="Arial,Helvetica"> to a
reference to the predefined <b>java.awt.Color</b>
"constant" which is held in the static final <b>.pink</b> field of the
<b>java.awt.Color</b>
class.</font>
<p><font face="Arial,Helvetica">More generally, <b>jpl_get/3</b> has
the following
interface:</font> </p>
<blockquote><b><tt><font size="+1">jpl_get( +Class_or_Object, +Field,
-Datum)</font></tt></b></blockquote>
<font face="Arial,Helvetica">If the first argument represents a class,
then
a static field of that class with FieldName is accessed.</font>
<p> </p>
<hr width="100%">
<h2><a name="setting_field_values_of_Java_objects..."></a> setting
field values of Java objects or classes</h2>
<font face="Arial,Helvetica">Object and class fields can be set (i.e.
have values or references assigned to them) by the <b>jpl_set/3</b>
API procedure, which has the following interface:</font>
<blockquote><b><tt><font size="+1">jpl_set( +Class_or_Object, +Field,
+Datum)</font></tt></b></blockquote>
<font face="Arial,Helvetica">where <b>Datum</b> must be a value or
reference of a type suitable for assignment to the named field of the
class or object.</font>
<p> </p>
<hr width="100%">
<h2><a name="a_slightly_longer_example"></a> a slightly longer example</h2>
<font face="Arial,Helvetica">This code fragment</font>
<pre><b><tt><font size="+1">&nbsp;&nbsp;&nbsp; findall(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Ar,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; (&nbsp;&nbsp; current_prolog_flag( N, V),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; term_to_atom( V, Va),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; jpl_new( '[Ljava.lang.String;', [N,Va], Ar)<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Ars<br>&nbsp;&nbsp;&nbsp; ),<br>&nbsp;&nbsp;&nbsp; jpl_new( '[[Ljava.lang.String;', Ars, Ac),<br>&nbsp;&nbsp;&nbsp; jpl_datums_to_array( [name,value], Ah),<br>&nbsp;&nbsp;&nbsp; jpl_new( 'javax.swing.JFrame', ['current_prolog_flag'], F),<br>&nbsp;&nbsp;&nbsp; jpl_call( F, getContentPane, [], CP),<br>&nbsp;&nbsp;&nbsp; jpl_new( 'javax.swing.JTable', [Ac,Ah], T),<br>&nbsp;&nbsp;&nbsp; jpl_new( 'javax.swing.JScrollPane', [T], SP),<br>&nbsp;&nbsp;&nbsp; jpl_call( CP, add, [SP,'Center'], _),<br>&nbsp;&nbsp;&nbsp; jpl_call( F, setSize, [600,400], _),</font></tt></b></pre>
<font face="Arial,Helvetica">builds an array of arrays of strings
containing the names and values of the current SWI-Prolog "flags", and
displays it in
a JTable within a ScrollPane within a JFrame:</font>
<blockquote><img src="screendump.jpg" height="269" width="524"></blockquote>
<font face="Arial,Helvetica">In addition to <span
 style="font-weight: bold; font-style: italic;">JPL</span> API calls,
this
code calls <b>jpl_datums_to_array/2</b>, a utility which converts any
list
of valid representations of Java values (or objects) into a new Java
array,
whose base type is the most specialised type of which all list members
are
instances, and which is defined thus:</font>
<blockquote>
  <pre><b><tt><font size="+1">jpl_datums_to_array( Ds, A) :-<br>&nbsp;&nbsp;&nbsp; ground( Ds),<br>&nbsp;&nbsp;&nbsp; jpl_datums_to_most_specific_common_ancestor_type( Ds, T),<br>&nbsp;&nbsp;&nbsp; jpl_new( array(T), Ds, A).</font></tt></b></pre>
</blockquote>
<font face="Arial,Helvetica">Having found the "most specific common
ancestor type" (my phrase :-), a new array of this type is created,
whose elements are initialised to the successive members of the list of
datums.</font>
<p><font face="Arial,Helvetica">This illustrates another mode of
operation of <b>jpl_new/3</b>:</font> </p>
<blockquote><b><tt><font size="+1">jpl_new( +ArrayType, +InitialValues,
-ArrayRef)</font></tt></b></blockquote>
<font face="Arial,Helvetica">See the relevant Appendix for fuller
details of the API procedures.</font>
<p><font face="Arial,Helvetica">Don't forget the possibility of writing
and
manipulating new Java classes to serve your Prolog applications: this
interface
is not designed to make Java programming redundant :-)</font> </p>
<p> </p>
<hr width="100%"> <br>
<b><tt><font size="+2"><a name="jpl_new3"></a>jpl_new( +X, +Argz, -V) :-</font></tt></b>
<blockquote><b><tt><font size="+1">X</font></tt></b><font
 face="Arial,Helvetica"> can be:</font>
  <ul>
    <li> <font face="Arial,Helvetica">a suitable <i>type</i></font></li>
    <ul>
      <li> <font face="Arial,Helvetica">i.e. any </font><b><tt><font
 size="+1">class(_,_)</font></tt></b><font face="Arial,Helvetica">, </font><b><tt><font
 size="+1">array(_)</font></tt></b><font face="Arial,Helvetica"> or
primitive
type (e.g. </font><b><tt><font size="+1">byte</font></tt></b><font
 face="Arial,Helvetica"> but not </font><b><tt><font size="+1">void</font></tt></b><font
 face="Arial,Helvetica">)</font></li>
    </ul>
    <li> <font face="Arial,Helvetica">an atomic <i>classname</i></font></li>
    <ul>
      <li> <font face="Arial,Helvetica">e.g. </font><b><tt><font
 size="+1">'java.lang.String'</font></tt></b></li>
      <li> <font face="Arial,Helvetica">e.g. </font><b><tt><font
 size="+1">'Ljava.lang.String;'</font></tt></b><font
 face="Arial,Helvetica">&nbsp;&nbsp; (a redundant but legitimate form)</font></li>
    </ul>
    <li> <font face="Arial,Helvetica">an atomic <i>descriptor</i></font></li>
    <ul>
      <li> <font face="Arial,Helvetica">e.g. </font><b><tt><font
 size="+1">'[I'</font></tt></b></li>
    </ul>
    <li> <font face="Arial,Helvetica">a class object</font></li>
    <ul>
      <li> <font face="Arial,Helvetica">i.e. an object whose type
is&nbsp; </font><b><tt><font size="+1">class([java,lang],['Class'])</font></tt></b></li>
    </ul>
  </ul>
</blockquote>
<blockquote><font face="Arial,Helvetica">if </font><b><tt><font
 size="+1">X</font></tt></b><font face="Arial,Helvetica"> denotes a
primitive
type and </font><b><tt><font size="+1">Argz</font></tt></b><font
 face="Arial,Helvetica"> is castable to a value of that type, then </font><b><tt><font
 size="+1">V</font></tt></b><font face="Arial,Helvetica"> is that value
(a
pointless mode of operation, but somehow complete...)</font></blockquote>
<blockquote><font face="Arial,Helvetica">if </font><b><tt><font
 size="+1">X</font></tt></b><font face="Arial,Helvetica"> denotes an
array
type and </font><b><tt><font size="+1">Argz</font></tt></b><font
 face="Arial,Helvetica"> is a non-negative integer, then </font><b><tt><font
 size="+1">V</font></tt></b><font face="Arial,Helvetica"> is a new
array
of that many elements, initialised to the appropriate default value</font></blockquote>
<blockquote><font face="Arial,Helvetica">if </font><b><tt><font
 size="+1">X</font></tt></b><font face="Arial,Helvetica"> denotes an
array
type and </font><b><tt><font size="+1">Argz</font></tt></b><font
 face="Arial,Helvetica"> is a list of datums, each of which is
(independently)
castable to the array element type, then </font><b><tt><font size="+1">V</font></tt></b><font
 face="Arial,Helvetica"> is a new array of as many elements as </font><b><tt><font
 size="+1">Argz</font></tt></b><font face="Arial,Helvetica"> has
members,
initialised to the results of casting the respective members of </font><b><tt><font
 size="+1">Argz</font></tt></b></blockquote>
<blockquote><font face="Arial,Helvetica">if </font><b><tt><font
 size="+1">X</font></tt></b><font face="Arial,Helvetica"> denotes a
non-array
object type and </font><b><tt><font size="+1">Argz</font></tt></b><font
 face="Arial,Helvetica"> is a list of datums, then </font><b><tt><font
 size="+1">V</font></tt></b><font face="Arial,Helvetica"> is the result
of
an invocation of that type's most specifically-typed constructor to
whose
respective parameters the members of </font><b><tt><font size="+1">Argz</font></tt></b><font
 face="Arial,Helvetica"> are assignable</font></blockquote>
<hr width="100%"> <br>
<b><tt><font size="+2"><a name="jpl_call4"></a>jpl_call( +X, +Method,
+Args, -R) :-</font></tt></b>
<blockquote><b><tt><font size="+1">X</font></tt></b><font
 face="Arial,Helvetica"> can be:</font>
  <blockquote> <li> <font face="Arial,Helvetica">a <i>type</i>, <i>class
object</i> or <i>classname</i> (for static methods of the denoted
class,
or for static or instance methods of java.lang.Class)</font></li>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote> <li> <font face="Arial,Helvetica">a class instance or
array
(for static or instance methods)</font></li>
  </blockquote>
  <b><tt><font size="+1">Method</font></tt></b><font
 face="Arial,Helvetica"> can be:</font>
  <blockquote> <li> <font face="Arial,Helvetica">an atomic method
name (if this name is ambiguous, as a result of method overloading,
then it will be resolved by considering the types of <span
 style="font-weight: bold;">Args</span>, as far as they can be inferred)</font></li>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote> <li> <font face="Arial,Helvetica">an integral method
index
(untested: for static overload resolution)</font></li>
  </blockquote>
</blockquote>
<blockquote>
  <blockquote> <li> <font face="Arial,Helvetica">a </font><b><tt><font
 size="+1">methodID/1</font></tt></b><font face="Arial,Helvetica">
structure
(ditto)</font></li>
  </blockquote>
  <b><tt><font size="+1">Args</font></tt></b><font
 face="Arial,Helvetica"> must be</font>
  <ul>
    <li> <font face="Arial,Helvetica">a proper list (possibly empty)
of ground
arguments</font></li>
  </ul>
  <font face="Arial,Helvetica">Finally, an attempt will be made to
unify </font><b><tt><font size="+1">R</font></tt></b><font
 face="Arial,Helvetica"> with the returned
result.</font></blockquote>
<hr width="100%"> <br>
<b><tt><font size="+2"><a name="jpl_set3"></a>jpl_set( +X, +Field, +V)
:-</font></tt></b>
<blockquote><font face="Arial,Helvetica">basically, sets the </font><b><tt><font
 size="+1">Fspec</font></tt></b><font face="Arial,Helvetica">-th field
of
object </font><b><tt><font size="+1">X</font></tt></b><font
 face="Arial,Helvetica"> to value </font><b><tt><font size="+1">V</font></tt></b>
  <p><b><tt><font size="+1">X</font></tt></b><font
 face="Arial,Helvetica"> can be:</font> </p>
  <ul>
    <li> <font face="Arial,Helvetica">a <i>class object</i>, a <i>classname</i>,
or an (object or array) <i>type</i> (for static fields, or
java.lang.Class fields)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">a <i>class instance</i> (for
non-static
fields)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">an <i>array</i> (for indexed
element or
subrange assignment)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">but not a <i>string</i> (no
fields to
retrieve)</font></li>
  </ul>
  <b><tt><font size="+1">Field</font></tt></b><font
 face="Arial,Helvetica"> can be:</font>
  <ul>
    <li> <font face="Arial,Helvetica">an atomic field name
(overloading will
be resolved dynamically, by considering the inferred type of <span
 style="font-family: helvetica,arial,sans-serif; font-weight: bold;">V</span>)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">an integral field index (static
resolution: not tried yet)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">a </font><b><tt><font size="+1">fieldID/1</font></tt></b><font
 face="Arial,Helvetica"> (static resolution: not tried yet)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">a variable (field names, or
array indices, are generated)(?!)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">an array index </font><b><tt><font
 size="+1">I</font></tt></b><font face="Arial,Helvetica"> (</font><b><tt><font
 size="+1">X</font></tt></b><font face="Arial,Helvetica"> must be an
array
object: </font><b><tt><font size="+1">X[I]</font></tt></b><font
 face="Arial,Helvetica"> is assigned </font><b><tt><font size="+1">V</font></tt></b><font
 face="Arial,Helvetica">)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">a pair </font><b><tt><font
 size="+1">I-J</font></tt></b><font face="Arial,Helvetica"> of integers
(</font><b><tt><font size="+1">J</font></tt></b><font
 face="Arial,Helvetica"> can be a variable) (</font><b><tt><font
 size="+1">X</font></tt></b><font face="Arial,Helvetica"> must be an
array
object, </font><b><tt><font size="+1">V</font></tt></b><font
 face="Arial,Helvetica"> must be a list of values: </font><b><tt><font
 size="+1">X[I-J]</font></tt></b><font face="Arial,Helvetica"> will be
assigned </font><b><tt><font size="+1">V</font></tt></b><font
 face="Arial,Helvetica">)</font></li>
  </ul>
  <b><tt><font size="+1">V</font></tt></b><font face="Arial,Helvetica">
must be ground (although one day we may pass variables to <span
 style="font-weight: bold; font-style: italic;">JPL</span>?!)</font></blockquote>
<hr width="100%"> <br>
<b><tt><font size="+2"><a name="jpl_get3"></a>jpl_get( +X, +Field, -V)
:-</font></tt></b>
<blockquote><b><tt><font size="+1">X</font></tt></b><font
 face="Arial,Helvetica"> can be:</font>
  <ul>
    <li> <font face="Arial,Helvetica">a <i>class object</i>, a <i>classname</i>,
or an (object or array) <i>type</i> (for static fields, or
java.lang.Class fields)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">a <i>class instance</i> (for
non-static
fields)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">an <i>array</i> (for the
'length' pseudo
field, or for indexed element retrieval)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">but not a String (clashes with
classname; anyway, java.lang.String has no fields to retrieve)</font></li>
  </ul>
  <b><tt><font size="+1">Field</font></tt></b><font
 face="Arial,Helvetica"> can be</font>
  <ul>
    <li> <font face="Arial,Helvetica">an atomic field name</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">or an integral field index
(these are
a secret :-)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">or a </font><b><tt><font
 size="+1">fieldID/1</font></tt></b><font face="Arial,Helvetica"> (not
for general consumption :-)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">or an integral array index
(high-bound
checking is done by JVM, maybe throwing an exception)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">or a variable (field names, or
array indices, are generated)</font></li>
  </ul>
</blockquote>
<blockquote>
  <ul>
    <li> <font face="Arial,Helvetica">or a pair </font><b><tt><font
 size="+1">I-J</font></tt></b><font face="Arial,Helvetica"> of integers
or
variables (array subranges are generated) (relational or what?!)<br>
      </font></li>
  </ul>
  <font face="Arial,Helvetica">Immediately before <span
 style="font-weight: bold;">jpl_get/4</span> returns</font><font
 face="Arial,Helvetica">, an attempt will be made to unify </font><b><tt><font
 size="+1">V</font></tt></b><font face="Arial,Helvetica"> with the
internally computed result.</font></blockquote>
<hr width="100%">
<h2><a name="exceptions"></a> exceptions thrown by Java<br>
</h2>
<font face="Arial,Helvetica">Uncaught exceptions thrown by the JVM in
the course of
handling a <span style="font-weight: bold; font-style: italic;">JPL</span><span
 style="font-weight: bold;">
3.x Prolog API</span> call are mapped onto Standard Prolog exceptions,
e.g.</font>
<blockquote>
  <pre><b><tt><font size="+1">jpl_new( 'java.util.Date', [yesterday], D)</font></tt></b></pre>
</blockquote>
<font face="Arial,Helvetica">raises the Prolog exception</font>
<blockquote><b><tt><font size="+1">java_exception('java.lang.IllegalArgumentException',
@'J#0008408972')</font></tt></b></blockquote>
<font face="Arial,Helvetica">because, as the exception suggests, <span
 style="font-weight: bold;">yesterday </span>is not a valid
constructor argument.</font><br>
&nbsp;<br>
<span style="font-family: helvetica,arial,sans-serif;">Java exceptions
are always returned as Prolog exceptions with this structure:</span><br
 style="font-family: helvetica,arial,sans-serif;">
<blockquote><b><tt><font size="+1">java_exception( <span
 style="font-style: italic;">classname</span>, <span
 style="font-style: italic;">reference_to_exception_object</span>)</font></tt></b></blockquote>
<hr width="100%">
<h2><a name="testing"></a>testing</h2>
<font face="Arial,Helvetica">For a rudimentary test, run</font>
<blockquote>
  <pre><b><tt><font size="+1">?- jpl_demo.</font></tt></b></pre>
</blockquote>
<font face="Arial,Helvetica">and wait patiently for some Swing windows
to
appear (but not too patiently, in case something is wrong...)</font>
<p> </p>
<hr width="100%">
<h2><a name="to_do"></a> to do</h2>
<font face="Arial,Helvetica">Apart from any bugs I don't know about,
this interface is usable and useful as it stands.&nbsp; Nevertheless
there are some things "to do" at some stage in the future, e.g.</font>
<ul>
  <li> <font face="Arial,Helvetica">support non-virtual method calls
(i.e.
explicitly call a method of some ancestor class despite there being an
overriding method (i.e. of the same name etc.) in a "nearer" class).
&nbsp;I believe
this is a fairly arcane Java feature, but it is needed for
completeness;
I want to accommodate it without complicating the syntax of regular
method
calls.</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">map the JVM's </font><b><tt><font
 size="+1">vprintf()</font></tt></b><font face="Arial,Helvetica">
messages
onto something in SWI-Prolog (the user_error stream?)</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">catch the JVM's </font><b><tt><font
 size="+1">abort()</font></tt></b><font face="Arial,Helvetica"> and </font><b><tt><font
 size="+1">exit()</font></tt></b><font face="Arial,Helvetica"> events,
and
handle them appropriately (e.g. stop a Java <i>abort</i> from killing
the
SWI-Prolog process)</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">propagate SWI-Prolog's ABORT
action into
the JVM as appropriate, e.g. to interrupt a pending <span
 style="font-weight: bold; font-style: italic;">JPL</span> call</font></li>
</ul>
<ul>
  <li> <font face="Arial,Helvetica">reduce the (extravagant) overheads
of
each <span style="font-weight: bold; font-style: italic;">JPL</span>
call
(without compromising functionality or safety)</font></li>
</ul>
<hr width="100%">
<address> <font size="+1"><a href="mailto:paul.singleton@bcs.org.uk">Paul
Singleton</a></font></address>
<address> <font size="+1">drafted 10th November 2000</font></address>
<address> <font size="+1">revised 14th December 2000<br>
</font>
</address>
<address> <font size="+1">revised 11th March 2003<br>
revised 18th February 2004<br>
<br>
</font></address>
<br>
<br>
</body>
</html>