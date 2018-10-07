# JPL 7.4+ - Java - SWI Prolog Interface

JPL is a "library using the SWI-Prolog foreign interface and the Java jni interface providing a _bidirectional interface between Java and Prolog_ that can be used to embed Prolog in Java _as well_ as for embedding Java in Prolog. In both setups it provides a reentrant bidirectional interface."

As far as I can tell, JPL has been developed originally by Paul Singleton and maintained by him and Jan Wielemaker (and possibly others?).

JPL has been integrated into the full SWI-Prolog distribution starting with version 5.4.x, including binaries for MS-Windows and a Linux RPM. 

There was an initial [JPL 3.x](http://www.swi-prolog.org/packages/jpl/), now discontinued, which then evolved into the current [JPL 7.4](https://jpl7.org/). 

The **objectives of JPL** are to:

* enable Prolog applications to exploit any Java classes, instances, methods etc. (without requiring any wrappers, metadata etc. to be set up first);
* enable Java applications to manipulate any Standard Prolog libraries, predicates, etc. (without requiring any wrappers, metadata etc. to be set up first);
* enable hybrid Prolog+Java applications to be designed and implemented so as to take best advantage of both language systems, and to be testable, debuggable, maintainable.

To learn about JPL refer to the main [JPL 7 site])(https://jpl7.org/), which includes the Java API (to access Prolog from Java) and the Prolog API (to access Java from Prolog), and our [Wiki](https://github.com/ssardina-research/packages-jpl/wiki) with some useful information and guides on how to use (and understand) JPL.


## WHY THIS FORK THEN?

I have forked the main [JPL Github repo](https://github.com/SWI-Prolog/packages-jpl) mainly for two reasons:

1. Fix this issue [#9](<https://github.com/SWI-Prolog/packages-jpl/issues/9>) that preclude Java to perform queries on specific SWI modules.
  * This issue has been resoled and merged into the master branch of JPL in pull request [#10](https://github.com/SWI-Prolog/packages-jpl/pull/10) and hopefully will be available in SWI 7.7.X release.
2. Expose JPL repo as a Maven repo, so it can be obtained automatically as a dependency via [JitPack](https://jitpack.io/).

More generaly, I am using SWI+JPL to provide Prolog knowledgebase management to [SARL agent systems](http://www.sarl.io/). To do so, I have developed a SARL Capacity and Skill [SARL-PROLOG-CAP](https://bitbucket.org/ssardina-research/sarl-prolog-cap) that SARL agents can use to access SWI Prolog knowledgbases.  

One example of such use is the SARL Agent System to play the [2017 Multi-Agent Agents in City game](https://multiagentcontest.org/2017/). A base sytem, showcasing how to use SWI Prolog via JPL, can be obtained in [this repo](https://bitbucket.org/ssardina-research/sarl-agtcity-base). 

## Important Links

* The main current JPL 7 site: <https://jpl7.org/)
  * It includes the Java API (to access Prolog from Java) and the Prolog API (to access Java from Prolog).
* The main Github repository for JPL 7 (which this one is a fork of): <https://github.com/SWI-Prolog/packages-jpl> 
* The documentation of SWI `library(jpl)`: <http://www.swi-prolog.org/pldoc/man?section=jpl>
* [Our Wiki](https://github.com/ssardina-research/packages-jpl/wiki) with some useful information and guides.
* The old JPL 3.x documentation: <http://www.swi-prolog.org/packages/jpl/>


## CONTACT

Sebastian Sardina - ssardina@gmail.com (for this fork)




