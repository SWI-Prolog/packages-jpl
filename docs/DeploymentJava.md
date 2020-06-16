## Which Java: Oracle Java JDK vs OpenJDK?

One can use Oracle JDK or OpenJDK; check a comparison [here](https://www.baeldung.com/oracle-jdk-vs-openjdk).

Note there has been some changes in Licenses from Java SE 11. The changes are fairly complex but a good sumary and explanation of impact can be found [here](https://blog.jetbrains.com/idea/2018/09/using-java-11-in-production-important-things-to-know/)

The current guide/documentation has been produced using the [Oracle Java](https://www.oracle.com/java/) SE 8.

However, others have reported success with [OpenJDK 8 with OpenJ9 as JVM](https://github.com/ssardina-research/packages-jpl/issues/23) (using the Hotspot may yield a fatal error). The new Java can be obtained from [AdaptOpenJDK](https://adoptopenjdk.net/).  

On the other hand, errors have been reported when using:

1. OpenJDK 8 with HotSpot VM; see [here](https://github.com/ssardina-research/packages-jpl/issues/23).
2. AdoptOpenJDK 11 (Hotspot), AdoptOpenJDK 11 (OpenJ9), and OracleJDK 13 (Hotspot); see [here](https://github.com/SWI-Prolog/packages-jpl/issues/34)




