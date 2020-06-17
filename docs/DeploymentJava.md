## Which Java: Oracle Java JDK vs OpenJDK?

One can use Oracle JDK or OpenJDK; check a comparison [here](https://www.baeldung.com/oracle-jdk-vs-openjdk).

Note there has been some changes in Licenses from Java SE 11. The changes are fairly complex but a good sumary and explanation of impact can be found [here](https://blog.jetbrains.com/idea/2018/09/using-java-11-in-production-important-things-to-know/)

The current guide/documentation has been produced using the [Oracle Java](https://www.oracle.com/java/) SE 8.

However, others have reported success with [OpenJDK 8 with OpenJ9 as JVM](https://github.com/ssardina-research/packages-jpl/issues/23) (using the Hotspot may yield a fatal error). The new Java can be obtained from [AdaptOpenJDK](https://adoptopenjdk.net/).  

On the other hand, errors have been reported when using:

1. OpenJDK 8 with HotSpot VM; see [here](https://github.com/ssardina-research/packages-jpl/issues/23).
2. AdoptOpenJDK 11 (Hotspot), AdoptOpenJDK 11 (OpenJ9), and OracleJDK 13 (Hotspot); see [here](https://github.com/SWI-Prolog/packages-jpl/issues/34)



## Java API `jpl.jar` via Maven

While the C Native library `libjpl.so` and Prolog API `jpl.pl` do not change much, the Java API provided in `jpl.jar` does tend to change and be updated more frequently to provide a better Prolog access from Java.

Because of this one may want to use a particular SWIPL standard install, like the latest 8.2.0 from the [Ubuntu PPA](https://www.swi-prolog.org/build/PPA.html), but use a more updated Java API `jpl.jar` that the one coming with such release.

One can then grab the latest JAR file from the [packages section in JPL repo](https://github.com/SWI-Prolog/packages-jpl/packages) or even better add the JPL as a Maven dependency of the Java application by including the following dependency in the POM:

```xml
<!--  JPL bidirectional Prolog-Java: https://github.com/SWI-Prolog/packages-jpl  -->
<dependency>
    <groupId>com.github.SWI-Prolog</groupId>
    <artifactId>packages-jpl</artifactId>
    <version>7.6.1</version>
</dependency>
```

and the following repository from where to satisfy the dependency:

```xml
<repository>
  <id>com.github</id>
  <name>GitHub SWI-Prolog/packages-jpl Apache Maven Packages</name>
  <url>https://maven.pkg.github.com/SWI-Prolog/packages-jpl/</url>
</repository>
```













