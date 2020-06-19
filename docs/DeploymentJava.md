## Which Java: Oracle Java JDK vs OpenJDK?

The **source version** for JPL 7.6.1 is Java 1.7, so no advanced features like lambdas are used. Next versions of JPL will probably use 1.8+ language features.

One can use Oracle JDK or OpenJDK; check a comparison [here](https://www.baeldung.com/oracle-jdk-vs-openjdk).

Note there has been some changes in Licenses from Java SE 11. The changes are fairly complex but a good summary and explanation of impact can be found [here](https://blog.jetbrains.com/idea/2018/09/using-java-11-in-production-important-things-to-know/)

The current guide/documentation has been produced using the [Oracle Java](https://www.oracle.com/java/) SE 8.

However, others have reported success with [OpenJDK 8 with OpenJ9 as JVM](https://github.com/ssardina-research/packages-jpl/issues/23) (using the Hotspot may yield a fatal error). The new Java can be obtained from [AdaptOpenJDK](https://adoptopenjdk.net/).  

On the other hand, errors have been reported when using:

1. OpenJDK 8 with HotSpot VM; see [here](https://github.com/ssardina-research/packages-jpl/issues/23).
2. AdoptOpenJDK 11 (Hotspot), AdoptOpenJDK 11 (OpenJ9), and OracleJDK 13 (Hotspot); see [here](https://github.com/SWI-Prolog/packages-jpl/issues/34)



## Java API `jpl.jar` via Maven

While the C Native library `libjpl.so` and Prolog API `jpl.pl` do not change much, the Java API provided in `jpl.jar` does tend to change and be updated more frequently to provide a better Prolog access from Java.

Because of this one may want to use a particular SWIPL standard install, like the latest 8.2.0 from the [Ubuntu PPA](https://www.swi-prolog.org/build/PPA.html), but use a more updated Java API `jpl.jar` that the one coming with such release.

One can then grab the latest JAR file from the [packages section in JPL repo](https://github.com/SWI-Prolog/packages-jpl/packages) or even better add the JPL as a Maven dependency of the Java application by including two repositories: GitHub Packages or JitPack (recommended as no token-based authorization is needed).

### Via JitPack

(JitPack](https://jitpack.io/) is a service that can serve maven artifacts by accessing GitHub repositores. JitPack will clone a Maven project from GitHub (in this case JPL's repo), compile it, and serve the JAR artifacts.

The first step is to add the following repository to the POM's application:

```xml
<!-- JitPack used for getting Maven packages from GitHub -->
<repository>
    <id>jitpack.io</id>
    <name>JitPack Repository</name>
    <url>https://jitpack.io</url>
</repository>
```        

and then include the following dependency:

```xml
<!--  JPL bidirectional Prolog-Java: https://github.com/SWI-Prolog/packages-jpl  -->
<dependency>
    <groupId>com.github.SWI-Prolog</groupId>
    <artifactId>packages-jpl</artifactId>
    <version>V8.3.2</version>	<!-- version 7.6.1 of JPL -->
</dependency>
```        
        
The `V8.3.2` version corresponds to a [tag in the repo](https://github.com/SWI-Prolog/packages-jpl/releases):
 
* Tags `V8.2.x` correspond to [JPL 7.6.0](https://jpl7.org/ReleaseNotes760)
* Tags `V8.3.x` correspond to [JPL 7.6.1](https://jpl7.org/ReleaseNotes761)
        
        
#### Via GitHub Packages

The JPL JAR artifact can also be satisfied directly from the [GitHub Packaging system](https://help.github.com/en/packages/publishing-and-managing-packages/about-github-packages), without the need to go via JitPack. 

The drawback today (June 2020) is that one as to have authenticate to GitHub (via TOKEN) to even just have _read_ access to a package from GitHub see [this post](https://github.community/t/download-from-github-package-registry-without-authentication/14407/32). Indeed, the [GitHub documentation](https://help.github.com/en/packages/publishing-and-managing-packages/about-github-packages) explains:

> * To download and install packages from a repository, your token must have the read:packages scope, and your user account must have read permissions for the repository. If the repository is private, your token must also have the repo scope.

So, to get the JPL dependency via GitHub packages we need to do as follows. First, add the following dependency to the application's POM file:

```xml
<!--  JPL bidirectional Prolog-Java: https://github.com/SWI-Prolog/packages-jpl  -->
<dependency>
    <groupId>com.github.SWI-Prolog</groupId>
    <artifactId>packages-jpl</artifactId>
    <version>7.6.1</version>
</dependency>
```

Second, add the following repository from where to satisfy the dependency:

```xml
<repository>
  <id>com.github</id>
  <name>GitHub SWI-Prolog/packages-jpl Apache Maven Packages</name>
  <url>https://maven.pkg.github.com/SWI-Prolog/packages-jpl/</url>
</repository>
```

Finally, make sure you generate a GitHub personal token with read permission and add the following to your `~/.m2/settings.xml` (replace `USERNAME` and `TOKEN`):

```xml
<servers>
    <server>
      <id>com.github</id>
      <username>USERNAME</username>
      <password>TOKEN</password>
    </server>
</servers>
```
 
For more information see [Configuring Apache Maven for use with GitHub Packages](https://help.github.com/en/packages/using-github-packages-with-your-projects-ecosystem/configuring-apache-maven-for-use-with-github-packages).













