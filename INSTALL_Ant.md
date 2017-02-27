# Installation of Apache Ant

### Requirements
* Java environment installed.

### Installation

To install Apache Ant, do the following steps:

1. Download Ant (http://www.pirbot.com/mirrors/apache//ant/binaries/apache-ant-1.10.1-bin.tar.gz)
2. Uncompress the downloaded archive in a specific directory.
3. Set the environment variable `ANT_HOME` to the uncompressed directory. Add
   `${ANT_HOME}/bin` to your path.
4. From the `ANT_HOME` directory execute `ant -f fetch.xml -Ddest=system`   
