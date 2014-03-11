#!/bin/bash

echo "."
read -p "This will publish the current version to sonatype ==> CHECK THE VERSION NUMBER"
echo "."

./sbt clean test && git push origin master && ./sbt "+ publish"

echo "."
echo "Now release via Nexus/Sonatype => https://oss.sonatype.org"
echo " - Log in as me"
echo " - go to the STAGING repositories"
echo " - Close the repo"
echo " - Release the repo"
echo " - Wait a couple of hours"
echo "."
echo "For more, see:"
echo " - https://docs.sonatype.org/display/Repository/Sonatype+OSS+Maven+Repository+Usage+Guide"
echo " - https://docs.sonatype.org/display/Repository/Sonatype+OSS+Maven+Repository+Usage+Guide"
echo " - http://www.cakesolutions.net/teamblogs/2012/01/28/publishing-sbt-projects-to-nexus   (See Channing's comment)"
echo "."
