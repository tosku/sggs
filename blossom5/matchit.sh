#! /bin/bash
basedir=~/Documents/sggs/
file=`ls -t  "$basedir"*matchdual | head -n1`
matchfile=$basedir`basename "$file" .matchdual`".matching"
echo $matchfile
./blossom5 -e "$file" -w "$matchfile" && \
echo "Done matching! :)" && \
exit 0
echo "Coulnd't match :("
exit 1
