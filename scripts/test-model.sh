if [ "$BIBROOT" = "" ]
then
    echo "set BIBROOT environment variable first"
else
    testfile="umass-citation/testing.docs"
    modelUrl="file://citationCRF.factorie"
    memSize="40G"
    lexiconPath="file://$BIBROOT/src/main/resources/lexicons"
    java -Xmx${memSize} -cp target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar \
    edu.umass.cs.iesl.bibie.TestCitationModel --test-file=$testfile --model-url=$modelUrl --lexicons=$lexiconPath
fi