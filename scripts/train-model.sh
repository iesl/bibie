if [ "$BIBROOT" = "" ]
then
    echo "set BIBROOT environment variable first"
else
    trainfile="umass-citation/training.docs"
    testfile="umass-citation/testing.docs"
    memSize="40G"
    lexiconPath="file://$BIBROOT/src/main/resources/lexicons"
    java -Xmx${memSize} -cp target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar \
    edu.umass.cs.iesl.bibie.TrainCitationModel --train-file=$trainfile --test-file=$testfile --lexicons=$lexiconPath
fi
