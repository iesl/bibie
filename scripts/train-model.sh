trainfile="umass-citation/training.docs"
testfile="umass-citation/testing.docs"
memSize="40G"

java -Xmx${memSize} -cp target/bibie-1.0-SNAPSHOT-jar-with-dependencies.jar \
bibie.TrainCitationModel --train-file=$trainfile --test-file=$testfile