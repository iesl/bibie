trainfile="umass-citation/training.docs"
testfile="umass-citation/testing.docs"
memSize="40G"

java -Xmx${memSize} -cp target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar \
edu.umass.cs.iesl.bibie.TrainCitationModel --train-file=$trainfile --test-file=$testfile