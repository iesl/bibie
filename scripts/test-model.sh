testfile="umass-citation/testing.docs"
modelUrl="file://citationCRF.factorie"
memSize="40G"

java -Xmx${memSize} -cp target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar \
edu.umass.cs.iesl.bibie.TestCitationModel --test-file=$testfile --model-url=$modelUrl