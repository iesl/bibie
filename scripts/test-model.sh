testfile="umass-citation/testing.docs"
modelUrl="file://citationCRF.factorie"
memSize="40G"

java -Xmx${memSize} -cp target/bibie-1.0-SNAPSHOT-jar-with-dependencies.jar \
bibie.TestCitationModel --test-file=$testfile --model-url=$modelUrl