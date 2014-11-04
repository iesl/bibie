NOTE: this code still needs a good amount of cleanup but works fine for basic citation extraction.

TO USE:

To use programmatically, call TestCitationModel.loadModel with the url of the trained model and lexicons. Then call TestCitationModel.process with a sequence of documents and the model, an URL for a trained model (which can be on the classpath using "classpath:" prefix or just a regular file URL), and optionally an URL for the lexicons (which are baked into the jar and so should just work). Each document you pass in should be one citation.

To train a model, call the TrainCitationModel class from the command line using scripts/train-model.sh

