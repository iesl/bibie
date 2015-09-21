package edu.umass.cs.iesl.bibie.load

import edu.umass.cs.iesl.bibie.model.{CitationCRFModel, CitationLabelDomain, CitationFeaturesDomain}
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

import cc.factorie.util.CubbieConversions._
import cc.factorie.util.BinarySerializer

import java.io._
import java.net.URL
import java.util.logging.Logger
/**
 * @author Kate Silverstein 
 *         created on 9/20/15
 */
object IOHelper {

  private val logger = Logger.getLogger(getClass.getName)

  def deserializeModel(path: String, lexiconsPath: String): CitationCRFModel = {
    val lexicons = new DefaultLexicons(lexiconsPath)
    val model = new CitationCRFModel(lexicons)
    val fis = new FileInputStream(path)
    val is = new DataInputStream(fis)
    BinarySerializer.deserialize(CitationLabelDomain, is)
    CitationLabelDomain.freeze()
    logger.info(s"deserialized CitationLabelDomain: ${CitationLabelDomain.dimensionSize} categories")
    BinarySerializer.deserialize(CitationFeaturesDomain, is)
    CitationFeaturesDomain.freeze()
    logger.info(s"deserialized CitationFeaturesDomain: ${CitationFeaturesDomain.dimensionSize} dimensions")
    BinarySerializer.deserialize(model, is)
    logger.info(s"deserialized model: ${model.sparsity} sparsity")
    is.close()
    fis.close()
    model
  }

  def serializeModel(path: String, model: CitationCRFModel): Unit = {
    val fos = new FileOutputStream(path)
    val os = new DataOutputStream(fos)
    if (!CitationLabelDomain.frozen) CitationLabelDomain.freeze()
    BinarySerializer.serialize(CitationLabelDomain, os)
    logger.info(s"serialized CitationLabelDomain: ${CitationLabelDomain.dimensionSize} categories")
    BinarySerializer.serialize(CitationFeaturesDomain, os)
    logger.info(s"serialized CitationFeaturesDomain: ${CitationFeaturesDomain.dimensionSize} dimensions")
    BinarySerializer.serialize(model, os)
    logger.info(s"serialized model: ${model.sparsity} sparsity")
    os.close()
    fos.close()
  }

}
