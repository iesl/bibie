package edu.umass.cs.iesl.bibie.load

import edu.umass.cs.iesl.bibie.model.{CitationCRFModel, CitationLabelDomain, CitationFeaturesDomain}
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

import cc.factorie.util.CubbieConversions._
import cc.factorie.util.BinarySerializer

import java.io._
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
    val modelPath = if (!path.startsWith("file://")) "file://" + path else path
    logger.info(s"deserializing model from $modelPath")
    val url = new java.net.URL(modelPath)
    val inputStream = url.openConnection.getInputStream
    logger.info("got inputStream")
    val dis = new DataInputStream(inputStream)
    BinarySerializer.deserialize(CitationLabelDomain, dis)
    logger.info(s"deserialized label domain (${CitationLabelDomain.size} categories)")
    CitationLabelDomain.freeze()
    BinarySerializer.deserialize(CitationFeaturesDomain, dis)
    logger.info(s"deserialized feature domain (${CitationFeaturesDomain.dimensionSize} dimensions)")
    CitationFeaturesDomain.freeze()
    BinarySerializer.deserialize(model, dis)
    logger.info("deserialized model.")
    dis.close()
    model
  }

  def serializeModel(path: String, model: CitationCRFModel): Unit = {
    val modelPath = if (!path.startsWith("file://")) "file://" + path else path
    val url = new java.net.URL(modelPath)
    val os = url.openConnection.getOutputStream
    val dos = new DataOutputStream(os)
    if (!CitationLabelDomain.frozen) CitationLabelDomain.freeze()
    BinarySerializer.serialize(CitationLabelDomain, dos)
    logger.info(s"serialized label domain (${CitationLabelDomain.size} categories)")
    BinarySerializer.serialize(CitationFeaturesDomain, dos)
    logger.info(s"serialized feature domain (${CitationFeaturesDomain.dimensionSize} dimensions)")
    BinarySerializer.serialize(model, dos)
    logger.info("serialized model.")
    dos.close()
  }

}
