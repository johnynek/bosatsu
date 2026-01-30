package dev.bosatsu.codegen.js

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Source Map V3 Generator for Bosatsu → JavaScript compilation.
 *
 * Generates source maps according to the Source Map V3 specification,
 * allowing browser DevTools to show original .bosatsu source.
 *
 * @see https://tc39.es/ecma426/ (Source Map V3 Spec)
 */
object SourceMapGenerator {

  /**
   * A single mapping from generated JS position to original Bosatsu position.
   *
   * @param generatedLine 0-indexed line in generated JS
   * @param generatedColumn 0-indexed column in generated JS
   * @param sourceIndex Index into the sources array
   * @param originalLine 0-indexed line in original source
   * @param originalColumn 0-indexed column in original source
   * @param nameIndex Optional index into the names array
   */
  case class Mapping(
    generatedLine: Int,
    generatedColumn: Int,
    sourceIndex: Int,
    originalLine: Int,
    originalColumn: Int,
    nameIndex: Option[Int] = None
  )

  /**
   * VLQ Base64 encoder for source map mappings.
   *
   * VLQ (Variable-Length Quantity) encoding uses 5 bits per digit with
   * a continuation bit, encoded in Base64.
   */
  object VLQ {
    private val Base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    /**
     * Encode a signed integer as a VLQ Base64 string.
     * Negative numbers use the LSB as a sign bit.
     */
    def encode(value: Int): String = {
      // Convert signed to unsigned with sign in LSB
      val vlq = if (value < 0) ((-value) << 1) | 1 else value << 1
      encodeUnsigned(vlq)
    }

    @annotation.tailrec
    private def encodeUnsigned(vlq: Int, acc: StringBuilder = new StringBuilder): String = {
      val digit = vlq & 31  // Take 5 bits
      val rest = vlq >>> 5
      if (rest > 0) {
        acc += Base64Chars(digit | 32)  // Set continuation bit
        encodeUnsigned(rest, acc)
      } else {
        acc += Base64Chars(digit)
        acc.toString
      }
    }

    /**
     * Encode a sequence of values as VLQ, used for multi-value segments.
     */
    def encodeSegment(values: Int*): String =
      values.map(encode).mkString
  }

  /**
   * Builds a source map from a collection of mappings.
   *
   * @param file Name of the generated file
   * @param sourceRoot Optional root path for sources
   * @param sources List of original source file paths
   * @param sourcesContent Optional list of source content (for embedded sources)
   * @param names List of symbol names referenced in mappings
   * @param mappings List of position mappings
   */
  case class SourceMap(
    file: String,
    sourceRoot: Option[String] = None,
    sources: List[String],
    sourcesContent: Option[List[String]] = None,
    names: List[String],
    mappings: List[Mapping]
  ) {
    /**
     * Escape a string for JSON output.
     */
    private def escapeJson(s: String): String =
      s.replace("\\", "\\\\")
        .replace("\"", "\\\"")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")

    /**
     * Generate the V3 source map JSON string.
     */
    def toJSON: String = {
      val mappingsStr = encodeMappings()
      val sourcesJson = sources.map(s => s""""${escapeJson(s)}"""").mkString("[", ",", "]")
      val namesJson = names.map(n => s""""${escapeJson(n)}"""").mkString("[", ",", "]")
      val sourceRootJson = sourceRoot.map(sr => s""""sourceRoot":"${escapeJson(sr)}",""").getOrElse("")
      val sourcesContentJson = sourcesContent.map { contents =>
        val escaped = contents.map { c =>
          if (c == null) "null"
          else s""""${escapeJson(c)}""""
        }
        s""""sourcesContent":${escaped.mkString("[", ",", "]")},"""
      }.getOrElse("")

      s"""{
  "version": 3,
  "file": "${escapeJson(file)}",
  $sourceRootJson$sourcesContentJson"sources": $sourcesJson,
  "names": $namesJson,
  "mappings": "$mappingsStr"
}"""
    }

    /**
     * Generate an inline data URL for embedding in JS/HTML.
     */
    def toInlineComment: String = {
      val json = toJSON
      val base64 = java.util.Base64.getEncoder.encodeToString(json.getBytes("UTF-8"))
      s"//# sourceMappingURL=data:application/json;charset=utf-8;base64,$base64"
    }

    /**
     * Encode all mappings to the V3 mappings string format.
     *
     * Format: semicolon-separated lines, comma-separated segments within lines.
     * Each segment is VLQ-encoded with relative values:
     * - Generated column (relative to previous segment in same line, or 0)
     * - Source file index (relative to previous mapping)
     * - Original line (relative to previous mapping)
     * - Original column (relative to previous mapping)
     * - Name index (relative to previous mapping, optional)
     */
    private def encodeMappings(): String = {
      if (mappings.isEmpty) return ""

      // Sort mappings by generated position
      val sorted = mappings.sortBy(m => (m.generatedLine, m.generatedColumn))

      // Track previous values for relative encoding
      var prevGenCol = 0
      var prevSourceIdx = 0
      var prevOrigLine = 0
      var prevOrigCol = 0
      var prevNameIdx = 0
      var prevGenLine = 0

      val lines = new ListBuffer[String]()
      val currentLine = new ListBuffer[String]()

      for (mapping <- sorted) {
        // Handle line breaks - emit empty strings for skipped lines
        while (prevGenLine < mapping.generatedLine) {
          lines += currentLine.mkString(",")
          currentLine.clear()
          prevGenLine += 1
          prevGenCol = 0  // Reset column at new line
        }

        // Encode the segment with relative values
        val genColDelta = mapping.generatedColumn - prevGenCol
        val sourceIdxDelta = mapping.sourceIndex - prevSourceIdx
        val origLineDelta = mapping.originalLine - prevOrigLine
        val origColDelta = mapping.originalColumn - prevOrigCol

        val segment = mapping.nameIndex match {
          case Some(nameIdx) =>
            val nameIdxDelta = nameIdx - prevNameIdx
            prevNameIdx = nameIdx
            VLQ.encodeSegment(genColDelta, sourceIdxDelta, origLineDelta, origColDelta, nameIdxDelta)
          case None =>
            VLQ.encodeSegment(genColDelta, sourceIdxDelta, origLineDelta, origColDelta)
        }

        currentLine += segment

        // Update previous values
        prevGenCol = mapping.generatedColumn
        prevSourceIdx = mapping.sourceIndex
        prevOrigLine = mapping.originalLine
        prevOrigCol = mapping.originalColumn
      }

      // Don't forget the last line
      lines += currentLine.mkString(",")
      lines.mkString(";")
    }
  }

  /**
   * Mutable builder for constructing source maps during code generation.
   */
  class Builder(
    file: String,
    sourceRoot: Option[String] = None
  ) {
    private val sources = new ArrayBuffer[String]()
    private val sourcesContent = new ArrayBuffer[String]()
    private val names = new ArrayBuffer[String]()
    private val mappings = new ArrayBuffer[Mapping]()
    private var hasSourcesContent = false

    /**
     * Register a source file and return its index.
     */
    def addSource(path: String, content: Option[String] = None): Int = {
      val idx = sources.indexOf(path)
      if (idx >= 0) idx
      else {
        sources += path
        content match {
          case Some(c) =>
            hasSourcesContent = true
            sourcesContent += c
          case None =>
            sourcesContent += null.asInstanceOf[String]
        }
        sources.length - 1
      }
    }

    /**
     * Register a name and return its index.
     */
    def addName(name: String): Int = {
      val idx = names.indexOf(name)
      if (idx >= 0) idx
      else {
        names += name
        names.length - 1
      }
    }

    /**
     * Add a mapping from generated position to original position.
     *
     * @param genLine Generated JS line (0-indexed)
     * @param genCol Generated JS column (0-indexed)
     * @param sourceIdx Source file index from addSource
     * @param origLine Original source line (0-indexed)
     * @param origCol Original source column (0-indexed)
     * @param name Optional symbol name
     */
    def addMapping(
      genLine: Int,
      genCol: Int,
      sourceIdx: Int,
      origLine: Int,
      origCol: Int,
      name: Option[String] = None
    ): Unit = {
      val nameIdx = name.map(addName)
      mappings += Mapping(genLine, genCol, sourceIdx, origLine, origCol, nameIdx)
    }

    /**
     * Build the final SourceMap.
     */
    def build(): SourceMap = {
      SourceMap(
        file = file,
        sourceRoot = sourceRoot,
        sources = sources.toList,
        sourcesContent = if (hasSourcesContent) Some(sourcesContent.toList) else None,
        names = names.toList,
        mappings = mappings.toList
      )
    }
  }
}
