/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool.subtitles.detector

import java.nio.file.Files
import java.nio.file.Path
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamConstants

/**
 * Detects timed-text file formats:
 * - WebVTT (header-based detection; strict validation provided in result)
 * - TTML1 vs TTML2 (TTML2 detected via vocabulary markers; otherwise TTML1)
 */
object TimedTextFormatDetector {
    private const val TTML_NS = "http://www.w3.org/ns/ttml"

    private val TTML2_ELEMENTS = setOf(
        "animate",
        "animation",
        "audio",
        "chunk",
        "data",
        "font",
        "image",
        "resources",
        "source",
        "initial"
    )

    private val TTML2_ATTRIBUTES = setOf(
        "condition",
        "xml:base",
        "tts:backgroundImage",
        "tts:backgroundPosition",
        "tts:ruby",
        "tts:textShadow",
        "tts:textEmphasis"
    )

    /**
     * @param requireAtLeastOneVttCue if true, WebVTT must contain at least one cue.
     */
    @JvmStatic
    @Throws(Exception::class)
    fun detect(path: Path, requireAtLeastOneVttCue: Boolean): Result {
        // 1) WebVTT: header-based detection + strict validation result
        val vtt = WebVttStrictValidator.validate(path, requireAtLeastOneVttCue)
        if (vtt.headerPresent) {
            return if (vtt.valid) {
                Result.ok(Format.WEBVTT, "Valid WebVTT, cues=${vtt.cueCount}")
            } else {
                Result.fail(Format.WEBVTT, "Invalid WebVTT: ${vtt.errors.joinToString(" | ")}")
            }
        }

        // 2) TTML sniff (XML/StAX)
        return when (detectTtmlVersion(path)) {
            TtmlVersion.TTML1 -> Result.ok(Format.TTML1, "TTML root detected; no TTML2 markers found.")
            TtmlVersion.TTML2 -> Result.ok(Format.TTML2, "TTML root detected; TTML2 marker(s) found.")
            TtmlVersion.NOT_TTML -> Result.fail(Format.UNKNOWN, "Not WebVTT, and XML root is not TTML <tt>.")
        }
    }

    private fun detectTtmlVersion(path: Path): TtmlVersion {
        try {
            Files.newInputStream(path).use { input ->
                val factory = XMLInputFactory.newFactory()
                factory.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE, true)
                factory.setProperty(XMLInputFactory.SUPPORT_DTD, false)
                factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)

                val reader = factory.createXMLStreamReader(input)
                var rootChecked = false

                while (reader.hasNext()) {
                    val event = reader.next()

                    if (event == XMLStreamConstants.START_ELEMENT) {
                        val localName = reader.localName
                        val namespace = reader.namespaceURI

                        if (!rootChecked) {
                            rootChecked = true
                            if (localName != "tt" || namespace != TTML_NS) {
                                return TtmlVersion.NOT_TTML
                            }
                        }

                        if (localName in TTML2_ELEMENTS) {
                            return TtmlVersion.TTML2
                        }

                        for (i in 0 until reader.attributeCount) {
                            val attrLocal = reader.getAttributeLocalName(i)
                            val attrPrefix = reader.getAttributePrefix(i)
                            val qualified = if (attrPrefix.isNullOrEmpty()) {
                                attrLocal
                            } else {
                                "$attrPrefix:$attrLocal"
                            }

                            if (qualified in TTML2_ATTRIBUTES || attrLocal in TTML2_ATTRIBUTES) {
                                return TtmlVersion.TTML2
                            }
                        }
                    }
                }

                return if (rootChecked) TtmlVersion.TTML1 else TtmlVersion.NOT_TTML
            }
        } catch (_: Exception) {
            return TtmlVersion.NOT_TTML
        }
    }

    enum class Format {
        WEBVTT,
        TTML1,
        TTML2,
        UNKNOWN
    }

    private enum class TtmlVersion {
        TTML1,
        TTML2,
        NOT_TTML
    }

    @JvmRecord
    data class Result(
        val format: Format,
        val valid: Boolean,
        val details: String
    ) {
        companion object {
            @JvmStatic
            fun ok(format: Format, details: String): Result {
                return Result(format, true, details)
            }

            @JvmStatic
            fun fail(format: Format, details: String): Result {
                return Result(format, false, details)
            }
        }
    }
}
