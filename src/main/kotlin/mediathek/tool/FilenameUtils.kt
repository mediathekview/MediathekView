/*
 * Copyright (c) 2014-2026 derreisende77.
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

package mediathek.tool

import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.io.File
import java.nio.CharBuffer
import java.nio.charset.Charset
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets
import java.text.Normalizer

class FilenameUtils private constructor() {
    companion object {
        /**
         * Valid characters for Windows in file names:
         * Based on <a href="http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx">MSDN sample</a>
         */
        const val REGEXP_ILLEGAL_CHARACTERS_WINDOWS: String = "[:\\\\/*?|<>\"]"
        const val REGEXP_ILLEGAL_CHARACTERS_WINDOWS_PATH: String = "[:/*?|<>\"]"

        /**
         * Valid characters for all UNIX-like OS.
         */
        const val REGEXP_ILLEGAL_CHARACTERS_OTHERS: String = "[:\\\\/*|<>]"
        const val REGEXP_ILLEGAL_CHARACTERS_OTHERS_PATH: String = "[:\\\\*|<>]"
        private val logger = LogManager.getLogger()

        fun checkFilenameForIllegalCharacters(name: String, isPath: Boolean): String {
            // dient nur zur Anzeige für Probleme (Textfeld wird rot)
            var ret = name
            var isWindowsPath = false

            if (SystemUtils.IS_OS_WINDOWS) {
                ret = removeWindowsTrailingDots(ret)
                if (isPath && ret.length > 1 && ret[1] == ':') {
                    // damit auch "d:" und nicht nur "d:\" als Pfad geht
                    isWindowsPath = true
                    ret = ret.replaceFirst(":", "") // muss zum Schluss wieder rein, kann aber so nicht ersetzt werden
                }
            } else {
                ret = stripStartingDots(ret)
            }

            ret = if (isPath) {
                convertPathToNativeEncoding(ret)
            } else {
                convertToNativeEncoding(ret, false)
            }

            if (isWindowsPath) {
                // c: wieder herstellen
                ret = when (ret.length) {
                    1 -> "$ret:"
                    else -> if (ret.length > 1) "${ret[0]}:${ret.substring(1)}" else ret
                }
            }

            return ret
        }

        /**
         * Remove all starting dots from a string, <b>if</b> it begins with them.
         *
         * @param input the input string
         * @return the stripped result
         */
        fun stripStartingDots(input: String): String =
            input.replaceFirst("^\\.+".toRegex(), "")

        /**
         * Remove stray trailing dots from string when we are on Windows OS.
         *
         * @param fileName A filename string that might include trailing dots.
         * @return Cleanup string with no dots anymore.
         */
        fun removeWindowsTrailingDots(fileName: String): String {
            var result = fileName
            // machte unter Win noch Probleme, zB. bei dem Titel: "betrifft: ..."
            // "." und " " am Ende machen Probleme
            while (result.isNotEmpty() && (result.endsWith(".") || result.endsWith(" "))) {
                result = result.substring(0, result.length - 1)
            }
            return result
        }

        /**
         * Convert a filename from Java´s native UTF-16 to OS native character encoding.
         *
         * @param fileName The UTF-16 filename string.
         * @return Natively encoded string for the OS.
         */
        private fun convertToNativeEncoding(fileName: String, isPath: Boolean): String {
            var ret = removeIllegalCharacters(fileName, isPath)

            // convert our filename to OS encoding...
            try {
                val charset = Charset.defaultCharset()
                val charsetEncoder = charset.newEncoder()
                charsetEncoder.onMalformedInput(CodingErrorAction.REPLACE) // otherwise breaks on first unconvertable char
                charsetEncoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
                charsetEncoder.replaceWith(byteArrayOf('_'.code.toByte()))

                val buf = charsetEncoder.encode(CharBuffer.wrap(ret))
                ret = charset.decode(buf).toString()

                // remove NUL character from conversion...
                ret = ret.replace("\u0000", "")
            } catch (e: CharacterCodingException) {
                logger.error("convertToNativeEncoding", e)
            }

            return ret
        }

        /**
         * Convert a filename from Java´s native UTF-16 to US-ASCII character encoding.
         *
         * @param fileName The UTF-16 filename string.
         * @return US-ASCII encoded string for the OS.
         */
        fun convertToASCIIEncoding(fileName: String, isPath: Boolean): String {
            var ret = fileName
            // remove NUL character from conversion...
            ret = ret.replace("\u0000", "")

            ret = transliterateToAscii(ret)
            ret = removeIllegalCharacters(ret, isPath)

            // convert our filename to OS encoding...
            try {
                val charsetEncoder = StandardCharsets.US_ASCII.newEncoder()
                charsetEncoder.onMalformedInput(CodingErrorAction.REPLACE) // otherwise breaks on first unconvertable char
                charsetEncoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
                charsetEncoder.replaceWith(byteArrayOf('_'.code.toByte()))

                val buf = charsetEncoder.encode(CharBuffer.wrap(ret))
                ret = StandardCharsets.US_ASCII.decode(buf).toString()
            } catch (e: CharacterCodingException) {
                logger.error("convertToASCIIEncoding", e)
            }

            return ret
        }

        private fun convertPathToNativeEncoding(path: String): String =
            convertPathSegments(path, onlyAscii = false)

        private fun convertPathToASCIIEncoding(path: String): String =
            convertPathSegments(path, onlyAscii = true)

        private fun convertPathSegments(path: String, onlyAscii: Boolean): String = buildString(path.length) {
            var segmentStart = 0
            for (index in path.indices) {
                if (path[index] == File.separatorChar) {
                    appendConvertedPathSegment(path, segmentStart, index, onlyAscii)
                    append(File.separatorChar)
                    segmentStart = index + 1
                }
            }
            appendConvertedPathSegment(path, segmentStart, path.length, onlyAscii)
        }

        private fun StringBuilder.appendConvertedPathSegment(
            path: String,
            start: Int,
            end: Int,
            onlyAscii: Boolean,
        ) {
            if (start == end) {
                return
            }

            val segment = path.substring(start, end)
            append(
                if (onlyAscii) {
                    convertToASCIIEncoding(segment, false)
                } else {
                    convertToNativeEncoding(segment, false)
                },
            )
        }

        /**
         * Convert Unicode text into a filename-safe ASCII representation without ICU4J.
         * German substitutions are applied first, then combining marks are stripped.
         */
        private fun transliterateToAscii(input: String): String {
            var ret = input

            ret = ret.replace("ä", "ae")
                .replace("ö", "oe")
                .replace("ü", "ue")
                .replace("Ä", "AE")
                .replace("Ö", "OE")
                .replace("Ü", "UE")
                .replace("ß", "ss")
                .replace("ẞ", "SS")

            // Characters that are not decomposed to ASCII by NFD.
            ret = ret.replace("ł", "l")
                .replace("Ł", "L")
                .replace("đ", "d")
                .replace("Đ", "D")
                .replace("ø", "o")
                .replace("Ø", "O")
                .replace("ð", "d")
                .replace("Ð", "D")
                .replace("þ", "th")
                .replace("Þ", "Th")
                .replace("œ", "oe")
                .replace("Œ", "OE")
                .replace("æ", "ae")
                .replace("Æ", "AE")

            ret = Normalizer.normalize(ret, Normalizer.Form.NFD)
            ret = ret.replace("\\p{M}+".toRegex(), "")
            return ret
        }

        /**
         * Remove illegal characters from String based on current OS.
         *
         * @param input The input string
         * @param isPath true if this is a path.
         * @return Cleaned-up string.
         */
        fun removeIllegalCharacters(input: String, isPath: Boolean): String {
            var ret = input

            if (SystemUtils.IS_OS_WINDOWS) {
                // we need to be more careful on Windows when using e.g. FAT32
                // Therefore be more conservative by default and replace more characters.
                ret = removeWindowsTrailingDots(ret)
                ret = ret.replace(
                    (if (isPath) REGEXP_ILLEGAL_CHARACTERS_WINDOWS_PATH else REGEXP_ILLEGAL_CHARACTERS_WINDOWS)
                        .toRegex(),
                    "_",
                )
            } else if (SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_MAC_OSX) {
                // On OSX the VFS take care of writing correct filenames to FAT filesystems...
                // Just remove the default illegal characters
                ret = stripStartingDots(ret)
                ret = ret.replace(
                    (if (isPath) REGEXP_ILLEGAL_CHARACTERS_OTHERS_PATH else REGEXP_ILLEGAL_CHARACTERS_OTHERS)
                        .toRegex(),
                    "_",
                )
            } else {
                error("Unsupported OS: ${SystemUtils.OS_NAME}")
            }

            return ret
        }

        /**
         * Entferne verbotene Zeichen aus Dateiname.
         *
         * @param name Dateiname
         * @return Bereinigte Fassung
         */
        fun replaceLeerDateiname(name: String, isPath: Boolean, userReplace: Boolean, onlyAscii: Boolean): String {
            var ret = name
            var isWindowsPath = false
            if (SystemUtils.IS_OS_WINDOWS && isPath && ret.length > 1 && ret[1] == ':') {
                // damit auch "d:" und nicht nur "d:\" als Pfad geht
                isWindowsPath = true
                ret = ret.replaceFirst(":", "") // muss zum Schluss wieder rein, kann aber so nicht ersetzt werden
            }

            // zuerst die Ersetzungstabelle mit den Wünschen des Users
            if (userReplace) {
                ret = ReplaceList.replace(ret, isPath)
            }

            // und wenn gewünscht: "NUR Ascii-Zeichen"
            ret = if (onlyAscii) {
                if (isPath) convertPathToASCIIEncoding(ret) else convertToASCIIEncoding(ret, false)
            } else {
                if (isPath) convertPathToNativeEncoding(ret) else convertToNativeEncoding(ret, false)
            }

            if (isWindowsPath) {
                // c: wieder herstellen
                ret = when (ret.length) {
                    1 -> "$ret:"
                    else -> if (ret.length > 1) "${ret[0]}:${ret.substring(1)}" else ret
                }
            }
            return ret
        }
    }
}
