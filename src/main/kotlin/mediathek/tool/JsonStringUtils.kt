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

package mediathek.tool

object JsonStringUtils {
    @JvmStatic
    fun escapeJsonString(value: String): String {
        val result = StringBuilder(value.length + 16)
        for (c in value) {
            when (c) {
                '"', '\\' -> {
                    result.append('\\')
                    result.append(c)
                }
                '\b' -> result.append("\\b")
                '\u000C' -> result.append("\\f")
                '\n' -> result.append("\\n")
                '\r' -> result.append("\\r")
                '\t' -> result.append("\\t")
                else -> {
                    if (c.code < 0x20) {
                        result.append("\\u")
                        result.append(c.code.toString(16).padStart(4, '0'))
                    } else {
                        result.append(c)
                    }
                }
            }
        }
        return result.toString()
    }

    @JvmStatic
    fun toJsonStringArray(values: Collection<String>): String {
        val result = StringBuilder(values.size * 8 + 2)
        result.append('[')
        var first = true
        for (value in values) {
            if (!first) {
                result.append(',')
            }
            first = false
            result.append('"')
            result.append(escapeJsonString(value))
            result.append('"')
        }
        result.append(']')
        return result.toString()
    }

    @JvmStatic
    fun unescapeJsonString(value: String): String {
        val result = StringBuilder(value.length)
        var i = 0
        while (i < value.length) {
            val c = value[i]
            if (c != '\\' || i + 1 >= value.length) {
                result.append(c)
                i++
                continue
            }

            i++
            when (val escaped = value[i]) {
                '"', '\\', '/' -> result.append(escaped)
                'b' -> result.append('\b')
                'f' -> result.append('\u000C')
                'n' -> result.append('\n')
                'r' -> result.append('\r')
                't' -> result.append('\t')
                'u' -> {
                    if (i + 4 < value.length) {
                        val hex = value.substring(i + 1, i + 5)
                        val decoded = hex.toIntOrNull(16)
                        if (decoded != null) {
                            result.append(decoded.toChar())
                        } else {
                            result.append("\\u").append(hex)
                        }
                        i += 4
                    } else {
                        result.append("\\u")
                    }
                }

                else -> result.append(escaped)
            }
            i++
        }
        return result.toString()
    }
}
