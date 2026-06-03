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

import mediathek.gui.messages.ReplaceListChangedEvent
import java.io.File

object ReplaceList {
    const val REPLACELIST: String = "Ersetzungstabelle"
    const val VON: String = "von"
    const val VON_NR: Int = 0
    const val NACH: String = "nach"
    const val NACH_NR: Int = 1
    const val MAX_ELEM: Int = 2

    private val entries = mutableListOf<ReplaceEntry>()

    @JvmStatic
    fun columnNames(): Array<String> = arrayOf(VON, NACH)

    @JvmStatic
    fun init() {
        entries.clear()
        add(" ", "_")
    }

    @JvmStatic
    fun clear() {
        entries.clear()
    }

    @JvmStatic
    fun add(from: String, to: String) {
        entries.add(ReplaceEntry(from, to))
    }

    fun add(values: Array<String>) {
        add(
            from = values.getOrElse(VON_NR) { "" },
            to = values.getOrElse(NACH_NR) { "" },
        )
    }

    @JvmStatic
    fun removeAt(index: Int) {
        entries.removeAt(index)
    }

    @JvmStatic
    fun setFrom(index: Int, value: String) {
        entries[index].from = value
    }

    @JvmStatic
    fun setTo(index: Int, value: String) {
        entries[index].to = value
    }

    @JvmStatic
    fun entries(): List<ReplaceEntry> =
        entries.map { it.copy() }

    fun valuesForXml(): List<Array<String>> =
        entries.map { it.toArray() }

    @JvmStatic
    fun replace(strCheck: String, pfad: Boolean): String {
        var result = strCheck
        val iterator = entries.iterator()
        while (iterator.hasNext()) {
            val entry = iterator.next()

            // hat der Nutzer als Suchbegriff "leer" eingegeben, dann weg damit
            if (entry.from.isEmpty()) {
                iterator.remove()
                MessageBus.messageBus.publishAsync(ReplaceListChangedEvent())
                continue
            }

            // bei Pfaden darf / oder \ natürlich nicht entfernt werden
            if (pfad && entry.from == File.separator) {
                continue
            }

            result = result.replace(entry.from, entry.to)
        }
        return result
    }

    @JvmStatic
    fun check(): Boolean {
        for (i in entries.indices) {
            val entry = entries[i]
            for (k in i + 1 until entries.size) {
                val nextEntry = entries[k]
                if (entry.to.contains(nextEntry.from)) {
                    return true
                }
            }
        }
        return false
    }

    @JvmStatic
    fun up(idx: Int, up: Boolean): Int {
        val replace = entries.removeAt(idx)
        var newIndex = idx
        if (up) {
            if (newIndex > 0) {
                --newIndex
            }
        } else if (newIndex < entries.size) {
            ++newIndex
        }
        entries.add(newIndex, replace)
        return newIndex
    }
}
