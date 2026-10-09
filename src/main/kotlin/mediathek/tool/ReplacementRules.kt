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

class ReplacementRules {
    private val lock = Any()
    private val entries = mutableListOf<ReplaceEntry>()

    fun columnNames(): Array<String> = COLUMN_NAMES.copyOf()

    fun initDefaults() {
        synchronized(lock) {
            entries.clear()
            entries.add(ReplaceEntry(" ", "_"))
        }
    }

    fun clear() {
        synchronized(lock) {
            entries.clear()
        }
    }

    fun add(from: String, to: String): Boolean {
        if (from.isEmpty()) {
            return false
        }
        synchronized(lock) {
            entries.add(ReplaceEntry(from, to))
        }
        return true
    }

    fun add(values: Array<String>): Boolean =
        add(
            from = values.getOrElse(VON_NR) { "" },
            to = values.getOrElse(NACH_NR) { "" },
        )

    fun removeAt(index: Int) {
        synchronized(lock) {
            entries.removeAt(index)
        }
    }

    fun setFrom(index: Int, value: String) {
        synchronized(lock) {
            if (value.isEmpty()) {
                entries.removeAt(index)
            } else {
                entries[index] = entries[index].copy(from = value)
            }
        }
    }

    fun setTo(index: Int, value: String) {
        synchronized(lock) {
            entries[index] = entries[index].copy(to = value)
        }
    }

    fun entries(): List<ReplaceEntry> = snapshot()

    fun valuesForXml(): List<Array<String>> =
        snapshot().map { it.toArray() }

    fun replace(strCheck: String, path: Boolean): String {
        var result = strCheck
        for (entry in snapshot()) {
            if (path && entry.from in PATH_SEPARATORS) {
                continue
            }
            result = result.replace(entry.from, entry.to)
        }
        return result
    }

    fun check(): Boolean {
        val snapshot = snapshot()
        for (i in snapshot.indices) {
            val entry = snapshot[i]
            for (k in i + 1 until snapshot.size) {
                val nextEntry = snapshot[k]
                if (entry.to.contains(nextEntry.from)) {
                    return true
                }
            }
        }
        return false
    }

    fun moveUp(idx: Int): Int = move(idx, -1)

    fun moveDown(idx: Int): Int = move(idx, 1)

    private fun move(idx: Int, offset: Int): Int =
        synchronized(lock) {
            val replace = entries.removeAt(idx)
            val newIndex = (idx + offset).coerceIn(0, entries.size)
            entries.add(newIndex, replace)
            newIndex
        }

    private fun snapshot(): List<ReplaceEntry> =
        synchronized(lock) {
            entries.toList()
        }

    companion object {
        const val REPLACELIST: String = "Ersetzungstabelle"
        const val VON: String = "von"
        const val VON_NR: Int = 0
        const val NACH: String = "nach"
        const val NACH_NR: Int = 1
        const val MAX_ELEM: Int = 2

        private val COLUMN_NAMES = arrayOf(VON, NACH)
        private val PATH_SEPARATORS = setOf("/", "\\")
    }
}