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

package mediathek.daten

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import mediathek.tool.withWriteLock

class ListeProg(
    private val entries: BasicEventList<DatenProg> = BasicEventList(),
) : EventList<DatenProg> by entries {
    fun addEntry(prog: DatenProg) {
        entries.withWriteLock {
            entries.add(prog)
        }
    }

    fun removeEntryAtIndex(index: Int): DatenProg {
        return entries.withWriteLock {
            entries.removeAt(index)
        }
    }

    fun remove(name: String): DatenProg? {
        return entries.withWriteLock {
            val index = entries.indexOfFirst { prog -> prog.arr[DatenProg.PROGRAMM_NAME] == name }
            if (index != -1) {
                entries.removeAt(index)
            } else {
                null
            }
        }
    }

    fun moveEntryAtIndex(idx: Int, up: Boolean): Int {
        return entries.withWriteLock {
            val newIndex = (idx + if (up) -1 else 1).coerceIn(0, entries.lastIndex)
            if (newIndex == idx) {
                return@withWriteLock idx
            }
            val prog = entries.removeAt(idx)
            entries.add(newIndex, prog)
            newIndex
        }
    }

    fun removeAllEntries(progs: Collection<DatenProg>) {
        entries.withWriteLock {
            entries.removeAll(progs.toSet())
        }
    }

    fun fireEntryChanged(index: Int) {
        entries.withWriteLock {
            entries[index] = entries[index]
        }
    }
}
