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

class ListeProg(
    private val entries: BasicEventList<DatenProg> = BasicEventList(),
) : EventList<DatenProg> by entries {
    fun addEntry(prog: DatenProg) {
        val lock = entries.readWriteLock.writeLock()
        lock.lock()
        try {
            entries.add(prog)
        } finally {
            lock.unlock()
        }
    }

    fun removeEntryAtIndex(index: Int): DatenProg {
        val lock = entries.readWriteLock.writeLock()
        lock.lock()
        try {
            return entries.removeAt(index)
        } finally {
            lock.unlock()
        }
    }

    fun remove(name: String): DatenProg? {
        val lock = entries.readWriteLock.writeLock()
        lock.lock()
        try {
            val index = entries.indexOfFirst { prog -> prog.arr[DatenProg.PROGRAMM_NAME] == name }
            return if (index != -1) {
                entries.removeAt(index)
            } else {
                null
            }
        } finally {
            lock.unlock()
        }
    }

    fun moveEntryAtIndex(idx: Int, up: Boolean): Int {
        val lock = entries.readWriteLock.writeLock()
        lock.lock()
        try {
            val newIndex = (idx + if (up) -1 else 1).coerceIn(0, entries.lastIndex)
            if (newIndex == idx) {
                return idx
            }
            val prog = entries.removeAt(idx)
            entries.add(newIndex, prog)
            return newIndex
        } finally {
            lock.unlock()
        }
    }

    fun removeAllEntries(progs: Collection<DatenProg>) {
        val lock = entries.readWriteLock.writeLock()
        lock.lock()
        try {
            entries.removeAll(progs.toSet())
        } finally {
            lock.unlock()
        }
    }

    fun fireEntryChanged(index: Int) {
        val lock = entries.readWriteLock.writeLock()
        lock.lock()
        try {
            entries[index] = entries[index]
        } finally {
            lock.unlock()
        }
    }
}
