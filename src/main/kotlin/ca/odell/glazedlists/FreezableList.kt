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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent

/**
 * An event list that can temporarily retain a snapshot while its source continues changing.
 */
class FreezableList<E>(source: EventList<E>) : TransformedList<E, E>(source) {
    private var frozen = false
    private val frozenData = mutableListOf<E>()

    init {
        source.addListEventListener(this)
    }

    override fun get(index: Int): E = if (frozen) frozenData[index] else source!![index]

    @get:JvmName("size")
    override val size: Int
        get() = if (frozen) frozenData.size else source!!.size

    override fun isWritable(): Boolean = !frozen

    fun isFrozen(): Boolean = frozen

    fun freeze() {
        check(!frozen) { "Cannot freeze a list that is already frozen" }

        val currentSource = source!!
        currentSource.removeListEventListener(this)
        frozenData.addAll(currentSource)
        frozen = true
    }

    fun thaw() {
        check(frozen) { "Cannot thaw a list that is not frozen" }

        updates.beginEvent()
        frozenData.forEach { updates.elementDeleted(0, it) }
        val currentSource = source!!
        currentSource.forEach { updates.elementInserted(0, it) }

        frozenData.clear()
        frozen = false
        currentSource.addListEventListener(this)
        updates.commitEvent()
    }

    override fun listChanged(listChanges: ListEvent<E>) {
        if (!frozen) updates.forwardEvent(listChanges)
    }
}
