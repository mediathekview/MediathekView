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
package ca.odell.glazedlists.impl

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import java.lang.ref.WeakReference

/** Forwards list events without retaining the target listener strongly. */
internal class WeakReferenceProxy<E>(
    source: EventList<E>,
    proxyTarget: ListEventListener<E>,
) : ListEventListener<E> {
    private val proxyTargetReference: WeakReference<ListEventListener<E>>
    private var source: EventList<E>?

    init {
        this.source = source
        proxyTargetReference = WeakReference(proxyTarget)
    }

    override fun listChanged(listChanges: ListEvent<E>) {
        val currentSource = source ?: return
        val target = referent
        if (target == null) {
            currentSource.removeListEventListener(this)
            dispose()
        } else {
            target.listChanged(listChanges)
        }
    }

    val referent: ListEventListener<E>?
        get() = proxyTargetReference.get()

    fun dispose() {
        source = null
    }
}
