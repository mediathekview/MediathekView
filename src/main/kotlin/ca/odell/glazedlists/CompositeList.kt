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

import ca.odell.glazedlists.event.ListEventPublisher
import java.util.concurrent.locks.ReadWriteLock

/** An [EventList] composed of multiple member [EventList]s. */
@Suppress("INAPPLICABLE_JVM_NAME", "UNCHECKED_CAST")
open class CompositeList<E> : CollectionList<EventList<E>, E> {
    constructor() : super(
        BasicEventList(),
        GlazedLists.listCollectionListModel<E>() as Model<EventList<E>, E>,
    )

    constructor(publisher: ListEventPublisher, lock: ReadWriteLock) : super(
        BasicEventList(publisher, lock),
        GlazedLists.listCollectionListModel<E>() as Model<EventList<E>, E>,
    )

    open fun addMemberList(member: EventList<E>) {
        require(publisher == member.publisher) {
            "Member list must share publisher with CompositeList"
        }
        require(readWriteLock == member.readWriteLock) {
            "Member list must share lock with CompositeList"
        }
        source!!.add(member)
    }

    open fun <T> createMemberList(): EventList<T> = BasicEventList(publisher, readWriteLock)

    open fun removeMemberList(list: EventList<E>) {
        val iterator = source!!.iterator()
        while (iterator.hasNext()) {
            if (iterator.next() === list) {
                iterator.remove()
                return
            }
        }
        throw IllegalArgumentException("Cannot remove list $list which is not in this CompositeList")
    }
}
