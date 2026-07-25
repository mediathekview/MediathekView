package mediathek.tool

import ca.odell.glazedlists.EventList
import kotlin.concurrent.withLock

inline fun <T, R> EventList<T>.withReadLock(action: EventList<T>.() -> R): R =
    readWriteLock.readLock().withLock { action() }

inline fun <T, R> EventList<T>.withWriteLock(action: EventList<T>.() -> R): R =
    readWriteLock.writeLock().withLock { action() }

fun <T> EventList<T>.snapshot(): List<T> = withReadLock { toList() }
