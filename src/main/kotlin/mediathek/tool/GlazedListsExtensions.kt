package mediathek.tool

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.util.concurrent.Lock

inline fun <R> Lock.withLock(action: () -> R): R {
    lock()
    return try {
        action()
    } finally {
        unlock()
    }
}

inline fun <T, R> EventList<T>.withReadLock(action: EventList<T>.() -> R): R =
    readWriteLock.readLock().withLock { action() }

inline fun <T, R> EventList<T>.withWriteLock(action: EventList<T>.() -> R): R =
    readWriteLock.writeLock().withLock { action() }
