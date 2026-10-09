package ca.odell.glazedlists.impl.gui

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.TransformedList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventAssembler
import ca.odell.glazedlists.event.ListEventListener

/**
 * An EventList that keeps a stable local snapshot and delivers source changes
 * on a thread selected by [schedule].
 */
internal abstract class ThreadProxyEventList<E>(source: EventList<E>) :
    TransformedList<E, E>(source), RandomAccess {
    private var localCache: List<E>
    private val updateRunner = UpdateRunner()
    private val cacheUpdates =
        ListEventAssembler(this, ListEventAssembler.createListEventPublisher())

    @Volatile
    private var scheduled = false

    @Volatile
    private var disposed = false

    init {
        val readLock = source.readWriteLock.readLock()
        readLock.lock()
        try {
            localCache = source.toList()
            cacheUpdates.addListEventListener(updateRunner)
            source.addListEventListener(this)
        } finally {
            readLock.unlock()
        }
    }

    final override fun listChanged(listChanges: ListEvent<E>) {
        readWriteLock.writeLock().lock()
        try {
            if (disposed) return
            if (!scheduled) {
                updates.beginEvent(true)
                cacheUpdates.beginEvent(true)
            }

            updates.forwardEvent(listChanges)
            cacheUpdates.forwardEvent(listChanges)

            if (!scheduled) {
                scheduled = true
                schedule(updateRunner)
            }
        } finally {
            readWriteLock.writeLock().unlock()
        }
    }

    protected abstract fun schedule(runnable: Runnable)

    final override val size: Int
        get() = localCache.size

    final override fun get(index: Int): E = localCache[index]

    final override fun isWritable(): Boolean = true

    /** Applies a source event to an older snapshot without exposing intermediate state. */
    protected open fun applyChangeToCache(
        source: EventList<E>,
        listChanges: ListEvent<E>,
        localCache: List<E>,
    ): List<E> {
        val result = ArrayList<E>(source.size)
        var resultIndex = 0
        var cacheOffset = 0

        while (true) {
            val changeIndex: Int
            val changeType: Int
            if (listChanges.next()) {
                changeIndex = listChanges.index
                changeType = listChanges.type
            } else {
                changeIndex = source.size
                changeType = -1
            }

            while (resultIndex < changeIndex) {
                result.add(resultIndex, localCache[resultIndex + cacheOffset])
                resultIndex++
            }

            when (changeType) {
                ListEvent.DELETE -> cacheOffset++
                ListEvent.UPDATE -> {
                    result.add(resultIndex, source[changeIndex])
                    resultIndex++
                }

                ListEvent.INSERT -> {
                    result.add(resultIndex, source[changeIndex])
                    resultIndex++
                    cacheOffset--
                }

                -1 -> return result
            }
        }
    }

    override fun dispose() {
        readWriteLock.writeLock().lock()
        try {
            if (disposed) return
            disposed = true
            super.dispose()
            if (scheduled) {
                cacheUpdates.discardEvent()
                updates.discardEvent()
                scheduled = false
            }
            cacheUpdates.removeListEventListener(updateRunner)
        } finally {
            readWriteLock.writeLock().unlock()
        }
    }

    private inner class UpdateRunner : Runnable, ListEventListener<E> {
        override fun run() {
            readWriteLock.writeLock().lock()
            try {
                if (disposed) return
                cacheUpdates.commitEvent()
                updates.commitEvent()
            } finally {
                scheduled = false
                readWriteLock.writeLock().unlock()
            }
        }

        override fun listChanged(listChanges: ListEvent<E>) {
            localCache = applyChangeToCache(source!!, listChanges, localCache)
        }
    }
}
