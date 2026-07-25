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
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.event.ListEventPublisher
import org.jspecify.annotations.NonNull
import java.util.*
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.Condition
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReadWriteLock
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.function.Consumer
import java.util.function.Predicate
import java.util.function.UnaryOperator

/**
 * A debugging root event list that can enforce thread and lock ownership rules.
 *
 * This class intentionally remains open so callers can extend the assertion
 * hooks used around each delegated operation.
 */
@Suppress(
    "ACCIDENTAL_OVERRIDE",
    "INAPPLICABLE_JVM_NAME",
    "PARAMETER_NAME_CHANGED_ON_OVERRIDE",
    "UNCHECKED_CAST",
)
open class DebugList<E> private constructor(
    publisher: ListEventPublisher?,
    debugReadWriteLock: DebugReadWriteLock,
) : AbstractEventList<E>() {
    private var lockCheckingEnabled = false

    open var isLockCheckingEnabled: Boolean
        get() = lockCheckingEnabled
        set(value) {
            lockCheckingEnabled = value
        }

    open val sanctionedReaderThreads: MutableSet<Thread> = HashSet()
    open val sanctionedWriterThreads: MutableSet<Thread> = HashSet()

    private var delegate: EventList<E>? = null

    private val delegateWatcher: ListEventListener<E> = ListEventForwarder()

    private val debugReadWriteLock = debugReadWriteLock

    constructor() : this(null, DebugReadWriteLock())

    init {
        delegate = BasicEventList(publisher, debugReadWriteLock)
        delegate!!.addListEventListener(delegateWatcher)
    }

    private open inner class ListEventForwarder : ListEventListener<E> {
        override fun listChanged(listChanges: ListEvent<E>) {
            updates.forwardEvent(listChanges)
        }
    }

    @get:JvmName("getReadWriteLock")
    open val delegatedReadWriteLock: ReadWriteLock
        get() = delegate!!.readWriteLock

    @get:JvmName("getPublisher")
    open val delegatedPublisher: ListEventPublisher
        get() = delegate!!.publisher

    open fun <E> createNewDebugList(): DebugList<E> = DebugList(delegatedPublisher, debugReadWriteLock)

    protected open fun beforeReadOperation() {
        if (sanctionedReaderThreads.isNotEmpty() && !sanctionedReaderThreads.contains(Thread.currentThread())) {
            throw IllegalStateException(
                "DebugList detected an unexpected Thread (${Thread.currentThread()}) attempting to perform a read operation",
            )
        }
        if (isLockCheckingEnabled && !debugReadWriteLock.isThreadHoldingReadOrWriteLock()) {
            throw IllegalStateException(
                "DebugList detected a failure to acquire the readLock prior to a read operation",
            )
        }
    }

    protected open fun afterReadOperation() = Unit

    protected open fun beforeWriteOperation() {
        if (sanctionedWriterThreads.isNotEmpty() && !sanctionedWriterThreads.contains(Thread.currentThread())) {
            throw IllegalStateException(
                "DebugList detected an unexpected Thread (${Thread.currentThread()}) attempting to perform a write operation",
            )
        }
        if (isLockCheckingEnabled && !debugReadWriteLock.isThreadHoldingWriteLock()) {
            throw IllegalStateException(
                "DebugList detected a failure to acquire the writeLock prior to a write operation",
            )
        }
    }

    protected open fun afterWriteOperation() = Unit

    override fun get(index: Int): E {
        beforeReadOperation()
        try {
            return delegate!![index]
        } finally {
            afterReadOperation()
        }
    }

    @get:JvmName("size")
    override val size: Int
        get() {
            beforeReadOperation()
            try {
                return delegate!!.size
            } finally {
                afterReadOperation()
            }
        }

    override fun contains(element: E): Boolean {
        beforeReadOperation()
        try {
            return delegate!!.contains(element)
        } finally {
            afterReadOperation()
        }
    }

    override fun containsAll(elements: Collection<E>): Boolean {
        beforeReadOperation()
        try {
            return HashSet(delegate!!).containsAll(elements)
        } finally {
            afterReadOperation()
        }
    }

    override fun equals(other: Any?): Boolean {
        beforeReadOperation()
        try {
            return delegate!! == other
        } finally {
            afterReadOperation()
        }
    }

    override fun hashCode(): Int {
        beforeReadOperation()
        try {
            return delegate!!.hashCode()
        } finally {
            afterReadOperation()
        }
    }

    override fun indexOf(element: E): Int {
        beforeReadOperation()
        try {
            return delegate!!.indexOf(element)
        } finally {
            afterReadOperation()
        }
    }

    override fun lastIndexOf(element: E): Int {
        beforeReadOperation()
        try {
            return delegate!!.lastIndexOf(element)
        } finally {
            afterReadOperation()
        }
    }

    override fun isEmpty(): Boolean {
        beforeReadOperation()
        try {
            return delegate!!.isEmpty()
        } finally {
            afterReadOperation()
        }
    }

    override fun forEach(action: Consumer<in E>) {
        beforeReadOperation()
        try {
            delegate!!.forEach(action)
        } finally {
            afterReadOperation()
        }
    }

    override fun toArray(): @NonNull Array<Any?> {
        beforeReadOperation()
        try {
            return (delegate!! as AbstractEventList<E>).toArray()
        } finally {
            afterReadOperation()
        }
    }

    override fun <T> toArray(array: @NonNull Array<T>): @NonNull Array<T> {
        beforeReadOperation()
        try {
            return (delegate!! as AbstractEventList<E>).toArray(array)
        } finally {
            afterReadOperation()
        }
    }

    override fun toString(): String {
        beforeReadOperation()
        try {
            return delegate!!.toString()
        } finally {
            afterReadOperation()
        }
    }

    override fun add(element: E): Boolean {
        beforeWriteOperation()
        try {
            return delegate!!.add(element)
        } finally {
            afterWriteOperation()
        }
    }

    override fun remove(element: E): Boolean {
        beforeWriteOperation()
        try {
            return delegate!!.remove(element)
        } finally {
            afterWriteOperation()
        }
    }

    override fun removeIf(filter: @NonNull Predicate<in E>): Boolean {
        beforeWriteOperation()
        try {
            return delegate!!.removeIf(filter)
        } finally {
            afterWriteOperation()
        }
    }

    override fun addAll(elements: @NonNull Collection<E>): Boolean {
        beforeWriteOperation()
        try {
            return delegate!!.addAll(elements)
        } finally {
            afterWriteOperation()
        }
    }

    override fun addAll(index: Int, elements: @NonNull Collection<E>): Boolean {
        beforeWriteOperation()
        try {
            return delegate!!.addAll(index, elements)
        } finally {
            afterWriteOperation()
        }
    }

    override fun removeAll(elements: @NonNull Collection<E>): Boolean {
        beforeWriteOperation()
        try {
            return delegate!!.removeAll(elements)
        } finally {
            afterWriteOperation()
        }
    }

    override fun retainAll(elements: @NonNull Collection<E>): Boolean {
        beforeWriteOperation()
        try {
            return delegate!!.retainAll(elements)
        } finally {
            afterWriteOperation()
        }
    }

    override fun replaceAll(operator: @NonNull UnaryOperator<E>) {
        beforeWriteOperation()
        try {
            delegate!!.replaceAll(operator)
        } finally {
            afterWriteOperation()
        }
    }

    override fun sort(comparator: Comparator<in E>?) {
        beforeWriteOperation()
        try {
            Collections.sort(delegate!!, comparator)
        } finally {
            afterWriteOperation()
        }
    }

    override fun clear() {
        beforeWriteOperation()
        try {
            delegate!!.clear()
        } finally {
            afterWriteOperation()
        }
    }

    override fun set(index: Int, element: E): E {
        beforeWriteOperation()
        try {
            return delegate!!.set(index, element)
        } finally {
            afterWriteOperation()
        }
    }

    override fun add(index: Int, element: E) {
        beforeWriteOperation()
        try {
            delegate!!.add(index, element)
        } finally {
            afterWriteOperation()
        }
    }

    @JvmName("remove")
    override fun removeAt(index: Int): E {
        beforeWriteOperation()
        try {
            return delegate!!.removeAt(index)
        } finally {
            afterWriteOperation()
        }
    }

    override fun dispose() {
        delegate!!.removeListEventListener(delegateWatcher)
        delegate = null
    }

    private open class DebugReadWriteLock : ReadWriteLock {
        private val readLock: DebugLock
        private val writeLock: DebugLock

        init {
            val decorated: ReadWriteLock = ReentrantReadWriteLock()
            readLock = DebugLock(decorated.readLock(), null)
            writeLock = DebugLock(decorated.writeLock(), readLock)
        }

        override fun readLock(): @NonNull Lock = readLock

        override fun writeLock(): @NonNull Lock = writeLock

        open fun isThreadHoldingWriteLock(): Boolean =
            writeLock.threadsHoldingLock.contains(Thread.currentThread())

        open fun isThreadHoldingReadOrWriteLock(): Boolean =
            readLock.threadsHoldingLock.contains(Thread.currentThread()) ||
                    writeLock.threadsHoldingLock.contains(Thread.currentThread())

        private open class DebugLock(
            private val delegate: Lock,
            private val readLock: DebugLock?,
        ) : Lock {
            open val threadsHoldingLock: MutableList<Thread> = Collections.synchronizedList(ArrayList())

            override fun lock() {
                checkForReadToWriteUpgrade()
                delegate.lock()
                recordLockAcquisition()
            }

            @Throws(InterruptedException::class)
            override fun lockInterruptibly() {
                checkForReadToWriteUpgrade()
                delegate.lockInterruptibly()
                recordLockAcquisition()
            }

            override fun tryLock(): Boolean {
                checkForReadToWriteUpgrade()
                val success = delegate.tryLock()
                if (success) recordLockAcquisition()
                return success
            }

            @Throws(InterruptedException::class)
            override fun tryLock(time: Long, unit: TimeUnit): Boolean {
                checkForReadToWriteUpgrade()
                val success = delegate.tryLock(time, unit)
                if (success) recordLockAcquisition()
                return success
            }

            override fun unlock() {
                delegate.unlock()
                threadsHoldingLock.remove(Thread.currentThread())
            }

            override fun newCondition(): @NonNull Condition = delegate.newCondition()

            private fun checkForReadToWriteUpgrade() {
                if (
                    readLock != null &&
                    readLock.threadsHoldingLock.contains(Thread.currentThread()) &&
                    !threadsHoldingLock.contains(Thread.currentThread())
                ) {
                    throw IllegalStateException(
                        "DebugList detected an attempt to acquire a writeLock from a thread already owning a readLock (deadlock)",
                    )
                }
            }

            private fun recordLockAcquisition() {
                threadsHoldingLock.add(Thread.currentThread())
            }
        }
    }
}
