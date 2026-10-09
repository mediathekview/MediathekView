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
@file:Suppress("PLATFORM_CLASS_MAPPED_TO_KOTLIN")

package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.impl.adt.Barcode
import java.util.*

/**
 * A list that fires update events whenever elements are modified in place.
 *
 * The supplied [Connector] installs and removes listeners as elements enter
 * and leave the source list. This class is thread ready but not thread safe;
 * [elementChanged], however, acquires the list's write lock.
 */
open class ObservableElementList<E>(
    source: EventList<E>,
    elementConnector: Connector<in E>,
) : TransformedList<E, E>(source), ObservableElementChangeHandler<E> {
    private var observedElements: MutableList<E>? = null
    private var elementConnector: Connector<in E>? = null
    private var singleListenerMode = true
    private var multiEventListenerRegistry: MutableList<EventListener?>? = null
    private var singleEventListener: EventListener? = null
    private var singleEventListenerRegistry: Barcode? = null

    @Volatile
    private var disposed = false

    private val disposalMonitor = Object()
    private var pendingElementCleanups: MutableList<ElementCleanup<E>>? = null
    private var connectorDetached = false
    private var sourceDetached = false
    private var cleanupComplete = false
    private var cleanupInProgress = false
    private var cleanupThread: Thread? = null

    init {
        val installedListeners = ArrayList<ElementCleanup<E>>()
        var connectorAttachmentAttempted = false
        var sourceListenerRegistrationAttempted = false
        var initializationFailure: Throwable? = null
        source.readWriteLock.writeLock().lock()
        try {
            this.elementConnector = elementConnector
            observedElements = ArrayList(source)
            singleEventListenerRegistry = Barcode().also { it.addWhite(0, source.size) }

            connectorAttachmentAttempted = true
            this.elementConnector!!.setObservableElementList(this)

            var index = 0
            val elementCount = size
            while (index < elementCount) {
                val element = get(index)
                val listener = connectElement(element)
                if (element != null && listener != null) {
                    installedListeners.add(ElementCleanup(element, listener))
                }
                registerListener(index, listener, false)
                index++
            }

            sourceListenerRegistrationAttempted = true
            source.addListEventListener(this)
        } catch (failure: RuntimeException) {
            disposed = true
            initializationFailure = failure
        } catch (failure: Error) {
            disposed = true
            initializationFailure = failure
        } finally {
            source.readWriteLock.writeLock().unlock()
        }

        if (initializationFailure != null) {
            pendingElementCleanups = ArrayList(installedListeners)
            sourceDetached = !sourceListenerRegistrationAttempted
            connectorDetached = !connectorAttachmentAttempted
            observedElements = null
            multiEventListenerRegistry = null
            singleEventListener = null
            singleEventListenerRegistry = null

            if (!sourceDetached) {
                try {
                    detachFromSource()
                } catch (cleanupFailure: RuntimeException) {
                    initializationFailure.addSuppressed(cleanupFailure)
                } catch (cleanupFailure: Error) {
                    initializationFailure.addSuppressed(cleanupFailure)
                }
            }

            synchronized(disposalMonitor) {
                cleanupInProgress = true
                cleanupThread = Thread.currentThread()
            }
            initializationFailure = cleanupConnector(initializationFailure)
            finishCleanupAttempt()

            when (val failure = initializationFailure) {
                is RuntimeException -> throw failure
                is Error -> throw failure
                else -> throw AssertionError(failure)
            }
        }
    }

    override fun listChanged(listChanges: ListEvent<E>) {
        if (disposed) return
        checkNotNull(observedElements) { "Cannot modify disposed ObservableElementList" }

        while (listChanges.next()) {
            val changeIndex = listChanges.index
            when (listChanges.type) {
                ListEvent.INSERT -> {
                    val inserted = get(changeIndex)
                    observedElements!!.add(changeIndex, inserted)
                    val listener = connectElement(inserted)
                    registerListener(changeIndex, listener, false)
                }

                ListEvent.DELETE -> {
                    var deleted = listChanges.oldValue
                    val deletedElementFromPrivateCopy = observedElements!!.removeAt(changeIndex)
                    if (deleted === ListEvent.UNKNOWN_VALUE) {
                        deleted = deletedElementFromPrivateCopy
                    }
                    val listener = unregisterListener(changeIndex)
                    disconnectElement(deleted, listener)
                }

                ListEvent.UPDATE -> {
                    var previousValue = listChanges.oldValue
                    if (previousValue === ListEvent.UNKNOWN_VALUE) {
                        previousValue = observedElements!![changeIndex]
                    }
                    val newValue = get(changeIndex)
                    if (newValue !== previousValue) {
                        observedElements!![changeIndex] = newValue
                        disconnectElement(previousValue, getListener(changeIndex))
                        val listener = connectElement(newValue)
                        registerListener(changeIndex, listener, true)
                    }
                }
            }
        }

        listChanges.reset()
        updates.forwardEvent(listChanges)
    }

    private fun registerListener(index: Int, listener: EventListener?, replace: Boolean) {
        if (replace) {
            if (singleListenerMode) {
                singleEventListenerRegistry!!.set(index, if (listener == null) Barcode.WHITE else Barcode.BLACK, 1)
            } else {
                multiEventListenerRegistry!![index] = listener
            }
        } else {
            if (singleListenerMode) {
                singleEventListenerRegistry!!.add(index, if (listener == null) Barcode.WHITE else Barcode.BLACK, 1)
            } else {
                multiEventListenerRegistry!!.add(index, listener)
            }
        }
    }

    private fun getListener(index: Int): EventListener? =
        if (singleListenerMode) {
            if (singleEventListenerRegistry!![index] === Barcode.BLACK) singleEventListener else null
        } else {
            multiEventListenerRegistry!![index]
        }

    private fun unregisterListener(index: Int): EventListener? {
        return if (singleListenerMode) {
            val listener = if (singleEventListenerRegistry!![index] === Barcode.BLACK) singleEventListener else null
            singleEventListenerRegistry!!.remove(index, 1)
            listener
        } else {
            multiEventListenerRegistry!!.removeAt(index)
        }
    }

    private fun connectElement(listElement: E): EventListener? {
        if (listElement == null) return null

        val listener = elementConnector!!.installListener(listElement)
        if (singleListenerMode && listener != null) {
            if (singleEventListener == null) {
                singleEventListener = listener
            } else if (listener !== singleEventListener) {
                switchToMultiListenerMode()
            }
        }
        return listener
    }

    private fun disconnectElement(listElement: E?, listener: EventListener?) {
        if (listElement != null && listener != null) {
            elementConnector!!.uninstallListener(listElement, listener)
        }
    }

    private fun switchToMultiListenerMode() {
        check(singleListenerMode)

        val registry = ArrayList<EventListener?>(source!!.size)
        var index = 0
        while (index < source!!.size) {
            registry.add(null)
            index++
        }
        multiEventListenerRegistry = registry

        val iterator = singleEventListenerRegistry!!.iterator()
        while (iterator.hasNextBlack()) {
            iterator.nextBlack()
            registry[iterator.index] = singleEventListener
        }

        singleEventListener = null
        singleEventListenerRegistry = null
        singleListenerMode = false
    }

    override fun isWritable(): Boolean = true

    override fun dispose() {
        var restoreInterrupt = false
        var failure: Throwable? = null
        synchronized(disposalMonitor) {
            while (cleanupInProgress) {
                if (cleanupThread === Thread.currentThread()) return
                try {
                    disposalMonitor.wait()
                } catch (_: InterruptedException) {
                    restoreInterrupt = true
                }
            }
            if (cleanupComplete) {
                if (restoreInterrupt) Thread.currentThread().interrupt()
                return
            }
            try {
                if (!disposed) {
                    prepareDisposal()
                } else if (!sourceDetached) {
                    detachFromSource()
                }
            } catch (disposalFailure: RuntimeException) {
                if (!disposed) {
                    if (restoreInterrupt) Thread.currentThread().interrupt()
                    throw disposalFailure
                }
                failure = disposalFailure
            } catch (disposalFailure: Error) {
                if (!disposed) {
                    if (restoreInterrupt) Thread.currentThread().interrupt()
                    throw disposalFailure
                }
                failure = disposalFailure
            }
            cleanupInProgress = true
            cleanupThread = Thread.currentThread()
        }

        failure = cleanupConnector(failure)
        finishCleanupAttempt()

        if (restoreInterrupt) Thread.currentThread().interrupt()
        when (val cleanupFailure = failure) {
            is RuntimeException -> throw cleanupFailure
            is Error -> throw cleanupFailure
            null -> Unit
            else -> throw AssertionError(cleanupFailure)
        }
    }

    private fun cleanupConnector(initialFailure: Throwable?): Throwable? {
        var failure = initialFailure
        val iterator = pendingElementCleanups!!.iterator()
        while (iterator.hasNext()) {
            val cleanup = iterator.next()
            try {
                elementConnector!!.uninstallListener(cleanup.element, cleanup.listener)
                iterator.remove()
            } catch (cleanupFailure: RuntimeException) {
                if (failure == null) {
                    failure = cleanupFailure
                } else if (failure !== cleanupFailure) {
                    failure.addSuppressed(cleanupFailure)
                }
            } catch (cleanupFailure: Error) {
                if (failure == null) {
                    failure = cleanupFailure
                } else if (failure !== cleanupFailure) {
                    failure.addSuppressed(cleanupFailure)
                }
            }
        }

        if (pendingElementCleanups!!.isEmpty() && sourceDetached && !connectorDetached) {
            try {
                elementConnector!!.setObservableElementList(null)
                connectorDetached = true
            } catch (cleanupFailure: RuntimeException) {
                if (failure == null) {
                    failure = cleanupFailure
                } else if (failure !== cleanupFailure) {
                    failure.addSuppressed(cleanupFailure)
                }
            } catch (cleanupFailure: Error) {
                if (failure == null) {
                    failure = cleanupFailure
                } else if (failure !== cleanupFailure) {
                    failure.addSuppressed(cleanupFailure)
                }
            }
        }
        return failure
    }

    private fun finishCleanupAttempt() {
        synchronized(disposalMonitor) {
            if (pendingElementCleanups!!.isEmpty() && sourceDetached && connectorDetached) {
                pendingElementCleanups = null
                elementConnector = null
                cleanupComplete = true
            }
            cleanupInProgress = false
            cleanupThread = null
            disposalMonitor.notifyAll()
        }
    }

    private fun prepareDisposal() {
        readWriteLock.writeLock().lock()
        try {
            if (disposed) return

            val cleanupEntries = ArrayList<ElementCleanup<E>>()
            var index = 0
            val elementCount = observedElements!!.size
            while (index < elementCount) {
                val element = observedElements!![index]
                val listener = getListener(index)
                if (element != null && listener != null) {
                    cleanupEntries.add(ElementCleanup(element, listener))
                }
                index++
            }

            pendingElementCleanups = cleanupEntries
            observedElements = null
            multiEventListenerRegistry = null
            singleEventListener = null
            singleEventListenerRegistry = null
            disposed = true
            super.dispose()
            sourceDetached = true
        } finally {
            readWriteLock.writeLock().unlock()
        }
    }

    private fun detachFromSource() {
        readWriteLock.writeLock().lock()
        try {
            super.dispose()
            sourceDetached = true
        } finally {
            readWriteLock.writeLock().unlock()
        }
    }

    override fun elementChanged(listElement: Any?) {
        readWriteLock.writeLock().lock()
        try {
            if (disposed) return
            updates.beginEvent()

            var index = 0
            val elementCount = size
            while (index < elementCount) {
                val currentElement = get(index)
                if (listElement === currentElement) {
                    updates.elementUpdated(index, currentElement, ListEvent.unknownValue())
                }
                index++
            }

            updates.commitEvent()
        } finally {
            readWriteLock.writeLock().unlock()
        }
    }

    interface Connector<E> {
        fun installListener(element: E): EventListener?

        fun uninstallListener(element: E, listener: EventListener)

        fun setObservableElementList(list: ObservableElementChangeHandler<out E>?)
    }

    private class ElementCleanup<E>(
        val element: E,
        val listener: EventListener,
    )
}
