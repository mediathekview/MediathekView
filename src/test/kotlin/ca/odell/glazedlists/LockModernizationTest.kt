package ca.odell.glazedlists

import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.MatcherEditor
import ca.odell.glazedlists.swing.DefaultEventTableModel
import ca.odell.glazedlists.swing.swingThreadProxyList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.*
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantReadWriteLock

internal class LockModernizationTest {
    @Test
    fun defaultLockIsReentrant() {
        val list = BasicEventList<String>()
        val writeLock = list.readWriteLock.writeLock()

        writeLock.lock()
        try {
            writeLock.lock()
            try {
                list.add("value")
            } finally {
                writeLock.unlock()
            }
        } finally {
            writeLock.unlock()
        }

        assertEquals(listOf("value"), list)
        assertInstanceOf(ReentrantReadWriteLock::class.java, list.readWriteLock)
    }

    @Test
    fun defaultLockRejectsReadToWriteUpgrade() {
        val lock = BasicEventList<String>().readWriteLock

        lock.readLock().lock()
        try {
            assertAll(
                { assertThrows(IllegalStateException::class.java) { lock.writeLock().lock() } },
                { assertThrows(IllegalStateException::class.java) { lock.writeLock().lockInterruptibly() } },
                { assertThrows(IllegalStateException::class.java) { lock.writeLock().tryLock() } },
                {
                    assertThrows(IllegalStateException::class.java) {
                        lock.writeLock().tryLock(1, TimeUnit.MILLISECONDS)
                    }
                },
            )
        } finally {
            lock.readLock().unlock()
        }
    }

    @Test
    fun debugLockDetectsReadToWriteUpgradeThroughJdkLockMethods() {
        val lock = DebugList<String>().readWriteLock
        lock.readLock().lockInterruptibly()
        try {
            assertThrows(IllegalStateException::class.java) {
                lock.writeLock().tryLock(1, TimeUnit.MILLISECONDS)
            }
        } finally {
            lock.readLock().unlock()
        }
    }

    @Test
    fun debugLockAllowsWriteReadWriteReentry() {
        val lock = DebugList<String>().readWriteLock

        lock.writeLock().lock()
        try {
            lock.readLock().lock()
            try {
                assertDoesNotThrow { lock.writeLock().lock() }
                lock.writeLock().unlock()
            } finally {
                lock.readLock().unlock()
            }
        } finally {
            lock.writeLock().unlock()
        }
    }

    @Test
    fun swingThreadProxyConstructionAcquiresSourceReadLock() {
        val source = lockCheckingList("value")
        val proxy = source.swingThreadProxyList()
        proxy.dispose()
    }

    @Test
    fun sortedListConstructionAcquiresSourceReadLock() {
        val source = lockCheckingList("bravo", "alpha")

        val sorted = SortedList(source)

        source.readWriteLock.readLock().lock()
        try {
            assertEquals(listOf("alpha", "bravo"), sorted)
        } finally {
            source.readWriteLock.readLock().unlock()
        }
        sorted.dispose()
    }

    @Test
    fun filterListConstructionAcquiresSourceReadLock() {
        val source = lockCheckingList("keep", "discard")

        val filtered = FilterList(source, Matcher<String> { it == "keep" })

        source.readWriteLock.readLock().lock()
        try {
            assertEquals(listOf("keep"), filtered)
        } finally {
            source.readWriteLock.readLock().unlock()
        }
        filtered.dispose()
    }

    @Test
    fun filterListConstructionRemovesMatcherListenerWhenInitializationFails() {
        val lock = ReentrantReadWriteLock()
        var installedListener: MatcherEditor.Listener<String>? = null
        var removedListener: MatcherEditor.Listener<String>? = null
        var cleanupHeldWriteLock = false
        val editor = object : MatcherEditor<String> {
            override fun addMatcherEditorListener(listener: MatcherEditor.Listener<String>) {
                installedListener = listener
                throw IllegalStateException("listener registration failed")
            }

            override fun removeMatcherEditorListener(listener: MatcherEditor.Listener<String>) {
                cleanupHeldWriteLock = lock.isWriteLockedByCurrentThread
                removedListener = listener
            }

            override val matcher: Matcher<String>
                get() = error("matcher must not be requested")
        }

        assertThrows(IllegalStateException::class.java) {
            FilterList(BasicEventList<String>(lock), editor)
        }

        assertNotNull(installedListener)
        assertSame(installedListener, removedListener)
        assertFalse(cleanupHeldWriteLock)
    }

    @Test
    fun observableElementListConstructionAcquiresSourceReadLock() {
        val source = lockCheckingList("value")
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener? = null

            override fun uninstallListener(element: String, listener: EventListener) = Unit

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) = Unit
        }

        val observed = ObservableElementList(source, connector)

        observed.dispose()
    }

    @Test
    fun observableElementListConstructionAllowsSynchronousElementCallbacks() {
        val source = BasicEventList<String>().apply { add("value") }
        var handler: ObservableElementChangeHandler<out String>? = null
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener? {
                handler!!.elementChanged(element)
                return null
            }

            override fun uninstallListener(element: String, listener: EventListener) = Unit

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) {
                handler = list
            }
        }

        val observed = ObservableElementList(source, connector)

        observed.dispose()
    }

    @Test
    fun observableElementListConstructionRollsBackInstalledListenersWhenInitializationFails() {
        val lock = ReentrantReadWriteLock()
        val source = BasicEventList<String>(lock).apply { addAll(listOf("first", "second")) }
        val listener = object : EventListener {}
        val uninstalledElements = mutableListOf<String>()
        var handler: ObservableElementChangeHandler<out String>? = null
        var cleanupHeldWriteLock = false
        var installCount = 0
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener {
                if (++installCount == 2) throw IllegalStateException("installation failed")
                return listener
            }

            override fun uninstallListener(element: String, listener: EventListener) {
                cleanupHeldWriteLock = cleanupHeldWriteLock || lock.isWriteLockedByCurrentThread
                uninstalledElements += element
            }

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) {
                if (list == null) cleanupHeldWriteLock = cleanupHeldWriteLock || lock.isWriteLockedByCurrentThread
                handler = list
            }
        }

        assertThrows(IllegalStateException::class.java) {
            ObservableElementList(source, connector)
        }

        assertEquals(listOf("first"), uninstalledElements)
        assertNull(handler)
        assertFalse(cleanupHeldWriteLock)
    }

    @Test
    fun failedObservableElementListConstructionSuppressesCallbacksWhenConnectorDetachmentFails() {
        val source = BasicEventList<String>().apply { add("value") }
        var retainedHandler: ObservableElementChangeHandler<out String>? = null
        var detachAttempts = 0
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener =
                throw IllegalStateException("installation failed")

            override fun uninstallListener(element: String, listener: EventListener) = Unit

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) {
                if (list == null && ++detachAttempts == 1) throw IllegalStateException("detachment failed")
                retainedHandler = list
            }
        }

        val failure = assertThrows(IllegalStateException::class.java) {
            ObservableElementList(source, connector)
        }

        assertEquals(1, failure.suppressed.size)
        @Suppress("UNCHECKED_CAST")
        val retainedList =
            assertInstanceOf(ObservableElementList::class.java, retainedHandler) as ObservableElementList<String>
        var updateEvents = 0
        retainedList.addListEventListener { updateEvents++ }

        retainedHandler!!.elementChanged("value")

        assertEquals(0, updateEvents)

        assertDoesNotThrow(retainedList::dispose)
        assertEquals(2, detachAttempts)
        assertNull(retainedHandler)
    }

    @Test
    fun failedObservableElementListConstructionRetainsFailedCleanupForRetry() {
        val source = BasicEventList<String>().apply { addAll(listOf("first", "second")) }
        var retainedHandler: ObservableElementChangeHandler<out String>? = null
        var installCalls = 0
        var uninstallCalls = 0
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener {
                if (++installCalls == 2) throw IllegalStateException("installation failed")
                return object : EventListener {}
            }

            override fun uninstallListener(element: String, listener: EventListener) {
                if (++uninstallCalls == 1) throw IllegalStateException("cleanup failed")
            }

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) {
                retainedHandler = list
            }
        }

        val failure = assertThrows(IllegalStateException::class.java) {
            ObservableElementList(source, connector)
        }

        assertEquals(1, failure.suppressed.size)
        @Suppress("UNCHECKED_CAST")
        val retainedList = retainedHandler as ObservableElementList<String>

        assertDoesNotThrow(retainedList::dispose)

        assertEquals(2, uninstallCalls)
        assertNull(retainedHandler)
    }

    @Test
    fun observableElementListDisposalReleasesSourceLockBeforeConnectorCleanup() {
        val source = BasicEventList<String>().apply { add("value") }
        val disposalStarted = CountDownLatch(1)
        val releaseDisposal = CountDownLatch(1)
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener = object : EventListener {}

            override fun uninstallListener(element: String, listener: EventListener) {
                disposalStarted.countDown()
                releaseDisposal.await()
            }

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) = Unit
        }
        val observed = ObservableElementList(source, connector)

        Executors.newVirtualThreadPerTaskExecutor().use { executor ->
            val disposal = executor.submit(observed::dispose)
            try {
                assertTrue(disposalStarted.await(1, TimeUnit.SECONDS))
                val mutationAttemptingLock = CountDownLatch(1)
                val mutation = executor.submit {
                    mutationAttemptingLock.countDown()
                    source.readWriteLock.writeLock().lock()
                    try {
                        source.add("concurrent")
                    } finally {
                        source.readWriteLock.writeLock().unlock()
                    }
                }

                assertTrue(mutationAttemptingLock.await(1, TimeUnit.SECONDS))
                mutation.get(1, TimeUnit.SECONDS)
            } finally {
                releaseDisposal.countDown()
                disposal.get(1, TimeUnit.SECONDS)
            }
        }
    }

    @Test
    fun observableElementListDisposalRetriesFailedConnectorCleanup() {
        val source = BasicEventList<String>().apply { addAll(listOf("first", "second")) }
        var handler: ObservableElementChangeHandler<out String>? = null
        val uninstallCalls = mutableMapOf<String, Int>()
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener = object : EventListener {}

            override fun uninstallListener(element: String, listener: EventListener) {
                val callCount = uninstallCalls.merge(element, 1, Int::plus)!!
                if (element == "first" && callCount == 1) throw IllegalStateException("cleanup failed")
            }

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) {
                handler = list
            }
        }
        val observed = ObservableElementList(source, connector)

        assertThrows(IllegalStateException::class.java, observed::dispose)
        assertNotNull(handler)
        assertEquals(mapOf("first" to 1, "second" to 1), uninstallCalls)

        assertDoesNotThrow(observed::dispose)

        assertEquals(mapOf("first" to 2, "second" to 1), uninstallCalls)
        assertNull(handler)
    }

    @Test
    fun observableElementListDisposalRetriesFailedConnectorDetachment() {
        val source = BasicEventList<String>()
        var handler: ObservableElementChangeHandler<out String>? = null
        var detachAttempts = 0
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener? = null

            override fun uninstallListener(element: String, listener: EventListener) = Unit

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) {
                if (list == null && ++detachAttempts == 1) throw IllegalStateException("detachment failed")
                handler = list
            }
        }
        val observed = ObservableElementList(source, connector)

        assertThrows(IllegalStateException::class.java, observed::dispose)
        assertNotNull(handler)

        assertDoesNotThrow(observed::dispose)

        assertEquals(2, detachAttempts)
        assertNull(handler)
    }

    @Test
    fun observableElementListDisposalToleratesReentrantConnectorCleanup() {
        val source = BasicEventList<String>().apply { add("value") }
        lateinit var observed: ObservableElementList<String>
        var cleanupCalls = 0
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener = object : EventListener {}

            override fun uninstallListener(element: String, listener: EventListener) {
                cleanupCalls++
                observed.dispose()
            }

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) = Unit
        }
        observed = ObservableElementList(source, connector)

        observed.dispose()

        assertEquals(1, cleanupCalls)
    }

    @Test
    fun concurrentObservableElementListDisposalWaitsForAndRetriesFailedCleanup() {
        val source = BasicEventList<String>().apply { add("value") }
        val cleanupStarted = CountDownLatch(1)
        val releaseCleanup = CountDownLatch(1)
        var cleanupCalls = 0
        val connector = object : ObservableElementList.Connector<String> {
            override fun installListener(element: String): EventListener = object : EventListener {}

            override fun uninstallListener(element: String, listener: EventListener) {
                if (++cleanupCalls == 1) {
                    cleanupStarted.countDown()
                    releaseCleanup.await()
                    throw IllegalStateException("cleanup failed")
                }
            }

            override fun setObservableElementList(list: ObservableElementChangeHandler<out String>?) = Unit
        }
        val observed = ObservableElementList(source, connector)

        Executors.newVirtualThreadPerTaskExecutor().use { executor ->
            val firstDisposal = executor.submit(observed::dispose)
            try {
                assertTrue(cleanupStarted.await(1, TimeUnit.SECONDS))
                val secondDisposal = executor.submit(observed::dispose)
                assertFalse(secondDisposal.isDone)

                releaseCleanup.countDown()
                assertThrows(java.util.concurrent.ExecutionException::class.java) {
                    firstDisposal.get(1, TimeUnit.SECONDS)
                }
                assertDoesNotThrow { secondDisposal.get(1, TimeUnit.SECONDS) }
            } finally {
                releaseCleanup.countDown()
            }
        }

        assertEquals(2, cleanupCalls)
    }

    @Test
    fun tableFormatCallbacksRunOutsideTheSourceReadLock() {
        val source = BasicEventList<String>().apply { add("value") }
        val format = object : TableFormat<String> {
            override fun getColumnCount(): Int = 1

            override fun getColumnName(column: Int): String = "value"

            override fun getColumnValue(baseObject: String, column: Int): Any {
                source.readWriteLock.writeLock().lock()
                source.readWriteLock.writeLock().unlock()
                return baseObject
            }
        }
        val model = DefaultEventTableModel(source, format)

        assertEquals("value", model.getValueAt(0, 0))

        model.dispose()
    }

    private fun <E> lockCheckingList(vararg elements: E): DebugList<E> {
        val source = DebugList<E>()
        source.readWriteLock.writeLock().lock()
        try {
            source.addAll(elements.asList())
        } finally {
            source.readWriteLock.writeLock().unlock()
        }
        source.isLockCheckingEnabled = true
        return source
    }
}
