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
import ca.odell.glazedlists.event.ListEventPublisher
import org.jspecify.annotations.NonNull
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import java.util.function.Consumer
import java.util.function.Predicate
import java.util.function.UnaryOperator

internal class DebugListBehaviorTest {
    @Test
    fun ordinaryReadsAndArraysMatchMutableListSemanticsIncludingNulls() {
        val list = DebugList<String?>().apply { addAll(listOf("alpha", null, "beta", "alpha")) }

        assertEquals(4, list.size)
        assertFalse(list.isEmpty())
        assertEquals("alpha", list[0])
        assertNull(list[1])
        assertTrue(list.contains(null))
        assertTrue(list.containsAll(listOf("beta", null)))
        assertFalse(list.containsAll(listOf("missing", null)))
        assertEquals(0, list.indexOf("alpha"))
        assertEquals(3, list.lastIndexOf("alpha"))
        assertEquals(-1, list.indexOf("missing"))
        assertEquals(listOf("alpha", null, "beta", "alpha"), list)
        assertEquals(listOf("alpha", null, "beta", "alpha").hashCode(), list.hashCode())
        assertEquals("[alpha, null, beta, alpha]", list.toString())

        val visited = mutableListOf<String?>()
        list.forEach(visited::add)
        assertEquals(list.toList(), visited)
        assertArrayEquals(arrayOf("alpha", null, "beta", "alpha"), list.toArray())

        val undersized = arrayOfNulls<String>(0)
        val grown = list.toArray(undersized)
        assertNotSame(undersized, grown)
        assertEquals(Array<String>::class.java, grown.javaClass)
        assertArrayEquals(arrayOf("alpha", null, "beta", "alpha"), grown)

        val oversized = arrayOf("old", "old", "old", "old", "old", "sentinel")
        val reused = list.toArray(oversized)
        assertSame(oversized, reused)
        assertArrayEquals(arrayOf("alpha", null, "beta", "alpha", null, "sentinel"), reused)
    }

    @Test
    fun directMutatorsReturnDelegateResultsAndForwardExactValues() {
        val list = DebugList<Box?>()
        val events = EventRecorder(list)
        val a = Box("a")
        val b = Box("b")
        val c = Box("c")
        val replacement = Box("replacement")

        assertTrue(list.add(a))
        assertEquals(listOf(event(change(ListEvent.INSERT, 0, unknown(), a))), events.take())

        list.add(0, null)
        assertEquals(listOf(event(change(ListEvent.INSERT, 0, unknown(), null))), events.take())

        assertTrue(list.addAll(listOf(b, c)))
        assertEquals(
            listOf(event(
                change(ListEvent.INSERT, 2, unknown(), b),
                change(ListEvent.INSERT, 3, unknown(), c),
            )),
            events.take(),
        )
        assertFalse(list.addAll(emptyList()))
        assertEquals(emptyList<RecordedEvent>(), events.take())

        assertTrue(list.addAll(1, listOf(replacement)))
        assertEquals(listOf(event(change(ListEvent.INSERT, 1, unknown(), replacement))), events.take())

        assertSame(a, list.set(2, b))
        assertEquals(listOf(event(change(ListEvent.UPDATE, 2, a, b))), events.take())

        assertTrue(list.remove(null))
        assertEquals(listOf(event(change(ListEvent.DELETE, 0, null, unknown()))), events.take())
        assertFalse(list.remove(Box("absent")))
        assertEquals(emptyList<RecordedEvent>(), events.take())

        assertSame(replacement, list.removeAt(0))
        assertEquals(listOf(event(change(ListEvent.DELETE, 0, replacement, unknown()))), events.take())

        list.clear()
        assertEquals(
            listOf(event(
                change(ListEvent.DELETE, 0, b, unknown()),
                change(ListEvent.DELETE, 0, b, unknown()),
                change(ListEvent.DELETE, 0, c, unknown()),
            )),
            events.take(),
        )
        list.clear()
        assertEquals(emptyList<RecordedEvent>(), events.take())
    }

    @Test
    fun bulkFunctionalMutatorsForwardDelegateBatchingAndSortUpdateEvents() {
        val one = Box("one")
        val two = Box("two")
        val three = Box("three")
        val list = DebugList<Box>().apply { addAll(listOf(three, one, two)) }
        val events = EventRecorder(list)

        assertTrue(list.removeIf { it === one || it === three })
        assertEquals(
            listOf(event(
                change(ListEvent.DELETE, 0, three, unknown()),
                change(ListEvent.DELETE, 0, one, unknown()),
            )),
            events.take(),
        )
        assertFalse(list.removeIf { false })
        assertEquals(emptyList<RecordedEvent>(), events.take())

        val oldTwo = list.single()
        list.replaceAll { Box(it.id.uppercase()) }
        val newTwo = list.single()
        assertNotSame(oldTwo, newTwo)
        assertEquals(
            listOf(event(change(ListEvent.UPDATE, 0, oldTwo, newTwo))),
            events.take(),
        )
        list.replaceAll { it }
        assertEquals(emptyList<RecordedEvent>(), events.take())

        val alpha = Box("alpha")
        val charlie = Box("charlie")
        list.addAll(listOf(charlie, alpha))
        events.take()
        java.util.Collections.sort(list, compareBy(Box::id))
        assertEquals(listOf("TWO", "alpha", "charlie"), list.map(Box::id))
        assertEquals(
            listOf(
                event(change(ListEvent.UPDATE, 0, newTwo, newTwo)),
                event(change(ListEvent.UPDATE, 1, charlie, alpha)),
                event(change(ListEvent.UPDATE, 2, alpha, charlie)),
            ),
            events.take(),
        )

        assertTrue(list.removeAll(listOf(newTwo, Box("absent"))))
        assertEquals(listOf(event(change(ListEvent.DELETE, 0, newTwo, unknown()))), events.take())
        assertFalse(list.removeAll(emptyList()))
        assertEquals(emptyList<RecordedEvent>(), events.take())

        assertTrue(list.retainAll(listOf(alpha)))
        assertEquals(listOf(event(change(ListEvent.DELETE, 1, charlie, unknown()))), events.take())
        assertFalse(list.retainAll(listOf(alpha)))
        assertEquals(emptyList<RecordedEvent>(), events.take())
    }

    @Test
    fun sanctionedSetsAreExactMutableInstancesAndEmptyMeansEveryThread() {
        val list = DebugList<String>().apply { add("value") }
        assertSame(list.sanctionedReaderThreads, list.sanctionedReaderThreads)
        assertSame(list.sanctionedWriterThreads, list.sanctionedWriterThreads)
        assertTrue(list.sanctionedReaderThreads is HashSet<*>)
        assertTrue(list.sanctionedWriterThreads is HashSet<*>)

        list.sanctionedReaderThreads.clear()
        list.sanctionedWriterThreads.clear()
        listOf(
            Thread.ofPlatform().name("debug-platform-empty").unstarted { list.size; list.add("platform") },
            Thread.ofVirtual().name("debug-virtual-empty").unstarted { list.size; list.add("virtual") },
        ).forEach { thread -> assertNull(runThread(thread), thread.toString()) }

        assertTrue(list.containsAll(listOf("value", "platform", "virtual")))
    }

    @Test
    fun sanctionedReadersAndWritersUseExactThreadIdentityAndDiagnostics() {
        val list = DebugList<String>().apply { add("value") }
        val current = Thread.currentThread()
        list.sanctionedReaderThreads += current
        list.sanctionedWriterThreads += current
        assertEquals(1, list.size)
        assertTrue(list.add("current"))

        val platformReadText = AtomicReference<String>()
        val platformRead = Thread.ofPlatform().name("unsanctioned-platform-reader").unstarted {
            platformReadText.set(Thread.currentThread().toString())
            list.size
        }
        val platformReadFailure = runThread(platformRead)
        assertInstanceOf(IllegalStateException::class.java, platformReadFailure)
        assertEquals(
            "DebugList detected an unexpected Thread (${platformReadText.get()}) attempting to perform a read operation",
            platformReadFailure!!.message,
        )

        val virtualWriteText = AtomicReference<String>()
        val virtualWrite = Thread.ofVirtual().name("unsanctioned-virtual-writer").unstarted {
            virtualWriteText.set(Thread.currentThread().toString())
            list.add("forbidden")
        }
        val virtualWriteFailure = runThread(virtualWrite)
        assertInstanceOf(IllegalStateException::class.java, virtualWriteFailure)
        assertEquals(
            "DebugList detected an unexpected Thread (${virtualWriteText.get()}) attempting to perform a write operation",
            virtualWriteFailure!!.message,
        )
        assertFalse(list.contains("forbidden"))

        list.sanctionedReaderThreads.clear()
        list.sanctionedWriterThreads.clear()
        val allowedAgain = Thread.ofVirtual().name("allowed-after-clear").unstarted { list.size; list.add("allowed") }
        assertNull(runThread(allowedAgain))
        assertTrue(list.contains("allowed"))
    }

    @Test
    fun lockCheckingCoversEveryOverriddenReadAndWriteOperation() {
        readOperations().forEach { operation ->
            val unlocked = seededList().apply { isLockCheckingEnabled = true }
            val readFailure = assertThrows(IllegalStateException::class.java, { operation.action(unlocked) }, operation.name)
            assertEquals(READ_LOCK_MESSAGE, readFailure.message, operation.name)

            val readLocked = seededList().apply { isLockCheckingEnabled = true }
            withLock(readLocked.readWriteLock.readLock()) { operation.action(readLocked) }

            val writeLocked = seededList().apply { isLockCheckingEnabled = true }
            withLock(writeLocked.readWriteLock.writeLock()) { operation.action(writeLocked) }
        }

        writeOperations().forEach { operation ->
            val unlocked = seededList().apply { isLockCheckingEnabled = true }
            val writeFailure = assertThrows(IllegalStateException::class.java, { operation.action(unlocked) }, operation.name)
            assertEquals(WRITE_LOCK_MESSAGE, writeFailure.message, operation.name)

            val readLocked = seededList().apply { isLockCheckingEnabled = true }
            withLock(readLocked.readWriteLock.readLock()) {
                val failure = assertThrows(IllegalStateException::class.java, { operation.action(readLocked) }, operation.name)
                assertEquals(WRITE_LOCK_MESSAGE, failure.message, operation.name)
            }

            val writeLocked = seededList().apply { isLockCheckingEnabled = true }
            withLock(writeLocked.readWriteLock.writeLock()) { operation.action(writeLocked) }
        }
    }

    @Test
    fun debugLockTracksReentrantCountsAndDoesNotRecordFailedUnlocks() {
        val list = DebugList<String>().apply { isLockCheckingEnabled = true }
        val read = list.readWriteLock.readLock()
        read.lock()
        read.lock()
        read.unlock()
        assertEquals(0, list.size)
        read.unlock()
        assertEquals(READ_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { list.size }.message)
        assertThrows(IllegalMonitorStateException::class.java, read::unlock)
        assertEquals(READ_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { list.size }.message)

        val write = list.readWriteLock.writeLock()
        write.lock()
        write.lock()
        write.unlock()
        assertTrue(list.add("held"))
        write.unlock()
        assertEquals(WRITE_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { list.add("not-held") }.message)
        assertThrows(IllegalMonitorStateException::class.java, write::unlock)
        assertEquals(WRITE_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { list.add("still-not-held") }.message)
    }

    @Test
    fun interruptedAndContendedFailedAcquisitionsAreNotRecorded() {
        val interrupted = DebugList<String>().apply { isLockCheckingEnabled = true }
        Thread.currentThread().interrupt()
        assertThrows(InterruptedException::class.java) { interrupted.readWriteLock.readLock().lockInterruptibly() }
        assertFalse(Thread.interrupted(), "lockInterruptibly clears the interrupted status when it throws")
        assertEquals(READ_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { interrupted.size }.message)

        Thread.currentThread().interrupt()
        assertThrows(InterruptedException::class.java) { interrupted.readWriteLock.writeLock().lockInterruptibly() }
        assertFalse(Thread.interrupted())
        assertEquals(WRITE_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { interrupted.add("x") }.message)

        val contended = DebugList<String>().apply { isLockCheckingEnabled = true }
        val acquired = CountDownLatch(1)
        val release = CountDownLatch(1)
        val holderFailure = AtomicReference<Throwable?>()
        val holder = Thread.ofVirtual().name("debug-lock-holder").start {
            try {
                contended.readWriteLock.writeLock().lock()
                acquired.countDown()
                release.await()
                contended.readWriteLock.writeLock().unlock()
            } catch (failure: Throwable) {
                holderFailure.set(failure)
            }
        }
        assertTrue(acquired.await(1, TimeUnit.SECONDS))
        try {
            assertFalse(contended.readWriteLock.readLock().tryLock())
            assertFalse(contended.readWriteLock.readLock().tryLock(1, TimeUnit.MILLISECONDS))
            assertFalse(contended.readWriteLock.writeLock().tryLock())
            assertFalse(contended.readWriteLock.writeLock().tryLock(1, TimeUnit.MILLISECONDS))
            assertEquals(READ_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { contended.size }.message)
            assertEquals(WRITE_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { contended.add("x") }.message)
        } finally {
            release.countDown()
            holder.join()
        }
        assertNull(holderFailure.get())
    }

    @Test
    fun allWriteAcquisitionMethodsRejectReadToWriteUpgradeWithExactMessage() {
        val lock = DebugList<String>().readWriteLock
        lock.readLock().lock()
        try {
            listOf<() -> Unit>(
                { lock.writeLock().lock() },
                { lock.writeLock().lockInterruptibly() },
                { lock.writeLock().tryLock() },
                { lock.writeLock().tryLock(1, TimeUnit.MILLISECONDS) },
            ).forEach { acquisition ->
                val failure = assertThrows(IllegalStateException::class.java, acquisition)
                assertEquals(UPGRADE_MESSAGE, failure.message)
            }
        } finally {
            lock.readLock().unlock()
        }
    }

    @Test
    fun writeReadWriteReentryAndConditionsPreserveAcquisitionRecording() {
        val list = DebugList<String>().apply { isLockCheckingEnabled = true }
        val lock = list.readWriteLock
        val write = lock.writeLock()
        val read = lock.readLock()
        write.lock()
        try {
            read.lockInterruptibly()
            try {
                assertTrue(write.tryLock())
                write.unlock()
                assertTrue(write.tryLock(1, TimeUnit.MILLISECONDS))
                write.unlock()
                assertTrue(list.add("reentrant"))
            } finally {
                read.unlock()
            }
        } finally {
            write.unlock()
        }
        assertEquals(WRITE_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { list.add("unlocked") }.message)

        assertThrows(UnsupportedOperationException::class.java) { read.newCondition() }
        val condition = write.newCondition()
        val readyToAwait = CountDownLatch(1)
        val signalFailure = AtomicReference<Throwable?>()
        write.lock()
        val signaler = Thread.ofVirtual().name("debug-condition-signaler").start {
            try {
                readyToAwait.await()
                write.lock()
                try {
                    condition.signal()
                } finally {
                    write.unlock()
                }
            } catch (failure: Throwable) {
                signalFailure.set(failure)
            }
        }
        readyToAwait.countDown()
        assertTrue(condition.await(1, TimeUnit.SECONDS))
        assertTrue(list.add("after-await"), "the reacquired write lock remains recorded")
        write.unlock()
        signaler.join()
        assertNull(signalFailure.get())
        assertEquals(WRITE_LOCK_MESSAGE, assertThrows(IllegalStateException::class.java) { list.add("after-unlock") }.message)
    }

    @Test
    fun createNewDebugListSharesOnlyInfrastructureAndWorksAsCompositeMembers() {
        val first = DebugList<String>().apply {
            add("first")
            isLockCheckingEnabled = true
            sanctionedReaderThreads += Thread.currentThread()
            sanctionedWriterThreads += Thread.currentThread()
        }
        val second = first.createNewDebugList<String>()

        assertTrue(second.isEmpty())
        assertSame(first.publisher, second.publisher)
        assertSame(first.readWriteLock, second.readWriteLock)
        assertSame(first.readWriteLock.readLock(), second.readWriteLock.readLock())
        assertSame(first.readWriteLock.writeLock(), second.readWriteLock.writeLock())
        assertFalse(second.isLockCheckingEnabled)
        assertTrue(second.sanctionedReaderThreads.isEmpty())
        assertTrue(second.sanctionedWriterThreads.isEmpty())
        assertNotSame(first.sanctionedReaderThreads, second.sanctionedReaderThreads)
        assertNotSame(first.sanctionedWriterThreads, second.sanctionedWriterThreads)

        first.isLockCheckingEnabled = false
        second.add("second")
        assertEquals(listOf("first"), first)
        assertEquals(listOf("second"), second)

        val composite = CompositeList<String>(first.publisher, first.readWriteLock)
        composite.addMemberList(first)
        composite.addMemberList(second)
        assertEquals(listOf("first", "second"), composite)
        second.add("pipeline")
        assertEquals(listOf("first", "second", "pipeline"), composite)
    }

    @Test
    fun disposalDetachesDelegateNullsItAndLeavesOuterInfrastructureUsable() {
        val list = seededList()
        val delegate = delegateOf(list)
        var forwardedEvents = 0
        val listener = { _: ListEvent<String> -> forwardedEvents++; Unit }
        list.addListEventListener(listener)
        val publisher = list.publisher
        val lock = list.readWriteLock
        val readers = list.sanctionedReaderThreads
        val writers = list.sanctionedWriterThreads

        list.dispose()
        assertNull(delegateField().get(list))
        delegate.add("detached")
        assertEquals(0, forwardedEvents)

        assertSame(readers, list.sanctionedReaderThreads)
        assertSame(writers, list.sanctionedWriterThreads)
        list.isLockCheckingEnabled = true
        assertTrue(list.isLockCheckingEnabled)
        assertDoesNotThrow { list.removeListEventListener(listener) }
        assertDoesNotThrow { list.addListEventListener(listener) }

        assertThrows(NullPointerException::class.java) { list.publisher }
        assertThrows(NullPointerException::class.java) { list.readWriteLock }
        assertThrows(NullPointerException::class.java) { list.createNewDebugList<Int>() }
        assertThrows(NullPointerException::class.java, list::dispose)
        assertSame(publisher, delegate.publisher)
        assertSame(lock, delegate.readWriteLock)
    }

    @Test
    fun outerAbiAndGenericSignaturesRemainJavaShapedAndOpen() {
        val type = DebugList::class.java
        assertTrue(Modifier.isPublic(type.modifiers))
        assertFalse(Modifier.isFinal(type.modifiers))
        assertEquals(listOf("E"), type.typeParameters.map { it.name })
        assertEquals("ca.odell.glazedlists.AbstractEventList<E>", type.genericSuperclass.typeName)

        val constructors = type.declaredConstructors.associateBy { it.parameterTypes.toList() }
        assertEquals(2, constructors.size)
        assertTrue(Modifier.isPublic(constructors.getValue(emptyList()).modifiers))
        val infrastructureConstructor = constructors.getValue(
            listOf(ListEventPublisher::class.java, nestedClass("DebugReadWriteLock")),
        )
        assertTrue(Modifier.isPrivate(infrastructureConstructor.modifiers))

        val actualSurface = type.declaredMethods
            .filter { Modifier.isPublic(it.modifiers) || Modifier.isProtected(it.modifiers) }
            .map(::surfaceSignature)
            .toSet()
        assertEquals(EXPECTED_DECLARED_SURFACE, actualSurface)
        type.declaredMethods
            .filter { Modifier.isPublic(it.modifiers) || Modifier.isProtected(it.modifiers) }
            .forEach { assertFalse(Modifier.isFinal(it.modifiers), it.toString()) }

        assertEquals(
            "public <E> ca.odell.glazedlists.DebugList<E> ca.odell.glazedlists.DebugList.createNewDebugList()",
            type.getDeclaredMethod("createNewDebugList").toGenericString(),
        )
        assertEquals(
            "public java.util.Set<java.lang.Thread>",
            type.getDeclaredMethod("getSanctionedReaderThreads").genericReturnType.typeName.let { "public $it" },
        )
        assertEquals(
            "public java.util.Set<java.lang.Thread>",
            type.getDeclaredMethod("getSanctionedWriterThreads").genericReturnType.typeName.let { "public $it" },
        )

        assertEquals("java.util.Iterator", type.getMethod("iterator").returnType.name)
        assertEquals("java.util.ListIterator", type.getMethod("listIterator").returnType.name)
        assertEquals("java.util.ListIterator", type.getMethod("listIterator", Int::class.javaPrimitiveType).returnType.name)
        assertEquals(
            "java.util.List",
            type.getMethod("subList", Int::class.javaPrimitiveType, Int::class.javaPrimitiveType).returnType.name,
        )
        assertEquals(java.util.Spliterator::class.java, type.getMethod("spliterator").returnType)
        assertEquals(java.util.stream.Stream::class.java, type.getMethod("stream").returnType)
        assertEquals(java.util.stream.Stream::class.java, type.getMethod("parallelStream").returnType)
        assertEquals(Void.TYPE, type.getMethod("close").returnType)
    }

    @Test
    fun jspecifyTypeUseAnnotationsRemainOnArraysCollectionsAndFunctions() {
        val type = DebugList::class.java
        val noArgArray = type.getDeclaredMethod("toArray")
        assertEquals(listOf(NonNull::class.java), noArgArray.annotatedReturnType.annotations.map { it.annotationClass.java })

        val typedArray = type.getDeclaredMethod("toArray", Array<Any>::class.java)
        assertEquals(listOf(NonNull::class.java), typedArray.annotatedReturnType.annotations.map { it.annotationClass.java })
        assertEquals(
            listOf(NonNull::class.java),
            typedArray.annotatedParameterTypes.single().annotations.map { it.annotationClass.java },
        )

        listOf(
            type.getDeclaredMethod("removeIf", Predicate::class.java),
            type.getDeclaredMethod("addAll", Collection::class.java),
            type.getDeclaredMethod("addAll", Int::class.javaPrimitiveType, Collection::class.java),
            type.getDeclaredMethod("removeAll", Collection::class.java),
            type.getDeclaredMethod("retainAll", Collection::class.java),
            type.getDeclaredMethod("replaceAll", UnaryOperator::class.java),
        ).forEach { method ->
            val annotated = method.annotatedParameterTypes.last().annotations.map { it.annotationClass.java }
            assertEquals(listOf(NonNull::class.java), annotated, method.toString())
        }
        assertTrue(type.getDeclaredMethod("forEach", Consumer::class.java).annotatedParameterTypes.single().annotations.isEmpty())
        assertTrue(type.getDeclaredMethod("sort", Comparator::class.java).annotatedParameterTypes.single().annotations.isEmpty())
    }

    @Test
    fun nestedImplementationTypesRetainEffectivePrivacyAndStaticShape() {
        val outer = DebugList::class.java
        val forwarder = nestedClass("ListEventForwarder")
        val readWrite = nestedClass("DebugReadWriteLock")
        val debugLock = readWrite.declaredClasses.single { it.simpleName == "DebugLock" }

        assertEquals(setOf("ListEventForwarder", "DebugReadWriteLock"), outer.declaredClasses.map { it.simpleName }.toSet())
        assertTrue(Modifier.isPrivate(forwarder.modifiers))
        assertFalse(Modifier.isStatic(forwarder.modifiers))
        assertTrue(Modifier.isPrivate(readWrite.modifiers))
        assertTrue(Modifier.isStatic(readWrite.modifiers))
        assertTrue(Modifier.isPrivate(debugLock.modifiers))
        assertTrue(Modifier.isStatic(debugLock.modifiers))

        assertEquals(1, forwarder.declaredConstructors.size)
        if (DebugList::class.java.isAnnotationPresent(Metadata::class.java)) {
            assertTrue(Modifier.isPublic(forwarder.declaredConstructors.single().modifiers))
        } else {
            assertTrue(Modifier.isPrivate(forwarder.declaredConstructors.single().modifiers))
        }
        assertArrayEquals(arrayOf<Class<*>>(DebugList::class.java), forwarder.declaredConstructors.single().parameterTypes)
        assertEquals(1, readWrite.declaredConstructors.size)
        assertTrue(Modifier.isPublic(readWrite.declaredConstructors.single().modifiers))
        assertEquals(1, debugLock.declaredConstructors.size)
        assertTrue(Modifier.isPublic(debugLock.declaredConstructors.single().modifiers))
        assertEquals(listOf("java.util.concurrent.locks.Lock", debugLock.name), debugLock.declaredConstructors.single().parameterTypes.map { it.name })
    }

    private fun readOperations(): List<NamedOperation> = listOf(
        NamedOperation("get") { it[0] },
        NamedOperation("size") { it.size },
        NamedOperation("contains") { it.contains("one") },
        NamedOperation("containsAll") { it.containsAll(listOf("one", "three")) },
        NamedOperation("equals") { check(it == listOf("one", "two", "three")) },
        NamedOperation("hashCode") { it.hashCode() },
        NamedOperation("indexOf") { it.indexOf("two") },
        NamedOperation("lastIndexOf") { it.lastIndexOf("two") },
        NamedOperation("isEmpty") { it.isEmpty() },
        NamedOperation("forEach") { it.forEach { _ -> } },
        NamedOperation("toArray") { it.toArray() },
        NamedOperation("toArray(T[])") { it.toArray(arrayOfNulls<String>(4)) },
        NamedOperation("toString") { it.toString() },
    )

    private fun writeOperations(): List<NamedOperation> = listOf(
        NamedOperation("add") { it.add("four") },
        NamedOperation("remove(Object)") { it.remove("two") },
        NamedOperation("removeIf") { it.removeIf { value -> value == "two" } },
        NamedOperation("addAll") { it.addAll(listOf("four", "five")) },
        NamedOperation("addAll(index)") { it.addAll(1, listOf("four", "five")) },
        NamedOperation("removeAll") { it.removeAll(listOf("two")) },
        NamedOperation("retainAll") { it.retainAll(listOf("one", "two")) },
        NamedOperation("replaceAll") { it.replaceAll { value -> value.uppercase() } },
        NamedOperation("sort") { java.util.Collections.sort(it, reverseOrder()) },
        NamedOperation("clear") { it.clear() },
        NamedOperation("set") { it[0] = "changed" },
        NamedOperation("add(index)") { it.add(1, "inserted") },
        NamedOperation("remove(index)") { it.removeAt(0) },
    )

    private fun seededList() = DebugList<String>().apply { addAll(listOf("one", "two", "three")) }

    private fun runThread(thread: Thread): Throwable? {
        val failure = AtomicReference<Throwable?>()
        thread.uncaughtExceptionHandler = Thread.UncaughtExceptionHandler { _, throwable -> failure.set(throwable) }
        thread.start()
        thread.join()
        return failure.get()
    }

    private fun withLock(lock: java.util.concurrent.locks.Lock, action: () -> Unit) {
        lock.lock()
        try {
            action()
        } finally {
            lock.unlock()
        }
    }

    @Suppress("UNCHECKED_CAST")
    private fun delegateOf(list: DebugList<String>): EventList<String> = delegateField().get(list) as EventList<String>

    private fun delegateField() = DebugList::class.java.getDeclaredField("delegate").apply { trySetAccessible() }

    private fun nestedClass(simpleName: String) = DebugList::class.java.declaredClasses.single { it.simpleName == simpleName }

    private fun surfaceSignature(method: Method): String {
        val visibility = if (Modifier.isPublic(method.modifiers)) "public" else "protected"
        return "$visibility ${method.returnType.typeName} ${method.name}(${method.parameterTypes.joinToString(",") { it.typeName }})"
    }

    private fun unknown(): Any = ListEvent.UNKNOWN_VALUE

    private fun change(type: Int, index: Int, old: Any?, new: Any?) = Change(type, index, old, new)

    private fun event(vararg changes: Change) = RecordedEvent(changes = changes.toList())

    private data class NamedOperation(val name: String, val action: (DebugList<String>) -> Unit)

    private class EventRecorder<E>(private val expectedSource: EventList<E>) {
        private val events = mutableListOf<RecordedEvent>()

        init {
            expectedSource.addListEventListener { event ->
                assertSame(expectedSource, event.sourceList)
                if (event.isReordering) {
                    events += RecordedEvent(reorderMap = event.reorderMap.toList())
                } else {
                    val changes = mutableListOf<Change>()
                    while (event.next()) {
                        changes += Change(event.type, event.index, event.oldValue, event.newValue)
                    }
                    events += RecordedEvent(changes = changes)
                }
            }
        }

        fun take(): List<RecordedEvent> = events.toList().also { events.clear() }
    }

    private data class RecordedEvent(
        val reorderMap: List<Int>? = null,
        val changes: List<Change> = emptyList(),
    )

    private data class Change(
        val type: Int,
        val index: Int,
        val oldValue: Any?,
        val newValue: Any?,
    )

    private class Box(val id: String) {
        override fun toString(): String = "Box($id)"
    }

    companion object {
        private const val READ_LOCK_MESSAGE =
            "DebugList detected a failure to acquire the readLock prior to a read operation"
        private const val WRITE_LOCK_MESSAGE =
            "DebugList detected a failure to acquire the writeLock prior to a write operation"
        private const val UPGRADE_MESSAGE =
            "DebugList detected an attempt to acquire a writeLock from a thread already owning a readLock (deadlock)"

        private val EXPECTED_DECLARED_SURFACE = setOf(
            "public boolean isLockCheckingEnabled()",
            "public void setLockCheckingEnabled(boolean)",
            "public java.util.Set getSanctionedReaderThreads()",
            "public java.util.Set getSanctionedWriterThreads()",
            "public ca.odell.glazedlists.DebugList createNewDebugList()",
            "protected void beforeReadOperation()",
            "protected void afterReadOperation()",
            "protected void beforeWriteOperation()",
            "protected void afterWriteOperation()",
            "public java.util.concurrent.locks.ReadWriteLock getReadWriteLock()",
            "public ca.odell.glazedlists.event.ListEventPublisher getPublisher()",
            "public java.lang.Object get(int)",
            "public int size()",
            "public boolean contains(java.lang.Object)",
            "public boolean containsAll(java.util.Collection)",
            "public boolean equals(java.lang.Object)",
            "public int hashCode()",
            "public int indexOf(java.lang.Object)",
            "public int lastIndexOf(java.lang.Object)",
            "public boolean isEmpty()",
            "public void forEach(java.util.function.Consumer)",
            "public java.lang.Object[] toArray()",
            "public java.lang.Object[] toArray(java.lang.Object[])",
            "public java.lang.String toString()",
            "public boolean add(java.lang.Object)",
            "public boolean remove(java.lang.Object)",
            "public boolean removeIf(java.util.function.Predicate)",
            "public boolean addAll(java.util.Collection)",
            "public boolean addAll(int,java.util.Collection)",
            "public boolean removeAll(java.util.Collection)",
            "public boolean retainAll(java.util.Collection)",
            "public void replaceAll(java.util.function.UnaryOperator)",
            "public void sort(java.util.Comparator)",
            "public void clear()",
            "public java.lang.Object set(int,java.lang.Object)",
            "public void add(int,java.lang.Object)",
            "public java.lang.Object remove(int)",
            "public void dispose()",
        )
    }
}
