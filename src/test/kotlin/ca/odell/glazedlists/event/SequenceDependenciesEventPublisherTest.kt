package ca.odell.glazedlists.event

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.impl.WeakReferenceProxy
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.ref.WeakReference
import java.lang.reflect.Modifier
import java.util.function.Consumer

internal class SequenceDependenciesEventPublisherTest {
    @Test
    fun removingUnknownListenerRemainsNoOp() {
        val publisher = SequenceDependenciesEventPublisher()

        assertDoesNotThrow {
            publisher.removeListener(Any(), Any())
        }
    }

    @Test
    fun repeatedUpdatesRetainOriginalAndLatestValues() {
        val source = BasicEventList<String>().apply { add("latest") }
        val assembler = ListEventAssembler(source, source.publisher)
        val previousValues = mutableListOf<String>()
        assembler.addListEventListener { event ->
            while (event.next()) previousValues += event.oldValue
        }

        assembler.beginEvent()
        assembler.elementUpdated(0, "original", "middle")
        assembler.elementUpdated(0, "middle", "latest")
        assembler.commitEvent()

        assertEquals(listOf("original"), previousValues)
        assertEquals("latest", source.single())
    }

    @Test
    fun publisherRecoversAfterListenerThrowsError() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val delivered = mutableListOf<String>()
        var failNext = true
        val listener = Consumer<String> { event ->
            if (failNext) {
                failNext = false
                throw AssertionError("expected test failure")
            }
            delivered += event
        }
        val format = consumerFormat<NamedSubject>()
        publisher.addListener(subject, listener, format)

        assertThrows(AssertionError::class.java) {
            publisher.fireEvent(subject, "first", format)
        }
        assertDoesNotThrow {
            publisher.fireEvent(subject, "second", format)
        }

        assertEquals(listOf("second"), delivered)
    }

    @Test
    fun secondaryListenerAndCleanupFailuresAreSuppressedInDeliveryOrder() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val firstFailure = IllegalStateException("first listener")
        val secondFailure = IllegalArgumentException("second listener")
        val cleanupFailure = UnsupportedOperationException("cleanup")
        val format = consumerFormat<NamedSubject>(onPost = { throw cleanupFailure })
        publisher.addListener(subject, Consumer { throw firstFailure }, format)
        publisher.addListener(subject, Consumer { throw secondFailure }, format)

        val thrown = assertThrows(IllegalStateException::class.java) {
            publisher.fireEvent(subject, "event", format)
        }

        assertSame(firstFailure, thrown)
        assertArrayEquals(arrayOf(secondFailure, cleanupFailure), thrown.suppressed)
    }

    @Test
    fun repeatedFailureInstanceIsNotSuppressedOnItself() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val failure = IllegalStateException("shared")
        val format = consumerFormat<NamedSubject>()
        publisher.addListener(subject, Consumer { throw failure }, format)
        publisher.addListener(subject, Consumer { throw failure }, format)

        val thrown = assertThrows(IllegalStateException::class.java) {
            publisher.fireEvent(subject, "event", format)
        }

        assertSame(failure, thrown)
        assertArrayEquals(emptyArray<Throwable>(), thrown.suppressed)
    }

    @Test
    fun equalSubjectsRemainDistinct() {
        val publisher = SequenceDependenciesEventPublisher()
        val firstSubject = EqualSubject("same")
        val secondSubject = EqualSubject("same")
        val firstEvents = mutableListOf<String>()
        val secondEvents = mutableListOf<String>()
        val format = consumerFormat<EqualSubject>()
        publisher.addListener(firstSubject, Consumer(firstEvents::add), format)
        publisher.addListener(secondSubject, Consumer(secondEvents::add), format)

        publisher.fireEvent(firstSubject, "first", format)
        publisher.fireEvent(secondSubject, "second", format)

        assertEquals(listOf("first"), firstEvents)
        assertEquals(listOf("second"), secondEvents)
    }

    @Test
    fun chainDependenciesPreserveOrderDuringReentrantPublication() {
        val publisher = SequenceDependenciesEventPublisher()
        val rootSubject = NamedSubject("root")
        val upstreamSubject = NamedSubject("upstream")
        val downstreamSubject = NamedSubject("downstream")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val downstreamListener = Consumer<String> { trace += "downstream:$it" }
        val upstreamListener = Consumer<String> {
            trace += "upstream:$it"
            publisher.fireEvent(downstreamSubject, "downstream-event", format)
        }
        val rootListener = Consumer<String> {
            trace += "root:$it"
            publisher.fireEvent(upstreamSubject, "upstream-event", format)
        }

        publisher.addListener(downstreamSubject, downstreamListener, format)
        publisher.addListener(upstreamSubject, upstreamListener, format)
        publisher.setRelatedListener(downstreamSubject, upstreamListener)
        publisher.addListener(rootSubject, rootListener, format)
        publisher.setRelatedListener(upstreamSubject, rootListener)

        publisher.fireEvent(rootSubject, "root-event", format)

        assertEquals(
            listOf(
                "root:root-event",
                "upstream:upstream-event",
                "downstream:downstream-event",
            ),
            trace,
        )
    }

    @Test
    fun multiPrerequisiteDependenciesOverrideReversePublicationOrder() {
        val publisher = SequenceDependenciesEventPublisher()
        val rootSubject = NamedSubject("root")
        val subjectB = NamedSubject("subject-b")
        val subjectC = NamedSubject("subject-c")
        val subjectD = NamedSubject("subject-d")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val listenerB = Consumer<String> { trace += "b:$it" }
        val listenerC = Consumer<String> { trace += "c:$it" }
        val listenerD = Consumer<String> { trace += "d:$it" }
        val rootListener = Consumer<String> {
            trace += "root:$it"
            publisher.fireEvent(subjectD, "event-d", format)
            publisher.fireEvent(subjectC, "event-c", format)
            publisher.fireEvent(subjectB, "event-b", format)
        }

        publisher.addListener(subjectD, listenerD, format)
        publisher.addListener(subjectC, listenerC, format)
        publisher.addListener(subjectB, listenerB, format)
        publisher.setRelatedListener(subjectC, listenerB)
        publisher.setRelatedListener(subjectD, listenerB)
        publisher.setRelatedListener(subjectD, listenerC)
        publisher.addListener(rootSubject, rootListener, format)

        publisher.fireEvent(rootSubject, "root-event", format)

        assertEquals(
            listOf(
                "root:root-event",
                "b:event-b",
                "c:event-c",
                "d:event-d",
            ),
            trace,
        )
    }

    @Test
    fun relatedSubjectRedirectDefersOwnerEventUntilChildListenerCompletes() {
        val publisher = SequenceDependenciesEventPublisher()
        val rootSubject = NamedSubject("root")
        val ownerSubject = NamedSubject("owner")
        val childSubject = NamedSubject("child")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val ownerListener = Consumer<String> { trace += "owner:$it" }
        val childListener = Consumer<String> { trace += "child:$it" }
        val rootListener = Consumer<String> {
            trace += "root:$it"
            publisher.fireEvent(ownerSubject, "owner-event", format)
            publisher.fireEvent(childSubject, "child-event", format)
        }

        publisher.addListener(ownerSubject, ownerListener, format)
        publisher.setRelatedSubject(childListener, ownerSubject)
        publisher.addListener(childSubject, childListener, format)
        publisher.addListener(rootSubject, rootListener, format)

        publisher.fireEvent(rootSubject, "root-event", format)

        assertEquals(
            listOf(
                "root:root-event",
                "child:child-event",
                "owner:owner-event",
            ),
            trace,
        )
    }

    @Test
    fun reentrantPublicationOfTheSameSubjectFailsAfterPendingListenersComplete() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>(onPost = { trace += "post:$it" })
        val firstListener = Consumer<String> {
            trace += "first:$it"
            publisher.fireEvent(subject, "nested", format)
        }
        val secondListener = Consumer<String> { trace += "second:$it" }

        publisher.addListener(subject, firstListener, format)
        publisher.addListener(subject, secondListener, format)

        val thrown = assertThrows(IllegalStateException::class.java) {
            publisher.fireEvent(subject, "outer", format)
        }

        assertEquals("Reentrant fireEvent() by \"$subject\"", thrown.message)
        assertArrayEquals(emptyArray<Throwable>(), thrown.suppressed)
        assertEquals(
            listOf(
                "first:outer",
                "second:outer",
                "post:$subject",
            ),
            trace,
        )
    }

    @Test
    fun listenerCyclesAreRejected() {
        val publisher = SequenceDependenciesEventPublisher()
        val first = NamedSubject("first")
        val second = NamedSubject("second")
        publisher.setRelatedListener(first, second)

        assertThrows(IllegalStateException::class.java) {
            publisher.setRelatedListener(second, first)
        }
    }

    @Test
    fun cyclesIntroducedByRelatedSubjectsFailOnTheNextStructuralReorder() {
        val publisher = SequenceDependenciesEventPublisher()
        val firstSubject = NamedSubject("first")
        val secondSubject = NamedSubject("second")
        val unrelatedSubject = NamedSubject("unrelated")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val firstListener = Consumer<String> { trace += "first:$it" }
        val secondListener = Consumer<String> { trace += "second:$it" }

        publisher.addListener(firstSubject, firstListener, format)
        publisher.addListener(secondSubject, secondListener, format)
        publisher.setRelatedSubject(firstListener, secondSubject)
        publisher.setRelatedSubject(secondListener, firstSubject)

        assertDoesNotThrow {
            publisher.fireEvent(firstSubject, "before-reorder", format)
        }

        val thrown = assertThrows(IllegalStateException::class.java) {
            publisher.addListener(unrelatedSubject, Consumer<String> { trace += "unrelated:$it" }, format)
        }

        assertTrue(thrown.message!!.startsWith("Listener cycle detected"))
        assertEquals(emptyList<Consumer<String>>(), publisher.getListeners<Consumer<String>>(unrelatedSubject))

        assertDoesNotThrow {
            publisher.fireEvent(secondSubject, "after-failed-add", format)
        }

        assertEquals(
            listOf(
                "first:before-reorder",
                "second:after-failed-add",
            ),
            trace,
        )
    }

    @Test
    fun equalButDistinctListenersRemainDistinctAndRemovalUsesIdentity() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val firstListener = EqualButDistinctListener("same", "first", trace)
        val secondListener = EqualButDistinctListener("same", "second", trace)

        publisher.addListener(subject, firstListener, format)
        publisher.addListener(subject, secondListener, format)
        publisher.fireEvent(subject, "event-1", format)
        publisher.removeListener(subject, firstListener)
        publisher.fireEvent(subject, "event-2", format)

        assertEquals(
            listOf(
                "first:event-1",
                "second:event-1",
                "second:event-2",
            ),
            trace,
        )
    }

    @Test
    fun duplicateRegistrationNotifiesTwiceAndRemovesOneRegistrationAtATime() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val listener = Consumer<String> { trace += it }

        publisher.addListener(subject, listener, format)
        publisher.addListener(subject, listener, format)
        publisher.fireEvent(subject, "event-1", format)
        publisher.removeListener(subject, listener)
        publisher.fireEvent(subject, "event-2", format)
        publisher.removeListener(subject, listener)
        publisher.fireEvent(subject, "event-3", format)

        assertEquals(listOf("event-1", "event-1", "event-2"), trace)
    }

    @Test
    fun listenersAddedDuringDispatchObserveOnlySubsequentEvents() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val lateListener = Consumer<String> { trace += "late:$it" }
        var registered = false
        val initialListener = Consumer<String> { event ->
            trace += "initial:$event"
            if (!registered) {
                publisher.addListener(subject, lateListener, format)
                registered = true
            }
        }

        publisher.addListener(subject, initialListener, format)

        publisher.fireEvent(subject, "event-1", format)
        publisher.fireEvent(subject, "event-2", format)

        assertEquals(
            listOf(
                "initial:event-1",
                "initial:event-2",
                "late:event-2",
            ),
            trace,
        )
    }

    @Test
    fun listenersRemovedDuringDispatchStillReceiveTheCurrentEventOnly() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val lateListener = Consumer<String> { trace += "late:$it" }
        val initialListener = Consumer<String> { event ->
            trace += "initial:$event"
            publisher.removeListener(subject, lateListener)
        }

        publisher.addListener(subject, initialListener, format)
        publisher.addListener(subject, lateListener, format)

        publisher.fireEvent(subject, "event-1", format)
        publisher.fireEvent(subject, "event-2", format)

        assertEquals(
            listOf(
                "initial:event-1",
                "late:event-1",
                "initial:event-2",
            ),
            trace,
        )
    }

    @Test
    fun postEventRunsAfterAllListenersOfASubject() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>(onPost = { trace += "post:$it" })

        publisher.addListener(subject, Consumer<String> { trace += "first:$it" }, format)
        publisher.addListener(subject, Consumer<String> { trace += "second:$it" }, format)

        publisher.fireEvent(subject, "event", format)

        assertEquals(
            listOf(
                "first:event",
                "second:event",
                "post:$subject",
            ),
            trace,
        )
    }

    @Test
    fun staleListenersArePrunedOnlyDuringStructuralUpdatesAndRetainLiveOrder() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val unrelatedSubject = NamedSubject("unrelated")
        val trace = mutableListOf<String>()
        val staleChecks = mutableListOf<String>()
        val firstListener = Consumer<String> { trace += "first:$it" }
        val lastListener = Consumer<String> { trace += "last:$it" }
        val staleListener = Consumer<String> { trace += "stale:$it" }
        val format = consumerFormat<NamedSubject>(
            isStale = { _, listener ->
                when {
                    listener === firstListener -> staleChecks += "first"
                    listener === lastListener -> staleChecks += "last"
                    listener === staleListener -> staleChecks += "stale"
                }
                listener === staleListener
            },
        )

        publisher.addListener(subject, firstListener, format)
        publisher.addListener(subject, lastListener, format)
        publisher.addListener(subject, staleListener, format)
        staleChecks.clear()

        publisher.fireEvent(subject, "before-prune", format)
        assertEquals(
            listOf(
                "first:before-prune",
                "last:before-prune",
                "stale:before-prune",
            ),
            trace,
        )
        assertEquals(emptyList<String>(), staleChecks)

        publisher.addListener(unrelatedSubject, Consumer<String> {}, format)

        assertEquals(listOf("first", "last", "stale"), staleChecks)
        assertEquals(listOf(firstListener, lastListener), publisher.getListeners<Consumer<String>>(subject))

        publisher.fireEvent(subject, "after-prune", format)

        assertEquals(
            listOf(
                "first:before-prune",
                "last:before-prune",
                "stale:before-prune",
                "first:after-prune",
                "last:after-prune",
            ),
            trace,
        )
    }

    @Test
    fun listEventAssemblerPrunesAndDisposesStaleWeakReferenceProxy() {
        val source = BasicEventList<String>()
        val assembler = ListEventAssembler(source, source.publisher)
        val proxyTarget = ListEventListener<String> {}
        val proxy = WeakReferenceProxy(source, proxyTarget)
        val liveListener = ListEventListener<String> {}

        assembler.addListEventListener(proxy)
        clearWeakReferenceProxyTarget(proxy)

        assembler.addListEventListener(liveListener)

        assertEquals(listOf(liveListener), assembler.getListEventListeners())
        assertNull(weakReferenceProxySource(proxy))
    }

    @Test
    fun clearRelatedSubjectRequiresTheNextStructuralReorderBeforeOrderChanges() {
        val publisher = SequenceDependenciesEventPublisher()
        val rootSubject = NamedSubject("root")
        val ownerSubject = NamedSubject("owner")
        val childSubject = NamedSubject("child")
        val reorderSubject = NamedSubject("reorder")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val ownerListener = Consumer<String> { trace += "owner:$it" }
        val childListener = Consumer<String> { trace += "child:$it" }
        var phase = "phase-1"
        val rootListener = Consumer<String> {
            trace += "root:$phase"
            publisher.fireEvent(childSubject, "child-$phase", format)
            publisher.fireEvent(ownerSubject, "owner-$phase", format)
        }

        publisher.addListener(ownerSubject, ownerListener, format)
        publisher.setRelatedSubject(childListener, ownerSubject)
        publisher.addListener(childSubject, childListener, format)
        publisher.addListener(rootSubject, rootListener, format)

        publisher.fireEvent(rootSubject, "root-1", format)

        phase = "phase-2"
        publisher.clearRelatedSubject(childListener)
        publisher.fireEvent(rootSubject, "root-2", format)

        phase = "phase-3"
        publisher.setRelatedSubject(ownerListener, childSubject)
        publisher.addListener(reorderSubject, Consumer<String> {}, format)
        publisher.fireEvent(rootSubject, "root-3", format)

        assertEquals(
            listOf(
                "root:phase-1",
                "child:child-phase-1",
                "owner:owner-phase-1",
                "root:phase-2",
                "child:child-phase-2",
                "owner:owner-phase-2",
                "root:phase-3",
                "owner:owner-phase-3",
                "child:child-phase-3",
            ),
            trace,
        )
    }

    @Test
    fun clearRelatedListenerRemovesTheSyntheticDependencyWithoutPerturbingEstablishedDeliveryOrder() {
        val publisher = SequenceDependenciesEventPublisher()
        val rootSubject = NamedSubject("root")
        val upstreamSubject = NamedSubject("upstream")
        val downstreamSubject = NamedSubject("downstream")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>()
        val upstreamListener = Consumer<String> { trace += "upstream:$it" }
        val downstreamListener = Consumer<String> { trace += "downstream:$it" }
        var phase = "phase-1"
        val rootListener = Consumer<String> {
            trace += "root:$phase"
            publisher.fireEvent(downstreamSubject, "downstream-$phase", format)
            publisher.fireEvent(upstreamSubject, "upstream-$phase", format)
        }

        publisher.addListener(downstreamSubject, downstreamListener, format)
        publisher.addListener(upstreamSubject, upstreamListener, format)
        publisher.setRelatedListener(downstreamSubject, upstreamListener)
        publisher.addListener(rootSubject, rootListener, format)

        assertEquals(listOf(downstreamSubject), publisher.getListeners<NamedSubject>(upstreamListener))

        publisher.fireEvent(rootSubject, "root-1", format)

        phase = "phase-2"
        publisher.clearRelatedListener(downstreamSubject, upstreamListener)
        assertEquals(emptyList<NamedSubject>(), publisher.getListeners<NamedSubject>(upstreamListener))
        publisher.fireEvent(rootSubject, "root-2", format)

        assertEquals(
            listOf(
                "root:phase-1",
                "upstream:upstream-phase-1",
                "downstream:downstream-phase-1",
                "root:phase-2",
                "upstream:upstream-phase-2",
                "downstream:downstream-phase-2",
            ),
            trace,
        )
    }

    @Test
    fun nestedPublicationRunsPostEventOnlyAfterAllNestedDeliveries() {
        val publisher = SequenceDependenciesEventPublisher()
        val rootSubject = NamedSubject("root")
        val upstreamSubject = NamedSubject("upstream")
        val downstreamSubject = NamedSubject("downstream")
        val trace = mutableListOf<String>()
        val format = consumerFormat<NamedSubject>(onPost = { trace += "post:$it" })
        val downstreamListener = Consumer<String> { trace += "downstream:$it" }
        val upstreamListener = Consumer<String> {
            trace += "upstream:$it"
            publisher.fireEvent(downstreamSubject, "downstream-event", format)
        }
        val rootListener = Consumer<String> {
            trace += "root:$it"
            publisher.fireEvent(upstreamSubject, "upstream-event", format)
        }

        publisher.addListener(downstreamSubject, downstreamListener, format)
        publisher.addListener(upstreamSubject, upstreamListener, format)
        publisher.setRelatedListener(downstreamSubject, upstreamListener)
        publisher.addListener(rootSubject, rootListener, format)
        publisher.setRelatedListener(upstreamSubject, rootListener)

        publisher.fireEvent(rootSubject, "root-event", format)

        assertEquals(
            listOf(
                "root:root-event",
                "upstream:upstream-event",
                "downstream:downstream-event",
            ),
            trace.take(3),
        )

        val lastDelivery = trace.indexOf("downstream:downstream-event")
        assertTrue(trace.indexOf("post:$rootSubject") > lastDelivery)
        assertTrue(trace.indexOf("post:$upstreamSubject") > lastDelivery)
        assertTrue(trace.indexOf("post:$downstreamSubject") > lastDelivery)
        assertTrue(trace.indexOf("post:$rootSubject") > trace.indexOf("root:root-event"))
        assertTrue(trace.indexOf("post:$upstreamSubject") > trace.indexOf("upstream:upstream-event"))
        assertTrue(trace.indexOf("post:$downstreamSubject") > trace.indexOf("downstream:downstream-event"))
    }

    @Test
    fun publisherRecoversAfterPostEventCleanupException() {
        val publisher = SequenceDependenciesEventPublisher()
        val subject = NamedSubject("subject")
        val trace = mutableListOf<String>()
        var failCleanup = true
        val format = consumerFormat<NamedSubject>(
            onPost = {
                trace += "post:$it"
                if (failCleanup) {
                    failCleanup = false
                    throw UnsupportedOperationException("cleanup")
                }
            },
        )

        publisher.addListener(subject, Consumer<String> { trace += "listener:$it" }, format)

        val thrown = assertThrows(UnsupportedOperationException::class.java) {
            publisher.fireEvent(subject, "first", format)
        }

        assertEquals("cleanup", thrown.message)

        assertDoesNotThrow {
            publisher.fireEvent(subject, "second", format)
        }

        assertEquals(
            listOf(
                "listener:first",
                "post:$subject",
                "listener:second",
                "post:$subject",
            ),
            trace,
        )
    }

    @Test
    fun publisherRetainsRequiredJvmAccessFlagsAndSynchronizedMethods() {
        val publisherClass = SequenceDependenciesEventPublisher::class.java
        val constructor = publisherClass.declaredConstructors.single()
        val addListener =
            publisherClass.getDeclaredMethod(
                "addListener",
                Any::class.java,
                Any::class.java,
                SequenceDependenciesEventPublisher.EventFormat::class.java,
            )
        val removeListener = publisherClass.getDeclaredMethod("removeListener", Any::class.java, Any::class.java)
        val getListeners = publisherClass.getDeclaredMethod("getListeners", Any::class.java)
        val fireEvent =
            publisherClass.getDeclaredMethod(
                "fireEvent",
                Any::class.java,
                Any::class.java,
                SequenceDependenciesEventPublisher.EventFormat::class.java,
            )

        assertTrue(Modifier.isFinal(publisherClass.modifiers))
        assertFalse(Modifier.isAbstract(publisherClass.modifiers))
        assertFalse(Modifier.isPrivate(publisherClass.modifiers))
        assertFalse(Modifier.isProtected(publisherClass.modifiers))
        assertEquals(0, constructor.parameterCount)
        assertFalse(Modifier.isPrivate(constructor.modifiers))
        assertFalse(Modifier.isProtected(constructor.modifiers))
        assertEquals(Modifier.isPublic(publisherClass.modifiers), Modifier.isPublic(constructor.modifiers))
        assertTrue(Modifier.isSynchronized(addListener.modifiers))
        assertTrue(Modifier.isSynchronized(removeListener.modifiers))
        assertTrue(Modifier.isSynchronized(getListeners.modifiers))
        assertFalse(Modifier.isSynchronized(fireEvent.modifiers))
    }

    private fun <S : Any> consumerFormat(
        onFire: (subject: S, event: String, listener: Consumer<String>) -> Unit = { _, event, listener ->
            listener.accept(event)
        },
        onPost: (S) -> Unit = {},
        isStale: (subject: S, listener: Consumer<String>) -> Boolean = { _, _ -> false },
    ) = object : SequenceDependenciesEventPublisher.EventFormat<S, Consumer<String>, String> {
        override fun fire(subject: S, event: String, listener: Consumer<String>) =
            onFire(subject, event, listener)

        override fun postEvent(subject: S) = onPost(subject)

        override fun isStale(subject: S, listener: Consumer<String>): Boolean = isStale(subject, listener)
    }

    private fun clearWeakReferenceProxyTarget(proxy: WeakReferenceProxy<*>) {
        val field = WeakReferenceProxy::class.java.getDeclaredField("proxyTargetReference")
        field.isAccessible = true
        @Suppress("UNCHECKED_CAST")
        val reference = field.get(proxy) as WeakReference<Any?>
        reference.clear()
    }

    private fun weakReferenceProxySource(proxy: WeakReferenceProxy<*>): Any? {
        val field = WeakReferenceProxy::class.java.getDeclaredField("source")
        field.isAccessible = true
        return field.get(proxy)
    }

    private data class EqualSubject(val value: String)

    private class NamedSubject(private val name: String) {
        override fun toString(): String = name
    }

    private class EqualButDistinctListener(
        private val equalityKey: String,
        private val id: String,
        private val trace: MutableList<String>,
    ) : Consumer<String> {
        override fun accept(event: String) {
            trace += "$id:$event"
        }

        override fun equals(other: Any?): Boolean =
            other is EqualButDistinctListener && other.equalityKey == equalityKey

        override fun hashCode(): Int = equalityKey.hashCode()
    }
}
