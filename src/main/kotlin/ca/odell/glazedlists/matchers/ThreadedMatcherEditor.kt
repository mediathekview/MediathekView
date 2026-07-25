package ca.odell.glazedlists.matchers

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import java.lang.Runnable
import java.util.concurrent.Executor
import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.coroutines.CoroutineContext

/**
 * Buffers matcher changes and delivers coalesced events from a coroutine.
 *
 * The supplied [executor] controls where the processing coroutine runs. If it
 * is an [ExecutorService], ownership remains with the caller.
 */
class ThreadedMatcherEditor<E>(
    private val source: MatcherEditor<E>,
    val executor: Executor = DEFAULT_EXECUTOR,
) : AbstractMatcherEditorListenerSupport<E>(), AutoCloseable {

    private val dispatcher = object : CoroutineDispatcher() {
        override fun dispatch(context: CoroutineContext, block: Runnable) {
            try {
                executor.execute(block)
            } catch (_: RuntimeException) {
                DEFAULT_EXECUTOR.execute(block)
            } catch (_: Error) {
                DEFAULT_EXECUTOR.execute(block)
            }
        }
    }
    private val scope = CoroutineScope(SupervisorJob() + dispatcher)
    private val matcherEvents = Channel<MatcherEditor.Event<E>>(Channel.UNLIMITED)
    private val closed = AtomicBoolean()
    private val queuingMatcherEditorListener = MatcherEditor.Listener { event: MatcherEditor.Event<E> ->
        if (!closed.get()) matcherEvents.trySend(event)
    }
    private val processingJob: Job

    init {
        source.addMatcherEditorListener(queuingMatcherEditorListener)
        processingJob = scope.launch(start = CoroutineStart.UNDISPATCHED) {
            for (firstEvent in matcherEvents) {
                val batch = ArrayList<MatcherEditor.Event<E>>()
                batch += firstEvent
                while (true) {
                    val nextEvent = matcherEvents.tryReceive().getOrNull() ?: break
                    batch += nextEvent
                }

                try {
                    fireChangedMatcher(coalesceMatcherEvents(batch))
                } catch (failure: RuntimeException) {
                    reportFailure(failure)
                } catch (failure: Error) {
                    reportFailure(failure)
                }
            }
        }
    }

    override val matcher: Matcher<E>
        get() = source.matcher

    private fun coalesceMatcherEvents(
        matcherEvents: List<MatcherEditor.Event<E>>,
    ): MatcherEditor.Event<E> {
        val lastMatcherEvent = matcherEvents.last()
        val lastMatcherEventType = lastMatcherEvent.type
        var changeType = false

        if (lastMatcherEventType != MatcherEditor.Event.MATCH_ALL &&
            lastMatcherEventType != MatcherEditor.Event.MATCH_NONE
        ) {
            var constrained = false
            var relaxed = false

            for (matcherEvent in matcherEvents) {
                when (matcherEvent.type) {
                    MatcherEditor.Event.MATCH_ALL,
                    MatcherEditor.Event.RELAXED,
                        -> relaxed = true

                    MatcherEditor.Event.MATCH_NONE,
                    MatcherEditor.Event.CONSTRAINED,
                        -> constrained = true

                    MatcherEditor.Event.CHANGED -> {
                        constrained = true
                        relaxed = true
                    }
                }
            }
            changeType = constrained && relaxed
        }

        return MatcherEditor.Event(
            this,
            if (changeType) MatcherEditor.Event.CHANGED else lastMatcherEventType,
            lastMatcherEvent.matcher,
        )
    }

    fun dispose() {
        close()
    }

    override fun close() {
        if (!closed.compareAndSet(false, true)) return
        source.removeMatcherEditorListener(queuingMatcherEditorListener)
        matcherEvents.close()
        processingJob.cancel()
        scope.cancel()
    }

    private fun reportFailure(failure: Throwable) {
        val thread = Thread.currentThread()
        thread.uncaughtExceptionHandler.uncaughtException(thread, failure)
    }

    private companion object {
        val DEFAULT_EXECUTOR = Executor { runnable ->
            Thread.ofVirtual().name("MatcherQueueThread").start(runnable)
        }
    }
}
