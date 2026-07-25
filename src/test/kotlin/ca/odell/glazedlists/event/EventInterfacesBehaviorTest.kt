package ca.odell.glazedlists.event

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class EventInterfacesBehaviorTest {
    @Test
    fun publisherContractForwardsRelatedObjects() {
        val publisher = RecordingPublisher()

        publisher.setRelatedSubject("listener", "subject")
        publisher.clearRelatedSubject("listener")
        publisher.setRelatedListener("subject", "related")
        publisher.clearRelatedListener("subject", "related")

        assertEquals(
            listOf(
                listOf("setSubject", "listener", "subject"),
                listOf("clearSubject", "listener"),
                listOf("setListener", "subject", "related"),
                listOf("clearListener", "subject", "related"),
            ),
            publisher.calls,
        )
    }

    private class RecordingPublisher : ListEventPublisher {
        val calls = mutableListOf<List<Any?>>()

        override fun setRelatedSubject(listener: Any, relatedSubject: Any) {
            calls += listOf("setSubject", listener, relatedSubject)
        }

        override fun clearRelatedSubject(listener: Any) {
            calls += listOf("clearSubject", listener)
        }

        override fun setRelatedListener(subject: Any, relatedListener: Any) {
            calls += listOf("setListener", subject, relatedListener)
        }

        override fun clearRelatedListener(subject: Any, relatedListener: Any) {
            calls += listOf("clearListener", subject, relatedListener)
        }
    }
}
