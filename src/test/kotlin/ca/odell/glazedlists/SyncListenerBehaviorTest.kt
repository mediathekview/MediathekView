package ca.odell.glazedlists

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

internal class SyncListenerBehaviorTest {
    @Test
    fun constructorReplacesTargetContentsWithSourceContents() {
        BasicEventList<String>().use { source ->
            source.addAll(listOf("first", "second"))
            val target = mutableListOf("stale")

            val listener = SyncListener(source, target)

            assertEquals(source, target)
            listener.dispose()
        }
    }

    @Test
    fun sourceInsertsUpdatesAndDeletesAreAppliedToTarget() {
        BasicEventList<String>().use { source ->
            source.addAll(listOf("first", "second"))
            val target = mutableListOf<String>()
            val listener = SyncListener(source, target)

            source.add(1, "inserted")
            source[0] = "updated"
            source.removeAt(2)

            assertEquals(listOf("updated", "inserted"), target)
            listener.dispose()
        }
    }

    @Test
    fun externallyModifiedTargetIsRejectedBeforeTheNextChangeIsApplied() {
        BasicEventList<String>().use { source ->
            source += "first"
            val target = mutableListOf<String>()
            val listener = SyncListener(source, target)
            target += "external"

            val failure = assertThrows(IllegalStateException::class.java) {
                source += "second"
            }

            assertEquals("Synchronize EventList target has been modified", failure.message)
            assertEquals(listOf("first", "external"), target)
            listener.dispose()
        }
    }

    @Test
    fun disposeIsIdempotentAndStopsSynchronization() {
        BasicEventList<String>().use { source ->
            val target = mutableListOf<String>()
            val listener = SyncListener(source, target)

            listener.dispose()
            listener.dispose()
            source += "ignored"

            assertEquals(emptyList<String>(), target)
        }
    }
}
