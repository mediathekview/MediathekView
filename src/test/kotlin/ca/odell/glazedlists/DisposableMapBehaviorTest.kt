package ca.odell.glazedlists

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class DisposableMapBehaviorTest {
    @Test
    fun disposableMapRetainsKotlinMutableMapOperations() {
        val map = TrackingDisposableMap<String, Int>()

        map["one"] = 1
        map.putAll(mapOf("two" to 2, "three" to 3))
        assertEquals(2, map.remove("two"))
        assertEquals(listOf("one" to 1, "three" to 3), map.entries.map { it.key to it.value })
        assertFalse(map.disposed)

        map.dispose()
        assertTrue(map.disposed)
    }

    private class TrackingDisposableMap<K, V> : LinkedHashMap<K, V>(), DisposableMap<K, V> {
        var disposed = false
            private set

        override fun dispose() {
            disposed = true
        }
    }
}
