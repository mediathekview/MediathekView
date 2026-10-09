package ca.odell.glazedlists

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class TypeSafetyListenerTest {
    @Test
    fun acceptsConfiguredTypesAndNullWhenExplicitlyAllowed() {
        val source = BasicEventList<Any?>()
        source.enforceTypes(linkedSetOf<Class<*>?>(String::class.java, null))

        assertDoesNotThrow {
            source += "allowed"
            source += null
        }
        assertThrows(IllegalArgumentException::class.java) { source += 1 }
    }

    @Test
    fun rejectsNullWhenItIsNotConfigured() {
        val source = BasicEventList<Any?>()
        source.enforceTypes(setOf<Class<*>>(String::class.java))

        assertThrows(IllegalArgumentException::class.java) { source += null }
    }

    @Test
    fun validatesUpdatesAndIncludesTheOffendingIndexAndValue() {
        val source = BasicEventList<Any>()
        source += "allowed"
        source.enforceTypes(setOf<Class<*>>(String::class.java))

        val exception = assertThrows(IllegalArgumentException::class.java) { source[0] = 42 }

        assertEquals(
            "Element with illegal type class java.lang.Integer updated at index 0: 42",
            exception.message,
        )
    }
}
