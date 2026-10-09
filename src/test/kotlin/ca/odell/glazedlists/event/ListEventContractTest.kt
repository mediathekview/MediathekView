package ca.odell.glazedlists.event

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test

internal class ListEventContractTest {
    @Test
    fun constantsAndUnknownValueRetainLegacyIdentity() {
        assertEquals(0, ListEvent.DELETE)
        assertEquals(1, ListEvent.UPDATE)
        assertEquals(2, ListEvent.INSERT)
        assertSame("UNKNOWN VALUE", ListEvent.UNKNOWN_VALUE)
        assertEquals("UNKNOWN VALUE", ListEvent.UNKNOWN_VALUE.toString())
        assertSame(ListEvent.unknownValue<Any>(), ListEvent.UNKNOWN_VALUE)
    }

    @Test
    fun unknownValueRetainsIdentityAcrossGenericViews() {
        val asString: String = ListEvent.unknownValue()
        val asAny: Any = ListEvent.unknownValue()
        val asCharSequence: CharSequence = ListEvent.unknownValue()

        assertSame(ListEvent.UNKNOWN_VALUE, asString as Any)
        assertSame(asAny, ListEvent.UNKNOWN_VALUE)
        assertSame(ListEvent.UNKNOWN_VALUE, asCharSequence as Any)
    }

    @Test
    fun protectedSourceFieldsRemainMutableAndCanDiverge() {
        val originalSource = BasicEventList<String>()
        val replacementSource = BasicEventList<String>()
        val event = MutableListEvent(originalSource)

        assertSame(originalSource, event.source)
        assertSame(originalSource, event.sourceList)

        event.replaceSourceList(replacementSource)

        assertSame(originalSource, event.source)
        assertSame(replacementSource, event.sourceList)

        event.replaceEventObjectSource(replacementSource)

        assertSame(replacementSource, event.source)
        assertSame(replacementSource, event.sourceList)
    }

    private class MutableListEvent<E>(sourceList: EventList<E>) : ListEvent<E>(sourceList) {
        fun replaceSourceList(replacement: EventList<E>) {
            sourceList = replacement
        }

        fun replaceEventObjectSource(replacement: EventList<E>) {
            source = replacement
        }

        override fun copy(): ListEvent<E> = this

        override fun reset() = Unit

        override fun next(): Boolean = false

        override fun hasNext(): Boolean = false

        override fun nextBlock(): Boolean = false

        override val isReordering: Boolean = false

        override val reorderMap: IntArray = IntArray(0)

        override val index: Int = 0

        override val blockStartIndex: Int = 0

        override val blockEndIndex: Int = 0

        override val type: Int = UPDATE

        override val oldValue: E = ListEvent.unknownValue()

        override val newValue: E = ListEvent.unknownValue()

        override val blocksRemaining: Int = 0

        override fun toString(): String = "MutableListEvent"
    }

}
