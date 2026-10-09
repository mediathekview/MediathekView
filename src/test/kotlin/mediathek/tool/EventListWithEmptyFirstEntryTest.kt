package mediathek.tool

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.event.ListEvent
import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class EventListWithEmptyFirstEntryTest {
    @Test
    fun sourceChangesAreReportedAfterThePermanentEmptyEntry() {
        BasicEventList<String>().use { source ->
            source.add("original")
            EventListWithEmptyFirstEntry(source).use { displayed ->
                val changes = mutableListOf<Change>()
                displayed.addListEventListener { event ->
                    while (event.next()) {
                        changes += Change(event.type, event.index)
                    }
                }

                source.add(0, "inserted")
                assertEquals(
                    listOf(Change(ListEvent.INSERT, 1)),
                    changes,
                )
                assertEquals(listOf("", "inserted", "original"), displayed.toList())

                changes.clear()
                source[0] = "updated"
                assertEquals(
                    listOf(Change(ListEvent.UPDATE, 1)),
                    changes,
                )
                assertEquals(listOf("", "updated", "original"), displayed.toList())

                changes.clear()
                source.removeAt(0)
                assertEquals(
                    listOf(Change(ListEvent.DELETE, 1)),
                    changes,
                )
                assertEquals(listOf("", "original"), displayed.toList())
            }
        }
    }

    @Test
    fun reorderMapKeepsThePermanentEmptyEntryAtIndexZero() {
        BasicEventList<String>().use { source ->
            source.addAll(listOf("B", "A"))
            SortedList(source, null).use { sorted ->
                EventListWithEmptyFirstEntry(sorted).use { displayed ->
                    var reorderMap: IntArray? = null
                    displayed.addListEventListener { event ->
                        if (event.isReordering) {
                            reorderMap = event.reorderMap
                        }
                    }

                    sorted.comparator = naturalOrder()

                    assertArrayEquals(intArrayOf(0, 2, 1), reorderMap)
                    assertEquals(listOf("", "A", "B"), displayed.toList())
                }
            }
        }
    }

    private data class Change(
        val type: Int,
        val index: Int,
    )
}
