package mediathek.gui.bookmark

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.ObservableElementList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.swing.DefaultEventSelectionModel
import ca.odell.glazedlists.swing.eventTableModel
import ca.odell.glazedlists.swing.swingThreadProxyList
import mediathek.tool.withWriteLock
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.time.LocalDate
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.SwingUtilities

class BookmarkTablePipelineTest {
    @Test
    fun `typed table format exposes bookmark values without bean lookup`() {
        val added = LocalDate.of(2026, 2, 3)
        val availableUntil = LocalDate.of(2026, 3, 4)
        val bookmark = BookmarkData().apply {
            seen = true
            originalSender = "Sender"
            originalThema = "Thema"
            originalTitle = "Titel"
            this.availableUntil = availableUntil
            note = "Notiz"
            filmHashCode = "hash"
            bookmarkAdded = added
        }

        assertEquals(11, BookmarkTableFormat.getColumnCount())
        assertEquals(
            listOf(
                "Gesehen",
                "Sender",
                "Thema",
                "Titel",
                "Dauer",
                "Sendedatum",
                "Verfügbar bis",
                "URL",
                "Notiz",
                "Hash Code",
                "hinzugefügt am",
            ),
            (0 until BookmarkTableFormat.getColumnCount()).map(BookmarkTableFormat::getColumnName),
        )
        assertEquals(
            listOf(true, "Sender", "Thema", "Titel", -1, null, availableUntil, null, "Notiz", "hash", added),
            (0 until BookmarkTableFormat.getColumnCount()).map { BookmarkTableFormat.getColumnValue(bookmark, it) },
        )
    }

    @Test
    fun `typed bookmark connector publishes in-place changes`() {
        val bookmark = BookmarkData()
        val source = BasicEventList<BookmarkData>().apply { add(bookmark) }
        val observed = ObservableElementList(source, BookmarkObservableConnector())
        var updateCount = 0
        observed.addListEventListener { event ->
            while (event.next()) {
                if (event.type == ListEvent.UPDATE) updateCount++
            }
        }

        bookmark.note = "Notiz"

        assertEquals(1, updateCount)
        observed.dispose()
        source.dispose()
    }

    @Test
    fun `existing bookmarks are present when table pipeline is created`() {
        val source = BasicEventList<BookmarkData>().apply {
            repeat(6) { add(BookmarkData()) }
        }
        val pipeline = BookmarkTablePipeline(source)
        val observed = pipeline.observedBookmarks
        val sorted = pipeline.sortedBookmarks
        val swingBookmarks = sorted.swingThreadProxyList()
        val model = swingBookmarks.eventTableModel(BookmarkTableFormat)

        assertEquals(6, source.size)
        assertEquals(6, observed.size)
        assertEquals(6, sorted.size)
        assertEquals(6, model.rowCount)

        model.dispose()
        swingBookmarks.dispose()
        sorted.dispose()
        observed.dispose()
        source.dispose()
    }

    @Test
    fun `shared Swing proxy keeps selection updates on the EDT`() {
        val first = BookmarkData().apply { bookmarkAdded = LocalDate.of(2026, 1, 2) }
        val selectedBookmark = BookmarkData().apply { bookmarkAdded = LocalDate.of(2026, 1, 3) }
        val source = BasicEventList<BookmarkData>().apply { addAll(listOf(first, selectedBookmark)) }
        val observed = ObservableElementList(source, BookmarkObservableConnector())
        val sorted = SortedList(observed, BookmarkAddedAtComparator())
        val swingBookmarks = sorted.swingThreadProxyList()
        val model = swingBookmarks.eventTableModel(BookmarkTableFormat)
        val selectionModel = DefaultEventSelectionModel(swingBookmarks)
        val selectionChanged = CountDownLatch(1)
        val selectionChangedOnEdt = AtomicBoolean()

        SwingUtilities.invokeAndWait {
            selectionModel.setSelectionInterval(1, 1)
            selectionModel.addListSelectionListener { event ->
                if (!event.valueIsAdjusting) {
                    selectionChangedOnEdt.set(SwingUtilities.isEventDispatchThread())
                    selectionChanged.countDown()
                }
            }
        }

        try {
            Thread.ofVirtual().start {
                source.withWriteLock {
                    add(BookmarkData().apply { bookmarkAdded = LocalDate.of(2026, 1, 1) })
                }
            }.join()

            assertTrue(selectionChanged.await(5, TimeUnit.SECONDS))
            assertTrue(selectionChangedOnEdt.get())
            SwingUtilities.invokeAndWait {
                assertEquals(3, model.rowCount)
                assertSame(selectedBookmark, selectionModel.selected.single())
            }
        } finally {
            SwingUtilities.invokeAndWait {
                selectionModel.dispose()
                model.dispose()
                swingBookmarks.dispose()
                sorted.dispose()
                observed.dispose()
                source.dispose()
            }
        }
    }
}
