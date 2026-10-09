package mediathek.mainwindow

import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.swing.SwingDispatcher
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.JLabel
import javax.swing.JProgressBar

class FilmlistProgressPresenterTest {
    @Test
    fun `progress events are coalesced until UI dispatch runs`() {
        val dispatcher = QueuedSwingDispatcher()
        val handle = TestStatusBarProgressHandle()
        val presenter = FilmlistProgressPresenter(dispatcher) { handle }

        presenter.start(event())
        dispatcher.runNext()

        presenter.progress(event(text = "first", max = 10, progress = 1))
        presenter.progress(event(text = "second", max = 10, progress = 2))
        presenter.progress(event(text = "third", max = 10, progress = 3))

        assertEquals(1, dispatcher.pendingActions)

        dispatcher.runNext()

        assertEquals("third", handle.label.text)
        assertEquals(10, handle.progressBar.maximum)
        assertEquals(3, handle.progressBar.value)
        assertFalse(handle.progressBar.isIndeterminate)
    }

    @Test
    fun `completed progress is shown as indeterminate`() {
        val dispatcher = QueuedSwingDispatcher()
        val handle = TestStatusBarProgressHandle()
        val presenter = FilmlistProgressPresenter(dispatcher) { handle }

        presenter.start(event())
        dispatcher.runNext()
        presenter.progress(event(text = "done", max = 10, progress = 10))
        dispatcher.runNext()

        assertEquals("done", handle.label.text)
        assertTrue(handle.progressBar.isIndeterminate)
    }

    private fun event(
        text: String = "",
        max: Int = 0,
        progress: Int = 0,
    ): ListenerFilmeLadenEvent = ListenerFilmeLadenEvent("", text, max, progress, false)

    private class QueuedSwingDispatcher : SwingDispatcher {
        private val actions = ArrayDeque<Runnable>()
        val pendingActions: Int
            get() = actions.size

        override fun dispatch(action: Runnable) {
            actions += action
        }

        fun runNext() {
            actions.removeFirst().run()
        }
    }

    private class TestStatusBarProgressHandle : StatusBarProgressHandle {
        val label = JLabel()
        val progressBar = JProgressBar()
        var closed = false
            private set

        override fun label(): JLabel = label

        override fun progressBar(): JProgressBar = progressBar

        override fun close() {
            closed = true
        }
    }
}
