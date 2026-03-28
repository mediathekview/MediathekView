/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.logging

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.selects.onTimeout
import kotlinx.coroutines.selects.select
import kotlinx.coroutines.swing.Swing
import mediathek.swing.IconUtils
import org.kordamp.ikonli.fontawesome6.FontAwesomeRegular
import java.awt.BorderLayout
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.BadLocationException
import javax.swing.text.Document
import javax.swing.text.PlainDocument

class LogPanel(
    private val repo: LogRepositoryFacade = DefaultRepoFacade,
    private val maxChars: Int = 2_000_000,
    private val batchSize: Int = 500,
    private val flushIntervalMs: Long = 75L
) : JPanel(BorderLayout()) {
    private val textArea = JTextArea().apply {
        isEditable = false
        lineWrap = false
        document = PlainDocument()
        font = UIManager.getFont("TextArea.font")
    }
    private val scrollPane = JScrollPane(textArea)

    private val autoScrollToggle = JToggleButton("Auto-scroll", true)
    private val btnClearFilter = JButton().apply {
        icon = IconUtils.of(FontAwesomeRegular.TRASH_ALT)
        toolTipText = "Clear Filter"
    }
    private val filterField = JTextField().apply {
        toolTipText = "Text filter"
    }

    @Volatile
    private var textFilter: String = ""

    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val workerScope = CoroutineScope(SupervisorJob() + Dispatchers.Default)

    private val tsFmt = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")
    private val zone = ZoneId.systemDefault()

    init {
        val toolbar = JToolBar().apply {
            isFloatable = false
            add(autoScrollToggle)
            addSeparator()
            add(JLabel("Filter: "))
            add(filterField)
            addSeparator()
            add(btnClearFilter)
        }

        add(toolbar, BorderLayout.NORTH)
        add(scrollPane, BorderLayout.CENTER)

        btnClearFilter.addActionListener {
            uiScope.launch { filterField.text = "" }
        }

        filterField.document.addDocumentListener(object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent) = onChange()
            override fun removeUpdate(e: DocumentEvent) = onChange()
            override fun changedUpdate(e: DocumentEvent) = onChange()
            private fun onChange() {
                textFilter = filterField.text.trim()
                reloadFromSnapshot()
            }
        })

        reloadFromSnapshot()
        startConsumer()
    }

    /** Reload view from repository snapshot according to current filters. */
    private fun reloadFromSnapshot() {
        val snapshot = repo.snapshot()

        uiScope.launch {
            val sb = StringBuilder(snapshot.size * 64)
            for (e in snapshot) {
                if (matchesFilters(e)) sb.append(formatLine(e))
            }
            textArea.text = sb.toString()
            capDocument(textArea.document, maxChars)
            if (autoScrollToggle.isSelected) {
                textArea.caretPosition = textArea.document.length
            }
        }
    }

    @OptIn(ExperimentalCoroutinesApi::class)
    private fun startConsumer() {
        workerScope.launch {
            val live = repo.channel()
            val buffer = ArrayList<LogEntry>(batchSize)

            while (isActive) {
                select {
                    live.onReceive { entry ->
                        if (matchesFilters(entry)) {
                            buffer.add(entry)
                            if (buffer.size >= batchSize)
                                flush(buffer)
                        }
                    }

                    onTimeout(flushIntervalMs) {
                        if (buffer.isNotEmpty())
                            flush(buffer)
                    }
                }
            }
        }
    }

    private suspend fun flush(buffer: MutableList<LogEntry>) {
        val chunk = buildString(buffer.size * 80) {
            for (e in buffer)
                append(formatLine(e))
        }
        buffer.clear()

        uiScope.launch {
            appendText(chunk)
        }.join()
    }

    private fun appendText(text: String) {
        val doc = textArea.document
        try {
            doc.insertString(doc.length, text, null)
            capDocument(doc, maxChars)
            if (autoScrollToggle.isSelected) {
                textArea.caretPosition = doc.length
            }
        } catch (_: BadLocationException) {
            // ignore
        }
    }

    private fun capDocument(doc: Document, maxChars: Int) {
        val extra = doc.length - maxChars
        if (extra <= 0) return
        try {
            doc.remove(0, extra)
        } catch (_: BadLocationException) {
            // ignore
        }
    }

    private fun matchesFilters(e: LogEntry): Boolean {
        val f = textFilter
        if (f.isEmpty())
            return true

        val hay = buildString {
            append(e.message)
            if (e.thrown != null)
                append('\n').append(e.thrown)
            append(' ').append(e.logger)
            append(' ').append(e.thread)
            append(' ').append(e.level.name())
        }
        return hay.contains(f, ignoreCase = true)
    }

    private fun formatLine(e: LogEntry): String {
        val ts = tsFmt.format(e.instant.atZone(zone))
        val base = "$ts ${e.level.name().padEnd(5)} ${e.logger} [${e.thread}] - ${e.message}"
        return if (e.thrown.isNullOrBlank()) {
            base + "\n"
        } else {
            base + "\n" + e.thrown + "\n"
        }
    }

    /** Call on window close to avoid coroutine leaks. */
    fun dispose() {
        workerScope.cancel()
        uiScope.cancel()
    }
}

/**
 * Facade makes the panel testable and keeps it decoupled from the singleton.
 */
interface LogRepositoryFacade {
    fun snapshot(): List<LogEntry>
    fun channel(): Channel<LogEntry>
}

object DefaultRepoFacade : LogRepositoryFacade {
    override fun snapshot(): List<LogEntry> = LogRepository.snapshot()
    override fun channel(): Channel<LogEntry> = LogRepository.channel()
}