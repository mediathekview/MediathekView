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
import mediathek.config.Konstanten
import mediathek.swing.IconUtils
import mediathek.tool.FileDialogs
import org.kordamp.ikonli.fontawesome6.FontAwesomeRegular
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.BorderLayout
import java.awt.Frame
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.BadLocationException
import javax.swing.text.DefaultCaret
import javax.swing.text.Document
import javax.swing.text.PlainDocument
import kotlin.time.Duration.Companion.milliseconds

class LogPanel(
    private val repo: LogRepositoryFacade = DefaultRepoFacade,
    private val maxChars: Int = 2_000_000,
    private val batchSize: Int = 500,
    private val flushIntervalMs: Long = 75L
) : JPanel(BorderLayout()) {
    companion object {
        private val hiddenLoggers = setOf("mediathek.tool.dns.DnsSelector")
    }

    private val textArea = JTextArea().apply {
        isEditable = false
        lineWrap = false
        document = PlainDocument()
        font = UIManager.getFont("TextArea.font")
    }
    private val scrollPane = JScrollPane(textArea)

    private val autoScrollToggle = JToggleButton("Auto-scroll", true)
    private val btnExport = JButton().apply {
        icon = IconUtils.of(FontAwesomeSolid.DOWNLOAD)
        toolTipText = "Export Log"
    }
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
            add(btnExport)
            addSeparator()
            add(btnClearFilter)
        }

        add(toolbar, BorderLayout.NORTH)
        add(scrollPane, BorderLayout.CENTER)

        autoScrollToggle.addActionListener {
            updateAutoScrollBehavior()
        }

        btnExport.addActionListener {
            exportVisibleContent()
        }

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

        updateAutoScrollBehavior()
        reloadFromSnapshot()
        startConsumer()
    }

    private fun updateAutoScrollBehavior() {
        val caret = textArea.caret as? DefaultCaret ?: return
        if (autoScrollToggle.isSelected) {
            caret.updatePolicy = DefaultCaret.ALWAYS_UPDATE
            textArea.caretPosition = textArea.document.length
        } else {
            caret.updatePolicy = DefaultCaret.NEVER_UPDATE
        }
    }

    private fun exportVisibleContent() {
        val content = textArea.text
        val targetFile = chooseExportFile() ?: return

        runCatching {
            targetFile.parentFile?.mkdirs()
            Files.writeString(targetFile.toPath(), content, StandardCharsets.UTF_8)
        }.onFailure { ex ->
            JOptionPane.showMessageDialog(
                this,
                "Logdatei konnte nicht geschrieben werden.\n${ex.message ?: targetFile.absolutePath}",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE
            )
        }.onSuccess {
            JOptionPane.showMessageDialog(
                this,
                "Logdatei erfolgreich gespeichert.\n${targetFile.absolutePath}",
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE
            )
        }
    }

    private fun chooseExportFile(): File? {
        val owner = SwingUtilities.getWindowAncestor(this)
        val defaultFileName = "mediathekview-log.txt"

        return when (owner) {
            is Frame -> FileDialogs.chooseSaveFileLocation(owner, "Log exportieren", defaultFileName)
            is JDialog -> FileDialogs.chooseSaveFileLocation(owner, "Log exportieren", defaultFileName)
            else -> JFileChooser().apply {
                dialogTitle = "Log exportieren"
                selectedFile = File(defaultFileName)
                fileSelectionMode = JFileChooser.FILES_ONLY
                isFileHidingEnabled = true
            }.takeIf { it.showSaveDialog(this) == JFileChooser.APPROVE_OPTION }?.selectedFile
        }
    }

    /** Reload view from repository snapshot according to current filters. */
    private fun reloadFromSnapshot() {
        val snapshot = repo.snapshot()

        uiScope.launch {
            val sb = StringBuilder(snapshot.size * 64)
            for (e in snapshot) {
                if (shouldDisplay(e)) sb.append(formatLine(e))
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
                        if (shouldDisplay(entry)) {
                            buffer.add(entry)
                            if (buffer.size >= batchSize)
                                flush(buffer)
                        }
                    }

                    onTimeout(flushIntervalMs.milliseconds) {
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
            append(LogPathRedactor.redact(e.message))
            if (e.thrown != null)
                append('\n').append(LogPathRedactor.redact(e.thrown))
            append(' ').append(e.logger)
            append(' ').append(e.thread)
            append(' ').append(e.level.name())
        }
        return hay.contains(f, ignoreCase = true)
    }

    private fun shouldDisplay(e: LogEntry): Boolean =
        e.logger !in hiddenLoggers && matchesFilters(e)

    private fun formatLine(e: LogEntry): String {
        val ts = tsFmt.format(e.instant.atZone(zone))
        val redactedMessage = LogPathRedactor.redact(e.message)
        val redactedThrown = e.thrown?.let(LogPathRedactor::redact)
        val base = "$ts ${e.level.name().padEnd(5)} ${e.logger} [${e.thread}] - $redactedMessage"
        return if (redactedThrown.isNullOrBlank()) {
            base + "\n"
        } else {
            base + "\n" + redactedThrown + "\n"
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
