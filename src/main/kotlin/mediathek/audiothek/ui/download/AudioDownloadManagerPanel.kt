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

package mediathek.audiothek.ui.download

import mediathek.audiothek.download.AudioDownloadTaskSnapshot
import mediathek.audiothek.download.AudioDownloadTaskState
import mediathek.swing.IconUtils
import org.apache.commons.lang3.SystemUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignC
import org.kordamp.ikonli.materialdesign2.MaterialDesignP
import org.kordamp.ikonli.materialdesign2.MaterialDesignR
import org.kordamp.ikonli.materialdesign2.MaterialDesignT
import java.awt.*
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.*
import kotlin.math.roundToInt

private const val DOWNLOAD_ROW_PREFERRED_WIDTH = 600
private const val DOWNLOAD_PANEL_INSET = 5
private const val DOWNLOAD_PANEL_PREFERRED_WIDTH = DOWNLOAD_ROW_PREFERRED_WIDTH + (DOWNLOAD_PANEL_INSET * 2)

class AudioDownloadManagerPanel : JPanel(BorderLayout()) {
    private val contentPanel = JPanel().apply {
        layout = BoxLayout(this, BoxLayout.Y_AXIS)
        isOpaque = false
        border = BorderFactory.createEmptyBorder(
            DOWNLOAD_PANEL_INSET,
            DOWNLOAD_PANEL_INSET,
            DOWNLOAD_PANEL_INSET,
            DOWNLOAD_PANEL_INSET
        )
    }
    private val scrollPane = JScrollPane(contentPanel)
    private var emptyListener: (() -> Unit)? = null
    private var progressListener: ((DownloadSummary) -> Unit)? = null
    private var primaryActionListener: ((String) -> Unit)? = null
    private var secondaryActionListener: ((String) -> Unit)? = null
    private var removeActionListener: ((String) -> Unit)? = null
    private var previousTaskIds: List<String> = emptyList()
    private var currentItems: List<AudioDownloadItem> = emptyList()
    private val rowPanels = LinkedHashMap<String, AudioDownloadRowPanel>()

    init {
        preferredSize = Dimension(DOWNLOAD_PANEL_PREFERRED_WIDTH, 320)
        minimumSize = Dimension(DOWNLOAD_PANEL_PREFERRED_WIDTH, 160)
        val popoverBackground = UIManager.getColor("Panel.background") ?: background
        background = popoverBackground
        isOpaque = true
        contentPanel.background = popoverBackground

        scrollPane.border = BorderFactory.createEmptyBorder()
        scrollPane.viewport.background = popoverBackground
        scrollPane.horizontalScrollBarPolicy = JScrollPane.HORIZONTAL_SCROLLBAR_NEVER

        add(scrollPane, BorderLayout.CENTER)
    }

    fun setTasks(tasks: List<AudioDownloadTaskSnapshot>) {
        val taskIds = tasks.map { it.id }
        val shouldScrollToEnd = taskIds.size > previousTaskIds.size || taskIds.lastOrNull() != previousTaskIds.lastOrNull()
        previousTaskIds = taskIds
        currentItems = tasks.map { it.toListItem() }

        val removedIds = rowPanels.keys - taskIds.toSet()
        removedIds.forEach(rowPanels::remove)

        currentItems.forEach { item ->
            val rowPanel = rowPanels[item.id]
            if (rowPanel == null) {
                rowPanels[item.id] = AudioDownloadRowPanel(
                    item,
                    primaryActionListener,
                    secondaryActionListener,
                    removeActionListener
                )
            } else {
                rowPanel.update(item)
            }
        }

        contentPanel.removeAll()
        currentItems.forEachIndexed { index, item ->
            contentPanel.add(rowPanels.getValue(item.id))
            if (index < currentItems.lastIndex) {
                contentPanel.add(Box.createVerticalStrut(8))
            }
        }
        contentPanel.add(Box.createVerticalGlue())
        contentPanel.revalidate()
        contentPanel.repaint()

        notifyProgressChanged()

        if (shouldScrollToEnd) {
            SwingUtilities.invokeLater {
                scrollPane.verticalScrollBar.value = scrollPane.verticalScrollBar.maximum
            }
        }
        if (currentItems.isEmpty()) {
            emptyListener?.invoke()
        }
    }

    fun addEmptyListener(listener: () -> Unit) {
        emptyListener = listener
    }

    fun addProgressListener(listener: (DownloadSummary) -> Unit) {
        progressListener = listener
        notifyProgressChanged()
    }

    fun addPrimaryActionListener(listener: (String) -> Unit) {
        primaryActionListener = listener
    }

    fun addSecondaryActionListener(listener: (String) -> Unit) {
        secondaryActionListener = listener
    }

    fun addRemoveActionListener(listener: (String) -> Unit) {
        removeActionListener = listener
    }

    private fun notifyProgressChanged() {
        progressListener?.invoke(
            DownloadSummary(
                activeCount = currentItems.count { it.state == AudioDownloadTaskState.DOWNLOADING },
                progress = calculateAggregateProgress(currentItems)
            )
        )
    }

    private fun calculateAggregateProgress(items: List<AudioDownloadItem>): Double {
        val activeItems = items.filter {
            it.state == AudioDownloadTaskState.DOWNLOADING &&
                !it.progressIndeterminate &&
                (it.totalBytes ?: 0L) > 0L
        }
        if (activeItems.isEmpty()) {
            return 0.0
        }

        val totalBytes = activeItems.sumOf { it.totalBytes ?: 0L }
        if (totalBytes <= 0L) {
            return 0.0
        }

        val downloadedBytes = activeItems.sumOf { it.downloadedBytes.coerceAtMost(it.totalBytes ?: 0L) }
        return downloadedBytes.toDouble() / totalBytes.toDouble()
    }
}

data class DownloadSummary(
    val activeCount: Int,
    val progress: Double
)

private data class AudioDownloadItem(
    val id: String,
    val state: AudioDownloadTaskState,
    val audioName: String,
    val status: String,
    val progressText: String,
    val progressPercent: Int,
    val progressIndeterminate: Boolean,
    val primaryLabel: String,
    val primaryEnabled: Boolean,
    val secondaryLabel: String,
    val secondaryEnabled: Boolean,
    val removeEnabled: Boolean,
    val downloadedBytes: Long = 0L,
    val totalBytes: Long? = null
)

private class AudioDownloadRowPanel(
    item: AudioDownloadItem,
    primaryActionListener: ((String) -> Unit)?,
    secondaryActionListener: ((String) -> Unit)?,
    removeActionListener: ((String) -> Unit)?
) : JPanel(GridBagLayout()) {
    private var currentItem: AudioDownloadItem = item
    private val showProgressText = !SystemUtils.IS_OS_MAC_OSX
    private val nameValueLabel = JLabel()
    private val statusValueLabel = JLabel()
    private val progressBar = JProgressBar(0, 100)
    private val primaryButton = JButton()
    private val secondaryButton = JButton()
    private val pauseIcon = IconUtils.of(MaterialDesignP.PAUSE)
    private val resumeIcon = IconUtils.of(MaterialDesignT.TRAY_ARROW_DOWN)
    private val restartIcon = IconUtils.of(MaterialDesignR.RESTART)
    private val cancelIcon = IconUtils.of(MaterialDesignC.CANCEL)
    private val removeButton = JLabel().apply {
        icon = IconUtils.of(MaterialDesignC.CLOSE_CIRCLE_OUTLINE, 18)
        toolTipText = "Entfernen"
        cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
        addMouseListener(object : MouseAdapter() {
            override fun mouseClicked(e: MouseEvent) {
                if (isEnabled) {
                    removeActionListener?.invoke(currentItem.id)
                }
            }
        })
    }

    init {
        val borderColor = UIManager.getColor("Component.borderColor") ?: Color.LIGHT_GRAY
        border = BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(borderColor, 1, true),
            BorderFactory.createEmptyBorder(10, 12, 10, 12)
        )
        background = UIManager.getColor("Panel.background") ?: background
        preferredSize = Dimension(DOWNLOAD_ROW_PREFERRED_WIDTH, 96)
        maximumSize = Dimension(Int.MAX_VALUE, 96)
        alignmentX = LEFT_ALIGNMENT

        progressBar.isStringPainted = showProgressText
        primaryButton.addActionListener { primaryActionListener?.invoke(currentItem.id) }
        secondaryButton.addActionListener { secondaryActionListener?.invoke(currentItem.id) }
        val gbc = GridBagConstraints().apply {
            insets = Insets(2, 2, 2, 2)
            fill = GridBagConstraints.HORIZONTAL
            anchor = GridBagConstraints.WEST
        }

        gbc.gridx = 0
        gbc.gridy = 0
        gbc.weightx = 0.0
        add(JLabel("Name:"), gbc)

        gbc.gridx = 1
        gbc.weightx = 1.0
        add(nameValueLabel, gbc)

        gbc.gridx = 0
        gbc.gridy = 1
        gbc.weightx = 0.0
        add(JLabel("Status:"), gbc)

        gbc.gridx = 1
        gbc.weightx = 1.0
        add(statusValueLabel, gbc)

        gbc.gridx = 0
        gbc.gridy = 2
        gbc.gridwidth = 2
        gbc.weightx = 1.0
        add(progressBar, gbc)

        val buttonPanel = JPanel().apply {
            layout = BoxLayout(this, BoxLayout.Y_AXIS)
            isOpaque = false
            add(primaryButton)
            add(Box.createVerticalStrut(8))
            add(secondaryButton)
        }

        gbc.gridx = 2
        gbc.gridy = 0
        gbc.gridheight = 3
        gbc.gridwidth = 1
        gbc.weightx = 0.0
        gbc.fill = GridBagConstraints.NONE
        gbc.anchor = GridBagConstraints.NORTHEAST
        gbc.insets = Insets(2, 12, 2, 2)
        add(buttonPanel, gbc)

        gbc.gridx = 3
        gbc.gridy = 0
        gbc.gridheight = 3
        gbc.anchor = GridBagConstraints.CENTER
        gbc.fill = GridBagConstraints.VERTICAL
        gbc.insets = Insets(2, 8, 2, 2)
        add(removeButton, gbc)

        update(item)
    }

    fun update(item: AudioDownloadItem) {
        currentItem = item
        background = if (item.state == AudioDownloadTaskState.FAILED) {
            Color(255, 0, 0, 76)
        } else {
            UIManager.getColor("Panel.background") ?: background
        }
        nameValueLabel.text = ellipsize(item.audioName, 90)
        nameValueLabel.toolTipText = item.audioName
        statusValueLabel.text = ellipsize(item.status, 80)
        statusValueLabel.toolTipText = item.status
        progressBar.isVisible = item.state != AudioDownloadTaskState.COMPLETED &&
            item.state != AudioDownloadTaskState.FAILED
        progressBar.isIndeterminate = item.progressIndeterminate
        progressBar.value = item.progressPercent
        progressBar.string = if (showProgressText) item.progressText else null

        val primaryIcon = when (item.primaryLabel) {
            "Pause" -> pauseIcon
            "Fortsetzen" -> resumeIcon
            "Neu starten" -> restartIcon
            else -> null
        }
        primaryButton.text = if (primaryIcon == null) item.primaryLabel else ""
        primaryButton.isEnabled = item.primaryEnabled
        primaryButton.isVisible = item.primaryLabel.isNotBlank()
        primaryButton.icon = primaryIcon
        primaryButton.toolTipText = item.primaryLabel.takeIf { primaryIcon != null }

        val secondaryIcon = when (item.secondaryLabel) {
            "Abbrechen" -> cancelIcon
            else -> null
        }
        secondaryButton.text = if (secondaryIcon == null) item.secondaryLabel else ""
        secondaryButton.isEnabled = item.secondaryEnabled
        secondaryButton.isVisible = item.secondaryLabel.isNotBlank()
        secondaryButton.icon = secondaryIcon
        secondaryButton.toolTipText = item.secondaryLabel.takeIf { secondaryIcon != null }

        removeButton.isEnabled = item.removeEnabled
        removeButton.isVisible = item.removeEnabled

        revalidate()
        repaint()
    }

    private fun ellipsize(text: String, maxChars: Int): String {
        if (text.length <= maxChars) {
            return text
        }
        return text.take(maxChars - 3) + "..."
    }
}

private fun AudioDownloadTaskSnapshot.toListItem(): AudioDownloadItem {
    val restartAllowed = isRestartAllowed(state, errorMessage)
    val primaryAction = resolvePrimaryAction(state, restartAllowed)
    val progressPercent = totalBytes
        ?.takeIf { it > 0L }
        ?.let { ((downloadedBytes.coerceAtMost(it).toDouble() / it.toDouble()) * 100.0).roundToInt().coerceIn(0, 100) }
        ?: if (state == AudioDownloadTaskState.COMPLETED) 100 else 0
    val progressText = when (state) {
        AudioDownloadTaskState.COMPLETED -> "100 %"
        AudioDownloadTaskState.DOWNLOADING,
        AudioDownloadTaskState.PAUSED -> totalBytes
            ?.takeIf { it > 0L }
            ?.let { "$progressPercent %" }
            ?: formatByteSize(downloadedBytes)
        AudioDownloadTaskState.CANCELLED -> "Abgebrochen"
        AudioDownloadTaskState.FAILED -> "Fehler"
    }

    val statusText = when (state) {
        AudioDownloadTaskState.DOWNLOADING -> buildTransferStatus("Läuft", downloadedBytes, totalBytes)
        AudioDownloadTaskState.PAUSED -> buildTransferStatus("Pausiert", downloadedBytes, totalBytes)
        AudioDownloadTaskState.COMPLETED -> "Abgeschlossen"
        AudioDownloadTaskState.CANCELLED -> "Abgebrochen"
        AudioDownloadTaskState.FAILED -> sanitizeFailureMessage(errorMessage)
    }

    val secondary = when (state) {
        AudioDownloadTaskState.DOWNLOADING,
        AudioDownloadTaskState.PAUSED -> "Abbrechen" to true
        AudioDownloadTaskState.FAILED,
        AudioDownloadTaskState.CANCELLED,
        AudioDownloadTaskState.COMPLETED -> "" to false
    }
    val removeEnabled = when (state) {
        AudioDownloadTaskState.FAILED,
        AudioDownloadTaskState.CANCELLED,
        AudioDownloadTaskState.COMPLETED -> true
        AudioDownloadTaskState.DOWNLOADING,
        AudioDownloadTaskState.PAUSED -> false
    }

    return AudioDownloadItem(
        id = id,
        state = state,
        audioName = audioName,
        status = statusText,
        progressText = progressText,
        progressPercent = progressPercent,
        progressIndeterminate = state == AudioDownloadTaskState.DOWNLOADING && (totalBytes ?: 0L) <= 0L,
        primaryLabel = primaryAction.first,
        primaryEnabled = primaryAction.second,
        secondaryLabel = secondary.first,
        secondaryEnabled = secondary.second,
        removeEnabled = removeEnabled,
        downloadedBytes = downloadedBytes,
        totalBytes = totalBytes
    )
}

private fun isRestartAllowed(state: AudioDownloadTaskState, errorMessage: String?): Boolean {
    val message = errorMessage?.lowercase().orEmpty()
    return when (state) {
        AudioDownloadTaskState.FAILED if message.contains("http 404") -> false
        else -> true
    }
}

private fun resolvePrimaryAction(
    state: AudioDownloadTaskState,
    restartAllowed: Boolean
): Pair<String, Boolean> = when (state) {
    AudioDownloadTaskState.DOWNLOADING -> "Pause" to true
    AudioDownloadTaskState.PAUSED -> "Fortsetzen" to true
    AudioDownloadTaskState.FAILED,
    AudioDownloadTaskState.CANCELLED -> if (restartAllowed) "Neu starten" to true else "" to false
    AudioDownloadTaskState.COMPLETED -> "" to false
}

private fun sanitizeFailureMessage(errorMessage: String?): String {
    val message = errorMessage?.trim().orEmpty()
    if (message.isBlank()) {
        return "Fehlgeschlagen"
    }
    if (looksLikePath(message)) {
        return "Fehlgeschlagen: Datei konnte nicht gespeichert werden"
    }
    return "Fehlgeschlagen: $message"
}

private fun looksLikePath(message: String): Boolean {
    return message.startsWith("/") ||
        message.startsWith("\\") ||
        Regex("^[A-Za-z]:[\\\\/]").containsMatchIn(message) ||
        (message.contains("/") && !message.startsWith("http")) ||
        message.contains("\\")
}

private fun buildTransferStatus(prefix: String, downloadedBytes: Long, totalBytes: Long?): String {
    val downloaded = formatByteSize(downloadedBytes)
    val total = totalBytes?.takeIf { it > 0L }?.let(::formatByteSize)
    return if (total == null) {
        "$prefix ($downloaded)"
    } else {
        "$prefix ($downloaded / $total)"
    }
}

private fun formatByteSize(bytes: Long): String {
    if (bytes < 1024L) {
        return "$bytes B"
    }
    val units = listOf("KB", "MB", "GB")
    var value = bytes.toDouble()
    var unitIndex = -1
    while (value >= 1024.0 && unitIndex < units.lastIndex) {
        value /= 1024.0
        unitIndex++
    }
    return String.format("%.1f %s", value, units[unitIndex])
}
