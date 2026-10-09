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

package mediathek.gui.actions

import mediathek.config.application.ApplicationConfigurationCleanupStatistics
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Frame
import javax.swing.*
import javax.swing.border.EmptyBorder

class ConfigurationCleanupStatisticsDialog(
    owner: Frame,
    statistics: ApplicationConfigurationCleanupStatistics,
) : JDialog(owner, if (statistics.dryRun) "Konfiguration prüfen" else "Konfiguration bereinigt", true) {
    init {
        val textArea = JTextArea(statisticsText(statistics)).apply {
            isEditable = false
            lineWrap = false
            caretPosition = 0
        }

        val closeButton = JButton("Schließen").apply {
            addActionListener { dispose() }
        }

        val buttonPanel = JPanel().apply {
            add(closeButton)
        }

        contentPane = JPanel(BorderLayout(8, 8)).apply {
            border = EmptyBorder(10, 10, 10, 10)
            add(JScrollPane(textArea), BorderLayout.CENTER)
            add(buttonPanel, BorderLayout.SOUTH)
        }
        minimumSize = Dimension(560, 360)
        preferredSize = Dimension(720, 480)
        pack()
        setLocationRelativeTo(owner)
    }

    private fun statisticsText(statistics: ApplicationConfigurationCleanupStatistics): String = buildString {
        if (statistics.dryRun) {
            appendLine("Trockenlauf abgeschlossen. Die Anwendungskonfiguration wurde nicht verändert.")
        } else {
            appendLine("Die Anwendungskonfiguration wurde bereinigt.")
        }
        appendLine()
        appendLine("Datei:")
        appendLine(statistics.settingsPath)
        appendLine()
        appendLine("Sicherung:")
        appendLine(
            when {
                statistics.dryRun -> "Keine Sicherung erstellt, da im Trockenlauf nichts verändert wurde."
                statistics.backupPath != null -> statistics.backupPath
                else -> "Keine Sicherung erstellt, da die Konfigurationsdatei noch nicht existierte."
            },
        )
        appendLine()
        appendLine("Schlüssel vorher:  ${statistics.totalKeysBefore}")
        appendLine("Schlüssel nachher: ${statistics.totalKeysAfter}")
        appendLine("Behalten:          ${statistics.keptKeyCount}")
        appendLine(if (statistics.dryRun) "Würden entfernt:   ${statistics.removedKeyCount}" else "Entfernt:          ${statistics.removedKeyCount}")
        appendLine()
        if (statistics.removedKeys.isEmpty()) {
            appendLine("Es wurden keine veralteten oder unbekannten Schlüssel gefunden.")
        } else {
            appendLine(if (statistics.dryRun) "Schlüssel, die entfernt würden:" else "Entfernte Schlüssel:")
            statistics.removedKeys.forEach { key -> appendLine("- $key") }
        }
    }
}
