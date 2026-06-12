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

import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.config.application.ApplicationConfigurationCleanupStatistics
import org.apache.logging.log4j.LogManager
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.SwingWorker

class CleanupApplicationConfigurationAction(
    private val owner: JFrame,
) : AbstractAction("Konfiguration bereinigen...") {
    init {
        putValue(
            SHORT_DESCRIPTION,
            "Entfernt veraltete oder unbekannte Einträge aus der Anwendungskonfiguration.",
        )
    }

    override fun actionPerformed(event: ActionEvent?) {
        val dryRunOption = "Trockenlauf"
        val cleanupOption = "Bereinigen"
        val cancelOption = "Abbrechen"
        val answer = JOptionPane.showOptionDialog(
            owner,
            "Die Anwendungskonfiguration kann zunächst als Trockenlauf geprüft werden.\n\n" +
                "Trockenlauf: zeigt nur, welche Einträge entfernt würden.\n" +
                "Bereinigen: entfernt unbekannte oder nicht mehr unterstützte Einträge.\n" +
                "Vor dem Bereinigen wird automatisch eine Sicherung der settings.xml erstellt.",
            "Konfiguration bereinigen",
            JOptionPane.YES_NO_CANCEL_OPTION,
            JOptionPane.WARNING_MESSAGE,
            null,
            arrayOf(dryRunOption, cleanupOption, cancelOption),
            dryRunOption,
        )
        val dryRun = when (answer) {
            0 -> true
            1 -> false
            else -> return
        }

        isEnabled = false
        object : SwingWorker<ApplicationConfigurationCleanupStatistics, Unit>() {
            override fun doInBackground(): ApplicationConfigurationCleanupStatistics =
                ApplicationConfiguration.getInstance().cleanupConfiguration(dryRun)

            override fun done() {
                isEnabled = true
                try {
                    ConfigurationCleanupStatisticsDialog(owner, get()).isVisible = true
                } catch (ex: Exception) {
                    logger.error("Unable to cleanup application configuration", ex)
                    JOptionPane.showMessageDialog(
                        owner,
                        "Die Anwendungskonfiguration konnte nicht bereinigt werden:\n${ex.message}",
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.ERROR_MESSAGE,
                    )
                }
            }
        }.execute()
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
