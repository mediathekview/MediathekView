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

package mediathek.gui.dialog

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.controller.starter.DownloadStartActions
import mediathek.daten.DatenDownload
import mediathek.swing.AppTerminationIndefiniteProgress
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.GetFile
import java.awt.BorderLayout
import java.awt.Window
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.swing.DefaultComboBoxModel
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.JPanel
import kotlin.coroutines.cancellation.CancellationException
import kotlin.time.Duration.Companion.seconds

class DialogBeendenZeit(
    parent: JFrame?,
    private val listeDownloadsStarten: ArrayList<DatenDownload>,
) : DialogBeendenZeitView(parent) {
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var downloadMonitorJob: Job? = null

    private var applicationCanTerminate = false
    private var shutdown = false
    private var progressPanel: AppTerminationIndefiniteProgress? = null

    fun applicationCanTerminate(): Boolean = applicationCanTerminate

    fun isShutdownRequested(): Boolean = shutdown

    private fun initializeDateTimePicker() {
        dateTimePicker.datePicker.setDateToToday()
        val timePicker = dateTimePicker.timePicker
        timePicker.setTimeToNow()
        timePicker.time = timePicker.time.plusHours(1).plusMinutes(1)
    }

    private fun selectedAction(): String = comboActions.selectedItem as? String ?: WAIT_FOR_DOWNLOADS_AND_TERMINATE

    private fun setCbShutdownCoputer() {
        if (selectedAction() == WAIT_FOR_DOWNLOADS_AND_TERMINATE) {
            cbShutdownComputer.isEnabled = true
        } else {
            cbShutdownComputer.isEnabled = false
            cbShutdownComputer.isSelected = false
            shutdown = false
        }
    }

    private val comboBoxModel: DefaultComboBoxModel<String>
        get() = DefaultComboBoxModel(
            arrayOf(
                WAIT_FOR_DOWNLOADS_AND_TERMINATE,
                WAIT_FOR_DOWNLOADS_AND_DONT_TERMINATE_PROGRAM,
                DONT_START,
            )
        )

    private fun escapeHandler() {
        cancelDownloadMonitoring()
        applicationCanTerminate = false
        dispose()
    }

    private fun createGlassPane(): JPanel {
        val panel = JPanel(BorderLayout(5, 5))
        val progress = AppTerminationIndefiniteProgress(isShutdownRequested())
        progressPanel = progress
        panel.add(progress, BorderLayout.CENTER)
        return panel
    }

    private fun setTextWait() {
        val dateTime = dateTimePicker.getDateTimePermissive()
        val time = dateTime.format(DateTimeFormatter.ofPattern("HH:mm"))
        val date = dateTime.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))
        progressPanel?.setMessage("Downloads werden am $date um $time gestartet.")
    }

    private fun waitUntilDownloadsHaveFinished() {
        cancelDownloadMonitoring()
        val waitingPane = createGlassPane()
        glassPane = waitingPane
        setTextWait()
        waitingPane.isVisible = true

        downloadMonitorJob = scope.launch {
            try {
                withContext(Dispatchers.IO) {
                    while (LocalDateTime.now().isBefore(dateTimePicker.getDateTimePermissive())) {
                        ensureActive()
                        delay(1.seconds)
                    }
                }

                progressPanel?.setMessage("Warte auf Abschluss der Downloads...")
                DownloadStartActions.startAll(listeDownloadsStarten)

                withContext(Dispatchers.IO) {
                    while (Daten.getInstance().listeDownloads.unfinishedDownloads() > 0) {
                        ensureActive()
                        delay(1.seconds)
                    }
                }
            } catch (_: CancellationException) {
                return@launch
            } finally {
                waitingPane.isVisible = false
                downloadMonitorJob = null
            }

            dispose()
        }
    }

    private fun cancelDownloadMonitoring() {
        downloadMonitorJob?.cancel()
        downloadMonitorJob = null
        (glassPane as? JPanel)?.isVisible = false
    }

    override fun dispose() {
        cancelDownloadMonitoring()
        scope.cancel()
        super.dispose()
    }

    companion object {
        private const val WAIT_FOR_DOWNLOADS_AND_TERMINATE =
            "Auf Abschluß aller Downloads warten und danach Programm beenden"
        private const val WAIT_FOR_DOWNLOADS_AND_DONT_TERMINATE_PROGRAM =
            "Auf Abschluß aller Downloads warten, Programm danach NICHT beenden"
        private const val DONT_START = "Downloads nicht starten"
    }

    init {
        initializeDateTimePicker()
        setLocationRelativeTo(parent)

        EscapeKeyHandler.installHandler(this) { escapeHandler() }
        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(event: WindowEvent) {
                escapeHandler()
            }
        })

        comboActions.model = comboBoxModel
        comboActions.addActionListener { setCbShutdownCoputer() }

        jButtonHilfe.addActionListener {
            val msg = GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_BEENDEN).trim()
            JOptionPane.showMessageDialog(
                parent as? Window ?: this,
                msg,
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE,
            )
        }
        setCbShutdownCoputer()

        cbShutdownComputer.addActionListener { shutdown = cbShutdownComputer.isSelected }

        btnContinue.addActionListener {
            when (selectedAction()) {
                WAIT_FOR_DOWNLOADS_AND_TERMINATE -> {
                    applicationCanTerminate = true
                    waitUntilDownloadsHaveFinished()
                }

                WAIT_FOR_DOWNLOADS_AND_DONT_TERMINATE_PROGRAM -> {
                    applicationCanTerminate = false
                    waitUntilDownloadsHaveFinished()
                }

                DONT_START -> {
                    applicationCanTerminate = false
                    dispose()
                }
            }
        }

        btnCancel.addActionListener { escapeHandler() }

        rootPane.defaultButton = btnContinue
        pack()
    }
}
