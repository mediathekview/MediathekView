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
import mediathek.config.application.ApplicationConfiguration
import mediathek.mainwindow.MediathekGui
import mediathek.swing.AppTerminationIndefiniteProgress
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.GetFile
import mediathek.tool.SVGIconUtilities
import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.*
import kotlin.time.Duration.Companion.milliseconds

class DialogBeenden(parent: JFrame) : JDialog(parent, true) {
    /**
     * Indicate whether the application can terminate.
     */
    var applicationCanTerminate = false
        private set

    /**
     * Indicate whether computer should be shut down.
     */
    var isShutdownRequested = false
        private set

    private val job = SupervisorJob()
    private val coroutineScope = CoroutineScope(job + Dispatchers.Swing)
    private var downloadMonitorJob: Job? = null
    private val comboActions: JComboBox<String> = JComboBox(comboBoxModel)
    private val btnContinue: JButton = JButton("Weiter")
    private val cbShutdownComputer: JCheckBox = JCheckBox("Rechner herunterfahren")
    private val btnCancel: JButton = JButton("Abbrechen")
    private val jButtonHilfe: JButton = JButton(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"))

    /**
     * Create the ComboBoxModel for user selection.
     *
     * @return The model with all valid user actions.
     */
    private val comboBoxModel: DefaultComboBoxModel<String>
        get() = DefaultComboBoxModel(
            arrayOf(
                CANCEL_AND_TERMINATE_PROGRAM, WAIT_FOR_DOWNLOADS_AND_TERMINATE,
                WAIT_FOR_RUNNING_DOWNLOADS_AND_TERMINATE
            )
        )

    /**
     * This will reset all necessary variables to default and cancel app termination.
     */
    private fun escapeHandler() {
        cancelDownloadMonitoring()
        applicationCanTerminate = false
        dispose()
    }

    /**
     * Handler which will wait untill all downloads have finished.
     *
     * @param waitForRunningDownloadsOnly if true stop all waiting DL and wait only for those running.
     */
    private fun waitUntilDownloadsHaveFinished(waitForRunningDownloadsOnly: Boolean = false) {
        cancelDownloadMonitoring()
        glassPane = AppTerminationIndefiniteProgress(isShutdownRequested)
        glassPane?.isVisible = true

        if (waitForRunningDownloadsOnly)
            MediathekGui.ui().stopAllWaitingDownloads()

        downloadMonitorJob = coroutineScope.launch {
            try {
                withContext(Dispatchers.IO) {
                    while (Daten.getInstance().listeDownloads.unfinishedDownloads() > 0) {
                        ensureActive()
                        delay(1_000.milliseconds)
                    }
                }

                applicationCanTerminate = true
                dispose()
            } catch (_: CancellationException) {
                // Dialog closure cancels the monitoring job as part of the normal lifecycle.
            } finally {
                glassPane?.isVisible = false
                downloadMonitorJob = null
            }
        }
    }

    private fun cancelDownloadMonitoring() {
        downloadMonitorJob?.cancel()
        downloadMonitorJob = null
        glassPane?.isVisible = false
    }

    private fun selectedAction(): String = comboActions.selectedItem as? String ?: CANCEL_AND_TERMINATE_PROGRAM

    private fun updateActionControls() {
        when (selectedAction()) {
            WAIT_FOR_DOWNLOADS_AND_TERMINATE, WAIT_FOR_RUNNING_DOWNLOADS_AND_TERMINATE -> {
                jButtonHilfe.isEnabled = true
                cbShutdownComputer.isEnabled = true
            }

            else -> {
                jButtonHilfe.isEnabled = false
                cbShutdownComputer.isEnabled = false
                cbShutdownComputer.isSelected = false
                isShutdownRequested = false
            }
        }
    }

    private fun restoreSelectedAction() {
        val savedAction = ApplicationConfiguration.getInstance().exitDialogAction ?: return

        for (index in 0 until comboActions.itemCount) {
            if (comboActions.getItemAt(index) == savedAction) {
                comboActions.selectedIndex = index
                return
            }
        }
    }

    override fun dispose() {
        cancelDownloadMonitoring()
        job.cancel()
        super.dispose()
    }

    private fun initComponents() {
        jButtonHilfe.toolTipText = "Hilfe anzeigen"

        val jLabel1 =
            JLabel("<html>Es sind noch nicht alle Downloads fertig.<br>Wie möchten Sie fortfahren?</html>")
        contentPane.layout = MigLayout(
            LC().insets("dialog").fillX(),
            AC().grow().fill().gap().gap().gap(),
            AC()
        )
        contentPane.add(jLabel1, CC().spanX().growX().wrap())
        contentPane.add(comboActions, CC().spanX().growX().wrap())
        contentPane.add(cbShutdownComputer, CC().spanX().wrap())
        contentPane.add(jButtonHilfe, CC().cell(1, 3))
        contentPane.add(btnCancel, CC().cell(2, 3))
        contentPane.add(btnContinue, CC().cell(3, 3))

        pack()
        setLocationRelativeTo(owner)
    }

    companion object {
        private const val CANCEL_AND_TERMINATE_PROGRAM = "Downloads abbrechen und Programm beenden"
        private const val WAIT_FOR_DOWNLOADS_AND_TERMINATE = "Auf Abschluß aller Downloads warten, danach beenden"
        private const val WAIT_FOR_RUNNING_DOWNLOADS_AND_TERMINATE =
            "Nur auf bereits laufende Downloads warten, danach beenden"
    }

    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        title = "MediathekView beenden"
        isResizable = false

        initComponents()
        EscapeKeyHandler.installHandler(this) { escapeHandler() }
        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(e: WindowEvent) {
                escapeHandler()
            }
        })

        jButtonHilfe.addActionListener {
            val msg = GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_BEENDEN).trim()
            JOptionPane.showMessageDialog(this, msg, Konstanten.PROGRAMMNAME, JOptionPane.PLAIN_MESSAGE)
        }

        jButtonHilfe.isEnabled = false
        cbShutdownComputer.isEnabled = false

        comboActions.addActionListener {
            ApplicationConfiguration.getInstance().exitDialogAction = selectedAction()
            updateActionControls()
        }

        cbShutdownComputer.addActionListener { isShutdownRequested = cbShutdownComputer.isSelected }

        btnContinue.addActionListener {
            when (selectedAction()) {
                WAIT_FOR_DOWNLOADS_AND_TERMINATE -> waitUntilDownloadsHaveFinished()
                WAIT_FOR_RUNNING_DOWNLOADS_AND_TERMINATE -> waitUntilDownloadsHaveFinished(true)
                CANCEL_AND_TERMINATE_PROGRAM -> {
                    applicationCanTerminate = true
                    dispose()
                }
            }
        }

        btnCancel.addActionListener { escapeHandler() }

        pack()
        getRootPane().defaultButton = btnContinue
        restoreSelectedAction()
        updateActionControls()
    }
}
