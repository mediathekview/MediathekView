package mediathek.gui.dialog

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.mainwindow.MediathekGui
import mediathek.swing.AppTerminationIndefiniteProgress
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.GetFile
import mediathek.tool.SVGIconUtilities
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.util.*
import java.util.concurrent.TimeUnit
import javax.swing.*

class DialogBeenden(parent: JFrame, shouldDownloadAndQuit: Boolean) : JDialog(parent, true) {
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

    /**
     * The download monitoring [javax.swing.SwingWorker].
     */
    private val downloadMonitorWorker: WaitForDownloadsWorker = WaitForDownloadsWorker()
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
        downloadMonitorWorker.cancel(true)
        glassPane?.isVisible = false
        applicationCanTerminate = false
        dispose()
    }

    /**
     * Handler which will wait untill all downloads have finished.
     *
     * @param waitForRunningDownloadsOnly if true stop all waiting DL and wait only for those running.
     */
    private fun waitUntilDownloadsHaveFinished(waitForRunningDownloadsOnly: Boolean = false) {
        glassPane = AppTerminationIndefiniteProgress(isShutdownRequested)
        glassPane?.isVisible = true

        if (waitForRunningDownloadsOnly)
            MediathekGui.ui().tabDownloads.stopAllWaitingDownloads()

        downloadMonitorWorker.execute()
    }

    private inner class WaitForDownloadsWorker : SwingWorker<Void, Void>() {
        override fun doInBackground(): Void? {
            while (Daten.getInstance().listeDownloads.unfinishedDownloads() > 0 && !isCancelled)
                TimeUnit.SECONDS.sleep(1)
            return null
        }

        override fun done() {
            applicationCanTerminate = true
            glassPane?.isVisible = false
            dispose()
        }
    }

    private fun initComponents() {
        jButtonHilfe.toolTipText = "Hilfe anzeigen"

        val contentPaneLayout = GroupLayout(contentPane)
        contentPane.layout = contentPaneLayout

        val jLabel1 =
            JLabel("<html>Es sind noch nicht alle Downloads fertig.<br>Wie m\u00f6chten Sie fortfahren?</html>")

        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(
                    contentPaneLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(
                            contentPaneLayout.createParallelGroup()
                                .addComponent(jLabel1, GroupLayout.DEFAULT_SIZE, 521, Short.MAX_VALUE.toInt())
                                .addComponent(comboActions, GroupLayout.DEFAULT_SIZE, 0, Short.MAX_VALUE.toInt())
                                .addGroup(
                                    GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                                        .addGap(0, 0, Short.MAX_VALUE.toInt())
                                        .addComponent(jButtonHilfe)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(btnCancel)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(btnContinue)
                                )
                                .addGroup(
                                    contentPaneLayout.createSequentialGroup().addComponent(cbShutdownComputer)
                                        .addGap(0, 351, Short.MAX_VALUE.toInt())
                                )
                        )
                        .addContainerGap()
                )
        )

        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(
                    GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(
                            jLabel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE,
                            GroupLayout.PREFERRED_SIZE
                        )
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(
                            comboActions, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE,
                            GroupLayout.PREFERRED_SIZE
                        )
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(cbShutdownComputer)
                        .addPreferredGap(
                            LayoutStyle.ComponentPlacement.RELATED,
                            GroupLayout.DEFAULT_SIZE,
                            Short.MAX_VALUE.toInt()
                        )
                        .addGroup(
                            contentPaneLayout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                                .addGroup(
                                    contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                        .addComponent(btnContinue)
                                        .addComponent(btnCancel)
                                )
                                .addComponent(jButtonHilfe)
                        )
                        .addContainerGap()
                )
        )

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
            DialogHilfe(parent, true, GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_BEENDEN)).isVisible = true
        }

        jButtonHilfe.isEnabled = false
        cbShutdownComputer.isEnabled = false

        comboActions.addActionListener {
            when (Objects.requireNonNull(comboActions.selectedItem) as String) {
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

        cbShutdownComputer.addActionListener { isShutdownRequested = cbShutdownComputer.isSelected }

        btnContinue.addActionListener {
            when (Objects.requireNonNull(comboActions.selectedItem) as String) {
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

        if (shouldDownloadAndQuit) {
            applicationCanTerminate = true
            SwingUtilities.invokeLater { waitUntilDownloadsHaveFinished() }
        }
    }
}