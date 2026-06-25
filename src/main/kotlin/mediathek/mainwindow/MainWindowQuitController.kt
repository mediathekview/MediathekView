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

package mediathek.mainwindow

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.gui.dialog.DialogBeenden
import mediathek.shutdown.ComputerShutdown
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.SwingUtilities
import kotlin.system.exitProcess

class MainWindowQuitController private constructor(
    private val quitConfirmer: (Boolean) -> QuitConfirmation,
    private val shutdownStarter: (shutdownComputer: Boolean, resetSettingsOnQuit: Boolean) -> Unit,
    private val edtRunner: ShutdownEdtRunner,
) {
    private val applicationQuitInProgress = AtomicBoolean()
    private val resetSettingsOnQuit = AtomicBoolean()

    constructor(
        owner: JFrame,
        daten: Daten,
        downloadControlHost: DownloadControlHost,
        dialogCoordinator: MainWindowDialogCoordinator,
        tabRegistry: MainWindowTabRegistry,
        computerShutdown: ComputerShutdown,
        activeAudiothekDownloads: () -> Int,
        pauseAudiothekDownloadsForShutdown: Runnable,
        closeAutomaticFilmlistUpdate: Runnable,
        closeProgramUpdateChecker: Runnable,
        closeSystemTray: Runnable,
        closeNotificationCenter: Runnable,
        shutdownTimerPool: Runnable,
        waitForCommonPoolToComplete: Runnable,
        edtRunner: ShutdownEdtRunner,
    ) : this(
        quitConfirmer = { requestShutdownComputer ->
            confirmApplicationQuit(
                owner,
                daten,
                downloadControlHost,
                activeAudiothekDownloads,
                pauseAudiothekDownloadsForShutdown,
                requestShutdownComputer,
            )
        },
        shutdownStarter = { shutdownComputer, resetSettingsOnQuit ->
            Thread.ofPlatform()
                .name("MediathekView-shutdown")
                .daemon(false)
                .start {
                    MainWindowShutdownCoordinator(
                        owner,
                        daten,
                        dialogCoordinator,
                        tabRegistry,
                        computerShutdown,
                        resetSettingsOnQuit,
                        closeAutomaticFilmlistUpdate,
                        closeProgramUpdateChecker,
                        closeSystemTray,
                        closeNotificationCenter,
                        shutdownTimerPool,
                        waitForCommonPoolToComplete,
                        edtRunner,
                    ).shutdown(shutdownComputer)
                    exitProcess(0)
                }
        },
        edtRunner,
    )

    internal constructor(
        edtRunner: ShutdownEdtRunner,
        quitConfirmer: (Boolean) -> QuitConfirmation,
        shutdownStarter: (shutdownComputer: Boolean, resetSettingsOnQuit: Boolean) -> Unit,
    ) : this(quitConfirmer, shutdownStarter, edtRunner)

    fun requestSettingsResetOnQuit() {
        resetSettingsOnQuit.set(true)
    }

    fun quitApplication(shutdownComputer: Boolean = false): Boolean {
        if (!applicationQuitInProgress.compareAndSet(false, true)) {
            return true
        }

        val confirmation = confirmApplicationQuitOnEdt(shutdownComputer)
        if (!confirmation.canQuit) {
            applicationQuitInProgress.set(false)
            return false
        }

        shutdownStarter(confirmation.shutdownComputer, resetSettingsOnQuit.get())
        return true
    }

    private fun confirmApplicationQuitOnEdt(shutdownComputer: Boolean): QuitConfirmation {
        if (SwingUtilities.isEventDispatchThread()) {
            return quitConfirmer(shutdownComputer)
        }

        val confirmation = AtomicReference<QuitConfirmation>()
        edtRunner.run(
            "Confirm application quit",
            Runnable { confirmation.set(quitConfirmer(shutdownComputer)) },
        )
        return confirmation.get() ?: QuitConfirmation.declined()
    }

    internal data class QuitConfirmation(
        val canQuit: Boolean,
        val shutdownComputer: Boolean,
    ) {
        companion object {
            fun declined(): QuitConfirmation = QuitConfirmation(canQuit = false, shutdownComputer = false)
        }
    }

    private companion object {
        private fun confirmApplicationQuit(
            owner: JFrame,
            daten: Daten,
            downloadControlHost: DownloadControlHost,
            activeAudiothekDownloads: () -> Int,
            pauseAudiothekDownloadsForShutdown: Runnable,
            requestShutdownComputer: Boolean,
        ): QuitConfirmation {
            var shutdownComputer = requestShutdownComputer
            if (daten.listeDownloads.unfinishedDownloads() > 0) {
                val dialogBeenden = DialogBeenden(owner, downloadControlHost)
                dialogBeenden.isVisible = true
                if (!dialogBeenden.applicationCanTerminate) {
                    return QuitConfirmation.declined()
                }
                shutdownComputer = dialogBeenden.isShutdownRequested
            }

            val activeAudiothekDownloadCount = activeAudiothekDownloads()
            if (activeAudiothekDownloadCount > 0) {
                val result = JOptionPane.showConfirmDialog(
                    owner,
                    if (activeAudiothekDownloadCount == 1) {
                        "Es ist noch ein Audiothek-Download aktiv.\nTrotzdem beenden?"
                    } else {
                        "Es sind noch $activeAudiothekDownloadCount Audiothek-Downloads aktiv.\nTrotzdem beenden?"
                    },
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.YES_NO_OPTION,
                    JOptionPane.WARNING_MESSAGE,
                )
                if (result != JOptionPane.YES_OPTION) {
                    return QuitConfirmation.declined()
                }
                pauseAudiothekDownloadsForShutdown.run()
            }

            return QuitConfirmation(true, shutdownComputer)
        }
    }
}
