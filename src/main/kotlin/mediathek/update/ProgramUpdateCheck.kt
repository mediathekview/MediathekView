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

package mediathek.update

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.*
import mediathek.gui.dialog.DialogNewSet
import mediathek.tool.GuiFunktionen
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.NetUtils
import org.apache.logging.log4j.LogManager
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.function.BiConsumer
import javax.swing.JFrame
import kotlin.time.Duration.Companion.hours
import kotlin.time.Duration.Companion.seconds

/**
 * Perform check for updates every 24 hours if program is running long enough.
 */
class ProgramUpdateCheck(
    private val host: ProgramUpdateHost,
    private val programSets: ProgramSetRepository,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
) : AutoCloseable {
    private val job = SupervisorJob()
    private val scope = CoroutineScope(job + Dispatchers.IO + CoroutineExceptionHandler { _, ex ->
        logger.error("Program update check failed", ex)
    })
    private val programUpdateSearch = ProgrammUpdateSuchen(host::ownerFrame, scope)
    private var updateCheckJob: Job? = null

    fun start() {
        logger.debug("ProgramUpdateCheck Started.")
        updateCheckJob?.cancel()
        updateCheckJob = scope.launch {
            delay(INITIAL_UPDATE_CHECK_DELAY)
            while (isActive) {
                performUpdateCheck()
                delay(UPDATE_CHECK_INTERVAL)
            }
        }
    }

    private suspend fun performUpdateCheck() {
        logger.debug("performUpdateCheck started.")
        var updateMenuItemDisabled = false
        try {
            if (NetUtils.isReachable(UPDATE_CHECK_HOST, 1.seconds)) {
                withContext(Dispatchers.Swing) {
                    host.enableUpdateMenuItem(false)
                }
                updateMenuItemDisabled = true

                if (GuiFunktionen.isNotUsingExternalUpdater()) {
                    searchForProgramUpdate()
                } else {
                    logger.info("External Update Mechanism in use -> skip program update check")
                }

                checkForPsetUpdates()
            } else {
                logger.warn("Update Check: Network is not reachable.")
            }
        } finally {
            if (updateMenuItemDisabled) {
                withContext(NonCancellable + Dispatchers.Swing) {
                    host.enableUpdateMenuItem(true)
                }
            }
            logger.debug("performUpdateCheck finished.")
        }
    }

    private suspend fun searchForProgramUpdate() {
        programUpdateSearch.checkVersionSuspending(
            showAlert = false,
            showProgramInformation = true,
            showAllInformation = false,
            silent = true,
            showErrors = false,
        )
    }

    private suspend fun checkForPsetUpdates() {
        val standardPset = withContext(Dispatchers.IO) {
            ListePsetVorlagen.getStandarset(null, false)
        } ?: return

        withContext(Dispatchers.Swing) {
            checkForPsetUpdatesOnSwingThread(standardPset)
        }
    }

    private fun checkForPsetUpdatesOnSwingThread(standardPset: ListePset) {
        val parent = host.ownerFrame()
        if (!shouldInstallStandardPset(parent, standardPset)) {
            return
        }

        installStandardPset(parent, standardPset)
    }

    private fun shouldInstallStandardPset(parent: JFrame, standardPset: ListePset): Boolean {
        if (currentPsets().isEmpty()) {
            return true
        }

        if (standardPset.version.isEmpty()) {
            return false
        }

        val installedVersion = ApplicationConfiguration.getInstance().standardProgramSetVersion
        if (installedVersion == standardPset.version) {
            return false
        }

        return confirmStandardPsetUpdate(parent, standardPset)
    }

    private fun confirmStandardPsetUpdate(parent: JFrame, standardPset: ListePset): Boolean {
        val dialogNewSet = DialogNewSet(parent, programSets)
        dialogNewSet.isVisible = true
        val decision = dialogNewSet.decision
        if (decision.accepted) {
            return true
        }

        logger.info("Setanlegen: Abbruch")
        if (!decision.askAgainTomorrow) {
            logger.info("Setanlegen: Nicht wieder nachfragen")
            updateInstalledStandardPsetVersion(standardPset)
        }
        return false
    }

    private fun installStandardPset(parent: JFrame, standardPset: ListePset) {
        ProgramSetTemplateResolver.replaceTemplates(parent, standardPset)

        updateInstalledStandardPsetVersion(standardPset)
        copySaveSettingsFromExistingSet(standardPset)
        prepareImportedSetsForExistingConfiguration(standardPset)

        GuiFunktionenProgramme.addSetVorlagen(parent, programSets, standardPset, true, programSetExporter)
        logger.info("Setanlegen: OK")
        logger.info("==========================================")
    }

    private fun updateInstalledStandardPsetVersion(standardPset: ListePset) {
        ApplicationConfiguration.getInstance().standardProgramSetVersion = standardPset.version
    }

    private fun copySaveSettingsFromExistingSet(standardPset: ListePset) {
        val existingSaveSets = currentPsets().listeSpeichern
        if (existingSaveSets.isEmpty()) {
            return
        }

        val existingPset = existingSaveSets.first()
        for (newPset in standardPset.listeSpeichern) {
            newPset.zielPfad = existingPset.zielPfad
            newPset.isThemaAnlegen = existingPset.isThemaAnlegen
            newPset.isLaengeBeschraenken = existingPset.isLaengeBeschraenken
            newPset.isLaengeFieldBeschraenken = existingPset.isLaengeFieldBeschraenken
            newPset.maxLaenge = existingPset.maxLaenge
            newPset.maxLaengeField = existingPset.maxLaengeField
        }
    }

    private fun prepareImportedSetsForExistingConfiguration(standardPset: ListePset) {
        if (currentPsets().isEmpty()) {
            return
        }

        disableImportedSets(standardPset)
        markImportedSetsAsNew(standardPset)
    }

    private fun disableImportedSets(standardPset: ListePset) {
        for (newPset in standardPset) {
            newPset[DatenPset.PROGRAMMSET_IST_ABSPIELEN] = false.toString()
            newPset[DatenPset.PROGRAMMSET_IST_ABO] = false.toString()
            newPset[DatenPset.PROGRAMMSET_IST_BUTTON] = false.toString()
            newPset[DatenPset.PROGRAMMSET_IST_SPEICHERN] = false.toString()
        }
    }

    private fun markImportedSetsAsNew(standardPset: ListePset) {
        val date = LocalDate.now().format(NEW_SET_DATE_FORMATTER)
        standardPset.forEach { newPset ->
            newPset.name += NEW_SET_NAME_SUFFIX_PREFIX + date
        }
    }

    private fun currentPsets(): ListePset = programSets.list

    override fun close() {
        job.cancel()
        logger.debug("ProgramUpdateCheck closed.")
    }

    private companion object {
        private val logger = LogManager.getLogger(ProgramUpdateCheck::class.java)
        private const val UPDATE_CHECK_HOST = "res.mediathekview.de"
        private val INITIAL_UPDATE_CHECK_DELAY = 60.seconds
        private val UPDATE_CHECK_INTERVAL = 24.hours
        private const val NEW_SET_NAME_SUFFIX_PREFIX = ", neu: "
        private val NEW_SET_DATE_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    }
}
