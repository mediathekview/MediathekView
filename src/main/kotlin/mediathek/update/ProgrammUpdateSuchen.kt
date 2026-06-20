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
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.SwingErrorDialog
import mediathek.tool.Version
import mediathek.tool.http.MVHttpClient
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import javax.swing.JDialog
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamReader

class ProgrammUpdateSuchen(
    private val ownerProvider: () -> JFrame,
    private val scope: CoroutineScope = CoroutineScope(SupervisorJob() + Dispatchers.IO + CoroutineExceptionHandler { _, ex ->
        logger.error("Program update search failed", ex)
    }),
) {
    /**
     * Prüft auf neue Version; Updates und Programminfos.
     * @param showAlert wenn true, dann AUCH wenn es keine neue Version gibt ein Fenster
     * @param showProgramInformation show program info dialog
     * @param showAllInformation show all(outdated) infos
     * @param silent If true, do not show no program info dialog
     */
    fun checkVersion(
        showAlert: Boolean,
        showProgramInformation: Boolean,
        showAllInformation: Boolean,
        silent: Boolean,
        showErrors: Boolean = true,
    ): Job =
        scope.launch {
            checkVersionSuspending(
                UpdateCheckOptions(
                    showAlert = showAlert,
                    showProgramInformation = showProgramInformation,
                    showAllInformation = showAllInformation,
                    silent = silent,
                    showErrors = showErrors,
                )
            )
        }

    suspend fun checkVersionSuspending(
        showAlert: Boolean,
        showProgramInformation: Boolean,
        showAllInformation: Boolean,
        silent: Boolean,
        showErrors: Boolean = true,
    ) {
        checkVersionSuspending(
            UpdateCheckOptions(
                showAlert = showAlert,
                showProgramInformation = showProgramInformation,
                showAllInformation = showAllInformation,
                silent = silent,
                showErrors = showErrors,
            )
        )
    }

    private suspend fun checkVersionSuspending(options: UpdateCheckOptions) {
        val programInformation = retrieveProgramInformation()
        if (programInformation == null) {
            logger.warn(SPI_RECEPTION_ERROR_MSG)
            if (options.showErrors) {
                withContext(Dispatchers.Swing) {
                    displayUpdateError(SPI_RECEPTION_ERROR_MSG)
                }
            }
            return
        }

        withContext(Dispatchers.Swing) {
            if (options.showProgramInformation) {
                showProgramInformation(
                    infos = programInformation.infos,
                    showAll = options.showAllInformation,
                    silent = options.silent,
                )
            }

            val remoteVersion = programInformation.version
            if (remoteVersion.isInvalid()) {
                logger.warn(PI_VERSION_INVALID_MSG)
                if (options.showErrors) {
                    displayUpdateError(PI_VERSION_INVALID_MSG)
                }
            } else if (Konstanten.MVVERSION.isOlderThan(remoteVersion)) {
                val dlg = UpdateNotificationDialog(ownerProvider(), "Software Update", remoteVersion)
                dlg.isVisible = true
            } else if (options.showAlert) {
                displayNoUpdateAvailableMessage()
            }
        }
    }

    private fun displayUpdateError(message: String) {
        SwingErrorDialog.showExceptionMessage(
            ownerProvider(),
            UPDATE_ERROR_MESSAGE,
            RuntimeException(message),
        )
    }

    private fun displayNoUpdateAvailableMessage() {
        JOptionPane.showMessageDialog(
            ownerProvider(),
            "Sie benutzen die aktuellste Version von MediathekView.",
            UPDATE_SEARCH_TITLE,
            JOptionPane.INFORMATION_MESSAGE,
        )
    }

    private fun displayInfoMessages(infos: List<ProgramInfo>, showAll: Boolean, silent: Boolean) {
        try {
            val displayedInfoNumber = readDisplayedInfoNumber()
            val infosToDisplay = infos.filter { showAll || displayedInfoNumber < it.number }

            if (infosToDisplay.isNotEmpty()) {
                val text = buildString {
                    infosToDisplay.forEach { info ->
                        append("=======================================\n")
                        append(info.text)
                        append('\n')
                        append('\n')
                    }
                }
                val dlg: JDialog = DialogHinweisUpdate(null, text)
                dlg.isVisible = true
                ApplicationConfiguration.getInstance().programInformationDisplayedNumber =
                    infosToDisplay.maxOf { it.number }
            } else if (!silent) {
                displayNoNewInfoMessage()
            }
        } catch (ex: Exception) {
            logger.error("displayInfoMessages failed", ex)
        }
    }

    private fun readDisplayedInfoNumber(): Int {
        return ApplicationConfiguration.getInstance().programInformationDisplayedNumber
    }

    private fun displayNoNewInfoMessage() {
        JOptionPane.showMessageDialog(
            ownerProvider(),
            "Es liegen keine aktuellen Informationen vor.",
            "Programminformationen",
            JOptionPane.INFORMATION_MESSAGE,
        )
    }

    private fun showProgramInformation(infos: List<ProgramInfo>, showAll: Boolean, silent: Boolean) {
        if (infos.isEmpty()) {
            if (showAll) {
                displayNoNewInfoMessage()
            }
        } else {
            displayInfoMessages(infos, showAll, silent)
        }
    }

    /**
     * Load and parse the update information.
     *
     * @return parsed update info for further use when successful
     */
    private fun retrieveProgramInformation(): RetrievedProgramInformation? {
        val url = requireNotNull(Konstanten.URL_MEDIATHEKVIEW_RESOURCES.resolve(Konstanten.PROGRAM_VERSION_PATH))
        val request = Request.Builder().url(url).get().build()
        return try {
            MVHttpClient.httpClient.newCall(request).execute().use { response ->
                response.body.use { body ->
                    if (!response.isSuccessful) {
                        logger.warn(
                            "Could not retrieve program information from {}: HTTP {} {}",
                            url,
                            response.code,
                            response.message,
                        )
                        return null
                    }

                    body.byteStream().use { inputStream ->
                        InputStreamReader(inputStream, StandardCharsets.UTF_8).use { reader ->
                            parseProgramInformation(reader)
                        }
                    }
                }
            }
        } catch (ex: Exception) {
            logger.warn("Could not retrieve program information from {}", url, ex)
            null
        }
    }

    private fun parseProgramInformation(reader: InputStreamReader): RetrievedProgramInformation {
        var parser: XMLStreamReader? = null
        return try {
            parser = createXmlInputFactory().createXMLStreamReader(reader)
            var version = ""
            val infos = mutableListOf<ProgramInfo>()

            while (parser.hasNext()) {
                if (parser.next() != XMLStreamConstants.START_ELEMENT) {
                    continue
                }

                when (parser.localName) {
                    XML_TAG_VERSION -> version = parser.elementText
                    XML_TAG_INFO -> {
                        val number = parser.readInfoNumber()
                        val info = parser.elementText
                        if (number != null && info.isNotEmpty()) {
                            infos += ProgramInfo(number, info)
                        }
                    }
                }
            }

            RetrievedProgramInformation(Version.fromString(version), infos)
        } finally {
            parser?.close()
        }
    }

    private fun XMLStreamReader.readInfoNumber(): Int? {
        for (index in 0 until attributeCount) {
            if (getAttributeName(index).toString() == XML_ATTRIBUTE_INFO_NUMBER) {
                return getAttributeValue(index).toIntOrNull()
            }
        }
        return null
    }

    private fun createXmlInputFactory(): XMLInputFactory =
        XMLInputFactory.newInstance().apply {
            setProperty(XMLInputFactory.IS_COALESCING, false)
            setProperty(XMLInputFactory.SUPPORT_DTD, false)
            setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
        }

    private data class UpdateCheckOptions(
        val showAlert: Boolean,
        val showProgramInformation: Boolean,
        val showAllInformation: Boolean,
        val silent: Boolean,
        val showErrors: Boolean,
    )

    private data class RetrievedProgramInformation(
        val version: Version,
        val infos: List<ProgramInfo>,
    )

    private data class ProgramInfo(
        val number: Int,
        val text: String,
    )

    private companion object {
        private const val UPDATE_SEARCH_TITLE = "Software-Aktualisierung"
        private const val UPDATE_ERROR_MESSAGE = "Es ist ein Fehler bei der Softwareaktualisierung aufgetreten.\n" +
            "Die aktuelle Version konnte nicht ermittelt werden."
        private val logger = LogManager.getLogger(ProgrammUpdateSuchen::class.java)
        private const val SPI_RECEPTION_ERROR_MSG = "Did not receive program update information"
        private const val PI_VERSION_INVALID_MSG = "progInfo.version() is invalid"
        private const val XML_TAG_INFO = "Info"
        private const val XML_TAG_VERSION = "Program_Version"
        private const val XML_ATTRIBUTE_INFO_NUMBER = "number"
    }
}
