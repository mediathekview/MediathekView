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

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.daten.Country
import mediathek.gui.messages.BlacklistChangedEvent
import mediathek.gui.messages.GeoStateChangedEvent
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.GeoLocationDetector
import mediathek.tool.MessageBus
import mediathek.tool.http.MVHttpClient
import okhttp3.OkHttpClient
import org.apache.logging.log4j.LogManager
import java.awt.Component
import javax.swing.JFrame
import javax.swing.JOptionPane

class GeoCountryStartupCheck @JvmOverloads constructor(
    private val owner: JFrame,
    private val onFinished: Runnable = Runnable {},
    private val httpClient: OkHttpClient = MVHttpClient.getInstance().httpClient,
    private val scope: CoroutineScope = CoroutineScope(SupervisorJob() + Dispatchers.IO),
) {
    fun perform() {
        scope.launch {
            runCatching { readMismatch() }
                .onSuccess { mismatch ->
                    withContext(Dispatchers.Swing) {
                        if (mismatch != null) {
                            confirmAndApplyMismatch(mismatch)
                        }
                        onFinished.run()
                    }
                }
                .onFailure { ex ->
                    logger.debug("Country startup check failed", ex)
                    withContext(Dispatchers.Swing) {
                        onFinished.run()
                    }
                }
        }
    }

    private fun readMismatch(): CountryMismatch? {
        val configuredCountry = ApplicationConfiguration.getInstance().geographicLocation
        val detectedLocation = detectLocation() ?: return null
        return CountryMismatch(
            configuredCountry = configuredCountry,
            detectedCountry = detectedLocation.mappedCountry,
            detectedCountryCode = detectedLocation.countryCode,
        ).takeIf { it.detectedCountry != it.configuredCountry }
    }

    private fun detectLocation(): DetectedLocation? {
        return GeoLocationDetector.detectLocation(
            httpClient = httpClient,
            userAgent = readUserAgent(),
        )?.let {
            DetectedLocation(
                countryCode = it.countryCode,
                mappedCountry = it.mappedCountry,
            )
        }
    }

    private fun confirmAndApplyMismatch(mismatch: CountryMismatch) {
        val answer = JOptionPane.showConfirmDialog(
            owner as Component,
            "<html>" +
                "Der über Ihre öffentliche IP erkannte Standort (${mismatch.detectedCountryCode} / ${mismatch.detectedCountry.name})<br>" +
                "stimmt nicht mit der gespeicherten Ländereinstellung (${mismatch.configuredCountry.name}) überein.<br>" +
                "<br>" +
                "Die Standorteinstellung ist nicht dazu gedacht, Geoblocking zu umgehen, sondern dabei zu helfen,<br>" +
                "geoblockierte Filme korrekt anzuzeigen.<br>" +
                "<br>" +
                "Soll die gespeicherte Ländereinstellung auf ${mismatch.detectedCountry.name} geändert werden?" +
                "</html>",
            Konstanten.PROGRAMMNAME,
            JOptionPane.YES_NO_OPTION,
            JOptionPane.WARNING_MESSAGE,
        )

        if (answer == JOptionPane.YES_OPTION) {
            ApplicationConfiguration.getInstance().geographicLocation = mismatch.detectedCountry
            Daten.getInstance().listeBlacklist.filterListe()
            MessageBus.messageBus.publishAsync(GeoStateChangedEvent())
            MessageBus.messageBus.publishAsync(BlacklistChangedEvent())
        }
    }

    private fun readUserAgent(): String =
        ApplicationConfiguration.getConfiguration()
            .getString(ApplicationConfiguration.APPLICATION_USER_AGENT, Konstanten.PROGRAMMNAME)
            .ifBlank { Konstanten.PROGRAMMNAME }

    internal data class CountryMismatch(
        val configuredCountry: Country,
        val detectedCountry: Country,
        val detectedCountryCode: String,
    )

    internal data class DetectedLocation(
        val countryCode: String,
        val mappedCountry: Country,
    )

    companion object {
        private val logger = LogManager.getLogger(GeoCountryStartupCheck::class.java)
    }
}
