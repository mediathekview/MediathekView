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

package mediathek.audiothek.repository

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import mediathek.audiothek.model.AudioDataset
import org.apache.logging.log4j.LogManager

class AudioRepository(
    private val sqliteExportSource: SqliteExportAudioSource = SqliteExportAudioSource(),
) {
    private val logger = LogManager.getLogger(AudioRepository::class.java)

    suspend fun loadAudiothek(): AudioLoadResult = withContext(Dispatchers.IO) {
        val downloadStatus = sqliteExportSource.updateLocalDatabase()
        if (downloadStatus == SqliteExportDownloadStatus.FAILED) {
            logger.warn("MediathekView Audiothek update failed, continuing with the previous local database if available")
        }

        val dataset = runCatching { sqliteExportSource.loadDataset() }
            .onFailure { logger.warn("Failed to load MediathekView Audiothek data", it) }
            .getOrNull()
            ?: error("Keine Audiothek-Daten verfügbar")

        AudioLoadResult(
            dataset = dataset,
            downloadStatus = downloadStatus,
        )
    }
}

data class AudioLoadResult(
    val dataset: AudioDataset,
    val downloadStatus: SqliteExportDownloadStatus,
) {
    fun hasUpdatedSource(): Boolean = downloadStatus == SqliteExportDownloadStatus.DOWNLOADED

    fun reloadMessage(): String? = when (downloadStatus) {
        SqliteExportDownloadStatus.DOWNLOADED -> null
        SqliteExportDownloadStatus.NOT_MODIFIED ->
            "Es konnte keine neue Datei geladen werden.\nDie vorhandene ist bereits aktuell."
        SqliteExportDownloadStatus.FAILED ->
            "Es konnte keine neue Datei geladen werden.\nDie vorhandene wird weiter verwendet."
    }
}
