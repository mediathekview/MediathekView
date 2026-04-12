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

package mediathek.controller.starter

import kotlinx.coroutines.*
import mediathek.daten.DatenDownload
import mediathek.tool.MVInfoFile
import mediathek.tool.subtitles.MVSubtitle
import org.apache.logging.log4j.Logger
import java.io.IOException

internal class DirectDownloadAncillaryFiles private constructor(
    private val jobs: List<Deferred<Unit>>,
    private val logger: Logger
) {

    suspend fun await() {
        try {
            for (job in jobs) {
                job.await()
            }
        } catch (_: CancellationException) {
            throw CancellationException()
        } catch (ex: Exception) {
            logger.error("awaitAncillaryDownloads().", ex)
        }
    }

    companion object {
        fun empty(logger: Logger): DirectDownloadAncillaryFiles {
            return DirectDownloadAncillaryFiles(emptyList(), logger)
        }

        fun start(scope: CoroutineScope, datenDownload: DatenDownload, logger: Logger): DirectDownloadAncillaryFiles {
            val infoJob = if (datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI].toBoolean()) {
                scope.async(Dispatchers.IO) {
                    try {
                        MVInfoFile().writeInfoFile(datenDownload)
                    } catch (ex: IOException) {
                        logger.error("Failed to write info file", ex)
                    }
                }
            } else {
                null
            }

            val subtitleJob = if (datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE].toBoolean()) {
                scope.async(Dispatchers.IO) {
                    try {
                        MVSubtitle().writeSubtitle(datenDownload)
                    } catch (ex: Exception) {
                        logger.error("Failed to write subtitle file", ex)
                    }
                }
            } else {
                null
            }

            return DirectDownloadAncillaryFiles(listOfNotNull(infoJob, subtitleJob), logger)
        }
    }
}
