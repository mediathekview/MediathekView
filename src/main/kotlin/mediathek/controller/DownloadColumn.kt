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

package mediathek.controller

import mediathek.daten.DatenDownload
import mediathek.tool.DownloadSizeState
import mediathek.tool.datum.Datum
import mediathek.tool.table.ColumnVisibilityStore

enum class DownloadColumn(
    val index: Int,
    val title: String,
    val valueType: Class<*>,
    private val valueProvider: ((DatenDownload) -> Any?)? = null,
) {
    NUMBER(0, "DL Nr", Int::class.javaObjectType),
    FILM_NUMBER(1, "Film Nr.", Int::class.javaObjectType),
    ABO(2, "Abo", String::class.java, DatenDownload::aboName),
    SENDER(3, "Sender", String::class.java, DatenDownload::sender),
    TOPIC(4, "Thema", String::class.java, DatenDownload::topic),
    TITLE(5, "Titel", String::class.java, DatenDownload::title),
    BUTTON_START(6, "", String::class.java),
    BUTTON_DELETE(7, "", String::class.java),
    PROGRESS(8, "Fortschritt", String::class.java),
    REMAINING_TIME(9, "Restzeit", String::class.java),
    BANDWIDTH(10, "Geschwindigkeit", String::class.java),
    SIZE(11, "Größe [MB]", DownloadSizeState::class.java),
    DATE(12, "Datum", Datum::class.java),
    TIME(13, "Zeit", String::class.java, DatenDownload::time),
    DURATION(14, "Dauer", String::class.java, DatenDownload::duration),
    HIGH_QUALITY(15, "HQ", Boolean::class.javaObjectType, { it.film?.isHighQuality == true }),
    SUBTITLE_AVAILABLE(16, "UT", Boolean::class.javaObjectType, { it.film?.hasSubtitle() == true }),
    INTERRUPTED(17, "Pause", Boolean::class.javaObjectType, DatenDownload::isInterrupted),
    GEO(18, "Geo", String::class.java, DatenDownload::geo),
    FILM_URL(19, "URL Film", String::class.java, DatenDownload::filmUrl),
    HISTORY_URL(20, "URL History", String::class.java, DatenDownload::historyUrl),
    URL(21, "URL", String::class.java, DatenDownload::downloadUrl),
    RTMP_URL(22, "URL RTMP", String::class.java, DatenDownload::rtmpUrl),
    SUBTITLE_URL(23, "URL Untertitel", String::class.java, DatenDownload::subtitleUrl),
    PROGRAM_SET(24, "Programmset", String::class.java, DatenDownload::programSetName),
    PROGRAM(25, "Programm", String::class.java, DatenDownload::programName),
    PROGRAM_INVOCATION(26, "Programmaufruf", String::class.java, DatenDownload::programInvocation),
    PROGRAM_INVOCATION_ARRAY(
        27,
        "Programmaufruf Array",
        String::class.java,
        DatenDownload::programInvocationArray,
    ),
    PROGRAM_RESTART(28, "Restart", Boolean::class.javaObjectType, DatenDownload::isRestart),
    TARGET_FILE_NAME(29, "Dateiname", String::class.java, DatenDownload::targetFileName),
    TARGET_PATH(30, "Pfad", String::class.java, DatenDownload::targetPath),
    TARGET_PATH_FILE_NAME(
        31,
        "Pfad-Dateiname",
        String::class.java,
        DatenDownload::targetPathFileName,
    ),
    TYPE(32, "Art", String::class.java, { it.art.legacyId.toString() }),
    SOURCE(33, "Quelle", String::class.java, { it.quelle.legacyId.toString() }),
    DEFERRED(34, "Zurückgestellt", Boolean::class.javaObjectType, DatenDownload::isDeferred),
    INFO_FILE(35, "Infodatei", Boolean::class.javaObjectType, DatenDownload::isInfoFile),
    SPOTLIGHT(36, "Spotlight", Boolean::class.javaObjectType, DatenDownload::isSpotlight),
    SUBTITLE(37, "Untertitel", Boolean::class.javaObjectType, DatenDownload::isSubtitle),
    DOWNLOAD_MANAGER(
        38,
        "Remote DL",
        Boolean::class.javaObjectType,
        DatenDownload::isDownloadManager,
    ),
    REF(39, "Ref", DatenDownload::class.java),
    ;

    fun valueFrom(download: DatenDownload): Any? = valueProvider?.invoke(download)

    companion object {
        val COUNT: Int = entries.size

        private val byIndex = entries.associateBy(DownloadColumn::index)
        private val columnVisibilityStore = ColumnVisibilityStore.create(COUNT)

        fun fromIndex(index: Int): DownloadColumn =
            byIndex[index] ?: throw IndexOutOfBoundsException("UNKNOWN DOWNLOAD COLUMN: $index")

        fun isVisible(index: Int): Boolean =
            columnVisibilityStore.isVisible(index)

        fun visibilityStore(): ColumnVisibilityStore =
            columnVisibilityStore
    }
}
