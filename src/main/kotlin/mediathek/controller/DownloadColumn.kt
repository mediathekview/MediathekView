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
import mediathek.i18n.UiMessages
import mediathek.tool.DownloadSizeState
import mediathek.tool.datum.Datum
import mediathek.tool.table.ColumnVisibilityStore

enum class DownloadColumn(
    val index: Int,
    private val titleKey: String,
    val valueType: Class<*>,
    private val valueProvider: ((DatenDownload) -> Any?)? = null,
) {
    NUMBER(0, "download.column.number", Int::class.javaObjectType),
    FILM_NUMBER(1, "download.column.film_number", Int::class.javaObjectType),
    ABO(2, "download.column.abo", String::class.java, DatenDownload::aboName),
    SENDER(3, "download.column.sender", String::class.java, DatenDownload::sender),
    TOPIC(4, "download.column.topic", String::class.java, DatenDownload::topic),
    TITLE(5, "download.column.title", String::class.java, DatenDownload::title),
    BUTTON_START(6, "", String::class.java),
    BUTTON_DELETE(7, "", String::class.java),
    PROGRESS(8, "download.column.progress", String::class.java),
    REMAINING_TIME(9, "download.column.remaining_time", String::class.java),
    BANDWIDTH(10, "download.column.bandwidth", String::class.java),
    SIZE(11, "download.column.size", DownloadSizeState::class.java),
    DATE(12, "download.column.date", Datum::class.java),
    TIME(13, "download.column.time", String::class.java, DatenDownload::time),
    DURATION(14, "download.column.duration", String::class.java, DatenDownload::duration),
    HIGH_QUALITY(15, "download.column.high_quality", Boolean::class.javaObjectType, { it.film?.isHighQuality == true }),
    SUBTITLE_AVAILABLE(16, "download.column.subtitle_available", Boolean::class.javaObjectType, { it.film?.hasSubtitle() == true }),
    INTERRUPTED(17, "download.column.interrupted", Boolean::class.javaObjectType, DatenDownload::isInterrupted),
    GEO(18, "download.column.geo", String::class.java, DatenDownload::geo),
    FILM_URL(19, "download.column.film_url", String::class.java, DatenDownload::filmUrl),
    HISTORY_URL(20, "download.column.history_url", String::class.java, DatenDownload::historyUrl),
    URL(21, "download.column.url", String::class.java, DatenDownload::downloadUrl),
    RTMP_URL(22, "download.column.rtmp_url", String::class.java, DatenDownload::rtmpUrl),
    SUBTITLE_URL(23, "download.column.subtitle_url", String::class.java, DatenDownload::subtitleUrl),
    PROGRAM_SET(24, "download.column.program_set", String::class.java, DatenDownload::programSetName),
    PROGRAM(25, "download.column.program", String::class.java, DatenDownload::programName),
    PROGRAM_INVOCATION(26, "download.column.program_invocation", String::class.java, DatenDownload::programInvocation),
    PROGRAM_INVOCATION_ARRAY(
        27,
        "download.column.program_invocation_array",
        String::class.java,
        DatenDownload::programInvocationArray,
    ),
    PROGRAM_RESTART(28, "download.column.program_restart", Boolean::class.javaObjectType, DatenDownload::isRestart),
    TARGET_FILE_NAME(29, "download.column.target_file_name", String::class.java, DatenDownload::targetFileName),
    TARGET_PATH(30, "download.column.target_path", String::class.java, DatenDownload::targetPath),
    TARGET_PATH_FILE_NAME(
        31,
        "download.column.target_path_file_name",
        String::class.java,
        DatenDownload::targetPathFileName,
    ),
    TYPE(32, "download.column.type", String::class.java, { it.art.legacyId.toString() }),
    SOURCE(33, "download.column.source", String::class.java, { it.quelle.legacyId.toString() }),
    DEFERRED(34, "download.column.deferred", Boolean::class.javaObjectType, DatenDownload::isDeferred),
    INFO_FILE(35, "download.column.info_file", Boolean::class.javaObjectType, DatenDownload::isInfoFile),
    SPOTLIGHT(36, "download.column.spotlight", Boolean::class.javaObjectType, DatenDownload::isSpotlight),
    SUBTITLE(37, "download.column.subtitle", Boolean::class.javaObjectType, DatenDownload::isSubtitle),
    DOWNLOAD_MANAGER(
        38,
        "download.column.download_manager",
        Boolean::class.javaObjectType,
        DatenDownload::isDownloadManager,
    ),
    REF(39, "download.column.ref", DatenDownload::class.java),
    ;

    /** Localized heading; column indexes remain stable for saved layouts and download data. */
    val title: String get() = if (titleKey.isEmpty()) "" else UiMessages.text(titleKey)

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
