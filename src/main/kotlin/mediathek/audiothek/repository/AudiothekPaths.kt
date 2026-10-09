package mediathek.audiothek.repository

import mediathek.config.StandardLocations
import java.nio.file.Path

internal object AudiothekPaths {
    private const val DEFAULT_SQLITE_EXPORT_FILENAME = "mv-audiothek.db"

    fun defaultAudiothekCachePath(): Path =
        StandardLocations.getSettingsDirectory().resolve("audiothek-cache")

    fun defaultSqliteExportPath(): Path =
        defaultAudiothekCachePath().resolve(DEFAULT_SQLITE_EXPORT_FILENAME)
}
