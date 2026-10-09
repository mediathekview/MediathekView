package mediathek.gui.tray

internal enum class TrayIconState {
    IDLE,
    DOWNLOADING,
    ERROR,
}

internal data class TrayDownloadSnapshot(
    val totalStarts: Int,
    val initialized: Int,
    val running: Int,
    val finished: Int,
    val error: Int,
    val bandwidthText: String = "",
    val hasValues: Boolean = totalStarts > 0 || initialized > 0 || running > 0 || finished > 0 || error > 0,
) {
    val iconState: TrayIconState
        get() = when {
            error > 0 -> TrayIconState.ERROR
            running > 0 -> TrayIconState.DOWNLOADING
            else -> TrayIconState.IDLE
        }
}

internal object TrayPresentation {
    fun downloadText(snapshot: TrayDownloadSnapshot): String = buildString {
        append("Downloads: ")
        append(snapshot.totalStarts)

        if (snapshot.hasValues) {
            append("   [ ")
            append(if (snapshot.running == 1) "1 läuft" else "${snapshot.running} laufen")

            if (snapshot.running > 0) {
                append(" (")
                append(snapshot.bandwidthText)
                append(')')
            }

            append(if (snapshot.initialized == 1) ", 1 wartet" else ", ${snapshot.initialized} warten")

            if (snapshot.finished > 0) {
                append(if (snapshot.finished == 1) ", 1 fertig" else ", ${snapshot.finished} fertig")
            }

            if (snapshot.error > 0) {
                append(if (snapshot.error == 1) ", 1 fehlerhaft" else ", ${snapshot.error} fehlerhaft")
            }

            append(" ]")
        }
    }

    fun informationText(
        filmListCreationTime: String,
        filmCount: Int,
        downloads: TrayDownloadSnapshot,
    ): String = buildString {
        append("Filmliste erstellt: ")
        append(filmListCreationTime)
        append(" Uhr  \n")
        append("Anz. Filme: ")
        append(filmCount)
        append('\n')
        append(downloadText(downloads))
    }
}
