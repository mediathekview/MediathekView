package mediathek.controller.starter

class DownloadStartInfo {
    /**
     * Size of the download list.
     */
    var totalDownloadListEntries: Int = 0

    var totalStarts: Int = 0

    var aboCount: Int = 0

    var downloadCount: Int = 0

    /**
     * not yet started but initialized.
     */
    var initialized: Int = 0

    var running: Int = 0

    /**
     * finished without error.
     */
    var finished: Int = 0

    /**
     * finished with error.
     */
    var error: Int = 0

    /**
     * determine if it has non standard values.
     * @return true if data was changed
     */
    fun hasValues(): Boolean =
        totalStarts > 0 ||
            aboCount > 0 ||
            downloadCount > 0 ||
            initialized > 0 ||
            running > 0 ||
            finished > 0 ||
            error > 0
}
