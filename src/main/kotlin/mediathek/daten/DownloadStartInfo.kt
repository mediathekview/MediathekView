package mediathek.daten

class DownloadStartInfo {
    /**
     * Size of the download list.
     */
    @JvmField
    var total_num_download_list_entries: Int = 0

    @JvmField
    var total_starts: Int = 0

    @JvmField
    var num_abos: Int = 0

    @JvmField
    var num_downloads: Int = 0

    /**
     * not yet started but initialized.
     */
    @JvmField
    var initialized: Int = 0

    @JvmField
    var running: Int = 0

    /**
     * finished without error.
     */
    @JvmField
    var finished: Int = 0

    /**
     * finished with error.
     */
    @JvmField
    var error: Int = 0

    /**
     * determine if it has non standard values.
     * @return true if data was changed
     */
    fun hasValues(): Boolean =
        total_starts > 0 ||
            num_abos > 0 ||
            num_downloads > 0 ||
            initialized > 0 ||
            running > 0 ||
            finished > 0 ||
            error > 0
}
