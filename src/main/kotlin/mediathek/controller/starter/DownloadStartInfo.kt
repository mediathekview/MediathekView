package mediathek.controller.starter

class DownloadStartInfo {
    /**
     * Size of the download list.
     */
    var total_num_download_list_entries: Int = 0

    var total_starts: Int = 0

    var num_abos: Int = 0

    var num_downloads: Int = 0

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
        total_starts > 0 ||
            num_abos > 0 ||
            num_downloads > 0 ||
            initialized > 0 ||
            running > 0 ||
            finished > 0 ||
            error > 0
}
