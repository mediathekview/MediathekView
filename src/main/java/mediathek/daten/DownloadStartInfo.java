package mediathek.daten;

public class DownloadStartInfo {
    /**
     * Size of the download list.
     */
    public int total_num_download_list_entries = 0;
    public int total_starts = 0;
    public int num_abos = 0;
    public int num_downloads = 0;
    /**
     * not yet started but initialized.
     */
    public int initialized = 0;
    public int running = 0;
    /**
     * finished without error.
     */
    public int finished = 0;
    /**
     * finished with error.
     */
    public int error = 0;

    /**
     * determine if it has non standard values.
     * @return true if data was changed
     */
    public boolean hasValues() {
        boolean result = false;
        if (total_starts > 0 || num_abos > 0 || num_downloads > 0 || initialized > 0 ||
                running > 0 || finished > 0 || error > 0)
            result = true;

        return result;
    }
}
