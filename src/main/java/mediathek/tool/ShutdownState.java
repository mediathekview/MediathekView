package mediathek.tool;

public enum ShutdownState {
    SHUTDOWN_NOTIFICATION_CENTER("Native Benachrichtigungen beenden..."),
    SHUTDOWN_THREAD_POOL("Beende Threadpools..."),
    PERFORM_SEEN_HISTORY_MAINTENANCE("Führe Wartung an der Download-Historie durch..."),
    SAVE_FILM_DATA("Film-Daten sichern"),
    SAVE_DOWNLOAD_DATA("Download-Daten sichern"),
    STOP_DOWNLOADS("Downloads anhalten"),
    SAVE_CONFIG("Programmkonfiguration schreiben"),
    SAVE_APP_DATA("Programmdaten sichern"),
    SAVE_BOOKMARKS("Merkliste sichern"),
    COMPLETE("Fertig.");
    private final String title;

    ShutdownState(String title) {
        this.title = title;
    }

    @Override
    public String toString() {
        return title;
    }
}
