package mediathek.tool;

public enum UIProgressState {
    INIT_FX("UI Toolkit initialisieren..."),
    FILE_CLEANUP("Dateien bereinigen..."),
    START_UI("Anwendungsklassen laden..."),
    LOAD_MAINWINDOW("Hauptfenster initialisieren..."),
    LOAD_APP_DATA("Anwendungsdaten laden..."),
    LOAD_HISTORY_DATA("Liste der angesehenen Filme laden..."),
    LOAD_ABO_HISTORY_DATA("Liste der erledigten Abos laden..."),
    LOAD_FILMINFO_DIALOG("Filminfo-Dialog initialisieren..."),
    LOAD_TABS("Tabs laden..."),
    LOAD_FILM_TAB("Filme-Tab laden"),
    LOAD_DOWNLOAD_TAB("Download-Tab laden"),
    ADD_TABS_TO_UI("Tabs zum UI hinzufügen"),
    CONFIGURE_TABS("Tabs konfigurieren"),
    INIT_MENUS("Menüs initialisieren..."),
    LOAD_MEDIADB_DIALOG("Mediendatenbank-Dialog initialisieren..."),
    LOAD_MEMORY_MONITOR("Speichermonitor initialisieren..."),
    LOAD_BANDWIDTH_MONITOR("Bandbreitendialog initialisieren..."),
    INIT_UI("Benutzeroberfläche initialisieren..."),
    LOAD_CONFIG("Einstellungsdateien laden..."),
    FINISHED("Fertig.");

    private final String title;

    UIProgressState(String title) {
        this.title = title;
    }

    @Override
    public String toString() {
        return title;
    }
}
