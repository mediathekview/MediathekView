package mediathek.tool

enum class UIProgressState(val title: String) {
    INIT_FX("UI Toolkit initialisieren..."),
    FILE_CLEANUP("Dateien bereinigen..."),
    START_UI("Anwendungsklassen laden..."),
    LOAD_MAINWINDOW("Hauptfenster initialisieren..."),
    WAIT_FOR_HISTORY_DATA("Warte auf Abschluss des Ladens der History..."),
    CREATE_STATUS_BAR("Statusbar initialisieren..."),
    SETUP_FILM_LISTENERS("Listener initialisieren..."),
    LOAD_TABS("Tabs laden..."),
    LOAD_FILM_TAB("Filme-Tab laden"),
    LOAD_DOWNLOAD_TAB("Download-Tab laden"),
    ADD_TABS_TO_UI("Tabs zum UI hinzufügen"),
    CONFIGURE_TABS("Tabs konfigurieren"),
    INIT_MENUS("Menüs initialisieren..."),

    FINISHED("Anwendungsfenster wird geöffnet...")
}
