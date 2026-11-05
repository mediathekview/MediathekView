package mediathek.tool

import mediathek.i18n.UiMessages
enum class UIProgressState(private val titleKey: String) {
    INIT_FX("progress.init_fx"),
    FILE_CLEANUP("progress.file_cleanup"),
    START_UI("progress.start_ui"),
    LOAD_MAINWINDOW("progress.load_mainwindow"),
    WAIT_FOR_HISTORY_DATA("progress.wait_for_history_data"),
    CREATE_STATUS_BAR("progress.create_status_bar"),
    SETUP_FILM_LISTENERS("progress.setup_film_listeners"),
    LOAD_TABS("progress.load_tabs"),
    LOAD_FILM_TAB("progress.load_film_tab"),
    LOAD_DOWNLOAD_TAB("progress.load_download_tab"),
    ADD_TABS_TO_UI("progress.add_tabs_to_ui"),
    CONFIGURE_TABS("progress.configure_tabs"),
    INIT_MENUS("progress.init_menus"),
    FINISHED("progress.finished");

    val title: String get() = UiMessages.text(titleKey)
}
