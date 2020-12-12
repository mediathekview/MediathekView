package mediathek.tool;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.daten.GeoblockingField;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.XMLConfiguration;
import org.apache.commons.configuration2.event.ConfigurationEvent;
import org.apache.commons.configuration2.event.EventListener;
import org.apache.commons.configuration2.ex.ConfigurationException;
import org.apache.commons.configuration2.io.FileHandler;
import org.apache.commons.configuration2.sync.ReadWriteSynchronizer;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.NoSuchElementException;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * The global application configuration class. This will contain all the config data in the future.
 */
public class ApplicationConfiguration {
    public static final String APPLICATION_USER_AGENT = "application.user_agent";
    public static final String APPLICATION_INSTALL_TAB_SWITCH_LISTENER =
            "application.ui.install_tab_listeners";
    public static final String APPLICATION_UI_TAB_POSITION_TOP = "application.ui.tab_position.top";
    public static final String APPLICATION_UI_MAINWINDOW_MAXIMIZED =
            "application.ui.mainwindow.maximized";
    public static final String APPLICATION_UI_MAINWINDOW_WIDTH = "application.ui.mainwindow.width";
    public static final String APPLICATION_UI_MAINWINDOW_HEIGHT = "application.ui.mainwindow.height";
    public static final String APPLICATION_UI_MAINWINDOW_LOCATION_X =
            "application.ui.mainwindow.location.x";
    public static final String APPLICATION_UI_MAINWINDOW_LOCATION_Y =
            "application.ui.mainwindow.location.y";
    public static final String APPLICATION_UI_MAINWINDOW_TAB_ICONS =
            "application.ui.mainwindow.tab_icons";
    public static final String APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE =
            "application.ui.bandwidth_monitor.visible";
    public static final String APPLICATION_UI_USE_TRAY = "application.ui.tray.use";
    public static final String APPLICATION_UI_FONT_SIZE = "application.ui.font_size";
    public static final String APPLICATION_UI_DOWNLOAD_TAB_DIVIDER_LOCATION =
            "application.ui.download.tab.divider.location";
    public static final String APPLICATION_UI_BOOKMARKLIST = "application.ui.bookmarklist";
    public static final String APPLICATION_HTTP_DOWNLOAD_FILE_BUFFER_SIZE =
            "application.http_download.file_buffer_size";
    public static final String APPLICATION_SHOW_NOTIFICATIONS = "application.notifications.show";
    public static final String APPLICATION_SHOW_NATIVE_NOTIFICATIONS =
            "application.notifications.show_native";
    public static final String APPLICATION_NATIVE_NOTIFICATIONS_SUPPORT =
            "application.notifications.native_support";
    public static final String APPLICATION_SHOW_ORF_CONFIG_HELP = "application.orf.show_config_help";
    /**
     * can be BASIC, HEADERS, BODY
     */
    public static final String APPLICATION_DEBUG_HTTP_TRAFFIC_TRACE_LEVEL =
            "application.debug.http_traffic_trace_level";
    public static final String APPLICATION_NETWORKING_DNS_MODE = "application.networking.dns.ip_mode";
    public static final String APPLICATION_BUTTONS_PANEL_VISIBLE =
            "application.buttons_panel.visible";
    public static final String GEO_REPORT = "geo.report";
    public static final String GEO_LOCATION = "geo.location";
    public static final String BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS = "blacklist.show_geoblocked";
    public static final String DATABASE_USE_CLEANER_INTERFACE = "database.cleanup.use_cleaner";
    public static final String DOWNLOAD_RATE_LIMIT = "download.rate.limit";
    public static final String DOWNLOAD_SHOW_LAST_USED_PATH = "download.path.last_used.show";
    public static final String DOWNLOAD_SOUND_BEEP = "download.sound.beep";
    public static final String DOWNLOAD_SHOW_DESCRIPTION = "download.show_description";
    public static final String DOWNLOAD_MAX_SIMULTANEOUS_NUM = "download.max_simultaneous.number";
    public static final String SEARCH_USE_FILM_DESCRIPTIONS =
            "searchfield.film.search_through_description";
    public static final String FILM_SHOW_DESCRIPTION = "film.show_description";
    public static final String CONFIG_AUTOMATIC_UPDATE_CHECK = "application.automatic_update_check";
    public static final String TAB_FILM_FONT_SIZE = "tab.film.font_size";
    public static final String CLI_CLIENT_DOWNLOAD_LIST_FORMAT = "cli.client.download_list_format";
    /**
     * logger for {@link TimerTaskListener} inner class.
     */
    private static final Logger logger = LogManager.getLogger(ApplicationConfiguration.class);
    private static ApplicationConfiguration ourInstance;
    private XMLConfiguration config;
    private FileHandler handler;
    /**
     * Stores the previous writer task in order to cancel it if necessary. We don´t want to write
     * several times in a row.
     */
    private ScheduledFuture<?> future;

    private ApplicationConfiguration() {
        setupXmlConfiguration();
        createFileHandler();

        loadOrCreateConfiguration();
        initializeTimedEventWriting();
    }

    public static ApplicationConfiguration getInstance() {
        if (ourInstance == null) ourInstance = new ApplicationConfiguration();

        return ourInstance;
    }

    public static Configuration getConfiguration() {
        return getInstance().config;
    }

    private void initializeTimedEventWriting() {
        config.addEventListener(ConfigurationEvent.ANY, new TimerTaskListener());
    }

    private void setupXmlConfiguration() {
        config = new XMLConfiguration();
        config.setConversionHandler(new CustomConversionHandler());
        config.setSynchronizer(new ReadWriteSynchronizer());
        config.setRootElementName("settings");
        config.setThrowExceptionOnMissing(true);
    }

    private void createFileHandler() {
        handler = new FileHandler(config);
        handler.setEncoding("UTF-8");
        final String path = Daten.getSettingsDirectory_String() + File.separatorChar;

        handler.setPath(path + "settings.xml");
    }

    private void loadOrCreateConfiguration() {
        try {
            handler.load();
            // but maybe from an older version created...
            updateNewerDefaults();
        } catch (ConfigurationException cex) {
            createDefaultConfigSettings();
        }
    }

    public void writeConfiguration() {
        try {
            // cancel a pending writer task first
            if (future != null) future.cancel(false);

            handler.save();
        } catch (ConfigurationException configurationException) {
            logger.debug("Something went wrong while saving the config.", configurationException);
        }
    }

    private void createDefaultConfigSettings() {
        try {
            config.setProperty(APPLICATION_USER_AGENT, Konstanten.PROGRAMMNAME);
            config.setProperty(GEO_REPORT, true);
            config.setProperty(GEO_LOCATION, GeoblockingField.GEO_DE);

            handler.save();
        } catch (ConfigurationException configurationException) {
            logger.error(
                    "Something went wrong while creating the default config.", configurationException);
        } catch (NoSuchElementException noSuchElementException) {
            logger.error("A config element is missing.", noSuchElementException);
            System.exit(2);
        }
    }

    private void updateNewerDefaults() {
        if (!config.containsKey(GEO_REPORT)) {
            config.setProperty(GEO_REPORT, true);
        }
        if (!config.containsKey(GEO_LOCATION)) {
            config.setProperty(GEO_LOCATION, GeoblockingField.GEO_DE);
        }
        if (!config.containsKey(APPLICATION_INSTALL_TAB_SWITCH_LISTENER)) {
            config.setProperty(APPLICATION_INSTALL_TAB_SWITCH_LISTENER, !SystemUtils.IS_OS_MAC_OSX);
        }
    }

    public static class FilmList {
        public static final String LOAD_TRAILER = "filmlist.load.trailer";
        public static final String LOAD_AUDIO_DESCRIPTION = "filmlist.load.audio_description";
        public static final String LOAD_SIGN_LANGUAGE = "filmlist.load.sign_language";
        public static final String LOAD_NUM_DAYS = "filmlist.load.days";
        public static final String LOAD_LIVESTREAMS = "filmlist.load.livestreams";
        public static final String EXTEND_OLD_FILMLIST = "filmlist.extend_old_filmlist";
    }

    public static class HttpProxy {
        public static final String HOST = "http.proxy.hostname";
        public static final String PORT = "http.proxy.port";
        public static final String USER = "http.proxy.user";
        public static final String PASSWORD = "http.proxy.password";
    }

    public static class LoadFilmListDialog {
        public static final String X = "dialog.load_filmlist.x";
        public static final String Y = "dialog.load_filmlist.y";
        public static final String WIDTH = "dialog.load_filmlist.width";
        public static final String HEIGHT = "dialog.load_filmlist.height";
    }

    public static class FilterDialog {
        public static final String WIDTH = "application.ui.filter_dialog.width";
        public static final String HEIGHT = "application.ui.filter_dialog.height";
        public static final String X = "application.ui.filter_dialog.location.x";
        public static final String Y = "application.ui.filter_dialog.location.y";
        public static final String VISIBLE = "application.ui.filter_dialog.visible";
    }

    /**
     * This class will issue a timer to write config to file 5 seconds after onEvent call. In case
     * this listener is called several times in a row the timer will get reset in order to ensure that
     * config is written only once.
     */
    private final class TimerTaskListener implements EventListener<ConfigurationEvent> {
        private void launchWriterTask() {
            future =
                    Daten.getInstance()
                            .getTimerPool()
                            .schedule(
                                    () -> {
                                        try {
                                            logger.trace("Writing app configuration file");
                                            handler.save();
                                        } catch (ConfigurationException e) {
                                            logger.error("writing app config file:", e);
                                        }
                                        future = null;
                                    },
                                    5,
                                    TimeUnit.SECONDS);
        }

        @Override
        public void onEvent(ConfigurationEvent configurationEvent) {
            if (!configurationEvent.isBeforeUpdate()) {
                if (future != null) {
                    future.cancel(false);
                }
                launchWriterTask();
            }
        }
    }
}
