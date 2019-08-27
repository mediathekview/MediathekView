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

import java.io.File;
import java.util.NoSuchElementException;
import java.util.Timer;
import java.util.TimerTask;

/**
 * The global application configuration class.
 * This will read/write all the config data in the future.
 */
public class ApplicationConfiguration {
    public static final String APPLICATION_USER_AGENT = "application.user_agent";
    public static final String APPLICATION_INSTALL_TAB_SWITCH_LISTENER = "application.ui.install_tab_listeners";
    public static final String APPLICATION_UI_TAB_POSITION_TOP = "application.ui.tab_position.top";

    public static final String APPLICATION_UI_MAINWINDOW_MAXIMIZED = "application.ui.mainwindow.maximized";
    public static final String APPLICATION_UI_MAINWINDOW_WIDTH = "application.ui.mainwindow.width";
    public static final String APPLICATION_UI_MAINWINDOW_HEIGHT = "application.ui.mainwindow.height";
    public static final String APPLICATION_UI_MAINWINDOW_LOCATION_X = "application.ui.mainwindow.location.x";
    public static final String APPLICATION_UI_MAINWINDOW_LOCATION_Y = "application.ui.mainwindow.location.y";

    public static final String APPLICATION_UI_FILTER_DIALOG_WIDTH = "application.ui.filter_dialog.width";
    public static final String APPLICATION_UI_FILTER_DIALOG_HEIGHT = "application.ui.filter_dialog.height";
    public static final String APPLICATION_UI_FILTER_DIALOG_LOCATION_X = "application.ui.filter_dialog.location.x";
    public static final String APPLICATION_UI_FILTER_DIALOG_LOCATION_Y = "application.ui.filter_dialog.location.y";
    public static final String APPLICATION_UI_FILTER_DIALOG_VISIBLE = "application.ui.filter_dialog.visible";

    public static final String APPLICATION_HTTP_DOWNLOAD_FILE_BUFFER_SIZE = "application.http_download.file_buffer_size";
    public static final String APPLICATION_SHOW_NOTIFICATIONS = "application.notifications.show";
    public static final String APPLICATION_SHOW_NATIVE_NOTIFICATIONS = "application.notifications.native";
    public static final String APPLICATION_SHOW_SPOTLIGHT_DISABLED_WARNING = "application.spotlight.disabled_warning.show";
    public static final String APPLICATION_DEBUG_HTTP_TRAFFIC = "application.debug.http_traffic";
    public static final String APPLICATION_SHOW_ORF_CONFIG_HELP = "application.orf.show_config_help";
    /**
     * can be BASIC, HEADERS, BODY
     */
    public static final String APPLICATION_DEBUG_HTTP_TRAFFIC_TRACE_LEVEL = "application.debug.http_traffic_trace_level";

    public static final String HTTP_PROXY_HOSTNAME = "http.proxy.hostname";
    public static final String HTTP_PROXY_PORT = "http.proxy.port";
    public static final String HTTP_PROXY_USERNAME = "http.proxy.user";
    public static final String HTTP_PROXY_PASSWORD = "http.proxy.password";

    public static final String FILTER_PANEL_SHOW_HD_ONLY = "filter.show.hd_only";
    public static final String FILTER_PANEL_SHOW_SUBTITLES_ONLY = "filter.show.subtitles_only";
    public static final String FILTER_PANEL_SHOW_NEW_ONLY = "filter.show.new_only";
    public static final String FILTER_PANEL_SHOW_UNSEEN_ONLY = "filter.show.unseen_only";
    public static final String FILTER_PANEL_SHOW_LIVESTREAMS_ONLY = "filter.show.livestreams_only";
    public static final String FILTER_PANEL_DONT_SHOW_ABOS = "filter.dont_show.abos";
    public static final String FILTER_PANEL_DONT_SHOW_TRAILERS = "filter.dont_show.trailers";
    public static final String FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE = "filter.dont_show.sign_language";
    public static final String FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS = "filter.dont_show.audio_versions";
    public static final String FILTER_PANEL_FILM_LENGTH_MIN = "filter.film_length.min";
    public static final String FILTER_PANEL_FILM_LENGTH_MAX = "filter.film_length.max";
    public static final String FILTER_PANEL_ZEITRAUM = "filter.zeitraum";

    public static final String FILMLIST_LOAD_TRAILER = "filmlist.load.trailer";
    public static final String FILMLIST_LOAD_AUDIODESCRIPTION = "filmlist.load.audio_description";
    public static final String FILMLIST_LOAD_SIGNLANGUAGE = "filmlist.load.sign_language";

    public static final String GEO_REPORT = "geo.report";
    public static final String GEO_LOCATION = "geo.location";

    public static final String DATABASE_USE_CLEANER_INTERFACE = "database.cleanup.use_cleaner";
    public static final String FILMLISTE_SAVE_HUMAN_READABLE = "filmlist.save.human_readable";

    public static final String DOWNLOAD_RATE_LIMIT = "download.rate.limit";
    public static final String DOWNLOAD_SHOW_LAST_USED_PATH = "download.path.last_used.show";
    public static final String DOWNLOAD_SOUND_BEEP = "download.sound.beep";

    public static final String SEARCH_USE_FILM_DESCRIPTIONS = "searchfield.film.search_through_description";

    public static final String FILM_SHOW_DESCRIPTION = "film.show_description";
    public static final String DOWNLOAD_SHOW_DESCRIPTION = "download.show_description";

    private static final ApplicationConfiguration ourInstance = new ApplicationConfiguration();

    private XMLConfiguration config;

    private FileHandler handler;

    private ApplicationConfiguration() {
        setupXmlConfiguration();
        createFileHandler();

        loadOrCreateConfiguration();
    }

    public static ApplicationConfiguration getInstance() {
        return ourInstance;
    }

    public static Configuration getConfiguration() {
        return ourInstance.config;
    }

    private void setupXmlConfiguration() {
        config = new XMLConfiguration();
        config.setSynchronizer(new ReadWriteSynchronizer());
        config.setRootElementName("settings");
        config.addEventListener(ConfigurationEvent.ANY, new TimerTaskListener());
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
            //but maybe from an older version created...
            updateNewerDefaults();
        } catch (ConfigurationException cex) {
            createDefaultConfigSettings();
        }
    }

    public void writeConfiguration() {
        try {
            handler.save();
        } catch (ConfigurationException ignored) {
        }
    }

    private void createDefaultConfigSettings() {
        try {
            config.setProperty(APPLICATION_USER_AGENT, Konstanten.PROGRAMMNAME);
            config.setProperty(GEO_REPORT, true);
            config.setProperty(GEO_LOCATION, GeoblockingField.GEO_DE);

            handler.save();
        } catch (ConfigurationException e) {
            e.printStackTrace();
        } catch (NoSuchElementException e) {
            e.printStackTrace();
            System.exit(2);
        }
    }

    private void updateNewerDefaults() {
        try {
            boolean b = config.getBoolean(GEO_REPORT);
        } catch (NoSuchElementException ignored) {
            config.setProperty(GEO_REPORT, true);
        }
        try {
            String s = config.getString(GEO_LOCATION);
        } catch (NoSuchElementException ignored) {
            config.setProperty(GEO_LOCATION, GeoblockingField.GEO_DE);
        }
        try {
            boolean b = config.getBoolean(APPLICATION_INSTALL_TAB_SWITCH_LISTENER);
        } catch (NoSuchElementException ignored) {
            if (SystemUtils.IS_OS_MAC_OSX)
                config.setProperty(APPLICATION_INSTALL_TAB_SWITCH_LISTENER, false);
            else
                config.setProperty(APPLICATION_INSTALL_TAB_SWITCH_LISTENER, true);
        }
    }

    /**
     * This class will issue a timer to write config to file 5 seconds after onEvent call.
     * In case this listener is called several times in a row the timer will get reset in order
     * to ensure that config is written only once.
     */
    private final class TimerTaskListener implements EventListener<ConfigurationEvent> {
        private Timer timer;

        public TimerTaskListener() {
            timer = new Timer();
        }

        @Override
        public void onEvent(ConfigurationEvent configurationEvent) {
            timer.cancel();
            timer = new Timer();
            timer.schedule(new WriteConfigurationTask(), 5_000);
        }

        /**
         * Task which saves the current configuration to disk.
         */
        private final class WriteConfigurationTask extends TimerTask {
            @Override
            public void run() {
                try {
                    handler.save();
                } catch (ConfigurationException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
