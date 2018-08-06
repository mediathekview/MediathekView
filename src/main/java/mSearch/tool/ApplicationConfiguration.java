package mSearch.tool;

import com.jidesoft.utils.SystemInfo;
import javafx.animation.PauseTransition;
import javafx.util.Duration;
import mSearch.daten.DatenFilm;
import mediathek.config.Daten;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.XMLConfiguration;
import org.apache.commons.configuration2.event.ConfigurationEvent;
import org.apache.commons.configuration2.event.EventListener;
import org.apache.commons.configuration2.ex.ConfigurationException;
import org.apache.commons.configuration2.io.FileHandler;
import org.apache.commons.configuration2.sync.ReadWriteSynchronizer;

import java.io.File;
import java.util.NoSuchElementException;

/**
 * The global application configuration class.
 * This will read/write all the config data in the future.
 */
public class ApplicationConfiguration {
    public static final String APPLICATION_USER_AGENT = "application.user_agent";
    public static final String APPLICATION_INSTALL_TAB_SWITCH_LISTENER = "application.ui.install_tab_listeners";
    public static final String APPLICATION_FORCE_UPDATE_SERVER_RELOAD_AKT = "application.force_update_server_reload.akt";
    public static final String APPLICATION_FORCE_UPDATE_SERVER_RELOAD_DIFF = "application.force_update_server_reload.diff";


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

    public static final String GEO_REPORT = "geo.report";
    public static final String GEO_LOCATION = "geo.location";

    public static final String DATABASE_USE_CLEANER_INTERFACE = "database.cleanup.use_cleaner";
    public static final String FILMLISTE_SAVE_HUMAN_READABLE = "filmlist.save.human_readable";

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

    private final class EvtListener implements EventListener<ConfigurationEvent> {
        private final PauseTransition pause;

        public EvtListener() {
            pause = new PauseTransition(Duration.millis(5000));
            pause.setOnFinished(evtl -> {
                try {
                    handler.save();
                } catch (ConfigurationException e) {
                    e.printStackTrace();
                }
            });
        }

        @Override
        public void onEvent(ConfigurationEvent configurationEvent) {
            if (!configurationEvent.isBeforeUpdate())
                pause.playFromStart();
        }
    }

    private void setupXmlConfiguration() {
        config = new XMLConfiguration();
        config.setSynchronizer(new ReadWriteSynchronizer());
        config.setRootElementName("settings");
        config.addEventListener(ConfigurationEvent.ANY, new EvtListener());
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
            config.setProperty(APPLICATION_USER_AGENT, "MediathekView");
            config.setProperty(GEO_REPORT, true);
            config.setProperty(GEO_LOCATION, DatenFilm.GEO_DE);

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
            config.setProperty(GEO_LOCATION, DatenFilm.GEO_DE);
        }
        try {
            boolean b = config.getBoolean(APPLICATION_INSTALL_TAB_SWITCH_LISTENER);
        } catch (NoSuchElementException ignored) {
            if (SystemInfo.isMacOSX())
                config.setProperty(APPLICATION_INSTALL_TAB_SWITCH_LISTENER, false);
            else
                config.setProperty(APPLICATION_INSTALL_TAB_SWITCH_LISTENER, true);
        }
    }
}
