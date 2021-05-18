package mediathek.tool.migrator;

import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.nio.file.Path;

public class SettingsMigrator {
    private static final Logger logger = LogManager.getLogger(SettingsMigrator.class);
    private final Path settingsFile;
    private final Configuration config = ApplicationConfiguration.getConfiguration();

    public SettingsMigrator(Path settingsFile) {
        this.settingsFile = settingsFile;
    }

    public void migrate() throws IOException, SAXException, ParserConfigurationException {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(settingsFile.toFile());
        document.getDocumentElement().normalize();
        var root = document.getDocumentElement();

        var systemNodeList = root.getElementsByTagName("system");
        if (systemNodeList.getLength() == 0) {
            logger.error("root element is empty");
            return;
        }

        var systemNode = systemNodeList.item(0);
        var systemChildNodeList = systemNode.getChildNodes();
        for (int temp = 0; temp < systemChildNodeList.getLength(); temp++) {
            Node node = systemChildNodeList.item(temp);
            if (node.getNodeType() == Node.ELEMENT_NODE) {
                Element element = (Element) node;
                var nodeName = element.getNodeName();
                switch (nodeName) {
                    case "Bandwidthmonitor-visible" -> migrateBandwidthMonitorVisibility(element);
                    case "Tray-anzeigen" -> migrateShowTray(element);
                    case "system-anz-tage-filmilste" -> // kein Fehler!!!
                            migrateFilmListAnzTage(element);
                    case "maxDownload" -> migrateMaxNumDownloads(element);
                    case "system-panel-videoplayer-anzeigen" -> migrateSystemPanelVideoplayerAnzeigen(element);
                    case "Blacklist-Geo-nicht-anzeigen" -> migrateDoNotShowGeoFilms(element);
                    case "Groesse-Einstellungen" -> migrateSettingsDialogSize(element);
                }
            }
        }
    }

    private void migrateSettingsDialogSize(Element element) {
        int width = 0, height = 0, x = 0, y = 0;
        var node = element.getFirstChild();
        if (node != null) {
            config.lock(LockMode.WRITE);
            try {
                var result = node.getNodeValue().split(":");
                if (result.length == 4) {
                    width = Integer.parseInt(result[0]);
                    height = Integer.parseInt(result[1]);
                    x = Integer.parseInt(result[2]);
                    y = Integer.parseInt(result[3]);
                }
            } catch (Exception e) {
                width = 0;
                height = 0;
                x = 0;
                y = 0;
            }

            config.setProperty(ApplicationConfiguration.SettingsDialog.WIDTH, width);
            config.setProperty(ApplicationConfiguration.SettingsDialog.HEIGHT, height);
            config.setProperty(ApplicationConfiguration.SettingsDialog.X, x);
            config.setProperty(ApplicationConfiguration.SettingsDialog.Y, y);
            config.unlock(LockMode.WRITE);
            logger.debug("migrateSettingsDialogSize");
        }
    }

    private void migrateDoNotShowGeoFilms(Element element) {
        var node = element.getFirstChild();
        if (node != null) {
            boolean result = Boolean.parseBoolean(node.getNodeValue());
            config.setProperty(ApplicationConfiguration.BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS, result);
            logger.debug("migrateDoNotShowGeoFilms");
        }
    }

    private void migrateSystemPanelVideoplayerAnzeigen(Element element) {
        var node = element.getFirstChild();
        if (node != null) {
            boolean result = Boolean.parseBoolean(node.getNodeValue());
            config.setProperty(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_VISIBLE, result);
            logger.debug("migrateSystemPanelVideoplayerAnzeigen");
        }
    }

    private void migrateMaxNumDownloads(Element element) {
        var node = element.getFirstChild();
        int maxDownloads;
        if (node != null) {
            try {
                maxDownloads = Integer.parseInt(node.getNodeValue());
            }
            catch (NumberFormatException ex) {
                maxDownloads = 1;
            }
            config.setProperty(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM, maxDownloads);
            logger.debug("migrateMaxNumDownloads");
        }
    }

    private void migrateFilmListAnzTage(Element element) {
        var node = element.getFirstChild();
        int anzahl;
        if (node != null) {
            try {
                anzahl = Integer.parseInt(node.getNodeValue());
            }
            catch (NumberFormatException ex) {
                anzahl = 0;
            }
            config.setProperty(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, anzahl);
            logger.debug("migrateFilmListAnzTage");
        }
    }

    private void migrateShowTray(Element element) {
        var node = element.getFirstChild();
        if (node != null) {
            boolean result = Boolean.parseBoolean(node.getNodeValue());
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_USE_TRAY, result);
            logger.debug("migrateShowTray");
        }
    }

    private void migrateBandwidthMonitorVisibility(Element element) {
        var node = element.getFirstChild();
        if (node != null) {
            boolean result = Boolean.parseBoolean(node.getNodeValue());
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, result);
            logger.debug("migrateBandwidthMonitorVisibility");
        }
    }
}
