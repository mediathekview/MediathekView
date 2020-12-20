package mediathek.update;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.MVHttpClient;
import mediathek.tool.Version;
import mediathek.tool.javafx.FXErrorDialog;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Optional;

public class ProgrammUpdateSuchen {
    private static final String UPDATE_SEARCH_TITLE = "Software-Aktualisierung";
    private static final String UPDATE_ERROR_MESSAGE = "Es ist ein Fehler bei der Softwareaktualisierung aufgetreten.\n" +
            "Die aktuelle Version konnte nicht ermittelt werden.";
    private static final Logger logger = LogManager.getLogger(ProgrammUpdateSuchen.class);
    private final ArrayList<String[]> listInfos = new ArrayList<>();

    public void checkVersion(boolean anzeigen, boolean showProgramInformation, boolean showAllInformation) {
        // prüft auf neue Version, aneigen: wenn true, dann AUCH wenn es keine neue Version gibt ein Fenster
        Optional<ServerProgramInformation> opt = retrieveProgramInformation();
        opt.ifPresentOrElse(remoteProgramInfo -> {
            // Update-Info anzeigen
            SwingUtilities.invokeLater(() -> {
                if (showProgramInformation)
                    showProgramInformation(showAllInformation);

                if (remoteProgramInfo.version().isInvalid()) {
                    Exception ex = new RuntimeException("progInfo.version() is invalid");
                    Platform.runLater(() -> FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, UPDATE_SEARCH_TITLE, UPDATE_ERROR_MESSAGE, ex));
                    logger.warn("progInfo.version() is invalid");
                } else {
                    if (Konstanten.MVVERSION.isOlderThan(remoteProgramInfo.version())) {
                        UpdateNotificationDialog dlg = new UpdateNotificationDialog(MediathekGui.ui(), "Software Update", remoteProgramInfo.version());
                        dlg.setVisible(true);
                    } else if (anzeigen) {
                        Platform.runLater(() -> {
                            Alert alert = new Alert(Alert.AlertType.INFORMATION);
                            alert.setTitle(Konstanten.PROGRAMMNAME);
                            alert.setHeaderText(UPDATE_SEARCH_TITLE);
                            alert.setContentText("Sie benutzen die neueste Version von MediathekView.");
                            alert.show();
                        });
                    }
                }
            });
        }, () -> {
            logger.warn("did not receive ServerProgramInformation");
            Exception ex = new RuntimeException("Did not receive ServerProgramInformation");
            Platform.runLater(() -> FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, UPDATE_SEARCH_TITLE, UPDATE_ERROR_MESSAGE, ex));
        });
    }

    private void displayInfoMessages(boolean showAll) {
        //display available info...
        try {
            int angezeigt = 0;
            if (MVConfig.get(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT).isEmpty()) {
                MVConfig.add(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT, Integer.toString(-1));
            } else {
                angezeigt = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT));
            }

            StringBuilder text = new StringBuilder();
            int index = 0;
            for (String[] h : listInfos) {
                index = Integer.parseInt(h[0]);
                if (showAll || angezeigt < index) {
                    text.append("=======================================\n");
                    text.append(h[1]);
                    text.append('\n');
                    text.append('\n');
                }
            }
            if (!text.isEmpty()) {
                //TODO add new dialog with web view here!
                JDialog dlg = new DialogHinweisUpdate(null, text.toString());
                dlg.setVisible(true);
                MVConfig.add(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT, Integer.toString(index));
            }
        } catch (Exception ex) {
            logger.error("displayInfoMessages failed", ex);
        }
    }

    private void displayNoNewInfoMessage() {
        Platform.runLater(() -> {
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle(Konstanten.PROGRAMMNAME);
            alert.setHeaderText(UPDATE_SEARCH_TITLE);
            alert.setContentText("Es liegen keine Programminfos vor.");
            alert.showAndWait();
        });
    }

    private void showProgramInformation(boolean showAll) {
        if (listInfos.isEmpty()) {
            if (showAll)
                displayNoNewInfoMessage();
        } else
            displayInfoMessages(showAll);
    }

    /**
     * Load and parse the update information.
     *
     * @return parsed update info for further use when successful
     */
    private Optional<ServerProgramInformation> retrieveProgramInformation() {
        XMLStreamReader parser = null;

        XMLInputFactory inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);

        var url = Konstanten.URL_MEDIATHEKVIEW_RESOURCES.resolve(Konstanten.PROGRAM_VERSION_PATH);
        assert url != null;
        final Request request = new Request.Builder().url(url).get().build();
        try (Response response = MVHttpClient.getInstance().getReducedTimeOutClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (response.isSuccessful() && body != null) {
                try (InputStream is = body.byteStream();
                     InputStreamReader inReader = new InputStreamReader(is, StandardCharsets.UTF_8)) {
                    parser = inFactory.createXMLStreamReader(inReader);
                    String version = "";

                    while (parser.hasNext()) {
                        final int event = parser.next();
                        if (event == XMLStreamConstants.START_ELEMENT) {
                            switch (parser.getLocalName()) {
                                case ServerProgramInformation.ParserTags.VERSION:
                                    version = parser.getElementText();
                                    break;
                                case ServerProgramInformation.ParserTags.INFO:
                                    int count = parser.getAttributeCount();
                                    String nummer = "";
                                    for (int i = 0; i < count; ++i) {
                                        if (parser.getAttributeName(i).toString().equals(ServerProgramInformation.ParserTags.INFO_NO)) {
                                            nummer = parser.getAttributeValue(i);
                                        }
                                    }
                                    String info = parser.getElementText();
                                    if (!nummer.isEmpty() && !info.isEmpty()) {
                                        listInfos.add(new String[]{nummer, info});
                                    }
                                    break;
                                default:
                                    break;
                            }
                        }
                    }

                    return Optional.of(new ServerProgramInformation(Version.fromString(version)));
                } finally {
                    if (parser != null) {
                        try {
                            parser.close();
                        } catch (XMLStreamException ignored) {
                        }
                    }
                }
            } else //unsuccessful...
                return Optional.empty();
        } catch (Exception ex) {
            return Optional.empty();
        }
    }

}
