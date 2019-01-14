package mediathek.gui.history;

import mSearch.tool.Functions;
import mediathek.controller.history.MVUsedUrl;
import mediathek.controller.history.MVUsedUrlModelHelper;
import mediathek.tool.MVMessageDialog;

import javax.swing.*;
import java.io.BufferedWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static mediathek.controller.history.MVUsedUrl.MAX_THEMA_LENGTH;
import static mediathek.controller.history.MVUsedUrl.MAX_TITLE_LENGTH;

class HistoryWriterThread extends Thread {
    private final String ziel;
    private final List<MVUsedUrl> liste;

    public HistoryWriterThread(final String ziel, List<MVUsedUrl> liste) {
        this.ziel = ziel;
        this.liste = liste;
        setName(HistoryWriterThread.class.getName());
    }

    @Override
    public void run() {
        Path logFilePath = Paths.get(ziel);
        try (OutputStream os = Files.newOutputStream(logFilePath);
             OutputStreamWriter osw = new OutputStreamWriter(os);
             BufferedWriter bw = new BufferedWriter(osw)) {
            bw.newLine();
            bw.write(getHeaderString());
            bw.newLine();
            bw.newLine();
            for (MVUsedUrl entry : liste) {
                bw.write(prepareUrlString(entry));
                bw.newLine();
            }
            bw.newLine();

            bw.flush();
        } catch (Exception ex) {
            SwingUtilities.invokeLater(() -> MVMessageDialog.showMessageDialog(null, "Datei konnte nicht geschrieben werden!",
                    "Fehler beim Schreiben", JOptionPane.ERROR_MESSAGE));
        }
    }

    private String getHeaderString() {
        return Functions.textLaenge(MAX_TITLE_LENGTH, MVUsedUrlModelHelper.TITLE_HEADER[MVUsedUrlModelHelper.USED_URL_TITEL], false, false)
                + "    " + Functions.textLaenge(MAX_THEMA_LENGTH, MVUsedUrlModelHelper.TITLE_HEADER[MVUsedUrlModelHelper.USED_URL_THEMA], false, false)
                + "    " + Functions.textLaenge(10, MVUsedUrlModelHelper.TITLE_HEADER[MVUsedUrlModelHelper.USED_URL_DATUM], false, false)
                + "    " + MVUsedUrlModelHelper.TITLE_HEADER[MVUsedUrlModelHelper.USED_URL_URL];
    }

    private String prepareUrlString(MVUsedUrl entry) {
        return Functions.textLaenge(MAX_TITLE_LENGTH, entry.getTitel(), false, false)
                + "    " + Functions.textLaenge(MAX_THEMA_LENGTH, entry.getThema(), false, false)
                + "    " + (entry.getDatum().isEmpty() ? "          " : entry.getDatum())
                + "    " + entry.getUrl();
    }
}
