package mediathek.gui.actions;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.stage.DirectoryChooser;
import javafx.stage.Modality;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.tool.Logfile;
import mediathek.tool.MVFunctionSys;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveOutputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.compress.utils.IOUtils;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * create a protocol tar bundle, gzip compressed.
 */
public class CreateProtocolFileAction extends AbstractAction {

    private static final String HEADER = "Protokolldatei erstellen";

    public CreateProtocolFileAction() {
        super();
        putValue(NAME, HEADER + "...");
    }

    private void addFileToArchive(File f, ArchiveOutputStream o, String entryName) throws IOException {
        ArchiveEntry entry = o.createArchiveEntry(f, entryName);
        o.putArchiveEntry(entry);
        if (f.isFile()) {
            try (InputStream i = Files.newInputStream(f.toPath())) {
                IOUtils.copy(i, o);
            }
        }
        o.closeArchiveEntry();
    }

    private void createArchive(File destDirectory) throws Exception {
        final String destDirFile = destDirectory.toString() + File.separator + "mv_protokoll.tar.gz";
        final String settingsDir = Daten.getSettingsDirectory_String();

        try (OutputStream fo = Files.newOutputStream(Paths.get(destDirFile));
             OutputStream gzo = new GzipCompressorOutputStream(fo);
             ArchiveOutputStream o = new TarArchiveOutputStream(gzo)) {
            File f = new File(settingsDir + File.separator + "mediathekview.log");
            addFileToArchive(f, o, "mediathekview.log");

            //f = new File(settingsDir + File.separator + "settings.xml");
            //addFileToArchive(f, o,"settings.xml");

            f = File.createTempFile("mediathekview_", "_suffix");
            boolean res = Logfile.LogDateiSchreiben(f.toString(),
                    MVFunctionSys.getProgVersionString(),
                    Daten.getSettingsDirectory_String(),
                    Daten.listePset.getListProg(), MVConfig.getAll());
            if (res) {
                addFileToArchive(f, o, "logfile_old.log");
            } else
                throw new Exception("failed to create old log file");

            o.finish();

            //cleanup
            Files.deleteIfExists(f.toPath());
        }
    }

    private void success() {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle("MediathekView");
        alert.setHeaderText(HEADER);
        alert.setContentText("Protokolldatei wurde erfolgreich angelegt.");
        alert.initModality(Modality.APPLICATION_MODAL);
        alert.showAndWait();
    }

    private void error() {
        Alert alert = new Alert(Alert.AlertType.ERROR);
        alert.setTitle("MediathekView");
        alert.setHeaderText(HEADER);
        alert.setContentText("Protokolldatei konnte nicht geschrieben werden!");
        alert.initModality(Modality.APPLICATION_MODAL);
        alert.showAndWait();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        Platform.runLater(() -> {
            DirectoryChooser directoryChooser = new DirectoryChooser();
            directoryChooser.setTitle("Protokoll sichern");
            File dir = directoryChooser.showDialog(null);
            if (dir != null) {
                System.out.println("DIRECTORY: " + dir.toString());
                try {
                    createArchive(dir);
                    success();
                } catch (Exception e1) {
                    e1.printStackTrace();
                    error();
                }
            }
        });
    }
}
