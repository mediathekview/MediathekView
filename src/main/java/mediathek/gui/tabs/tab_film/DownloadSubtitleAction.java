package mediathek.gui.tabs.tab_film;

import mediathek.config.Konstanten;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FileDialogs;
import mediathek.tool.FileUtils;
import mediathek.tool.PathExtensions;
import mediathek.tool.SwingErrorDialog;
import mediathek.tool.subtitles.MVSubtitle;
import mediathek.tool.subtitles.detector.TimedTextFormatDetector;
import mediathek.tool.subtitles.ttml2.AssExporter;
import mediathek.tool.subtitles.ttml2.SubRipHtmlExporter;
import mediathek.tool.subtitles.ttml2.Ttml2Parser;
import mediathek.tool.subtitles.vtt.WebVttToTtml2Converter;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class DownloadSubtitleAction extends AbstractAction {
    private final GuiFilme guiFilme;

    public DownloadSubtitleAction(GuiFilme guiFilme) {
        this.guiFilme = guiFilme;
        putValue(Action.NAME, "Untertitel-Datei sofort laden...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiFilme.getCurrentlySelectedFilm().ifPresent(film -> {
            var selectedFile = FileDialogs.chooseSaveFileLocation(MediathekGui.ui(), "Untertitel speichern", "");
            if (selectedFile != null) {
                Path tempSubtitleFile = null;
                var selectedFilePath = selectedFile.toPath();

                try {
                    tempSubtitleFile = FileUtils.downloadToTempFile(film.getSubtitleUrl());
                    MVSubtitle.moveWithFallback(tempSubtitleFile, selectedFilePath);

                    var res = TimedTextFormatDetector.detect(selectedFilePath, true);
                    if (!res.valid()) {
                        //invalid result
                        JOptionPane.showMessageDialog(MediathekGui.ui(), "Untertitelformat konnte nicht erkannt werden.",
                                Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        //valid result
                        if (res.format() == TimedTextFormatDetector.Format.UNKNOWN) {
                            JOptionPane.showMessageDialog(MediathekGui.ui(), "Untertitelformat wird nicht unterstützt.",
                                    Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
                            return;
                        }

                        selectedFilePath = MVSubtitle.addFileExtension(selectedFilePath, res.format());

                        if (res.format() == TimedTextFormatDetector.Format.WEBVTT) {
                            //convert WebVTT to TTML2
                            WebVttToTtml2Converter converter = new WebVttToTtml2Converter();
                            var newPath = PathExtensions.withExtension(selectedFilePath, ".ttml");
                            converter.convert(selectedFilePath, newPath);
                            selectedFilePath = newPath;
                        }

                        Ttml2Parser parser = new Ttml2Parser();
                        var ttmlDoc = parser.parse(selectedFilePath);

                        //SRT conversion
                        var srtPath = PathExtensions.withExtension(selectedFilePath, ".srt");
                        var srtStr = new SubRipHtmlExporter().export(ttmlDoc);
                        Files.writeString(srtPath, srtStr);

                        //ASS conversion
                        var assPath = PathExtensions.withExtension(selectedFilePath, ".ass");
                        var assOptions = new AssExporter.Options(384, 288, false);
                        var assStr = new AssExporter(assOptions).export(ttmlDoc);
                        Files.writeString(assPath, assStr);

                        JOptionPane.showMessageDialog(MediathekGui.ui(), "Vorgang erfolgreich.",
                                Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE);
                    }
                }
                catch (Exception ex) {
                    SwingErrorDialog.showExceptionMessage(MediathekGui.ui(),
                            "Untertitel konnte nicht geladen werden.", ex);
                }
                finally {
                    try {
                        if (tempSubtitleFile != null)
                            Files.deleteIfExists(tempSubtitleFile);
                    }
                    catch (IOException ignored) {
                    }
                }
            }
            else
                JOptionPane.showMessageDialog(MediathekGui.ui(), "Vorgang wurde abgebrochen.",
                        Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE);
        });
    }
}
