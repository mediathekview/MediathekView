package mediathek.gui.bookmark;

import static mediathek.config.StandardLocations.getBookmarkFilePath;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import mediathek.daten.bookmark.DatenBookmark;
import mediathek.daten.bookmark.ListeBookmark;
import mediathek.tool.models.BookmarkModel;

public class BookmarkApp {

  private static final String JSON_DATEI = getBookmarkFilePath().toString();

  public static void main(String[] args) {
    List<DatenBookmark> bookmarks = ladeBookmarks(JSON_DATEI);

    SwingUtilities.invokeLater(
        () -> {
          JFrame frame = new JFrame("Lesezeichen");
          BookmarkModel model = new BookmarkModel(bookmarks);
          JTable table = new JTable(model);

          table.setPreferredScrollableViewportSize(new Dimension(800, 400));
          table.setFillsViewportHeight(true);

          JScrollPane scrollPane = new JScrollPane(table);
          frame.add(scrollPane);

          frame.pack();
          frame.setLocationRelativeTo(null);
          frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
          frame.setVisible(true);

          frame.addWindowListener(
              new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                  speichereBookmarks(model.getBookmarks(), JSON_DATEI);
                }
              });
        });
  }

  private static List<DatenBookmark> ladeBookmarks(String pfad) {
    ObjectMapper mapper = new ObjectMapper();
    File file = new File(pfad);
    if (!file.exists()) return new ArrayList<>();
    try {
      ListeBookmark liste = mapper.readValue(file, ListeBookmark.class);
      return liste.getBookmarks();
    } catch (IOException e) {
      e.printStackTrace();
      return new ArrayList<>();
    }
  }

  private static void speichereBookmarks(List<DatenBookmark> bookmarks, String pfad) {
    ObjectMapper mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);
    try {
      mapper.writeValue(new File(pfad), new ListeBookmark(bookmarks));
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}