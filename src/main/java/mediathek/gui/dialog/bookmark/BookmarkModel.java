package mediathek.gui.dialog.bookmark;

import java.util.List;
import javax.swing.SwingUtilities;
import javax.swing.table.AbstractTableModel;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;

public class BookmarkModel extends AbstractTableModel {

  private static final String[] columnNames = { "Sender", "Thema", "Titel", "Dauer", "Sendedatum", "","","URL", "Verfügbar bis", "Notiz" };
    /**
     * shortcut zum DatenBookmark objekt via getValueAt.
     */
  public static final int BOOKMARK_REF = columnNames.length;
  private final List<DatenBookmark> bookmarks;
  public BookmarkModel(List<DatenBookmark> bookmarks) {
    this.bookmarks = bookmarks;
    MessageBus.getMessageBus().subscribe(this);
  }

  @Override
  public int getRowCount() {
    return bookmarks.size();
  }

    /**
     * Remove a BlacklistRule from model
     *
     * @param modelIndex index from blacklist to delete
     */
    public void removeRow(int modelIndex) {
        bookmarks.remove(modelIndex);
        fireTableRowsDeleted(modelIndex, modelIndex);
    }

  public void removeRow(DatenBookmark bookmark) {
    int index = bookmarks.indexOf(bookmark);
    if (index != -1) {
      bookmarks.remove(index);
      fireTableRowsDeleted(index, index);
    }
  }


  @Override
  public int getColumnCount() {
    return columnNames.length;
  }

  @Override
  public String getColumnName(int col) {
    return columnNames[col];
  }

  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    DatenBookmark bookmark = bookmarks.get(rowIndex);
      return switch (columnIndex) {
          case 0 -> bookmark.getSender();
          case 1 -> bookmark.getThema();
          case 2 -> bookmark.getTitel();
          case 3 -> bookmark.getDauer();
          case 4 -> bookmark.getSendDate();
          case 5 -> BookmarkModel.ButtonType.PLAY;
          case 6 -> BookmarkModel.ButtonType.DOWNLOAD;
          case 7 -> bookmark.getUrl();
          case 8 -> bookmark.getExpiry();
          case 9 -> bookmark.getNote();
          case 10 -> bookmark; //BOOKMARK_REF
          default -> null;
      };
  }


    /*
    Warum implementierst Du das?
    Wenn Du in einem Dialog die DatenBookmark anpasst dann ist dies definitiv der falsche Weg da es zum Editieren von Tabellenzellen dient.
    Und das ist hier nicht gewollt. Falscher Ansatz.
     */
  @Override
  public void setValueAt(Object value, int row, int col) {
    DatenBookmark bm = bookmarks.get(row);
    switch (col) {
      case 0 -> bm.setSeen((Boolean) value);
      case 1 -> bm.setTitel((String) value);
      case 2 -> bm.setSender((String) value);
      case 3 -> bm.setExpiry((String) value);
      case 4 -> bm.setUrl((String) value);
    }
    fireTableCellUpdated(row, col);
  }

  public void addRow(DatenBookmark bookmark) {
    bookmarks.add(bookmark);
    fireTableRowsInserted(bookmarks.size() - 1, bookmarks.size() - 1);
  }

  @Handler // engio mbassador annotation
  private void handleBookmarkEvent(BookmarkEvent e) {
    SwingUtilities.invokeLater(() -> {fireTableDataChanged();});
  }

  public enum ButtonType {
    PLAY,
    DOWNLOAD
  }
}
