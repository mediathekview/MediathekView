package mediathek.gui.dialog.bookmark;

import javax.swing.JTable;

public class BaseTable extends JTable {
  private boolean showSenderIcon;
  public boolean showSenderIcons() {
    return showSenderIcon;
  }

  public void setShowIcon(boolean newVal) {
    showSenderIcon = newVal;
  }

}
