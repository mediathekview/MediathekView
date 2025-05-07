package mediathek.gui.dialog.bookmark;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;
import mediathek.tool.SVGIconUtilities;

class IconRenderer extends DefaultTableCellRenderer {
  @Override
  public Component getTableCellRendererComponent(JTable table, Object value,
      boolean isSelected, boolean hasFocus, int row, int column) {

    JLabel label = (JLabel) super.getTableCellRendererComponent(table, "", isSelected, hasFocus, row, column);
    label.setHorizontalAlignment(JLabel.CENTER);

    if (value instanceof BookmarkModel.ButtonType bt) {
      switch (bt) {
        case PLAY -> label.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg"));
        case DOWNLOAD -> label.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg"));
      }
    }
    return label;
  }
}

