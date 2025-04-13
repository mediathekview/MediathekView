package mediathek.gui.dialog.bookmark;

import javax.swing.Icon;
import javax.swing.table.AbstractTableModel;
import java.util.ArrayList;
import java.util.List;
import mediathek.gui.IconCheckBoxItem;
import mediathek.tool.SVGIconUtilities;

public class BaseTableModel extends AbstractTableModel {

  private final String[] columnNames;
  private final int[] BUTTON_COLUMNS;
  private final String[] COLUMN_ICONS = {"icons/fontawesome/play.svg","icons/fontawesome/bookmark.svg"};
  public final IconCheckBoxItem showAllColumns = new IconCheckBoxItem("Alle Spalten auswählen");
  public final IconCheckBoxItem hideAllColumns = new IconCheckBoxItem("Alle Spalten abwählen");

  public BaseTableModel(String[] columnNames) {
    this.columnNames = columnNames;
    List<Integer> buttonCols = new ArrayList<>();
    for (int i = 0; i < columnNames.length; i++) {
      if (columnNames[i].equals("")) {
        buttonCols.add(i);
      }
    }

    BUTTON_COLUMNS = new int[buttonCols.size()];
    for (int i = 0; i < buttonCols.size(); i++) {
      BUTTON_COLUMNS[i] = buttonCols.get(i);
    }
  }

  @Override
  public int getRowCount() {
    return 0;
  }

  @Override
  public int getColumnCount() {
    return columnNames.length;
  }

  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    return null;
  }

  public int[] getButtonColumns() {
    return BUTTON_COLUMNS;
  }

  public boolean isButtonColumn(int columnIndex) {
    for (int buttonColumn : BUTTON_COLUMNS) {
      if (buttonColumn == columnIndex) {
        return true;
      }
    }
    return false;
  }

  public Icon getColumnIconAt(int index){
    return SVGIconUtilities.createSVGIcon(COLUMN_ICONS[index]);
  }
}
