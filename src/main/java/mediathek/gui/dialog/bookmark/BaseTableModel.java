package mediathek.gui.dialog.bookmark;

import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.table.AbstractTableModel;
import java.util.ArrayList;
import java.util.List;
import mediathek.tool.SVGIconUtilities;

public class BaseTableModel extends AbstractTableModel {

  private final String[] columnNames;
  private final int[] buttonColumns;
  private final String[] columnIcons = {"play","download","bookmark"};
  public IconCheckBoxItem[] columnItems;
  public final JMenuItem showAllColumns = new JMenuItem("Alle Spalten auswählen");
  public final JMenuItem hideAllColumns = new JMenuItem("Alle Spalten abwählen");

  public BaseTableModel(String[] columnNames) {
    this.columnNames = columnNames;
    this.columnItems = new IconCheckBoxItem[columnNames.length];
    List<Integer> buttonCols = new ArrayList<>();
    for (int i = 0; i < columnNames.length; i++) {
      if (columnNames[i].equals("")) {
        buttonCols.add(i);
      }
    }

    buttonColumns = new int[buttonCols.size()];
    for (int i = 0; i < buttonCols.size(); i++) {
      buttonColumns[i] = buttonCols.get(i);
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

  public boolean isButtonColumn(int columnIndex) {
    for (int buttonColumn : buttonColumns) {
      if (buttonColumn == columnIndex) {
        return true;
      }
    }
    return false;
  }

  public Icon getColumnIconAt(int index){
    return SVGIconUtilities.createSVGIcon("icons/fontawesome/" + columnIcons[index] + ".svg");
  }

  public int getNumOfButtonColums() {
    return buttonColumns.length;
  }

  public int getButtonColumnAt(int index) {
    return buttonColumns[index];
  }

}
