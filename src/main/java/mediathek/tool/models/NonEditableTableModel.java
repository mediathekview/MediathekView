package mediathek.tool.models;

import javax.swing.table.DefaultTableModel;

/**
 * Simple extension of DefaultTableModel which prevents cell from being editable
 */
public class NonEditableTableModel extends DefaultTableModel {
    public NonEditableTableModel() {
        super();
    }

    public NonEditableTableModel(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
    }

    @Override
    public boolean isCellEditable(int i, int j) {
        return false;
    }

}
