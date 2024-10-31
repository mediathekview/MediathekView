package mediathek.tool.models;

@Deprecated
public class TModel extends NonEditableTableModel {
    public TModel() {
        super();
    }

    public TModel(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
    }
}
