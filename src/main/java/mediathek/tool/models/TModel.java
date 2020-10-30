package mediathek.tool.models;

import java.util.List;

@SuppressWarnings("serial")
public class TModel extends NonEditableTableModel {
    public TModel() {
        super();
    }

    public TModel(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
    }

    @SuppressWarnings("unchecked")
    public int getIdxRow(int idxWert) {
        // liefert die Zeile in der die erste Spalte idx enthält
        // die Indexspalte ist die SPALTE 0!!!!
        int ret = 0;
        for (List<Integer> list : getDataVector()) {
            if (list.get(0) == idxWert) {
                return ret;
            }
            ++ret;
        }
        return -1;
    }
}
