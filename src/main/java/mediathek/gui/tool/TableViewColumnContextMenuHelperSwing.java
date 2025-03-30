package mediathek.gui.tool;

import mediathek.tool.SVGIconUtilities;
import javax.swing.*;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class TableViewColumnContextMenuHelperSwing {

    private final JTable table;
    private JPopupMenu columnMenu;

    public TableViewColumnContextMenuHelperSwing(final JTable table) {
        this.table = table;
        setupColumnMenu();
    }

    private void setupColumnMenu() {
        // Create the popup menu
        columnMenu = new JPopupMenu();

        // Add "Select All" menu item
        JMenuItem selectAllItem = new JMenuItem("Alle auswählen");
        selectAllItem.addActionListener(this::selectAllColumns);
        columnMenu.add(selectAllItem);

        // Add "Deselect All" menu item
        JMenuItem deselectAllItem = new JMenuItem("Alle abwählen");
        deselectAllItem.addActionListener(this::deselectAllColumns);
        columnMenu.add(deselectAllItem);

        columnMenu.addSeparator();

        // Add checkboxes for each column
        TableColumnModel columnModel = table.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            TableColumn column = columnModel.getColumn(i);
            JCheckBoxMenuItem item;
            if(i == 5){
                item = new JCheckBoxMenuItem(SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg"));
            }else if(i == 6){
                item = new JCheckBoxMenuItem(SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg"));
            }else{
                item = new JCheckBoxMenuItem(column.getHeaderValue().toString());
            }
            item.setSelected(true);
            item.addActionListener(e -> {
                boolean visible = item.isSelected();
                column.setMinWidth(visible ? 50 : 0);
                column.setMaxWidth(visible ? Integer.MAX_VALUE : 0);
                column.setWidth(visible ? 100 : 0);
                column.setPreferredWidth(visible ? 100 : 0);
            });
            columnMenu.add(item);
        }

        // Add mouse listener to table header to show popup
        JTableHeader header = table.getTableHeader();
        header.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (SwingUtilities.isRightMouseButton(e)) {
                    showColumnMenu(header, e.getPoint());
                }
            }
        });
    }

    private void showColumnMenu(Component invoker, Point point) {
        columnMenu.show(invoker, point.x, point.y);
    }

    private void selectAllColumns(ActionEvent e) {
        TableColumnModel columnModel = table.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            TableColumn column = columnModel.getColumn(i);
            column.setMinWidth(50);
            column.setMaxWidth(Integer.MAX_VALUE);
            column.setWidth(100);
            column.setPreferredWidth(100);
        }
    }

    private void deselectAllColumns(ActionEvent e) {
        TableColumnModel columnModel = table.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            TableColumn column = columnModel.getColumn(i);
            column.setMinWidth(0);
            column.setMaxWidth(0);
            column.setWidth(0);
            column.setPreferredWidth(0);
        }
    }
}
