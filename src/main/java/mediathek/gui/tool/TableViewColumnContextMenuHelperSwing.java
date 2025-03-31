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
    private final ImageIcon playIcon;
    private final ImageIcon downloadIcon;

    public TableViewColumnContextMenuHelperSwing(final JTable table) {
        this.table = table;
        this.playIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg");
        this.downloadIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg");
        setupColumnMenu();
    }

    private void setupColumnMenu() {
        columnMenu = new JPopupMenu();

        JMenuItem selectAllItem = new JMenuItem("Alle auswählen");
        selectAllItem.addActionListener(this::selectAllColumns);
        columnMenu.add(selectAllItem);

        JMenuItem deselectAllItem = new JMenuItem("Alle abwählen");
        deselectAllItem.addActionListener(this::deselectAllColumns);
        columnMenu.add(deselectAllItem);

        columnMenu.addSeparator();

        TableColumnModel columnModel = table.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            TableColumn column = columnModel.getColumn(i);
            String columnName = column.getHeaderValue().toString();

            if (i == 5) {
                JCheckBoxMenuItem item = new JCheckBoxMenuItem(columnName, playIcon);
                item.setSelected(true);
                item.addActionListener(e -> toggleColumnVisibility(column, item));
                columnMenu.add(item);
            }
            else if (i == 6) {
                JCheckBoxMenuItem item = new JCheckBoxMenuItem(columnName, downloadIcon);
                item.setSelected(true);
                item.addActionListener(e -> toggleColumnVisibility(column, item));
                columnMenu.add(item);
            }
            else {
                JCheckBoxMenuItem item = new JCheckBoxMenuItem(columnName);
                item.setSelected(true);
                item.addActionListener(e -> toggleColumnVisibility(column, item));
                columnMenu.add(item);
            }
        }

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

    private void toggleColumnVisibility(TableColumn column, JCheckBoxMenuItem item) {
        boolean visible = item.isSelected();
        column.setMinWidth(visible ? 50 : 0);
        column.setMaxWidth(visible ? Integer.MAX_VALUE : 0);
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