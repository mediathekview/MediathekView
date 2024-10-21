/*
 * Created by JFormDesigner on Mon Oct 21 17:52:41 CEST 2024
 */

package mediathek.gui.duplicates;

import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.config.Daten;
import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Comparator;
import java.util.NoSuchElementException;

/**
 * @author christianfranzke
 */
public class DuplicateStatisticsDialog extends JDialog {
    private final AbstractAction action;
    private static final String CONFIG_X = "duplicate_statistics_dialog.x";
    private static final String CONFIG_Y = "duplicate_statistics_dialog.y";
    private static final String CONFIG_HEIGHT = "duplicate_statistics_dialog.height";
    private static final String CONFIG_WIDTH = "duplicate_statistics_dialog.width";
    private static final Logger logger = LogManager.getLogger();

    private void restorePosition() {
        var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.READ);
            int x = config.getInt(CONFIG_X);
            int y = config.getInt(CONFIG_Y);
            int width = config.getInt(CONFIG_WIDTH);
            int height = config.getInt(CONFIG_HEIGHT);

            setSize(width, height);
            setLocation(x, y);
        }
        catch (NoSuchElementException e) {
            pack();
        }
        catch (Exception ex) {
            logger.error("Unhandled Exception", ex);
            pack();
        } finally {
            config.unlock(LockMode.READ);
        }
    }

    private void savePosition() {
        var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.WRITE);
            var size = getSize();
            var location = getLocation();
            config.setProperty(CONFIG_WIDTH, size.width);
            config.setProperty(CONFIG_HEIGHT, size.height);
            config.setProperty(CONFIG_X, location.x);
            config.setProperty(CONFIG_Y, location.y);
        } finally {
            config.unlock(LockMode.WRITE);
        }
    }

    public DuplicateStatisticsDialog(@NotNull Window owner, @NotNull AbstractAction action) {
        super(owner);
        this.action = action;

        initComponents();

        var tableFormat = new DuplicateStatisticsTableFormat();
        final var dupeStatsList = Daten.getInstance().getFilmListDuplicateStatisticsList();
        SortedList<DuplicateStatistics> sortedList = new SortedList<>(dupeStatsList, Comparator.comparing(DuplicateStatistics::sender));
        var model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, tableFormat);
        model.addTableModelListener(e -> updateTotalStats());
        table.setModel(model);

        updateTotalStats();

        //table.getColumnModel().getColumn(0).setPreferredWidth(120);
        table.getColumnModel().getColumn(1).setPreferredWidth(130);
        resizeSenderColumnWidth();

        action.setEnabled(false);

        restorePosition();
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosed(WindowEvent e) {
                savePosition();
            }
        });
    }

    @Override
    public void dispose() {
        action.setEnabled(true);

        super.dispose();
    }

    private void resizeSenderColumnWidth() {
        final TableColumnModel columnModel = table.getColumnModel();
        int width = 120; // Min width
        for (int row = 0; row < table.getRowCount(); row++) {
            TableCellRenderer renderer = table.getCellRenderer(row, 0);
            Component comp = table.prepareRenderer(renderer, row, 0);
            width = Math.max(comp.getPreferredSize().width + 1, width);
        }
        columnModel.getColumn(0).setPreferredWidth(width);
    }

    private void updateTotalStats() {
        resizeSenderColumnWidth();

        var statisticsEventList = Daten.getInstance().getFilmListDuplicateStatisticsList();
        statisticsEventList.getReadWriteLock().readLock().lock();
        long dupes = 0;
        for (var item: statisticsEventList) {
            dupes += item.count();
        }
        label1.setText(String.format("Gesamtanzahl Duplikate: %d", dupes));
        statisticsEventList.getReadWriteLock().readLock().unlock();
        pack();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var contentPane = new JPanel();
        scrollPane1 = new JScrollPane();
        table = new JTable();
        label1 = new JLabel();

        //======== this ========
        setTitle("Filmduplikat-Statistik"); //NON-NLS
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Window.Type.UTILITY);
        var contentPane2 = getContentPane();
        contentPane2.setLayout(new BorderLayout());

        //======== contentPane ========
        {
            contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
            contentPane.setLayout(new BorderLayout(0, 5));

            //======== scrollPane1 ========
            {

                //---- table ----
                table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                table.setShowHorizontalLines(false);
                table.setShowVerticalLines(false);
                table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                table.setPreferredScrollableViewportSize(new Dimension(250, 400));
                scrollPane1.setViewportView(table);
            }
            contentPane.add(scrollPane1, BorderLayout.CENTER);

            //---- label1 ----
            label1.setText("Gesamtanzahl Duplikate:"); //NON-NLS
            contentPane.add(label1, BorderLayout.PAGE_END);
        }
        contentPane2.add(contentPane, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JScrollPane scrollPane1;
    private JTable table;
    private JLabel label1;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
