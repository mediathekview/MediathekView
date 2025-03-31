/*
 * Created by JFormDesigner on Mon Oct 21 17:52:41 CEST 2024
 */

package mediathek.gui.duplicates.statistics;

import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.config.Daten;
import mediathek.gui.duplicates.FilmStatistics;
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
    private static final String CONFIG_X = "duplicate_statistics_dialog.x";
    private static final String CONFIG_Y = "duplicate_statistics_dialog.y";
    private static final String CONFIG_HEIGHT = "duplicate_statistics_dialog.height";
    private static final String CONFIG_WIDTH = "duplicate_statistics_dialog.width";
    private static final Logger logger = LogManager.getLogger();
    private static final int COL_NUM_WIDTH = 90;
    private static final int IDX_NUM = 1;
    private final AbstractAction action;
    private final DuplicateStatisticsTableFormat tableFormat = new DuplicateStatisticsTableFormat();

    public DuplicateStatisticsDialog(@NotNull Window owner, @NotNull AbstractAction action) {
        super(owner);
        this.action = action;

        initComponents();

        setupCommonTable();
        setupDuplicatesTable();

        updateTotalCommonStats();
        updateTotalDuplicatesStats();

        //table.getColumnModel().getColumn(0).setPreferredWidth(120);
        tblCommon.getColumnModel().getColumn(IDX_NUM).setPreferredWidth(COL_NUM_WIDTH);
        tblDuplicates.getColumnModel().getColumn(IDX_NUM).setPreferredWidth(COL_NUM_WIDTH);
        resizeSenderColumnWidth(tblCommon);
        resizeSenderColumnWidth(tblDuplicates);

        action.setEnabled(false);

        restorePosition();
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosed(WindowEvent e) {
                savePosition();
            }
        });
    }

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

    private void setupDuplicatesTable() {
        final var dupeStatsList = Daten.getInstance().getDuplicateStatistics();
        SortedList<FilmStatistics> sortedList = new SortedList<>(dupeStatsList, Comparator.comparing(FilmStatistics::sender));
        var model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, tableFormat);
        model.addTableModelListener(e -> updateTotalDuplicatesStats());
        tblDuplicates.setModel(model);
    }

    private void setupCommonTable() {
        final var commonStats = Daten.getInstance().getCommonStatistics();
        SortedList<FilmStatistics> sortedList = new SortedList<>(commonStats, Comparator.comparing(FilmStatistics::sender));
        var model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, tableFormat);
        model.addTableModelListener(e -> updateTotalCommonStats());
        tblCommon.setModel(model);
    }

    @Override
    public void dispose() {
        action.setEnabled(true);

        super.dispose();
    }

    private void resizeSenderColumnWidth(@NotNull JTable table) {
        final TableColumnModel columnModel = table.getColumnModel();
        int width = 120; // Min width
        for (int row = 0; row < table.getRowCount(); row++) {
            TableCellRenderer renderer = table.getCellRenderer(row, 0);
            Component comp = table.prepareRenderer(renderer, row, 0);
            width = Math.max(comp.getPreferredSize().width + 1, width);
        }
        columnModel.getColumn(0).setPreferredWidth(width);
    }

    private void updateTotalCommonStats() {
        resizeSenderColumnWidth(tblCommon);

        var statisticsList = Daten.getInstance().getCommonStatistics();
        statisticsList.getReadWriteLock().readLock().lock();
        long dupes = 0;
        for (var item: statisticsList) {
            dupes += item.count();
        }
        statisticsList.getReadWriteLock().readLock().unlock();

        lblTotalCommon.setText(String.format("Gesamtanzahl Filme: %d", dupes));
    }

    private void updateTotalDuplicatesStats() {
        resizeSenderColumnWidth(tblDuplicates);

        var statisticsEventList = Daten.getInstance().getDuplicateStatistics();
        statisticsEventList.getReadWriteLock().readLock().lock();
        long dupes = 0;
        for (var item: statisticsEventList) {
            dupes += item.count();
        }
        statisticsEventList.getReadWriteLock().readLock().unlock();

        lblTotalDuplicates.setText(String.format("Gesamtanzahl Duplikate: %d", dupes));
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var tabbedPane = new JTabbedPane();
        var commonStats = new JPanel();
        var scrollPane2 = new JScrollPane();
        tblCommon = new JTable();
        lblTotalCommon = new JLabel();
        var duplicatePanel = new JPanel();
        var scrollPane1 = new JScrollPane();
        tblDuplicates = new JTable();
        lblTotalDuplicates = new JLabel();

        //======== this ========
        setTitle("Film-Statistik"); //NON-NLS
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Window.Type.UTILITY);
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());

        //======== tabbedPane ========
        {
            tabbedPane.setBorder(new EmptyBorder(5, 5, 5, 5));

            //======== commonStats ========
            {
                commonStats.setBorder(new EmptyBorder(5, 5, 5, 5));
                commonStats.setLayout(new BorderLayout(0, 5));

                //======== scrollPane2 ========
                {

                    //---- tblCommon ----
                    tblCommon.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                    tblCommon.setPreferredScrollableViewportSize(new Dimension(250, 400));
                    tblCommon.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    tblCommon.setShowHorizontalLines(false);
                    tblCommon.setShowVerticalLines(false);
                    scrollPane2.setViewportView(tblCommon);
                }
                commonStats.add(scrollPane2, BorderLayout.CENTER);

                //---- lblTotalCommon ----
                lblTotalCommon.setText("Gesamtanzahl Filme:"); //NON-NLS
                commonStats.add(lblTotalCommon, BorderLayout.SOUTH);
            }
            tabbedPane.addTab("Allgemein", commonStats); //NON-NLS

            //======== duplicatePanel ========
            {
                duplicatePanel.setBorder(new EmptyBorder(5, 5, 5, 5));
                duplicatePanel.setLayout(new BorderLayout(0, 5));

                //======== scrollPane1 ========
                {

                    //---- tblDuplicates ----
                    tblDuplicates.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    tblDuplicates.setShowHorizontalLines(false);
                    tblDuplicates.setShowVerticalLines(false);
                    tblDuplicates.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                    tblDuplicates.setPreferredScrollableViewportSize(new Dimension(250, 400));
                    scrollPane1.setViewportView(tblDuplicates);
                }
                duplicatePanel.add(scrollPane1, BorderLayout.CENTER);

                //---- lblTotalDuplicates ----
                lblTotalDuplicates.setText("Gesamtanzahl Duplikate:"); //NON-NLS
                duplicatePanel.add(lblTotalDuplicates, BorderLayout.PAGE_END);
            }
            tabbedPane.addTab("Duplikate", duplicatePanel); //NON-NLS
        }
        contentPane.add(tabbedPane, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JTable tblCommon;
    private JLabel lblTotalCommon;
    private JTable tblDuplicates;
    private JLabel lblTotalDuplicates;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
