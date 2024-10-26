/*
 * Created by JFormDesigner on Tue Oct 22 13:37:47 CEST 2024
 */

package mediathek.gui.duplicates.details;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import ca.odell.glazedlists.swing.TableComparatorChooser;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.NoSuchElementException;

/**
 * @author christianfranzke
 */
public class DuplicateFilmDetailsDialog extends JDialog {
    private static final String CONFIG_X = "duplicate_film_details_dialog.x";
    private static final String CONFIG_Y = "duplicate_film_details_dialog.y";
    private static final String CONFIG_HEIGHT = "duplicate_film_details_dialog.height";
    private static final String CONFIG_WIDTH = "duplicate_film_details_dialog.width";
    private static final Logger logger = LogManager.getLogger();

    public DuplicateFilmDetailsDialog(Window owner, @NotNull DatenFilm film) {
        super(owner);
        initComponents();

        okButton.addActionListener(_ -> dispose());

        BasicEventList<DatenFilm> duplicateList = new BasicEventList<>();
        SortedList<DatenFilm> sortedList = new SortedList<>(duplicateList);
        var model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, new DuplicateFilmDetailsTableFormat());
        table1.setModel(model);
        TableComparatorChooser.install(table1, sortedList, AbstractTableComparatorChooser.SINGLE_COLUMN);

        table1.getColumnModel().getColumn(0).setPreferredWidth(90);
        table1.getColumnModel().getColumn(1).setPreferredWidth(120);
        table1.getColumnModel().getColumn(2).setPreferredWidth(200);
        table1.getColumnModel().getColumn(5).setPreferredWidth(400);
        table1.getColumnModel().getColumn(6).setPreferredWidth(400);

        var url = film.getUrlNormalQuality();
        Daten.getInstance().getListeFilme().parallelStream()
                .filter(f -> f.getUrlNormalQuality().equals(url))
                .forEach(duplicateList::add);

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

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var dialogPane = new JPanel();
        var contentPanel = new JPanel();
        var scrollPane1 = new JScrollPane();
        table1 = new JTable();
        var buttonBar = new JPanel();
        okButton = new JButton();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setModal(true);
        setTitle("Zusammengeh\u00f6rige Filme"); //NON-NLS
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());

        //======== dialogPane ========
        {
            dialogPane.setBorder(new EmptyBorder(12, 12, 12, 12));
            dialogPane.setLayout(new BorderLayout());

            //======== contentPanel ========
            {
                contentPanel.setLayout(new BorderLayout());

                //======== scrollPane1 ========
                {

                    //---- table1 ----
                    table1.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                    table1.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    table1.setShowHorizontalLines(false);
                    table1.setShowVerticalLines(false);
                    table1.setPreferredScrollableViewportSize(new Dimension(500, 150));
                    scrollPane1.setViewportView(table1);
                }
                contentPanel.add(scrollPane1, BorderLayout.CENTER);
            }
            dialogPane.add(contentPanel, BorderLayout.CENTER);

            //======== buttonBar ========
            {
                buttonBar.setBorder(new EmptyBorder(12, 0, 0, 0));
                buttonBar.setLayout(new GridBagLayout());
                ((GridBagLayout)buttonBar.getLayout()).columnWidths = new int[] {0, 80};
                ((GridBagLayout)buttonBar.getLayout()).columnWeights = new double[] {1.0, 0.0};

                //---- okButton ----
                okButton.setText("Schlie\u00dfen"); //NON-NLS
                buttonBar.add(okButton, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                    new Insets(0, 0, 0, 0), 0, 0));
            }
            dialogPane.add(buttonBar, BorderLayout.SOUTH);
        }
        contentPane.add(dialogPane, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JTable table1;
    private JButton okButton;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
