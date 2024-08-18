/*
 * Created by JFormDesigner on Sat Apr 27 12:49:11 CEST 2024
 */

package mediathek.gui.tabs.tab_film;

import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.swing.DefaultEventListModel;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.SVGIconUtilities;
import org.apache.commons.configuration2.sync.LockMode;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.function.Function;

/**
 * @author christianfranzke
 */
public class EditHistoryDialog extends JDialog {
    private static final String CONFIG_X = "edit_history.x";
    private static final String CONFIG_Y = "edit_history.y";
    private static final String CONFIG_HEIGHT = "edit_history.height";
    private static final String CONFIG_WIDTH = "edit_history.width";
    private final EventList<String> eventList;
    private final Function<Integer,Integer> inc_op = f -> f + 1;
    private final Function<Integer,Integer> dec_op = f -> f - 1;
    private final DeleteKeyAdapter keyAdapter = new DeleteKeyAdapter();
    private boolean keyAdapterInstalled;

    private class DeleteKeyAdapter extends KeyAdapter {
        @Override
        public void keyReleased(KeyEvent e) {
            if (e.getKeyCode() == KeyEvent.VK_DELETE) {
                e.consume();
                deleteEntries();
            }
        }
    }

    public EditHistoryDialog(Window owner, JMenuItem menuItem, EventList<String> eventList) {
        super(owner);
        initComponents();

        this.eventList = eventList;

        menuItem.setEnabled(false);
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosed(WindowEvent e) {
                menuItem.setEnabled(true);
                savePosition();
            }
        });

        DefaultEventListModel<String> model = GlazedListsSwing.eventListModelWithThreadProxyList(eventList);
        list.setModel(model);
        list.getSelectionModel().addListSelectionListener(l -> {
            if (l.getValueIsAdjusting())
                return;
            adjustButtons();
        });
        adjustButtons();

        btnDeleteEntries.addActionListener(l -> deleteEntries());

        btnUp.addActionListener(l -> {
            var idx = list.getSelectedIndex();
            idx = moveEntry(idx, dec_op);
            list.setSelectedIndex(idx);
        });

        btnDown.addActionListener(l -> {
            var idx = list.getSelectedIndex();
            idx = moveEntry(idx, inc_op);
            list.setSelectedIndex(idx);
        });

        restorePosition();
    }

    private void deleteEntries() {
        var changeList = new ArrayList<String>();
        var listModel = list.getModel();
        for (var idx : list.getSelectedIndices()) {
            changeList.add(listModel.getElementAt(idx));
        }

        var lock = eventList.getReadWriteLock().writeLock();
        lock.lock();
        try {
            changeList.forEach(eventList::remove);
        }
        finally {
            lock.unlock();
        }
        changeList.clear();
    }

    private int moveEntry(int idx, Function<Integer,Integer> operator) {
        var lock = eventList.getReadWriteLock().writeLock();
        lock.lock();
        try {
            var obj = eventList.get(idx);
            eventList.remove(idx);
            idx = operator.apply(idx);
            eventList.add(idx, obj);
        }
        finally {
            lock.unlock();
        }

        return idx;
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
        } catch (Exception ignored) {
            System.out.println("COULD NOT FIND CONFIGURATION");
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

    private void adjustButtons() {
        final var itemCount = list.getSelectionModel().getSelectedItemsCount();
        final var singleSelection = itemCount == 1;
        btnDeleteEntries.setEnabled(itemCount > 0);
        btnUp.setEnabled(singleSelection);
        btnDown.setEnabled(singleSelection);
        if (singleSelection) {
            //check if entry is either first or last entry
            var idx = list.getSelectionModel().getLeadSelectionIndex();
            if (idx == 0) //first
                btnUp.setEnabled(false);
            if (idx == list.getModel().getSize() - 1) //last
                btnDown.setEnabled(false);
        }
        setupKeyListener(itemCount);
    }

    private void setupKeyListener(int itemCount) {
        if (itemCount > 0) {
            if (!keyAdapterInstalled) {
                list.addKeyListener(keyAdapter);
                keyAdapterInstalled = true;
            }
        }
        else {
            if (keyAdapterInstalled) {
                list.removeKeyListener(keyAdapter);
                keyAdapterInstalled = false;
            }
        }
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var dialogPane = new JPanel();
        var contentPanel = new JPanel();
        var scrollPane1 = new JScrollPane();
        list = new JList<>();
        var toolBar1 = new JToolBar();
        btnDeleteEntries = new JButton();
        btnDeleteEntries.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg")); //NON-NLS
        btnUp = new JButton();
        btnUp.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-up.svg")); //NON-NLS
        btnDown = new JButton();
        btnDown.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-down.svg")); //NON-NLS

        //======== this ========
        setTitle("Suchhistorie bearbeiten"); //NON-NLS
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Window.Type.UTILITY);
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
                    scrollPane1.setViewportView(list);
                }
                contentPanel.add(scrollPane1, BorderLayout.CENTER);

                //======== toolBar1 ========
                {
                    toolBar1.setFloatable(false);

                    //---- btnDeleteEntries ----
                    btnDeleteEntries.setToolTipText("Ausgew\u00e4hlte Eintr\u00e4ge l\u00f6schen"); //NON-NLS
                    toolBar1.add(btnDeleteEntries);

                    //---- btnUp ----
                    btnUp.setToolTipText("Element nach oben verschieben"); //NON-NLS
                    toolBar1.add(btnUp);

                    //---- btnDown ----
                    btnDown.setToolTipText("Element nach unten verschieben"); //NON-NLS
                    toolBar1.add(btnDown);
                }
                contentPanel.add(toolBar1, BorderLayout.NORTH);
            }
            dialogPane.add(contentPanel, BorderLayout.CENTER);
        }
        contentPane.add(dialogPane, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JList<String> list;
    private JButton btnDeleteEntries;
    private JButton btnUp;
    private JButton btnDown;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
