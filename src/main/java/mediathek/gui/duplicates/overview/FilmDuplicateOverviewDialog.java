/*
 * Created by JFormDesigner on Wed Oct 23 21:39:11 CEST 2024
 */

package mediathek.gui.duplicates.overview;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.gui.duplicates.details.DuplicateFilmDetailsTableFormat;
import mediathek.tool.EscapeKeyHandler;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;
import java.util.Comparator;
import java.util.List;

/**
 * @author christianfranzke
 */
public class FilmDuplicateOverviewDialog extends JDialog {

    private final EventList<DatenFilm> filmList = new BasicEventList<>();

    public FilmDuplicateOverviewDialog(Window owner) {
        super(owner);
        initComponents();
        EscapeKeyHandler.installHandler(this, this::dispose);

        okButton.addActionListener(e -> dispose());
        //must be called for tooltips working
        ToolTipManager.sharedInstance().registerComponent(tree);

        var rootNode = new DefaultMutableTreeNode("Filmduplikate", true);
        var senderList = getSenderList();
        for (var sender : senderList) {
            var node = getDuplicatesForSender(sender);
            //skip empty nodes
            if (node.getChildCount() > 0)
                rootNode.add(node);
        }

        var model = GlazedListsSwing.eventTableModelWithThreadProxyList(filmList, new DuplicateFilmDetailsTableFormat());
        table.setModel(model);
        resetColumnWidths();

        var treeModel = new DefaultTreeModel(rootNode);
        tree.setModel(treeModel);
        var selectionModel = tree.getSelectionModel();
        selectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        tree.setCellRenderer(new CustomTreeCellRenderer());
        selectionModel.addTreeSelectionListener(e -> {
            var node = (DefaultMutableTreeNode)tree.getLastSelectedPathComponent();
            if (node == null)
                return;
            if (node.getUserObject() instanceof DatenFilm f) {
                var list = Daten.getInstance().getListeFilme()
                        .parallelStream()
                        .filter(item -> !item.isLivestream())
                        .filter(item -> item.getUrlNormalQuality().equals(f.getUrlNormalQuality())
                                        && item.getHighQualityUrl().equals(f.getHighQualityUrl()))
                        .toList();

                filmList.clear();
                filmList.addAll(list);
                calculateColumnWidths();
            }
            else {
                // just clear table
                filmList.clear();
                resetColumnWidths();
            }
        });
    }

    private void calculateColumnWidths() {
        table.getColumnModel().getColumns().asIterator().forEachRemaining(column -> {
            int preferredWidth = column.getMinWidth();
            int maxWidth;
            int colIdx = column.getModelIndex();
            var defaultHeaderRenderer = table.getTableHeader().getDefaultRenderer();
            var columnHeaderRenderer = column.getHeaderRenderer();
            if (columnHeaderRenderer == null)
                columnHeaderRenderer = defaultHeaderRenderer;
            var header = columnHeaderRenderer.getTableCellRendererComponent(table, column.getHeaderValue(), false, false, 0, colIdx);
            maxWidth = header.getPreferredSize().width;

            for (int row = 0; row < table.getRowCount(); row++){
                var cellRenderer = table.getCellRenderer(row, colIdx);
                var c = table.prepareRenderer(cellRenderer, row, colIdx);
                int width = c.getPreferredSize().width + table.getIntercellSpacing().width;
                preferredWidth = Math.max(preferredWidth, width);

                //  We've exceeded the maximum width, no need to check other rows
                if (preferredWidth <= maxWidth){
                    preferredWidth = maxWidth;
                    break;
                }
            }
            column.setPreferredWidth(preferredWidth);
        });

        //update must be manually triggered after width modification
        table.doLayout();
    }

    private void resetColumnWidths() {
        table.getColumnModel().getColumns().asIterator().forEachRemaining(column -> column.setPreferredWidth(90));
        table.doLayout();
    }

    private DefaultMutableTreeNode getDuplicatesForSender(String sender)
    {
        var root = new DefaultMutableTreeNode(sender);

        var list = Daten.getInstance().getListeFilme().parallelStream()
                .filter(DatenFilm::isDuplicate)
                .filter(f -> f.getSender().equals(sender))
                .sorted(Comparator.comparing(DatenFilm::getTitle))
                .toList();
        //System.out.println("List size: " + list.size() + " for sender: " + sender);
        for (var f : list) {
            var node = new DefaultMutableTreeNode(f);
            root.add(node);
        }

        return root;
    }

    private List<String> getSenderList() {
        // "ARD" und "ZDF" immer am Ende um die kleineren Mediatheken nicht zu benachteiligen
        // Alphabetisch sortieren für alle anderen

        return Daten.getInstance().getListeFilme().parallelStream()
                .map(DatenFilm::getSender)
                .distinct()
                .sorted((o1, o2) -> {
                    // "ARD" und "ZDF" immer am Ende um die kleineren Mediatheken nicht zu benachteiligen
                    if (o1.equals("ARD") || o1.equals("ZDF")) {
                        return 1;
                    }
                    if (o2.equals("ARD") || o2.equals("ZDF")) {
                        return -1;
                    }
                    // Alphabetisch sortieren für alle anderen
                    return o1.compareTo(o2);
                })
                .toList();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var dialogPane = new JPanel();
        var contentPanel = new JPanel();
        var splitPane1 = new JSplitPane();
        var scrollPane1 = new JScrollPane();
        tree = new JTree();
        var scrollPane2 = new JScrollPane();
        table = new JTable();
        var buttonBar = new JPanel();
        okButton = new JButton();

        //======== this ========
        setTitle("\u00dcbersicht aller Duplikate"); //NON-NLS
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setModal(true);
        setPreferredSize(new Dimension(640, 480));
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());

        //======== dialogPane ========
        {
            dialogPane.setBorder(new EmptyBorder(12, 12, 12, 12));
            dialogPane.setPreferredSize(new Dimension(800, 600));
            dialogPane.setLayout(new BorderLayout());

            //======== contentPanel ========
            {
                contentPanel.setLayout(new BorderLayout());

                //======== splitPane1 ========
                {
                    splitPane1.setDividerLocation(350);

                    //======== scrollPane1 ========
                    {
                        scrollPane1.setViewportView(tree);
                    }
                    splitPane1.setLeftComponent(scrollPane1);

                    //======== scrollPane2 ========
                    {

                        //---- table ----
                        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                        scrollPane2.setViewportView(table);
                    }
                    splitPane1.setRightComponent(scrollPane2);
                }
                contentPanel.add(splitPane1, BorderLayout.CENTER);
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
    private JTree tree;
    private JTable table;
    private JButton okButton;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
