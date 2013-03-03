package com.explodingpixels.macwidgets.plaf;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.UIManager;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.AbstractLayoutCache;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import com.explodingpixels.macwidgets.MacWidgetFactory;
import com.explodingpixels.macwidgets.SourceList;
import com.explodingpixels.macwidgets.SourceListBadgeContentProvider;
import com.explodingpixels.macwidgets.SourceListCategory;
import com.explodingpixels.macwidgets.SourceListColorScheme;
import com.explodingpixels.macwidgets.SourceListCountBadgeRenderer;
import com.explodingpixels.macwidgets.SourceListModel;
import com.explodingpixels.macwidgets.SourceListStandardColorScheme;
import com.explodingpixels.painter.FocusStatePainter;
import com.explodingpixels.painter.RectanglePainter;
import com.explodingpixels.widgets.IconProvider;
import com.explodingpixels.widgets.TextProvider;
import com.explodingpixels.widgets.TreeUtils;
import com.explodingpixels.widgets.WindowUtils;
import com.jgoodies.forms.builder.PanelBuilder;
import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

/**
 * <p>
 * A UI delegate that paints a {@link JTree} as an <a href="http://developer.apple.com/documentation/UserExperience/Conceptual/AppleHIGuidelines/XHIGWindows/chapter_18_section_4.html#//apple_ref/doc/uid/20000961-CHDDIGDE">Apple defined</a>
 * Source List. Consider using this UI delegate with
 * {@link MacWidgetFactory#createSourceListScrollPane(javax.swing.JComponent)}.
 * </p>
 * <p>
 * For the best development experience, it is recommended that you migrate your code to use the
 * {@link SourceList} with the {@link SourceListModel}, as this component abstracts away many of the
 * complexities of {@code JTree}.
 * </p>
 * <p>
 * Pictured below are the two different rendering styles of a Source List: focused and unfocused.
 * The corresponding {@code JTree}'s focusable property drives this rendering style.
 * </p>
 * <br>
 * <table>
 * <tr><td align="center"><img src="../../../../../graphics/iTunesSourceList.png"></td>
 * <td align="center"><img src="../../../../../graphics/MailSourceList.png"></td></tr>
 * <tr><td align="center"><font size="2" face="arial"><b>Focusable SourceList<b></font></td>
 * <td align="center"><font size="2" face="arial"><b>Non-focusable SourceList<b></font></td></tr>
 * </table>
 * <br>
 * <h3>Providing Category and Item text and icons</h3>
 * <p/>
 * During the rendering process, each Category and Item node will be consulted for the text to be
 * displayed. The renderer determines the text based on these prioritized checks:
 * <ol>
 * <li>If the node is an instance of {@link DefaultMutableTreeNode}, and the
 * {@link DefaultMutableTreeNode#getUserObject()} is an instance of {@link TextProvider}, then
 * the {@code TextProvider} will be queried for the node text.</li>
 * <li>If no implementation of {@code TextProvider} is found, the standard
 * {@link JTree#convertValueToText(Object, boolean, boolean, boolean, int, boolean)} method will
 * be consulted.</li>
 * </ol>
 * Also, during rendering, each Item node will be consulted for an icon. Similarly to the above
 * mechanism for determining text, the render determines a nodes icon by the following check:
 * <ol>
 * <li>If the node is an instance of {@link DefaultMutableTreeNode}, and the
 * {@link DefaultMutableTreeNode#getUserObject()} is an instance of {@link IconProvider}, then
 * the {@code IconProvider} will be queried for the node icon.</li>
 * </ol>
 */
public class SourceListTreeUI extends BasicTreeUI {

    private Font categoryFont = UIManager.getFont("Label.font").deriveFont(Font.BOLD, 11.0f);
    private Font itemFont = UIManager.getFont("Label.font").deriveFont(11.0f);
    private Font itemSelectedFont = itemFont.deriveFont(Font.BOLD);

    private static final Color TRANSPARENT_COLOR = new Color(0, 0, 0, 0);

    private final String SELECT_NEXT = "selectNext";
    private final String SELECT_PREVIOUS = "selectPrevious";

    private SourceListColorScheme fColorScheme;
    private FocusStatePainter fBackgroundPainter;
    private FocusStatePainter fSelectionBackgroundPainter;

    private CustomTreeModelListener fTreeModelListener = new CustomTreeModelListener();

    @Override
    protected void completeUIInstall() {
        super.completeUIInstall();

        tree.setSelectionModel(new SourceListTreeSelectionModel());

        tree.setOpaque(false);
        tree.setRootVisible(false);
        tree.setLargeModel(true);
        tree.setRootVisible(false);
        tree.setShowsRootHandles(true);
        // TODO key height off font size.
        tree.setRowHeight(20);

        // install the default color scheme.
        setColorScheme(new SourceListStandardColorScheme());
    }

    public Font getCategoryFont() {
		return categoryFont;
	}
    
    public void setCategoryFont(Font categoryFont) {
		this.categoryFont = categoryFont;
	}
    
    public Font getItemFont() {
		return itemFont;
	}
    
    public void setItemFont(Font itemFont) {
		this.itemFont = itemFont;
	}
    
    public Font getItemSelectedFont() {
		return itemSelectedFont;
	}
    
    public void setItemSelectedFont(Font itemSelectedFont) {
		this.itemSelectedFont = itemSelectedFont;
	}
    
    @Override
    protected void installListeners() {
        super.installListeners();
        // install a property change listener that repaints the JTree when the parent window's
        // focus state changes.
        WindowUtils.installJComponentRepainterOnWindowFocusChanged(tree);
    }

    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();
        tree.getInputMap().put(KeyStroke.getKeyStroke("pressed DOWN"), SELECT_NEXT);
        tree.getInputMap().put(KeyStroke.getKeyStroke("pressed UP"), SELECT_PREVIOUS);
        tree.getActionMap().put(SELECT_NEXT, createNextAction());
        tree.getActionMap().put(SELECT_PREVIOUS, createPreviousAction());
    }

    @Override
    protected void setModel(TreeModel model) {
        // if there was a previously installed TreeModel, uninstall our listener from it.
        if (treeModel != null) {
            treeModel.removeTreeModelListener(fTreeModelListener);
        }

        super.setModel(model);

        // install our listener on the new TreeModel if neccessary.
        if (model != null) {
            model.addTreeModelListener(new CustomTreeModelListener());
        }
    }

    /**
     * Gets the {@link SourceListColorScheme} that this {@code SourceListTreeUI} uses to paint.
     *
     * @return the {@link SourceListColorScheme} that this {@code SourceList} uses to paint.
     */
    public SourceListColorScheme getColorScheme() {
        return fColorScheme;
    }

    /**
     * Sets the {@link SourceListColorScheme} that this {@code SourceListTreeUI} uses to paint.
     *
     * @param colorScheme the {@link SourceListColorScheme} that this {@code SourceList} uses to
     *                    paint.
     */
    public void setColorScheme(SourceListColorScheme colorScheme) {
        checkColorSchemeNotNull(colorScheme);
        fColorScheme = colorScheme;
        fBackgroundPainter = new FocusStatePainter(
                new RectanglePainter(fColorScheme.getActiveBackgroundColor()),
                new RectanglePainter(fColorScheme.getActiveBackgroundColor()),
                new RectanglePainter(fColorScheme.getInactiveBackgroundColor()));
        fSelectionBackgroundPainter = new FocusStatePainter(
                fColorScheme.getActiveFocusedSelectedItemPainter(),
                fColorScheme.getActiveUnfocusedSelectedItemPainter(),
                fColorScheme.getInactiveSelectedItemPainter());
        // create a new tree cell renderer in order to pick up the new colors.
        tree.setCellRenderer(new SourceListTreeCellRenderer());
        installDisclosureIcons();
    }

    private void installDisclosureIcons() {
        // install the collapsed and expanded icons as well as the margins to indent nodes.
        setCollapsedIcon(fColorScheme.getUnselectedCollapsedIcon());
        setExpandedIcon(fColorScheme.getUnselectedExpandedIcon());
        int indent = fColorScheme.getUnselectedCollapsedIcon().getIconWidth() / 2 + 4;
        setLeftChildIndent(indent);
        setRightChildIndent(indent);
    }

    @Override
    protected void paintExpandControl(Graphics g, Rectangle clipBounds, Insets insets,
                                      Rectangle bounds, TreePath path, int row, boolean isExpanded,
                                      boolean hasBeenExpanded, boolean isLeaf) {
        // if the given path is selected, then
        boolean isPathSelected = tree.getSelectionModel().isPathSelected(path);

        Icon expandIcon = isPathSelected ? fColorScheme.getSelectedExpandedIcon()
                : fColorScheme.getUnselectedExpandedIcon();
        Icon collapseIcon = isPathSelected ? fColorScheme.getSelectedCollapsedIcon()
                : fColorScheme.getUnselectedCollapsedIcon();

        Object categoryOrItem =
                ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
        boolean setIcon = !(categoryOrItem instanceof SourceListCategory)
                || ((SourceListCategory) categoryOrItem).isCollapsable();

        setExpandedIcon(setIcon ? expandIcon : null);
        setCollapsedIcon(setIcon ? collapseIcon : null);

        super.paintExpandControl(g, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
    }

    @Override
    protected AbstractLayoutCache.NodeDimensions createNodeDimensions() {
        return new NodeDimensionsHandler() {
            @Override
            public Rectangle getNodeDimensions(
                    Object value, int row, int depth, boolean expanded, Rectangle size) {

                Rectangle dimensions = super.getNodeDimensions(value, row, depth, expanded, size);
                int containerWidth = tree.getParent() instanceof JViewport
                        ? tree.getParent().getWidth() : tree.getWidth();

                dimensions.width = containerWidth - getRowX(row, depth);

                return dimensions;
            }
        };
    }

    @Override
    public Rectangle getPathBounds(JTree tree, TreePath path) {
        Rectangle bounds = super.getPathBounds(tree, path);
        // if there are valid bounds for the given path, then stretch them to fill the entire width
        // of the tree. this allows repaints on focus events to follow the standard code path, and
        // still repaint the entire selected area.
        if (bounds != null) {
            bounds.x = 0;
            bounds.width = tree.getWidth();
        }
        return bounds;
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        // TODO use c.getVisibleRect to trim painting to minimum rectangle.
        // paint the background for the tree.
        Graphics2D backgroundGraphics = (Graphics2D) g.create();
        fBackgroundPainter.paint(backgroundGraphics, c, c.getWidth(), c.getHeight());
        backgroundGraphics.dispose();

        // TODO use c.getVisibleRect to trim painting to minimum rectangle.
        // paint the background for the selected entry, if there is one.
        int selectedRow = getSelectionModel().getLeadSelectionRow();
        if (selectedRow >= 0 && tree.isVisible(tree.getPathForRow(selectedRow))) {

            Rectangle bounds = tree.getRowBounds(selectedRow);

            Graphics2D selectionBackgroundGraphics = (Graphics2D) g.create();
            selectionBackgroundGraphics.translate(0, bounds.y);
            fSelectionBackgroundPainter.paint(
                    selectionBackgroundGraphics, c, c.getWidth(), bounds.height);
            selectionBackgroundGraphics.dispose();
        }

        super.paint(g, c);
    }

    @Override
    protected void paintHorizontalLine(Graphics g, JComponent c, int y, int left, int right) {
        // do nothing - don't paint horizontal lines.
    }

    @Override
    protected void paintVerticalPartOfLeg(Graphics g, Rectangle clipBounds, Insets insets,
                                          TreePath path) {
        // do nothing - don't paint vertical lines.
    }

    @Override
    protected void selectPathForEvent(TreePath path, MouseEvent event) {
        // only forward on the selection event if an area other than the expand/collapse control
        // was clicked. this typically isn't an issue with regular Swing JTrees, however, SourceList
        // tree nodes fill the entire width of the tree. thus their bounds are underneath the 
        // expand/collapse control.
        if (!isLocationInExpandControl(path, event.getX(), event.getY())) {
            super.selectPathForEvent(path, event);
        }
    }

    private Action createNextAction() {
        return new AbstractAction() {
            public void actionPerformed(ActionEvent e) {
                int selectedRow = tree.getLeadSelectionRow();
                int rowToSelect = selectedRow + 1;
                while (rowToSelect >= 0 && rowToSelect < tree.getRowCount()) {
                    if (isItemRow(rowToSelect)) {
                        tree.setSelectionRow(rowToSelect);
                        break;
                    } else {
                        rowToSelect++;
                    }
                }
            }
        };
    }

    private Action createPreviousAction() {
        return new AbstractAction() {
            public void actionPerformed(ActionEvent e) {
                int selectedRow = tree.getLeadSelectionRow();
                int rowToSelect = selectedRow - 1;
                while (rowToSelect >= 0 && rowToSelect < tree.getRowCount()) {
                    if (isItemRow(rowToSelect)) {
                        tree.setSelectionRow(rowToSelect);
                        break;
                    } else {
                        rowToSelect--;
                    }
                }
            }
        };
    }

    // Utility methods. ///////////////////////////////////////////////////////////////////////////

    private boolean isCategoryRow(int row) {
        return !isItemRow(row);
    }

    private boolean isItemRow(int row) {
        return isItemPath(tree.getPathForRow(row));
    }

    private boolean isItemPath(TreePath path) {
        return path != null && path.getPathCount() > 2;
    }

    private String getTextForNode(TreeNode node, boolean selected, boolean expanded, boolean leaf,
                                  int row, boolean hasFocus) {
        String retVal;

        if (node instanceof DefaultMutableTreeNode
                && ((DefaultMutableTreeNode) node).getUserObject() instanceof TextProvider) {
            Object userObject = ((DefaultMutableTreeNode) node).getUserObject();
            retVal = ((TextProvider) userObject).getText();
        } else {
            retVal = tree.convertValueToText(node, selected, expanded, leaf, row, hasFocus);
        }

        return retVal;
    }

    private Icon getIconForNode(TreeNode node) {
        Icon retVal = null;
        if (node instanceof DefaultMutableTreeNode
                && ((DefaultMutableTreeNode) node).getUserObject() instanceof IconProvider) {
            Object userObject = ((DefaultMutableTreeNode) node).getUserObject();
            retVal = ((IconProvider) userObject).getIcon();
        }
        return retVal;
    }

    private static void checkColorSchemeNotNull(SourceListColorScheme colorScheme) {
        if (colorScheme == null) {
            throw new IllegalArgumentException("The given SourceListColorScheme cannot be null.");
        }
    }

    // Custom TreeModelListener. //////////////////////////////////////////////////////////////////

    private class CustomTreeModelListener implements TreeModelListener {

        public void treeNodesChanged(TreeModelEvent e) {
            // no implementation.
        }

        public void treeNodesInserted(TreeModelEvent e) {
            TreePath path = e.getTreePath();
            Object root = tree.getModel().getRoot();
            TreePath pathToRoot = new TreePath(root);
            if (path != null && path.getParentPath() != null
                    && path.getParentPath().getLastPathComponent().equals(root)
                    && !tree.isExpanded(pathToRoot)) {
                TreeUtils.expandPathOnEdt(tree, new TreePath(root));
            }
        }

        public void treeNodesRemoved(TreeModelEvent e) {
            // no implementation.
        }

        public void treeStructureChanged(TreeModelEvent e) {
            // no implementation.
        }
    }

    // Custom TreeCellRenderer. ///////////////////////////////////////////////////////////////////

    private class SourceListTreeCellRenderer implements TreeCellRenderer {

        private CategoryTreeCellRenderer iCategoryRenderer = new CategoryTreeCellRenderer();

        private ItemTreeCellRenderer iItemRenderer = new ItemTreeCellRenderer();

        public Component getTreeCellRendererComponent(
                JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row,
                boolean hasFocus) {

            TreeCellRenderer render = isCategoryRow(row) ? iCategoryRenderer : iItemRenderer;
            return render.getTreeCellRendererComponent(
                    tree, value, selected, expanded, leaf, row, hasFocus);
        }

    }

    private class CategoryTreeCellRenderer implements TreeCellRenderer {

        private JLabel fLabel = MacWidgetFactory.makeEmphasizedLabel(new JLabel(),
                fColorScheme.getCategoryTextColor(),
                fColorScheme.getCategoryTextColor(),
                fColorScheme.getCategoryTextShadowColor());

        private CategoryTreeCellRenderer() {
        }

        public Component getTreeCellRendererComponent(
                JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row,
                boolean hasFocus) {
            fLabel.setFont(getCategoryFont());
            TreeNode node = (TreeNode) value;
            fLabel.setText(getTextForNode(node, selected, expanded, leaf, row, hasFocus).toUpperCase());
            return fLabel;
        }
    }

    private class ItemTreeCellRenderer implements TreeCellRenderer {

        private PanelBuilder fBuilder;

        private SourceListCountBadgeRenderer fCountRenderer = new SourceListCountBadgeRenderer(
                fColorScheme.getSelectedBadgeColor(), fColorScheme.getActiveUnselectedBadgeColor(),
                fColorScheme.getInativeUnselectedBadgeColor(), fColorScheme.getBadgeTextColor());

        private JLabel fSelectedLabel = MacWidgetFactory.makeEmphasizedLabel(new JLabel(),
                fColorScheme.getSelectedItemTextColor(),
                fColorScheme.getSelectedItemTextColor(),
                fColorScheme.getSelectedItemFontShadowColor());

        private JLabel fUnselectedLabel = MacWidgetFactory.makeEmphasizedLabel(new JLabel(),
                fColorScheme.getUnselectedItemTextColor(),
                fColorScheme.getUnselectedItemTextColor(),
                TRANSPARENT_COLOR);

        private ItemTreeCellRenderer() {
            // definte the FormLayout columns and rows.
            FormLayout layout = new FormLayout("fill:0px:grow, 5px, p, 5px", "3px, fill:p:grow, 3px");
            // create the builders with our panels as the component to be filled.
            fBuilder = new PanelBuilder(layout);
            fBuilder.getPanel().setOpaque(false);
        }

        public Component getTreeCellRendererComponent(
                JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row,
                boolean hasFocus) {
            fSelectedLabel.setFont(getItemSelectedFont());
            fUnselectedLabel.setFont(getItemFont());

            TreeNode node = (TreeNode) value;
            JLabel label = selected ? fSelectedLabel : fUnselectedLabel;
            label.setText(getTextForNode(node, selected, expanded, leaf, row, hasFocus));
            label.setIcon(getIconForNode(node));

            fBuilder.getPanel().removeAll();
            CellConstraints cc = new CellConstraints();
            fBuilder.add(label, cc.xywh(1, 1, 1, 3));

            if (value instanceof DefaultMutableTreeNode
                    && ((DefaultMutableTreeNode) value).getUserObject() instanceof SourceListBadgeContentProvider) {
                Object userObject = ((DefaultMutableTreeNode) node).getUserObject();
                SourceListBadgeContentProvider badgeContentProvider =
                        (SourceListBadgeContentProvider) userObject;
                if (badgeContentProvider.getCounterValue() > 0) {
                    fBuilder.add(fCountRenderer.getComponent(), cc.xy(3, 2, "center, fill"));
                    fCountRenderer.setState(badgeContentProvider.getCounterValue(), selected);
                }
            }

            return fBuilder.getPanel();
        }
    }

    // SourceListTreeSelectionModel implementation. ///////////////////////////////////////////////

    private class SourceListTreeSelectionModel extends DefaultTreeSelectionModel {
        public SourceListTreeSelectionModel() {
            setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        }

        private boolean canSelect(TreePath path) {
            return isItemPath(path);
        }

        @Override
        public void setSelectionPath(TreePath path) {
            if (canSelect(path)) {
                super.setSelectionPath(path);
            }
        }

        @Override
        public void setSelectionPaths(TreePath[] paths) {
            if (canSelect(paths[0])) {
                super.setSelectionPaths(paths);
            }
        }
    }

}
