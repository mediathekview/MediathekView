package com.explodingpixels.widgets;

import java.awt.Rectangle;

import javax.swing.Icon;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.TreePath;

public class TreeUtils {

    // TODO add Javadoc.

    private TreeUtils() {
        // no constructor - utility class.
    }

    /**
     * Set's the collapsed icon to use for the given {@link JTree} if that
     * tree's UI delegate exetends from {@link BasicTreeUI}. If the given tree's
     * UI delegate does not extend from {@code BasicTreeUI} then the given
     * tree will not be changed.
     *
     * @param tree the tree to set the collapsed icon for.
     * @param icon the new collapsed icon to use.
     * @see BasicTreeUI#setCollapsedIcon(javax.swing.Icon)
     */
    public static void setCollapsedIcon(JTree tree, Icon icon) {
        if (tree.getUI() instanceof BasicTreeUI) {
            ((BasicTreeUI) tree.getUI()).setCollapsedIcon(icon);
        }
    }

    /**
     * Set's the expanded icon to use for the given {@link JTree} if that
     * tree's UI delegate extends from {@link BasicTreeUI}. If the given tree's
     * UI delegate does not extend from {@code BasicTreeUI} then the given
     * tree will not be changed.
     *
     * @param tree the tree to set the expanded icon for.
     * @param icon the new collapsed icon to use.
     * @see BasicTreeUI#setExpandedIcon(javax.swing.Icon)
     */
    public static void setExpandedIcon(JTree tree, Icon icon) {
        ((BasicTreeUI) tree.getUI()).setExpandedIcon(icon);
    }

    /**
     * Set's the left indent in pixels to use for the given {@link JTree}'s
     * collapsed and expanded icon. This value in conjuction with the right
     * indent comprises the total amount of space that the collapsed and
     * expanded icon draw into. If the given tree's UI delegate does not extend
     * from {@code BasicTreeUI} then the given tree will not be changed.
     *
     * @param tree   the tree to set the left indent for.
     * @param indent the new left indent in pixels.
     * @see BasicTreeUI#setLeftChildIndent(int)
     */
    public static void setLeftChildIndent(JTree tree, int indent) {
        ((BasicTreeUI) tree.getUI()).setLeftChildIndent(indent);
    }

    /**
     * Set's the right indent in pixels to use for the given {@link JTree}'s
     * collapsed and expanded icon. This value in conjuction with the left
     * indent comprises the total amount of space that the collapsed and
     * expanded icon draw into. If the given tree's UI delegate does not extend
     * from {@code BasicTreeUI} then the given tree will not be changed.
     *
     * @param tree   the tree to set the right indent for.
     * @param indent the new left indent in pixels.
     * @see BasicTreeUI#setRightChildIndent(int)
     */
    public static void setRightChildIndent(JTree tree, int indent) {
        ((BasicTreeUI) tree.getUI()).setRightChildIndent(indent);
    }

    /**
     * Repaints the selection. This allows the row selection to have a
     * background color that changes based on the focus state of the
     * component.
     *
     * @param tree the {@code JTree} to repaint the selection of.
     */
    public static void repaintSelection(JTree tree) {
        int[] selectedRows = tree.getSelectionRows();

        // if there is a selected row, then repaint it.
        if (selectedRows != null && selectedRows.length > 0) {
            // grab the bounds of the first and last selected cells in column
            // one (index zero).
            Rectangle firstSelectedCell = tree.getRowBounds(selectedRows[0]);
            Rectangle lastSelectedCell =
                    tree.getRowBounds(selectedRows[selectedRows.length - 1]);

            // create the rectangle to repaint by unioning the first and last
            // selected cells in column one and then extending that rectangle
            // to extend from the left edge of the table all the way to the
            // right edge.
            Rectangle repaintRectangle = firstSelectedCell.union(lastSelectedCell);
            repaintRectangle.x = 0;
            repaintRectangle.width = tree.getWidth();

            // repaint the selection area.
            tree.repaint(repaintRectangle);
        }

    }

    public static void setExpandedOnEdt(JTree tree, TreePath path, boolean expanded) {
        if (expanded) {
            expandPathOnEdt(tree, path);
        } else {
            collapsePathOnEdt(tree, path);
        }
    }

    public static void expandPathOnEdt(final JTree tree, final TreePath path) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                tree.expandPath(path);
            }
        });
    }

    public static void collapsePathOnEdt(final JTree tree, final TreePath path) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                tree.collapsePath(path);
            }
        });
    }

    public static void installRootExpandingTreeModelListener(final JTree tree) {

        TreeModelListener listener = new TreeModelListener() {

            public void treeNodesChanged(TreeModelEvent e) {
            }

            public void treeNodesInserted(TreeModelEvent e) {
                // if this is the root node, and it hasn't yet been expanded
                // (because no children have been added, then expand the root now).
                if (e.getTreePath().getParentPath() == null
                        && tree.isCollapsed(e.getTreePath())) {
                    TreeUtils.expandPathOnEdt(tree, e.getTreePath());
                }
            }

            public void treeNodesRemoved(TreeModelEvent e) {
            }

            public void treeStructureChanged(TreeModelEvent e) {
            }
        };

        tree.getModel().addTreeModelListener(listener);
    }

}
