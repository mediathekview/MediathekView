package com.explodingpixels.macwidgets;

import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.ToolTipManager;
import javax.swing.TransferHandler;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import com.explodingpixels.macwidgets.plaf.SourceListTreeUI;
import com.explodingpixels.widgets.TreeUtils;

/**
 * An implementation of an OS X Source List. For a full descrption of what a Source List is, see the
 * <a href="http://developer.apple.com/documentation/UserExperience/Conceptual/AppleHIGuidelines/XHIGWindows/chapter_18_section_4.html#//apple_ref/doc/uid/20000961-CHDDIGDE">Source Lists</a>
 * section of Apple's Human Interface Guidelines.
 * <p/>
 * This component provides the two basic sytles of Source List: focusble and non-focusable.
 * As the name implies, focusable Source Lists and recieve keyboard focus, and thus can be navigated
 * using the arrow keys. Non-focusable, cannot receive keyboard focus, and thus cannot be
 * navigated via the arrow keys. The two styles of {@code SourceList} are pictured below:
 * <br>
 * <table>
 * <tr><td align="center"><img src="../../../../graphics/iTunesSourceList.png"></td>
 * <td align="center"><img src="../../../../graphics/MailSourceList.png"></td></tr>
 * <tr><td align="center"><font size="2" face="arial"><b>Focusable SourceList<b></font></td>
 * <td align="center"><font size="2" face="arial"><b>Non-focusable SourceList<b></font></td></tr>
 * </table>
 * <br>
 * Here's how to create a simple {@code SourceList} with one item:
 * <pre>
 * SourceListModel model = new SourceListModel();
 * SourceListCategory category = new SourceListCategory("Category");
 * model.addCategory(category);
 * model.addItemToCategory(new SourceListItem("Item"), category);
 * SourceList sourceList = new SourceList(model);
 * </pre>
 * <p>
 * To install a selection listener on the {@code SourceList}, add a
 * {@link SourceListSelectionListener}.
 * </p>
 * <p/>
 * To install a context-menu provider, call
 * {@link #setSourceListContextMenuProvider(SourceListContextMenuProvider)} with an implementation
 * of {@link SourceListContextMenuProvider}.
 */
public class SourceList {

    private final SourceListModel fModel;

    private final SourceListModelListener fModelListener = createSourceListModelListener();

    private final List<SourceListSelectionListener> fSourceListSelectionListeners =
            new ArrayList<SourceListSelectionListener>();

    private final List<SourceListExpansionListener> fSourceListExpansionListeners =
            new ArrayList<SourceListExpansionListener>();

    private DefaultMutableTreeNode fRoot = new DefaultMutableTreeNode("root");
    private DefaultTreeModel fTreeModel = new DefaultTreeModel(fRoot);
    private JTree fTree = new CustomJTree(fTreeModel);

    private JScrollPane fScrollPane = MacWidgetFactory.createSourceListScrollPane(fTree);
    private final JPanel fComponent = new JPanel(new BorderLayout());
    private TreeSelectionListener fTreeSelectionListener = createTreeSelectionListener();
    private TreeExpansionListener fTreeExpansionListener = createTreeExpansionListener();
    private TreeWillExpandListener fTreeWillExpandListener = createTreeWillExpandListener();
    private MouseListener fMouseListener = createMouseListener();

    private SourceListControlBar fSourceListControlBar;

    private SourceListContextMenuProvider fContextMenuProvider =
            new EmptySourceListContextMenuProvider();
    private List<SourceListClickListener> fSourceListClickListeners =
            new ArrayList<SourceListClickListener>();
    private SourceListToolTipProvider fToolTipProvider = new EmptyToolTipProvider();

    /**
     * Creates a {@code SourceList} with an empty {@link SourceListModel}.
     */
    public SourceList() {
        this(new SourceListModel());
    }

    /**
     * Creates a {@code SourceList} with the given {@link SourceListModel}.
     *
     * @param model the {@code SourceListModel} to use.
     */
    public SourceList(SourceListModel model) {
        if (model == null) {
            throw new IllegalArgumentException("Groups cannot be null.");
        }

        fModel = model;
        fModel.addSourceListModelListener(fModelListener);

        initUi();

        // add each category and its sub-items to backing JTree.
        for (int i = 0; i < model.getCategories().size(); i++) {
            doAddCategory(model.getCategories().get(i), i);
        }
    }

    private void initUi() {
        fComponent.add(fScrollPane, BorderLayout.CENTER);
        fTree.addTreeSelectionListener(fTreeSelectionListener);
        fTree.addTreeExpansionListener(fTreeExpansionListener);
        fTree.addTreeWillExpandListener(fTreeWillExpandListener);
        fTree.addMouseListener(fMouseListener);
    }


    /**
     * Installs the given {@link SourceListControlBar} at the base of this {@code SourceList}. This
     * method can be called only once, and should generally be called during creation of the
     * {@code SourceList}.
     *
     * @param sourceListControlBar the {@link SourceListControlBar} to add.
     * @throws IllegalStateException    if a {@code SourceListControlBar} has already been installed
     *                                  on this {@code SourceList}.
     * @throws IllegalArgumentException if the {@code SourceListControlBar} is null.
     */
    public void installSourceListControlBar(SourceListControlBar sourceListControlBar) {
        if (fSourceListControlBar != null) {
            throw new IllegalStateException("A SourceListControlBar has already been installed on" +
                    " this SourceList.");
        }
        if (sourceListControlBar == null) {
            throw new IllegalArgumentException("SourceListControlBar cannot be null.");
        }
        fSourceListControlBar = sourceListControlBar;
        fComponent.add(fSourceListControlBar.getComponent(), BorderLayout.SOUTH);
    }

    /**
     * True if there is a {@link SourceListControlBar} installed on this {@code SourceList}.
     *
     * @return true if there is a {@link SourceListControlBar} installed on this {@code SourceList}.
     */
    public boolean isSourceListControlBarInstalled() {
        return fSourceListControlBar != null;
    }

    /**
     * Sets the {@link SourceListContextMenuProvider} to use for this {@code SourceList}.
     *
     * @param contextMenuProvider the {@link SourceListContextMenuProvider} to use for this
     *                            {@code SourceList}.
     * @throws IllegalArgumentException if the {@code SourceListContextMenuProvider} is null.
     */
    public void setSourceListContextMenuProvider(SourceListContextMenuProvider contextMenuProvider) {
        if (contextMenuProvider == null) {
            throw new IllegalArgumentException("SourceListContextMenuProvider cannot be null.");
        }
        fContextMenuProvider = contextMenuProvider;
    }

    /**
     * Uninstalls any listeners that this {@code SourceList} installed on creation, thereby allowing
     * it to be garbage collected.
     */
    public void dispose() {
        fModel.removeSourceListModelListener(fModelListener);
    }

    /**
     * Gets the selected {@link SourceListItem}.
     *
     * @return the selected {@code SourceListItem}.
     */
    public SourceListItem getSelectedItem() {
        SourceListItem selectedItem = null;
        if (fTree.getSelectionPath() != null
                && fTree.getSelectionPath().getLastPathComponent() != null) {
            DefaultMutableTreeNode selectedNode =
                    (DefaultMutableTreeNode) fTree.getSelectionPath().getLastPathComponent();
            assert selectedNode.getUserObject() instanceof SourceListItem
                    : "Only SourceListItems can be selected.";
            selectedItem = (SourceListItem) selectedNode.getUserObject();
        }
        return selectedItem;
    }

    /**
     * Selects the given {@link SourceListItem} in the list.
     *
     * @param item the item to select.
     * @throws IllegalArgumentException if the given item is not in the list.
     */
    public void setSelectedItem(SourceListItem item) {
        getModel().validateItemIsInModel(item);
        DefaultMutableTreeNode treeNode = getNodeForObject(fRoot, item);
        fTree.setSelectionPath(new TreePath(treeNode.getPath()));
    }

    /**
     * Clears the current selection, if there is one.
     */
    public void clearSelection() {
        fTree.clearSelection();
    }

    /**
     * Sets whether this {@code SourceList} can have focus. When focusable and this
     * {@code SourceList} has focus, the keyboard can be used for navigation.
     *
     * @param focusable true if this {@code SourceList} should be focusable.
     */
    public void setFocusable(boolean focusable) {
        fTree.setFocusable(focusable);
    }

    /**
     * Installs iApp style scroll bars on this {@code SourceList}.
     *
     * @see IAppWidgetFactory#makeIAppScrollPane
     */
    public void useIAppStyleScrollBars() {
        IAppWidgetFactory.makeIAppScrollPane(fScrollPane);
    }

    /**
     * Gets the {@link SourceListColorScheme} that this {@code SourceList} uses.
     *
     * @return the {@link SourceListColorScheme} that this {@code SourceList} uses.
     */
    public SourceListColorScheme getColorScheme() {
        return ((SourceListTreeUI) fTree.getUI()).getColorScheme();
    }

    /**
     * Sets the {@link SourceListColorScheme} that this {@code SourceList} uses.
     *
     * @param colorScheme the {@link SourceListColorScheme} that this {@code SourceList} uses.
     */
    public void setColorScheme(SourceListColorScheme colorScheme) {
        ((SourceListTreeUI) fTree.getUI()).setColorScheme(colorScheme);
    }

    /**
     * Set's the {@link TransferHandler} that this {@code SourceList} should use
     * during drag and drop operations. If the given handler not null, then
     * dragging will be turned on for the {@code SourceList}. If the handler is
     * null, then dragging will be turned off.
     *
     * @param transferHandler the {@code TransferHandler} for this
     *                        {@code SourceList}. Can be null.
     */
    public void setTransferHandler(TransferHandler transferHandler) {
        fTree.setDragEnabled(transferHandler != null);
        fTree.setTransferHandler(transferHandler);
    }

    /**
     * Scrolls the given {@link SourceListItem} to be visible.
     *
     * @param item the {@code SourceListItem} to scroll to visible.
     */
    public void scrollItemToVisible(SourceListItem item) {
        getModel().validateItemIsInModel(item);
        DefaultMutableTreeNode treeNode = getNodeForObject(fRoot, item);
        fTree.scrollPathToVisible(new TreePath(treeNode.getPath()));
    }

    /**
     * Sets the expanded state of the given {@link SourceListCategory}.
     *
     * @param category the category to set the expanded state on.
     * @param expanded true if the given category should be expanded, false if it should be
     *                 collapsed.
     * @throws IllegalArgumentException if the given {@code SourceListCategory} is not part of the
     *                                  associated {@link SourceListModel}.
     */
    public void setExpanded(SourceListCategory category, boolean expanded) {
        DefaultMutableTreeNode categoryNode = getNodeForObject(category);
        checkCategoryNodeNotNull(categoryNode);
        TreeUtils.setExpandedOnEdt(fTree, new TreePath(categoryNode.getPath()), expanded);
    }

    /**
     * Sets the expanded state of the given {@link SourceListItem}.
     *
     * @param item     the item to set the expanded state on.
     * @param expanded true if the given item should be expanded, false if it should be
     *                 collapsed.
     * @throws IllegalArgumentException if the given {@code SourceListItem} is not part of the
     *                                  associated {@link SourceListModel}.
     */
    public void setExpanded(SourceListItem item, boolean expanded) {
        DefaultMutableTreeNode itemNode = getNodeForObject(item);
        checkItemNodeNotNull(itemNode);
        TreeUtils.setExpandedOnEdt(fTree, new TreePath(itemNode.getPath()), expanded);
    }

    private DefaultMutableTreeNode getNodeForObject(Object userObject) {
        return getNodeForObject(fRoot, userObject);
    }

    private static DefaultMutableTreeNode getNodeForObject(DefaultMutableTreeNode parentNode,
                                                           Object userObject) {
        if (parentNode.getUserObject().equals(userObject)) {
            return parentNode;
        } else if (parentNode.children().hasMoreElements()) {
            for (int i = 0; i < parentNode.getChildCount(); i++) {
                DefaultMutableTreeNode childNode =
                        (DefaultMutableTreeNode) parentNode.getChildAt(i);
                DefaultMutableTreeNode retVal =
                        getNodeForObject(childNode, userObject);
                if (retVal != null) {
                    return retVal;
                }
            }
        } else {
            return null;
        }

        return null;
    }

    /**
     * Gets the user interface component representing this {@code SourceList}. The returned
     * {@link JComponent} should be added to a container that will be displayed.
     *
     * @return the user interface component representing this {@code SourceList}.
     */
    public JComponent getComponent() {
        return fComponent;
    }

    /**
     * Gets the {@link SourceListModel} backing this {@code SourceList}.
     *
     * @return the {@code SourceListModel} backing this {@code SourceList}.
     */
    public SourceListModel getModel() {
        return fModel;
    }

    /**
     * Sets the {@link SourceListToolTipProvider} to use.
     *
     * @param toolTipProvider the {@code SourceListToolTipProvider to use.
     */
    public void setToolTipProvider(SourceListToolTipProvider toolTipProvider) {
        if (toolTipProvider == null) {
            throw new IllegalArgumentException("SourceListToolTipProvider cannot be null.");
        }
        fToolTipProvider = toolTipProvider;
    }

    private void doAddCategory(SourceListCategory category, int index) {
        DefaultMutableTreeNode categoryNode = new DefaultMutableTreeNode(category);
        fTreeModel.insertNodeInto(categoryNode, fRoot, index);
        // add each of the categories child items to the tree.
        for (int i = 0; i < category.getItems().size(); i++) {
            doAddItemToCategory(category.getItems().get(i), category, i);
        }

        TreeUtils.expandPathOnEdt(fTree, new TreePath(categoryNode.getPath()));
    }

    private void doRemoveCategory(SourceListCategory category) {
        DefaultMutableTreeNode categoryNode = getNodeForObject(fRoot, category);
        checkCategoryNodeNotNull(categoryNode);
        fTreeModel.removeNodeFromParent(categoryNode);
    }

    private void doAddItemToCategory(SourceListItem itemToAdd, SourceListCategory category, int index) {
        DefaultMutableTreeNode categoryNode = getNodeForObject(fRoot, category);
        checkCategoryNodeNotNull(categoryNode);
        doAddItemToNode(itemToAdd, categoryNode, index);
    }

    private void doRemoveItemFromCategory(SourceListItem itemToRemove, SourceListCategory category) {
        DefaultMutableTreeNode categoryNode = getNodeForObject(fRoot, category);
        checkCategoryNodeNotNull(categoryNode);
        DefaultMutableTreeNode itemNode = getNodeForObject(categoryNode, itemToRemove);
        checkCategoryNodeNotNull(itemNode);
        fTreeModel.removeNodeFromParent(itemNode);
    }

    private void doAddItemToItem(SourceListItem itemToAdd, SourceListItem parentItem, int index) {
        DefaultMutableTreeNode parentItemNode = getNodeForObject(fRoot, parentItem);
        checkCategoryNodeNotNull(parentItemNode);
        doAddItemToNode(itemToAdd, parentItemNode, index);
    }

    private void doRemoveItemFromItem(SourceListItem itemToRemove, SourceListItem parentItem) {
        DefaultMutableTreeNode parentNode = getNodeForObject(fRoot, parentItem);
        checkCategoryNodeNotNull(parentNode);
        DefaultMutableTreeNode itemNode = getNodeForObject(parentNode, itemToRemove);
        checkCategoryNodeNotNull(itemNode);
        fTreeModel.removeNodeFromParent(itemNode);
    }

    private void doAddItemToNode(SourceListItem itemToAdd, DefaultMutableTreeNode parentNode, int index) {
        DefaultMutableTreeNode itemNode = new DefaultMutableTreeNode(itemToAdd);
        fTreeModel.insertNodeInto(itemNode, parentNode, index);
        // add each of the newly added item's children nodes.
        for (int i = 0; i < itemToAdd.getChildItems().size(); i++) {
            doAddItemToItem(itemToAdd.getChildItems().get(i), itemToAdd, i);
        }
        // if the parent node is a new node, expand it. thus the default behavior is to expand a
        // parent node.
        if (parentNode.getChildCount() == 1) {
            TreeUtils.expandPathOnEdt(fTree, new TreePath(parentNode.getPath()));
        }
    }

    private void doItemChanged(SourceListItem item) {
        DefaultMutableTreeNode itemNode = getNodeForObject(fRoot, item);
        checkItemNodeNotNull(itemNode);
        fTreeModel.nodeChanged(itemNode);
    }

    private void doShowContextMenu(MouseEvent event) {
        // grab the item or category under the mouse events point if there is
        // there is an item or category under this point.
        Object itemOrCategory = getItemOrCategoryUnderPoint(event.getPoint());

        // if there was no item under the click, then call the generic contribution method.
        // else if there was a SourceListItem under the click, call the corresponding contribution
        //         method.
        // else if there was a SourceListCategory under the click, call the corresponding contribution
        //         method.
        JPopupMenu popup = null;
        if (itemOrCategory == null) {
            popup = fContextMenuProvider.createContextMenu();
        } else if (itemOrCategory instanceof SourceListItem) {
            popup = fContextMenuProvider.createContextMenu((SourceListItem) itemOrCategory);
        } else if (itemOrCategory instanceof SourceListCategory) {
            popup = fContextMenuProvider.createContextMenu((SourceListCategory) itemOrCategory);
        }

        // only show the context-menu if menu items have been added to it.
        if (popup != null && popup.getComponentCount() > 0) {
            popup.show(fTree, event.getX(), event.getY());
        }
    }

    private void doSourceListClicked(MouseEvent event) {
        // grab the item or category under the mouse events point if there is
        // there is an item or category under this point.
        Object itemOrCategory = getItemOrCategoryUnderPoint(event.getPoint());

        SourceListClickListener.Button button =
                SourceListClickListener.Button.getButton(event.getButton());
        int clickCount = event.getClickCount();

        if (itemOrCategory == null) {
            // do nothing.
        } else if (itemOrCategory instanceof SourceListItem) {
            fireSourceListItemClicked((SourceListItem) itemOrCategory, button, clickCount);
        } else if (itemOrCategory instanceof SourceListCategory) {
            fireSourceListCategoryClicked((SourceListCategory) itemOrCategory, button, clickCount);
        }
    }

    private Object getItemOrCategoryUnderPoint(Point point) {
        // grab the path under the given point.
        TreePath path = fTree.getPathForLocation(point.x, point.y);
        // if there is a tree item under that point, cast it to a DefaultMutableTreeNode and grab
        // the user object which will either be a SourceListItem or SourceListCategory.
        return path == null
                ? null : ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
    }

    private TreeSelectionListener createTreeSelectionListener() {
        return new TreeSelectionListener() {
            public void valueChanged(TreeSelectionEvent e) {
                fireSourceListItemSelected(getSelectedItem());
            }
        };
    }

    private TreeExpansionListener createTreeExpansionListener() {
        return new TreeExpansionListener() {
            public void treeExpanded(TreeExpansionEvent event) {
                onExpandedOrCollapsed(event, true);
            }

            public void treeCollapsed(TreeExpansionEvent event) {
                onExpandedOrCollapsed(event, false);
            }

            private void onExpandedOrCollapsed(TreeExpansionEvent event, boolean expanded) {
                Object itemOrCategory = getItemOrCategoryFromTreeExpansionEvent(event);

                if (itemOrCategory != null) {
                    if (itemOrCategory instanceof SourceListCategory) {
                        SourceListCategory category = (SourceListCategory) itemOrCategory;
                        if (expanded) {
                            fireSourceListCategoryExpanded(category);
                        } else {
                            fireSourceListCategoryCollapsed(category);
                        }
                    } else if (itemOrCategory instanceof SourceListItem) {
                        SourceListItem sourceListItem = (SourceListItem) itemOrCategory;
                        if (expanded) {
                            fireSourceListItemExpanded(sourceListItem);
                        } else {
                            fireSourceListItemCollapsed(sourceListItem);
                        }
                    }
                }
            }
        };
    }

    private TreeWillExpandListener createTreeWillExpandListener() {
        return new TreeWillExpandListener() {
            public void treeWillExpand(TreeExpansionEvent event) throws ExpandVetoException {
                onWillExpandOrCollapse(event, true);
            }

            public void treeWillCollapse(TreeExpansionEvent event) throws ExpandVetoException {
                onWillExpandOrCollapse(event, false);
            }

            private void onWillExpandOrCollapse(TreeExpansionEvent event, boolean expanded) throws ExpandVetoException {
                Object itemOrCategory = getItemOrCategoryFromTreeExpansionEvent(event);

                if (itemOrCategory != null) {
                    if (itemOrCategory instanceof SourceListCategory) {
                        SourceListCategory category = (SourceListCategory) itemOrCategory;
                        if (expanded) {
                            fireSourceListCategoryWillExpand(event, category);
                        } else {
                            fireSourceListCategoryWillCollapse(event, category);
                        }
                    } else if (itemOrCategory instanceof SourceListItem) {
                        SourceListItem sourceListItem = (SourceListItem) itemOrCategory;
                        if (expanded) {
                            fireSourceListItemWillExpand(event, sourceListItem);
                        } else {
                            fireSourceListItemWillCollapse(event, sourceListItem);
                        }
                    }
                }
            }
        };
    }

    private SourceListModelListener createSourceListModelListener() {
        return new SourceListModelListener() {
            public void categoryAdded(SourceListCategory category, int index) {
                doAddCategory(category, index);
            }

            public void categoryRemoved(SourceListCategory category) {
                doRemoveCategory(category);
            }

            public void itemAddedToCategory(SourceListItem item, SourceListCategory category, int index) {
                doAddItemToCategory(item, category, index);
            }

            public void itemRemovedFromCategory(SourceListItem item, SourceListCategory category) {
                doRemoveItemFromCategory(item, category);
            }

            public void itemAddedToItem(SourceListItem item, SourceListItem parentItem, int index) {
                doAddItemToItem(item, parentItem, index);
            }

            public void itemRemovedFromItem(SourceListItem item, SourceListItem parentItem) {
                doRemoveItemFromItem(item, parentItem);
            }

            public void itemChanged(SourceListItem item) {
                doItemChanged(item);
            }
        };
    }

    private MouseListener createMouseListener() {
        return new MouseAdapter() {
            // TODO there is an interesting point of contention here: should
            // TODO the context menu always be shown as if it were on a Mac (on
            // TODO mouse press) or based on the platform on which it is running.
            // TODO always doing the same thing would actually be harder,
            // TODO because we wouldn't be able to rely on the isPopupTrigger
            // TODO method and there is no way to determine when the 
            // TODO popup-menu-trigger button is.
            @Override
            public void mousePressed(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    doShowContextMenu(e);
                }
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    doShowContextMenu(e);
                }
            }

            @Override
            public void mouseClicked(MouseEvent e) {
                doSourceListClicked(e);
            }
        };
    }

    private Object getItemOrCategoryFromTreeExpansionEvent(TreeExpansionEvent event) {
        // Get the object that was expanded/collapsed from the expasion event
        Object lastPathComponent = event.getPath().getLastPathComponent();

        // Cast it to a DefaultMutableTreeNode and grab
        // the user object which will either be a SourceListItem or SourceListCategory.
        return ((DefaultMutableTreeNode) lastPathComponent).getUserObject();
    }

    // SourceListClickListener support. ///////////////////////////////////////    

    private void fireSourceListItemClicked(
            SourceListItem item, SourceListClickListener.Button button,
            int clickCount) {
        for (SourceListClickListener listener : fSourceListClickListeners) {
            listener.sourceListItemClicked(item, button, clickCount);
        }
    }

    private void fireSourceListCategoryClicked(
            SourceListCategory category, SourceListClickListener.Button button,
            int clickCount) {
        for (SourceListClickListener listener : fSourceListClickListeners) {
            listener.sourceListCategoryClicked(category, button, clickCount);
        }
    }

    /**
     * Adds the {@link SourceListClickListener} to the list of listeners.
     *
     * @param listener the {@code SourceListClickListener} to add.
     */
    public void addSourceListClickListener(SourceListClickListener listener) {
        fSourceListClickListeners.add(listener);
    }

    /**
     * Removes the {@link SourceListClickListener} to the list of listeners.
     *
     * @param listener the {@code SourceListClickListener} to remove.
     */
    public void removeSourceListClickListener(SourceListClickListener listener) {
        fSourceListClickListeners.remove(listener);
    }

    // SourceListSelectionListener support. ///////////////////////////////////

    private void fireSourceListItemSelected(SourceListItem item) {
        for (SourceListSelectionListener listener : fSourceListSelectionListeners) {
            listener.sourceListItemSelected(item);
        }
    }

    /**
     * Adds the {@link SourceListSelectionListener} to the list of listeners.
     *
     * @param listener the {@code SourceListSelectionListener} to add.
     */
    public void addSourceListSelectionListener(SourceListSelectionListener listener) {
        fSourceListSelectionListeners.add(listener);
    }

    /**
     * Removes the {@link SourceListSelectionListener} from the list of listeners.
     *
     * @param listener the {@code SourceListSelectionListener} to remove.
     */
    public void removeSourceListSelectionListener(SourceListSelectionListener listener) {
        fSourceListSelectionListeners.remove(listener);
    }

    // SourceListExpansionListener support. ///////////////////////////////////

    private void fireSourceListItemExpanded(SourceListItem item) {
        for (SourceListExpansionListener listener : fSourceListExpansionListeners) {
            listener.sourceListItemExpanded(item);
        }
    }

    private void fireSourceListItemCollapsed(SourceListItem item) {
        for (SourceListExpansionListener listener : fSourceListExpansionListeners) {
            listener.sourceListItemCollapsed(item);
        }
    }

    private void fireSourceListItemWillExpand(TreeExpansionEvent event, SourceListItem item) throws ExpandVetoException {
        for (SourceListExpansionListener listener : fSourceListExpansionListeners) {
            handleShouldExpandOrCollapseResponse(listener.shouldExpandSourceListItem(item), event);
        }
    }

    private void fireSourceListItemWillCollapse(TreeExpansionEvent event, SourceListItem item) throws ExpandVetoException {
        for (SourceListExpansionListener listener : fSourceListExpansionListeners) {
            handleShouldExpandOrCollapseResponse(listener.shouldCollapseSourceListItem(item), event);
        }
    }

    private void fireSourceListCategoryExpanded(SourceListCategory category) {
        for (SourceListExpansionListener listener : fSourceListExpansionListeners) {
            listener.sourceListCategoryExpanded(category);
        }
    }

    private void fireSourceListCategoryCollapsed(SourceListCategory category) {
        for (SourceListExpansionListener listener : fSourceListExpansionListeners) {
            listener.sourceListCategoryCollapsed(category);
        }
    }

    private void fireSourceListCategoryWillExpand(TreeExpansionEvent event, SourceListCategory category) throws ExpandVetoException {
        for (SourceListExpansionListener listener : fSourceListExpansionListeners) {
            handleShouldExpandOrCollapseResponse(listener.shouldExpandSourceListCategory(category), event);
        }
    }

    private void fireSourceListCategoryWillCollapse(TreeExpansionEvent event, SourceListCategory category) throws ExpandVetoException {
        for (SourceListExpansionListener listener : fSourceListExpansionListeners) {
            handleShouldExpandOrCollapseResponse(listener.shouldToCollapseSourceListCategory(category), event);
        }
    }

    private void handleShouldExpandOrCollapseResponse(boolean shouldExpandOrCollapse, TreeExpansionEvent event) throws ExpandVetoException {
        if (!shouldExpandOrCollapse) {
            throw new ExpandVetoException(event);
        }
    }

    /**
     * Adds the {@link SourceListExpansionListener} to the list of listeners.
     *
     * @param listener the {@code SourceListExpansionListener} to add.
     */
    public void addSourceListExpansionListener(SourceListExpansionListener listener) {
        fSourceListExpansionListeners.add(listener);
    }

    /**
     * Removes the {@link SourceListExpansionListener} from the list of listeners.
     *
     * @param listener the {@code SourceListExpansionListener} to remove.
     */
    public void removeSourceListExpansionListener(SourceListExpansionListener listener) {
        fSourceListExpansionListeners.remove(listener);
    }

    // Utility methods. ///////////////////////////////////////////////////////////////////////////

    private static void checkCategoryNodeNotNull(MutableTreeNode node) {
        if (node == null) {
            throw new IllegalArgumentException("The given SourceListCategory " +
                    "does not exist in this SourceList.");
        }
    }

    private static void checkItemNodeNotNull(MutableTreeNode node) {
        if (node == null) {
            throw new IllegalArgumentException("The given SourceListItem " +
                    "does not exist in this SourceList.");
        }
    }

    // EmptySourceListContextMenuProvider implementation. /////////////////////////////////////////

    private static class EmptySourceListContextMenuProvider implements SourceListContextMenuProvider {
        public JPopupMenu createContextMenu() {
            return null;
        }

        public JPopupMenu createContextMenu(SourceListItem item) {
            return null;
        }

        public JPopupMenu createContextMenu(SourceListCategory category) {
            return null;
        }
    }

    // Custom JTree implementation that always returns SourceListTreeUI delegate. /////////////////

    private class CustomJTree extends JTree {
        public CustomJTree(TreeModel newModel) {
            super(newModel);
            ToolTipManager.sharedInstance().registerComponent(this);
        }

        @Override
        public void updateUI() {
            setUI(new SourceListTreeUI());
            invalidate();
        }

        @Override
        public void collapsePath(TreePath path) {
            DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
            Object categoryOrItem = node.getUserObject();

            if (categoryOrItem instanceof SourceListCategory
                    && !((SourceListCategory) categoryOrItem).isCollapsable()) {
                // ignore the request to collapse, as this is a non-collapsable category.
            } else {
                super.collapsePath(path);
            }
        }

        @Override
        public String getToolTipText(MouseEvent event) {
            TreePath path = getPathForLocation(event.getX(), event.getY());
            Object userObject = path == null
                    ? null : ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
            String toolTipText = null;
            if (userObject instanceof SourceListCategory) {
                toolTipText = fToolTipProvider.getTooltip((SourceListCategory) userObject);
            } else if (userObject instanceof SourceListItem) {
                toolTipText = fToolTipProvider.getTooltip((SourceListItem) userObject);
            }
            return toolTipText;
        }
    }

    // Empty SourceListTooltipProvider. ///////////////////////////////////////////////////////////

    private static class EmptyToolTipProvider implements SourceListToolTipProvider {
        public String getTooltip(SourceListCategory category) {
            return null;
        }

        public String getTooltip(SourceListItem item) {
            return null;
        }
    }
}
