package com.explodingpixels.macwidgets;

import javax.swing.JPopupMenu;

/**
 * <p>
 * An interface to hook into the context-menu showing process. When installed on a
 * {@link SourceList}, this interface will be notified just prior to a context menu being shown.
 * </p>
 * <p>
 * Here's a sample implementation and installation of this interface:
 * </p>
 * <pre>
 * SourceListContextMenuProvider menuProvider = new SourceListContextMenuProvider() {
 *           public JPopupMenu createContextMenu() {
 *               // create and install your custom menu items for context-menu's on the SourceList.
 *               JPopupMenu menu = new JPopupMenu();
 *               popupMenu.add(new JMenuItem("Generic Menu for SourceList"));
 *               return popupMenu;
 *           }
 *           public JPopupMenu createContextMenu(JPopupMenu popupMenu, SourceListItem item) {
 *               // create and install your custom menu items for context-menu's on a SourceListItem.
 *               JPopupMenu menu = new JPopupMenu();
 *               popupMenu.add(new JMenuItem("Menu for " + item.getText()));
 *               return menu;
 *           }
 *           public JPopupMenu createContextMenu(SourceListCategory category) {
 *               // create and install your custom menu items for context-menu's on a SourceListCategory.
 *               JPopupMenu menu = new JPopupMenu();
 *               popupMenu.add(new JMenuItem("Menu for " + category.getText()));
 *               return menu;
 *           }
 *       };
 * mySourceList.setSourceListContextMenuProvider(menuProvider);
 * <pre>
 */
public interface SourceListContextMenuProvider {

    /**
     * Called when the user requests that a context-menu be shown on the {@link SourceList} itself.
     * This will only be called if the {@code SourceList} does not fill the entire view (doesn't
     * have scroll bars) and the user clicks below any item or category. If the returned menu is
     * null or if no menu items are added to the menu, then the menu will not be shown.
     *
     * @return the context menu for the associated {@code SourceList}. Can be null or have no menu
     *         items to indicate no menu should be shown.
     */
    JPopupMenu createContextMenu();

    /**
     * Called when the user requests that a context-menu be shown on a {@link SourceListItem}.
     * If the returned menu is null or if no menu items are added to the menu, then the menu will
     * not be shown.
     *
     * @param item the {@code SourceListItem} that the context-menu was requested for.
     * @return the context menu for the associated {@code SourceListItem}. Can be null or have no
     *         menu items to indicate no menu should be shown.
     */
    JPopupMenu createContextMenu(SourceListItem item);

    /**
     * Called when the user requests that a context-menu be shown on a {@link SourceListCategory}.
     * If the returned menu is null or no menu items are added to the menu, then the menu will not
     * be shown.
     *
     * @param category the {@code SourceListCategory} that the context-menu was requested for.
     * @return the context menu for the associated {@code SourceListCategory}.  Can be null or have
     *         no menu items to indicate no menu should be shown.
     */
    JPopupMenu createContextMenu(SourceListCategory category);

}
