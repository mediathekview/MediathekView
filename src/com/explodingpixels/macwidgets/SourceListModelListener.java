package com.explodingpixels.macwidgets;

/**
 * An interface for listening to changes in a {@link SourceListModel}.
 */
public interface SourceListModelListener {

    /**
     * Called when a category is adeded.
     *
     * @param category the category that was removed.
     * @param index    the index at which the category was added.
     */
    void categoryAdded(SourceListCategory category, int index);

    /**
     * Called when a category is removed.
     *
     * @param category the category that was removed.
     */
    void categoryRemoved(SourceListCategory category);

    /**
     * Called when an item is added to a category.
     *
     * @param itemAdded the item that was added.
     * @param category  the category that the item was added to.
     * @param index     the index at which the category was added.
     */
    void itemAddedToCategory(SourceListItem itemAdded, SourceListCategory category, int index);

    /**
     * Called when an item is removed from a category.
     *
     * @param itemRemoved the item that was removed.
     * @param category    the category that the item was removed from.
     */
    void itemRemovedFromCategory(SourceListItem itemRemoved, SourceListCategory category);

    /**
     * Called when an item is added to another item.
     *
     * @param itemAdded  the item that was added.
     * @param parentItem the item that the child item was added to.
     * @param index      the index at which the category was added.
     */
    void itemAddedToItem(SourceListItem itemAdded, SourceListItem parentItem, int index);

    /**
     * Called when an item is removed from another item.
     *
     * @param itemRemoved the item that was removed.
     * @param parentItem  the item that the child item was removed from.
     */
    void itemRemovedFromItem(SourceListItem itemRemoved, SourceListItem parentItem);

    /**
     * Called when an item's content changes (e.g. text, icon etc.).
     *
     * @param item the item that was changed.
     */
    void itemChanged(SourceListItem item);
}
