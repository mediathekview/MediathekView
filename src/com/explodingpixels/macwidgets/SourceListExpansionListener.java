package com.explodingpixels.macwidgets;

/**
 * An interface for listening for expansion events.
 * It handles expansion events for both {@link SourceListItem} and {@link SourceListCategory}.
 */
public interface SourceListExpansionListener {

    /**
     * Called before expanding a {@link SourceListItem} in a {@link SourceList}.
     * Determines whether an item is allowed to be expanded or not
     * @param item the item that requests to be expanded.
     * @return true if the item is expandable, false otherwise
     */
    boolean shouldExpandSourceListItem(SourceListItem item);

    /**
     * Called when a {@link SourceListItem} is expanded in a {@link SourceList}.
     * The method will only be called if {@link SourceListExpansionListener#shouldExpandSourceListItem(SourceListItem)}
     * returns true.
     * @param item the item that was expanded.
     */
    void sourceListItemExpanded(SourceListItem item);

    /**
     * Called before collapsing a {@link SourceListItem} in a {@link SourceList}.
     * Determines whether an item is allowed to be collapsed or not
     * @param item the item that requests to be collapsed.
     * @return true if the item is collapsable, false otherwise
     */
    boolean shouldCollapseSourceListItem(SourceListItem item);

    /**
     * Called when a {@link SourceListItem} is collapsed in a {@link SourceList}.
     * The method will only be called if {@link SourceListExpansionListener#shouldCollapseSourceListItem(SourceListItem)}
     * returns true.
     * @param item the item that was collapsed.
     */
    void sourceListItemCollapsed(SourceListItem item);

    /**
     * Called before expanding a {@link SourceListCategory} in a {@link SourceList}.
     * Determines whether a category is allowed to be expanded or not
     * @param category the category that requests to be expanded.
     * @return true if the item is expandable, false otherwise
     */
    boolean shouldExpandSourceListCategory(SourceListCategory category);

    /**
     * Called when a {@link SourceListCategory} is expanded in a {@link SourceList}.
     * The method will only be called if {@link SourceListExpansionListener#shouldExpandSourceListCategory(SourceListCategory)}
     * returns true.
     * @param category the category that was expanded.
     */
    void sourceListCategoryExpanded(SourceListCategory category);

    /**
     * Called before collapsing a {@link SourceListCategory} in a {@link SourceList}.
     * Determines whether a category is allowed to be collapsed or not
     * @param category the category that requests to be collapsed.
     * @return true if the item is collapsable, false otherwise
     */
    boolean shouldToCollapseSourceListCategory(SourceListCategory category);

    /**
     * Called when a {@link SourceListCategory} is collapsed in a {@link SourceList}.
     * The method will only be called if {@link SourceListExpansionListener#shouldToCollapseSourceListCategory(SourceListCategory)}
     * returns true.
     * @param category the category that was collapsed.
     */
    void sourceListCategoryCollapsed(SourceListCategory category);
}
