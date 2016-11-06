package com.explodingpixels.macwidgets;

/**
 * An interface that allows implementors to supply the tool tip for a {@link SourceListCategory} or
 * {@link SourceListItem}.
 */
public interface SourceListToolTipProvider {

    /**
     * Gets the tool tip to use for the given {@link SourceListCategory}.
     *
     * @param category the {@code SourceListCategory} to get the tooltip for.
     * @return the tool tip, or null if no tool tip should be shown.
     */
    String getTooltip(SourceListCategory category);

    /**
     * Gets the tool tip to use for the given {@link SourceListItem}.
     *
     * @param item the {@code SourceListItem} to get the tooltip for.
     * @return the tool tip, or null if no tool tip should be shown.
     */
    String getTooltip(SourceListItem item);

}
