package com.explodingpixels.macwidgets;

import java.awt.Component;
import java.awt.Window;

import javax.swing.JComponent;
import javax.swing.JPanel;

import com.explodingpixels.painter.MacWidgetsPainter;
import com.explodingpixels.swingx.EPPanel;
import com.explodingpixels.widgets.WindowDragger;
import com.jgoodies.forms.builder.PanelBuilder;
import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

/**
 * A component that has three areas in which it widgets can be added.
 */
public class TriAreaComponent {

	private PanelBuilder fLeftPanelBuilder = new PanelBuilder(new FormLayout(
			"", "fill:p:grow"), new JPanel());
	private PanelBuilder fCenterPanelBuilder = new PanelBuilder(new FormLayout(
			"", "fill:p:grow"), new JPanel());
	private PanelBuilder fRightPanelBuilder = new PanelBuilder(new FormLayout(
			"", "fill:p:grow"), new JPanel());

	private final EPPanel fPanel = new EPPanel();

	private int fSpacer_pixels;

	/**
	 * Creates a {@code TriAreaComponent} that uses a padding of 0 pixels
	 * between components and forcing ends to have the same widths
	 */
	public TriAreaComponent() {
		this(0);
	}

	/**
	 * Creates a {@code TriAreaComponent} that uses the given padding between
	 * components and forcing ends to have the same widths.
	 * 
	 * @param spacer_pixels
	 *            the space in pixels to add between components.
	 */
	public TriAreaComponent(int spacer_pixels) {
		this(spacer_pixels, true);
	}
		
	/**
	 * Creates a {@code TriAreaComponent} that uses the given padding between
	 * components.
	 * 
	 * @param spacer_pixels
	 *            the space in pixels to add between components.
	 * @param forceSameWidth
	 *            whether the two ends should have the same width to keep the component balanced.
	 */
	public TriAreaComponent(int spacer_pixels, boolean forceSameWidth) {
		fSpacer_pixels = spacer_pixels;

		// definte the FormLayout columns and rows.
		FormLayout layout = new FormLayout(
				"left:p:grow, center:p:grow, right:p:grow", "fill:p:grow");
		// TODO decide whether to offer the option to force left, center and
		// TODO right groups to be the same size
		// layout.setColumnGroups(new int[][]{{1,2,3}});
		// create the cell constraints to use in the layout.
		CellConstraints cc = new CellConstraints();
		// create the builder with our panel as the component to be filled.
		PanelBuilder builder = new PanelBuilder(layout, fPanel);

		builder.add(fLeftPanelBuilder.getPanel(), cc.xy(1, 1));
		builder.add(fCenterPanelBuilder.getPanel(), cc.xy(2, 1));
		builder.add(fRightPanelBuilder.getPanel(), cc.xy(3, 1));

		fLeftPanelBuilder.getPanel().setOpaque(false);
		fCenterPanelBuilder.getPanel().setOpaque(false);
		fRightPanelBuilder.getPanel().setOpaque(false);

		// fLeftPanelBuilder.getPanel().setBorder(
		// BorderFactory.createLineBorder(Color.RED));
		// fCenterPanelBuilder.getPanel().setBorder(
		// BorderFactory.createLineBorder(Color.BLUE));
		// fRightPanelBuilder.getPanel().setBorder(
		// BorderFactory.createLineBorder(Color.GREEN));

		if (forceSameWidth)
			forceOuterAreasToHaveTheSameWidth();
	}


    /**
	 * Forces each of the areas (left, center and right) to have the same widths
	 * regardless of the size of the items each of the area contains.
	 */
	void forceAreasToHaveTheSameWidth() {
        ((FormLayout) fPanel.getLayout()).setColumnGroups(new int[][]{{1, 2, 3}});
    }

    /**
	 * Forces outer areas (left and right) to have the same widths
	 * regardless of the size of the items each of the area contains.
	 */
	void forceOuterAreasToHaveTheSameWidth() {
		((FormLayout) fPanel.getLayout())
				.setColumnGroups(new int[][] { { 1, 3 } });
	}

	/**
	 * Gets the user interface component representing this {@code SourceList}.
	 * The returned {@link JComponent} should be added to a container that will
	 * be displayed.
	 * 
	 * @return the user interface component representing this {@code SourceList}
	 *         .
	 */
	public JComponent getComponent() {
		return fPanel;
	}

	/**
	 * Installs a {@link WindowDragger} on the given {@link Window}.
	 * 
	 * @param window
	 *            the {@code Window} to install the {@code WindowDragger} on.
	 */
	public void installWindowDraggerOnWindow(Window window) {
		new WindowDragger(window, getComponent());
	}

	/**
	 * Adds the given component to the left side of this
	 * {@code TriAreaComponent}.
	 * 
	 * @param toolToAdd
	 *            the tool to add to this {@code TriAreaComponent}.
	 */
	public void addComponentToLeft(JComponent toolToAdd) {
		addComponentToLeft(toolToAdd, fSpacer_pixels);
	}

	/**
	 * Adds the given component to the left side of this
	 * {@code TriAreaComponent} followed by the given an empty space of the
	 * given pixel width.
	 * 
	 * @param toolToAdd
	 *            the tool to add to this {@code TriAreaComponent}.
	 * @param spacer_pixels
	 *            the amount of space to post-pend the added component with.
	 */
	public void addComponentToLeft(JComponent toolToAdd, int spacer_pixels) {
		fLeftPanelBuilder.appendColumn("p");
		fLeftPanelBuilder.add(toolToAdd);
		fLeftPanelBuilder.nextColumn();
		fLeftPanelBuilder.appendColumn("p");
		fLeftPanelBuilder.add(MacWidgetFactory.createSpacer(spacer_pixels, 0));
		fLeftPanelBuilder.nextColumn();
	}

	/**
	 * Adds the given component to the center of this {@code TriAreaComponent}.
	 * 
	 * @param toolToAdd
	 *            the tool to add to this {@code TriAreaComponent}.
	 */
	public void addComponentToCenter(JComponent toolToAdd) {
		addComponentToCenter(toolToAdd, fSpacer_pixels);
	}

	/**
	 * Adds the given component to the center of this {@code TriAreaComponent}.
	 * If this is not the first component to be added to the center, then the
	 * given component will be preceeded by a space of the given width.
	 * 
	 * @param toolToAdd
	 *            the tool to add to this {@code TriAreaComponent}.
	 * @param spacer_pixels
	 *            the amount of space to pre-pend the added component with *if*
	 *            the given component is *not* the first component to be added
	 *            to the center.
	 */
	public void addComponentToCenter(JComponent toolToAdd, int spacer_pixels) {
		if (getCenterComponentCount() > 0) {
			fCenterPanelBuilder.appendColumn("p");
			fCenterPanelBuilder.add(MacWidgetFactory.createSpacer(
					spacer_pixels, 0));
			fCenterPanelBuilder.nextColumn();
		}

		fCenterPanelBuilder.appendColumn("p");
		fCenterPanelBuilder.add(toolToAdd);
		fCenterPanelBuilder.nextColumn();
	}

	/**
	 * Adds the given component to the right side of this
	 * {@code TriAreaComponent}.
	 * 
	 * @param toolToAdd
	 *            the tool to add to this {@code TriAreaComponent}.
	 */
	public void addComponentToRight(JComponent toolToAdd) {
		addComponentToRight(toolToAdd, fSpacer_pixels);
	}

	/**
	 * Adds the given component to the right side of this
	 * {@code TriAreaComponent}. If this is not the first component to be added
	 * to the right, then the given component will be followed by a space of the
	 * given width.
	 * 
	 * @param toolToAdd
	 *            the tool to add to this {@code TriAreaComponent}.
	 * @param spacer_pixels
	 *            the amount of space to post-pend the added component with *if*
	 *            the given component is *not* the first component to be added
	 *            to the center.
	 */
	public void addComponentToRight(JComponent toolToAdd, int spacer_pixels) {
		// first, add a spacer if there is already an component on the right.
		if (getRightComponentCount() > 0) {
			fRightPanelBuilder.appendColumn("p");
			fRightPanelBuilder.add(MacWidgetFactory.createSpacer(spacer_pixels,
					0));
			fRightPanelBuilder.nextColumn();
		}

		// next, add the given component.
		fRightPanelBuilder.appendColumn("p");
		fRightPanelBuilder.add(toolToAdd);
		fRightPanelBuilder.nextColumn();
	}

	/**
	 * Set's the background {@link com.explodingpixels.painter.MacWidgetsPainter} that this {@code TriAreaComponent}
	 * uses.
	 * 
	 * @param backgroundPainter
	 *            the background {@link com.explodingpixels.painter.MacWidgetsPainter} that this
	 *            {@code TriAreaComponent} uses.
	 */
	public void setBackgroundPainter(MacWidgetsPainter<Component> backgroundPainter) {
		fPanel.setBackgroundPainter(backgroundPainter);
	}

	protected final int getLeftComponentCount() {
		return fLeftPanelBuilder.getPanel().getComponentCount();
	}

	protected final int getCenterComponentCount() {
		return fCenterPanelBuilder.getPanel().getComponentCount();
	}

	protected final int getRightComponentCount() {
		return fRightPanelBuilder.getPanel().getComponentCount();
	}
}
