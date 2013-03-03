package com.explodingpixels.macwidgets;

import java.awt.Window;

import javax.swing.BorderFactory;
import javax.swing.JComponent;

import com.explodingpixels.border.FocusStateMatteBorder;
import com.explodingpixels.widgets.WindowDragger;
import com.explodingpixels.widgets.WindowUtils;
import com.jgoodies.forms.factories.Borders;

/**
 * A Mac style Unified Tool Bar. For a full description of what a Unified Tool
 * Bar is, see the <a href=
 * "http://developer.apple.com/documentation/UserExperience/Conceptual/AppleHIGuidelines/XHIGWindows/chapter_18_section_4.html#//apple_ref/doc/uid/20000961-BABIFCFJ"
 * >Toolbars</a> section of Apple's Human Interface Guidelines. Here's an
 * example of the what this method creates: <br>
 * <img src="../../../../graphics/UnifiedToolBar.png"> <br>
 * Here's a simple example that creates a Unified Tool Bar with a single button:
 * 
 * <pre>
 * UnifiedToolBar toolBar = new UnifiedToolBar();
 * JButton button = new JButton(&quot;My Button&quot;);
 * button.putClientProperty(&quot;JButton.buttonType&quot;, &quot;textured&quot;);
 * toolBar.addComponentToLeft(button);
 * </pre>
 */
public class UnifiedToolBar {

	private final TriAreaComponent fUnifiedToolBar;

	/**
	 * Creates a {@code UnifiedToolBar} with balanced ends.
	 */
	public UnifiedToolBar() {
		this(true);
	}
		
	/**
	 * Creates a {@code UnifiedToolBar}.
	 * 	 
	 * @param forceSameWidth
	 *            whether the two ends should have the same width to keep the center balanced.
	 */
	public UnifiedToolBar(boolean forceSameWidth) {
		
		fUnifiedToolBar = new TriAreaComponent(4, forceSameWidth);
		
		// TODO remove below call when Apple fixes bug in Java that doesn't
		// correctly paint the
		// TODO textured window.
		fixUnifiedToolBarOnMacIfNeccessary(fUnifiedToolBar);
		fUnifiedToolBar.getComponent().setBorder(
				Borders.createEmptyBorder("3dlu, 4dlu, 3dlu, 4dlu"));
		installUnifiedToolBarBorder(fUnifiedToolBar.getComponent());
		WindowUtils
				.installJComponentRepainterOnWindowFocusChanged(fUnifiedToolBar
						.getComponent());
	}

	/**
	 * Adds the given component to the left side of this {@code UnifiedToolbar}.
	 * 
	 * @param toolToAdd
	 *            the tool to add to this {@code UnifiedToolbar}.
	 */
	public void addComponentToLeft(JComponent toolToAdd) {
		fUnifiedToolBar.addComponentToLeft(toolToAdd);
	}

	/**
	 * Adds the given component to the side of this {@code UnifiedToolbar}.
	 * 
	 * @param toolToAdd
	 *            the tool to add to this {@code UnifiedToolbar}.
	 */
	public void addComponentToCenter(JComponent toolToAdd) {
		fUnifiedToolBar.addComponentToCenter(toolToAdd);
	}

	/**
	 * Adds the given component to the right side of this {@code UnifiedToolBar}
	 * .
	 * 
	 * @param toolToAdd
	 *            the tool to add to this {@code UnifiedToolBar}.
	 */
	public void addComponentToRight(JComponent toolToAdd) {
		fUnifiedToolBar.addComponentToRight(toolToAdd);
	}

	/**
	 * Installs a drag listener on this {@code UnifiedToolBar} such that if it
	 * is dragged, it will move the given {@link Window}.
	 * 
	 * @param window
	 *            the {@code Window} to move when the this
	 *            {@code UnifiedToolbar} is dragged.
	 */
	public void installWindowDraggerOnWindow(Window window) {
		new WindowDragger(window, getComponent());
	}

	/**
	 * Gets the user interface component representing this
	 * {@code UnifiedToolBar}. The returned {@link JComponent} should be added
	 * to a container that will be displayed.
	 * 
	 * @return the user interface component representing this
	 *         {@code UnifiedToolBar}.
	 */
	public JComponent getComponent() {
		return fUnifiedToolBar.getComponent();
	}

	/**
	 * Disables any custom background painter that may be installed.
	 */
	public void disableBackgroundPainter() {
		fUnifiedToolBar.setBackgroundPainter(null);
	}

	/**
	 * Installs a custom painter on the given {@link TriAreaComponent} that
	 * paints the Mac style unified toolbar gradient on non-Mac platforms as
	 * well as Mac platforms running using Java 6.
	 * 
	 * @param unifiedToolBar
	 *            the {@link TriAreaComponent} to install the custom painter on
	 *            if necessary.
	 */
	private static void fixUnifiedToolBarOnMacIfNeccessary(
			TriAreaComponent unifiedToolBar) {
		// install the custom painter if on non-Mac platforms or in other
		// various Mac cases.
		if (MacUtils.shouldManuallyPaintTexturedWindowBackground()) {
			unifiedToolBar.setBackgroundPainter(MacPainterFactory
					.createTexturedWindowWorkaroundPainter());
		}
	}

	static void installUnifiedToolBarBorder(final JComponent component) {

		FocusStateMatteBorder border = new FocusStateMatteBorder(0, 0, 1, 0,
				MacColorUtils.getTexturedWindowToolbarBorderFocusedColor(),
				MacColorUtils.getTexturedWindowToolbarBorderUnfocusedColor(),
				component);

		component.setBorder(BorderFactory.createCompoundBorder(border,
				component.getBorder()));
	}
}
