package com.explodingpixels.painter;

import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.KeyboardFocusManager;
import java.awt.Window;

import javax.swing.SwingUtilities;

import com.explodingpixels.widgets.WindowUtils;

/**
 * An implementation of {@link MacWidgetsPainter} that delegates to given {@code Painter}
 * based on the focused state of the {@link Component} supplied in the
 * {@link #paint(java.awt.Graphics2D, java.awt.Component, int, int)} method.
 */
public class FocusStatePainter implements MacWidgetsPainter<Component> {

	private MacWidgetsPainter<Component> fComponentFocusedPainter;
	private MacWidgetsPainter<Component> fWindowFocusedPainter;
	private MacWidgetsPainter<Component> fWindowUnfocusedPainter;

	/**
	 * Creates a {@link MacWidgetsPainter} that delegates to the given {@code Painter}s
	 * based on the focus state of the supplied {@link Component} or the focus
	 * state of it's parent {@link java.awt.Window}.
	 * 
	 * @param componentFocusedPainter
	 *            the {@code Painter} to use when the given {@code Component} is
	 *            focused or it's parent {@code java.awt.Window} is focused.
	 * @param windowUnfocusedPainter
	 *            the {@code Painter} to use when the given {@code Component}'s
	 *            parent {@code java.awt.Window} is unfocused.
	 */
	public FocusStatePainter(MacWidgetsPainter<Component> componentFocusedPainter,
			MacWidgetsPainter<Component> windowUnfocusedPainter) {
		this(componentFocusedPainter, windowUnfocusedPainter,
				windowUnfocusedPainter);
	}

	/**
	 * Creates a {@link MacWidgetsPainter} that delegates to the given {@code Painter}s
	 * based on the focus state of the supplied {@link Component} or the focus
	 * state of it's parent {@link java.awt.Window}.
	 * 
	 * @param componentFocusedPainter
	 *            the {@code Painter} to use when the given {@code Component} is
	 *            focused.
	 * @param windowFocusedPainter
	 *            the {@code Painter} to use when the given {@code Component} is
	 *            unfocused but the {@code Component}'s parent window is
	 *            focused.
	 * @param windowUnfocusedPainter
	 *            the {@code Painter} to use when the given {@code Component}'s
	 *            parent {@code java.awt.Window} is unfocused.
	 */
	public FocusStatePainter(MacWidgetsPainter<Component> componentFocusedPainter,
			MacWidgetsPainter<Component> windowFocusedPainter,
			MacWidgetsPainter<Component> windowUnfocusedPainter) {

		if (componentFocusedPainter == null) {
			throw new IllegalArgumentException(
					"Component focused Painter cannot be null.");
		}

		if (windowFocusedPainter == null) {
			throw new IllegalArgumentException(
					"Window focused Painter cannot be null.");
		}

		if (windowUnfocusedPainter == null) {
			throw new IllegalArgumentException(
					"Window unfocused Painter cannot be null.");
		}

		fComponentFocusedPainter = componentFocusedPainter;
		fWindowFocusedPainter = windowFocusedPainter;
		fWindowUnfocusedPainter = windowUnfocusedPainter;

	}

	public void paint(Graphics2D g, Component component, int width, int height) {
		MacWidgetsPainter<Component> painterToUse;

		Window activeAncestor = null;

		Window active = KeyboardFocusManager.getCurrentKeyboardFocusManager()
				.getActiveWindow();
		if (active != null) {
			activeAncestor = SwingUtilities.getWindowAncestor(active);
		}
		Window componentAncestor = SwingUtilities.getWindowAncestor(component);
		if (component.hasFocus() || componentAncestor.equals(activeAncestor)) {
			painterToUse = fComponentFocusedPainter;
		} else if (WindowUtils.isParentWindowFocused(component)
				|| componentAncestor.equals(activeAncestor)) {
			painterToUse = fWindowFocusedPainter;
		} else {
			painterToUse = fWindowUnfocusedPainter;
		}

		painterToUse.paint(g, component, width, height);
	}
}
