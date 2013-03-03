package com.explodingpixels.widgets;

import java.awt.Color;
import java.awt.Component;
import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.lang.ref.WeakReference;
import java.lang.reflect.Method;

import javax.swing.FocusManager;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

import com.explodingpixels.util.PlatformUtils;

/**
 * Utility methods for dealing with {@link Window}s.
 */
public class WindowUtils {

	/**
	 * Try's to make the given {@link Window} non-opqaue (transparent) across
	 * platforms and JREs. This method is not guaranteed to succeed, and will
	 * fail silently if the given {@code Window} cannot be made non-opaque.
	 * <p/>
	 * This method is useful, for example, when creating a HUD style window that
	 * is semi-transparent, and thus doesn't want the window background to be
	 * drawn.
	 * 
	 * @param window
	 *            the {@code Window} to make non-opaque.
	 */
	public static void makeWindowNonOpaque(Window window) {
		// on the mac, simply setting the window's background color to be fully
		// transparent makes
		// the window non-opaque.
		window.setBackground(new Color(0, 0, 0, 0));
		// on non-mac platforms, try to use the facilities of Java 6 update 10.
		if (!PlatformUtils.isMac()) {
			quietlyTryToMakeWindowNonOqaque(window);
		}
	}

	/**
	 * Trys to invoke
	 * {@code com.sun.awt.AWTUtilities.setWindowOpaque(window,false)} on the
	 * given {@link Window}. This will only work when running with JRE 6 update
	 * 10 or higher. This method will silently fail if the method cannot be
	 * invoked.
	 * 
	 * @param window
	 *            the {@code Window} to try and make non-opaque.
	 */
	private static void quietlyTryToMakeWindowNonOqaque(Window window) {
		try {
            @SuppressWarnings("rawtypes")
			Class clazz = Class.forName("com.sun.awt.AWTUtilities");
            
            @SuppressWarnings("unchecked")
            Method method = clazz.getMethod("setWindowOpaque",
					java.awt.Window.class, Boolean.TYPE);
			method.invoke(clazz, window, false);
		} catch (Exception e) {
			// silently ignore this exception.
		}
	}

	/**
	 * Creates and installs a {@link WindowFocusListener} on the given
	 * {@link Window} which calls the {@code Window}'s {@code repaint()} method
	 * on focus state changes.
	 * 
	 * @param window
	 *            the {@code Window} to repaint on focus state changes.
	 * @return the listener installed.
	 * @deprecated use the more targeted
	 *             {@link WindowUtils#installJComponentRepainterOnWindowFocusChanged(JComponent)}
	 *             method.
	 */
	@Deprecated
	public static WindowFocusListener createAndInstallRepaintWindowFocusListener(
			Window window) {
		WindowFocusListener windowFocusListener = new WindowFocusListener() {
			public void windowGainedFocus(WindowEvent e) {
				e.getWindow().repaint();
			}

			public void windowLostFocus(WindowEvent e) {
				e.getWindow().repaint();
			}
		};
		window.addWindowFocusListener(windowFocusListener);
		return windowFocusListener;
	}

	/**
	 * {@code true} if the given {@link Component}'s has a parent {@link Window}
	 * (i.e. it's not null) and that {@link Window} is currently active
	 * (focused).
	 * 
	 * @param component
	 *            the {@code Component} to check the parent {@code Window}'s
	 *            focus for.
	 * @return {@code true} if the given {@code Component}'s parent
	 *         {@code Window} is currently active.
	 */
	public static boolean isParentWindowFocused(Component component) {
		Window window = SwingUtilities.getWindowAncestor(component);
		return window != null && window.isFocused();
	}

	/**
	 * Installs a {@link WindowFocusListener} on the given {@link JComponent}'s
	 * parent {@link Window}. If the {@code JComponent} doesn't yet have a
	 * parent, then the listener will be installed when the component is added
	 * to a container.
	 * 
	 * @param component
	 *            the component who's parent frame to listen to focus changes
	 *            on.
	 * @param focusListener
	 *            the {@code WindowFocusListener} to notify when focus changes.
	 */
	public static void installWeakWindowFocusListener(JComponent component,
			WindowFocusListener focusListener) {
		AncestorListener ancestorListener = createAncestorListener(component,
				focusListener);
		component.addAncestorListener(ancestorListener);
	}

	/**
	 * Installs a listener on the given {@link JComponent}'s parent
	 * {@link Window} that repaints the given component when the parent window's
	 * focused state changes. If the given component does not have a parent at
	 * the time this method is called, then an ancestor listener will be
	 * installed that installs a window listener when the components parent
	 * changes.
	 * 
	 * @param component
	 *            the {@code JComponent} to add the repaint focus listener to.
	 */
	public static void installJComponentRepainterOnWindowFocusChanged(
			JComponent component) {
		// TODO check to see if the component already has an ancestor.
		WindowFocusListener windowListener = createRepaintWindowListener(component);
		AncestorListener ancestorListener = createAncestorListener(component,
				windowListener);
		component.addAncestorListener(ancestorListener);
	}

	/**
	 * Creates an {@link AncestorListener} that installs a weakly referenced
	 * version of the given {@link WindowFocusListener} when the given
	 * {@link JComponent}'s parent changes.
	 */
	private static AncestorListener createAncestorListener(
			JComponent component, final WindowFocusListener windowListener) {
		final WeakReference<JComponent> weakReference = new WeakReference<JComponent>(
				component);
		return new AncestorListener() {
			public void ancestorAdded(AncestorEvent event) {
				// TODO if the WeakReference's object is null, remove the
				// WeakReference as an
				// TODO AncestorListener.
				final Window window = weakReference.get() == null ? null
						: SwingUtilities.getWindowAncestor(weakReference.get());
				if (window != null) {
					window.removeWindowFocusListener(windowListener);
					window.addWindowFocusListener(windowListener);
					// notify the listener of the original focus state of the
					// window, which ensures
					// that the listener is in sync with the actual window
					// state.
					fireInitialFocusEvent(windowListener, window);
				}
			}

			private void fireInitialFocusEvent(
					WindowFocusListener windowListener, Window window) {
				Window focusedWindow = FocusManager.getCurrentManager()
						.getFocusedWindow();
				// fire a fake event to the given listener indicating the actual
				// focus state of the
				// given window.
				if (window == focusedWindow) {
					windowListener.windowGainedFocus(new WindowEvent(window,
							WindowEvent.WINDOW_GAINED_FOCUS));
				} else {
					windowListener.windowGainedFocus(new WindowEvent(window,
							WindowEvent.WINDOW_LOST_FOCUS));
				}
			}

			public void ancestorRemoved(AncestorEvent event) {
				Window window = weakReference.get() == null ? null
						: SwingUtilities.getWindowAncestor(weakReference.get());
				if (window != null) {
					window.removeWindowFocusListener(windowListener);
				}
			}

			public void ancestorMoved(AncestorEvent event) {
				// no implementation.
			}
		};
	}

	/**
	 * Creates a {@link WindowFocusListener} that calls repaint on the given
	 * {@link JComponent} when focused gained or focus lost is called.
	 */
	private static WindowFocusListener createRepaintWindowListener(
			final JComponent component) {
		return new WindowFocusListener() {
			public void windowGainedFocus(WindowEvent e) {
				component.repaint();
			}

			public void windowLostFocus(WindowEvent e) {
				component.repaint();
			}
		};
	}

}
