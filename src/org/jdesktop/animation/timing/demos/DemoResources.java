package org.jdesktop.animation.timing.demos;

import java.net.URL;

/**
 * Manages all the resources used by demonstration programs. This avoids
 * duplication of resources if they are used by more than one demonstration
 * program.
 * 
 * @author Tim Halloran
 */
public final class DemoResources {

	public static final String BEETLE_RED = "beetle-red.gif";
	public static final String DRIFT = "drift.wav";
	public static final String TRACK = "track.jpg";
	public static final String VROOM = "vroom.wav";

	private static final String PREFIX = "org/jdesktop/animation/timing/demos/";

	/**
	 * Gets the passed resource in the classpath.
	 * 
	 * @param name
	 *            the resource name.
	 * @return a reference to the resource that can be used to load it.
	 */
	public static URL getResource(String name) {
		final URL result = Thread.currentThread().getContextClassLoader()
				.getResource(PREFIX + name);
		if (result == null)
			throw new IllegalStateException("Unable to load resource: " + name);
		else
			return result;
	}
}
