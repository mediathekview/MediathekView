/*
 * $Id: LookAndFeelAddons.java 4250 2012-11-13 18:15:34Z kschaefe $
 *
 * Copyright 2004 Sun Microsystems, Inc., 4150 Network Circle,
 * Santa Clara, California 95054, U.S.A. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
package org.jdesktop.swingx.plaf;

import org.jdesktop.swingx.BackgroundPaintable;
import org.jdesktop.swingx.painter.Painter;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Method;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Provides additional pluggable UI for new components added by the library. By default, the library
 * uses the pluggable UI returned by {@link #getBestMatchAddonClassName()}.
 * <p>
 * The default addon can be configured using the <code>swing.addon</code> system property as follow:
 * <ul>
 * <li>on the command line, <code>java -Dswing.addon=ADDONCLASSNAME ...</code></li>
 * <li>at runtime and before using the library components
 * <code>System.getProperties().put("swing.addon", ADDONCLASSNAME);</code></li>
 * </ul>
 * <p>
 * The default {@link #getCrossPlatformAddonClassName() cross platform addon} can be configured
 * using the <code>swing.crossplatformlafaddon</code> system property as follow:
 * <ul>
 * <li>on the command line, <code>java -Dswing.crossplatformlafaddon=ADDONCLASSNAME ...</code></li>
 * <li>at runtime and before using the library components
 * <code>System.getProperties().put("swing.crossplatformlafaddon", ADDONCLASSNAME);</code> <br>
 * Note: changing this property after the UI has been initialized may result in unexpected behavior.
 * </li>
 * </ul>
 * <p>
 * The addon can also be installed directly by calling the {@link #setAddon(String)}method. For
 * example, to install the Windows addons, add the following statement
 * <code>LookAndFeelAddons.setAddon("org.jdesktop.swingx.plaf.windows.WindowsLookAndFeelAddons");</code>.
 * 
 * @author <a href="mailto:fred@L2FProd.com">Frederic Lavigne</a>
 * @author Karl Schaefer
 */
@SuppressWarnings("nls")
public abstract class LookAndFeelAddons {

    private static List<ComponentAddon> contributedComponents = new ArrayList<ComponentAddon>();

    /**
     * Key used to ensure the current UIManager has been populated by the LookAndFeelAddons.
     */
    private static final Object APPCONTEXT_INITIALIZED = new Object();

    private static boolean trackingChanges = false;
    private static PropertyChangeListener changeListener;

    static {
        // load the default addon
        String addonClassname = getBestMatchAddonClassName();

        try {
            addonClassname = System.getProperty("swing.addon", addonClassname);
        } catch (SecurityException e) {
            // security exception may arise in Java Web Start
        }

        try {
            setAddon(addonClassname);
        } catch (Exception e) {
            // PENDING(fred) do we want to log an error and continue with a default
            // addon class or do we just fail?
            throw new ExceptionInInitializerError(e);
        }

        setTrackingLookAndFeelChanges(true);
    }

    private static LookAndFeelAddons currentAddon;

    /**
     * Determines if the addon is a match for the {@link UIManager#getLookAndFeel() current Look and
     * Feel}.
     * 
     * @return {@code true} if this addon matches (is compatible); {@code false} otherwise
     */
    protected boolean matches() {
        return false;
    }

    /**
     * Determines if the addon is a match for the system Look and Feel.
     * 
     * @return {@code true} if this addon matches (is compatible with) the system Look and Feel;
     *         {@code false} otherwise
     */
    protected boolean isSystemAddon() {
        return false;
    }

    /**
     * Initializes the look and feel addon. This method is
     * 
     * @see #uninitialize
     * @see UIManager#setLookAndFeel
     */
    public void initialize() {
        for (Iterator<ComponentAddon> iter = contributedComponents.iterator(); iter.hasNext();) {
            ComponentAddon addon = iter.next();
            addon.initialize(this);
        }
    }

    public void uninitialize() {
        for (Iterator<ComponentAddon> iter = contributedComponents.iterator(); iter.hasNext();) {
            ComponentAddon addon = iter.next();
            addon.uninitialize(this);
        }
    }

    /**
     * Adds the given defaults in UIManager.
     * 
     * Note: the values are added only if they do not exist in the existing look and feel defaults.
     * This makes it possible for look and feel implementors to override SwingX defaults.
     * 
     * Note: the array is traversed in reverse order. If a key is found twice in the array, the
     * key/value with the highest position in the array gets precedence over the other key in the
     * array
     * 
     * @param keysAndValues
     */
    public void loadDefaults(Object[] keysAndValues) {
        // Go in reverse order so the most recent keys get added first...
        for (int i = keysAndValues.length - 2; i >= 0; i = i - 2) {
            if (UIManager.getLookAndFeelDefaults().get(keysAndValues[i]) == null) {
                UIManager.getLookAndFeelDefaults().put(keysAndValues[i], keysAndValues[i + 1]);
            }
        }
    }

    public void unloadDefaults(@SuppressWarnings("unused") Object[] keysAndValues) {
        // commented after Issue 446.
        /*
         * for (int i = 0, c = keysAndValues.length; i < c; i = i + 2) {
         * UIManager.getLookAndFeelDefaults().put(keysAndValues[i], null); }
         */
    }

    public static void setAddon(String addonClassName) throws InstantiationException,
            IllegalAccessException, ClassNotFoundException {
        setAddon(Class.forName(addonClassName, true, getClassLoader()));
    }

    public static void setAddon(Class<?> addonClass) throws InstantiationException,
            IllegalAccessException {
        LookAndFeelAddons addon = (LookAndFeelAddons) addonClass.newInstance();
        setAddon(addon);
    }

    public static void setAddon(LookAndFeelAddons addon) {
        if (currentAddon != null) {
            currentAddon.uninitialize();
        }

        addon.initialize();
        currentAddon = addon;
        // JW: we want a marker to discover if the LookAndFeelDefaults have been
        // swept from under our feet. The following line looks suspicious,
        // as it is setting a user default instead of a LF default. User defaults
        // are not touched when resetting a LF
        UIManager.put(APPCONTEXT_INITIALIZED, Boolean.TRUE);
        // trying to fix #784-swingx: frequent NPE on getUI
        // JW: we want a marker to discover if the LookAndFeelDefaults have been
        // swept from under our feet.
        UIManager.getLookAndFeelDefaults().put(APPCONTEXT_INITIALIZED, Boolean.TRUE);
    }

    public static LookAndFeelAddons getAddon() {
        return currentAddon;
    }

    private static ClassLoader getClassLoader() {
        ClassLoader cl = null;
        
        try {
            cl = AccessController.doPrivileged(new PrivilegedAction<ClassLoader>() {
                @Override
                public ClassLoader run() {
                    return LookAndFeelAddons.class.getClassLoader();
                }
            });
        } catch (SecurityException ignore) { }
        
        if (cl == null) {
            final Thread t = Thread.currentThread();
            
            try {
                cl = AccessController.doPrivileged(new PrivilegedAction<ClassLoader>() {
                    @Override
                    public ClassLoader run() {
                        return t.getContextClassLoader();
                    }
                });
            } catch (SecurityException ignore) { }
        }
        
        if (cl == null) {
            try {
                cl = AccessController.doPrivileged(new PrivilegedAction<ClassLoader>() {
                    @Override
                    public ClassLoader run() {
                        return ClassLoader.getSystemClassLoader();
                    }
                });
            } catch (SecurityException ignore) { }
        }
        
        return cl;
    }
    
    /**
     * Based on the current look and feel (as returned by <code>UIManager.getLookAndFeel()</code>),
     * this method returns the name of the closest <code>LookAndFeelAddons</code> to use.
     * 
     * @return the addon matching the currently installed look and feel
     */
    public static String getBestMatchAddonClassName() {
        LookAndFeel laf = UIManager.getLookAndFeel();
        String className = null;

        if (UIManager.getCrossPlatformLookAndFeelClassName().equals(laf.getClass().getName())) {
            className = getCrossPlatformAddonClassName();
        } else if (UIManager.getSystemLookAndFeelClassName().equals(laf.getClass().getName())) {
            className = getSystemAddonClassName();
        } else {
            ServiceLoader<LookAndFeelAddons> addonLoader = ServiceLoader.load(LookAndFeelAddons.class,
                    getClassLoader());

            for (LookAndFeelAddons addon : addonLoader) {
                if (addon.matches()) {
                    className = addon.getClass().getName();
                    break;
                }
            }
        }

        if (className == null) {
            className = getSystemAddonClassName();
        }

        return className;
    }

    public static String getCrossPlatformAddonClassName() {
        try {
            return AccessController.doPrivileged(new PrivilegedAction<String>() {
                @Override
                public String run() {
                    return System.getProperty("swing.crossplatformlafaddon",
                            "org.jdesktop.swingx.plaf.metal.MetalLookAndFeelAddons");
                }
            });
        } catch (SecurityException ignore) {
        }

        return "org.jdesktop.swing.plaf.metal.MetalLookAndFeelAddons";
    }

    /**
     * Gets the addon best suited for the operating system where the virtual machine is running.
     * 
     * @return the addon matching the native operating system platform.
     */
    public static String getSystemAddonClassName() {
        ServiceLoader<LookAndFeelAddons> addonLoader = ServiceLoader.load(LookAndFeelAddons.class,
                getClassLoader());
        String className = null;

        for (LookAndFeelAddons addon : addonLoader) {
            if (addon.isSystemAddon()) {
                className = addon.getClass().getName();
                break;
            }
        }

        if (className == null) {
            className = getCrossPlatformAddonClassName();
        }

        return className;
    }

    /**
     * Each new component added by the library will contribute its default UI classes, colors and
     * fonts to the LookAndFeelAddons. See {@link ComponentAddon}.
     * 
     * @param component
     */
    public static void contribute(ComponentAddon component) {
        contributedComponents.add(component);

        if (currentAddon != null) {
            // make sure to initialize any addons added after the
            // LookAndFeelAddons has been installed
            component.initialize(currentAddon);
        }
    }

    /**
     * Removes the contribution of the given addon
     * 
     * @param component
     */
    public static void uncontribute(ComponentAddon component) {
        contributedComponents.remove(component);

        if (currentAddon != null) {
            component.uninitialize(currentAddon);
        }
    }

    /**
     * Workaround for IDE mixing up with classloaders and Applets environments. Consider this method
     * as API private. It must not be called directly.
     * 
     * @param component
     * @param expectedUIClass
     * @return an instance of expectedUIClass
     */
    public static ComponentUI getUI(JComponent component, Class<?> expectedUIClass) {
        maybeInitialize();

        // solve issue with ClassLoader not able to find classes
        String uiClassname = (String) UIManager.get(component.getUIClassID());
        // possible workaround and more debug info on #784
        if (uiClassname == null) {
            Logger logger = Logger.getLogger("LookAndFeelAddons");
            logger.warning("Failed to retrieve UI for " + component.getClass().getName()
                    + " with UIClassID " + component.getUIClassID());
            if (logger.isLoggable(Level.FINE)) {
                logger.fine("Existing UI defaults keys: "
                        + new ArrayList<Object>(UIManager.getDefaults().keySet()));
            }
            // really ugly hack. Should be removed as soon as we figure out what is causing the
            // issue
            uiClassname = "org.jdesktop.swingx.plaf.basic.Basic" + expectedUIClass.getSimpleName();
        }
        try {
            Class<?> uiClass = Class.forName(uiClassname);
            UIManager.put(uiClassname, uiClass);
        } catch (ClassNotFoundException e) {
            // we ignore the ClassNotFoundException
        }

        ComponentUI ui = UIManager.getUI(component);

        if (expectedUIClass.isInstance(ui)) {
            return ui;
        } else if (ui == null) {
            barkOnUIError("no ComponentUI class for: " + component);
        } else {
            String realUI = ui.getClass().getName();
            Class<?> realUIClass = null;

            try {
                realUIClass = expectedUIClass.getClassLoader().loadClass(realUI);
            } catch (ClassNotFoundException e) {
                barkOnUIError("failed to load class " + realUI);
            }

            if (realUIClass != null) {
                try {
                    Method createUIMethod = realUIClass.getMethod("createUI",
                            new Class[] { JComponent.class });

                    return (ComponentUI) createUIMethod.invoke(null, new Object[] { component });
                } catch (NoSuchMethodException e) {
                    barkOnUIError("static createUI() method not found in " + realUIClass);
                } catch (Exception e) {
                    barkOnUIError("createUI() failed for " + component + " " + e);
                }
            }
        }

        return null;
    }

    // this is how core UIDefaults yells about bad components; we do the same
    private static void barkOnUIError(String message) {
        System.err.println(message);
        new Error().printStackTrace();
    }

    /**
     * With applets, if you reload the current applet, the UIManager will be reinitialized (entries
     * previously added by LookAndFeelAddons will be removed) but the addon will not reinitialize
     * because addon initialize itself through the static block in components and the classes do not
     * get reloaded. This means component.updateUI will fail because it will not find its UI.
     * 
     * This method ensures LookAndFeelAddons get re-initialized if needed. It must be called in
     * every component updateUI methods.
     */
    private static synchronized void maybeInitialize() {
        if (currentAddon != null) {
            // this is to ensure "UIManager#maybeInitialize" gets called and the
            // LAFState initialized
            UIDefaults defaults = UIManager.getLookAndFeelDefaults();
            // if (!UIManager.getBoolean(APPCONTEXT_INITIALIZED)) {
            // JW: trying to fix #784-swingx: frequent NPE in getUI
            // moved the "marker" property into the LookAndFeelDefaults
            if (!defaults.getBoolean(APPCONTEXT_INITIALIZED)) {
                setAddon(currentAddon);
            }
        }
    }

    //
    // TRACKING OF THE CURRENT LOOK AND FEEL
    //
    private static class UpdateAddon implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            try {
                setAddon(getBestMatchAddonClassName());
            } catch (Exception e) {
                // should not happen
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * If true, everytime the Swing look and feel is changed, the addon which best matches the
     * current look and feel will be automatically selected.
     * 
     * @param tracking
     *            true to automatically update the addon, false to not automatically track the
     *            addon. Defaults to false.
     * @see #getBestMatchAddonClassName()
     */
    public static synchronized void setTrackingLookAndFeelChanges(boolean tracking) {
        if (trackingChanges != tracking) {
            if (tracking) {
                if (changeListener == null) {
                    changeListener = new UpdateAddon();
                }
                UIManager.addPropertyChangeListener(changeListener);
            } else {
                if (changeListener != null) {
                    UIManager.removePropertyChangeListener(changeListener);
                }
                changeListener = null;
            }
            trackingChanges = tracking;
        }
    }

    /**
     * @return true if the addon will be automatically change to match the current look and feel
     * @see #setTrackingLookAndFeelChanges(boolean)
     */
    public static synchronized boolean isTrackingLookAndFeelChanges() {
        return trackingChanges;
    }

    /**
     * Convenience method for setting a component's background painter property with a value from
     * the defaults. The painter is only set if the painter is {@code null} or an instance of
     * {@code UIResource}.
     * 
     * @param c
     *            component to set the painter on
     * @param painter
     *            key specifying the painter
     * @throws NullPointerException
     *             if the component or painter is {@code null}
     * @throws IllegalArgumentException
     *             if the component does not contain the "backgroundPainter" property or the
     *             property cannot be set
     */
    public static <T extends JComponent & BackgroundPaintable> void installBackgroundPainter(T c, String painter) {
        Painter<?> p = c.getBackgroundPainter();

        if (p == null || p instanceof UIResource) {
            c.setBackgroundPainter(UIManagerExt.getPainter(painter));
        }
    }

    /**
     * Convenience method for uninstalling a background painter. If the painter of the component is
     * a {@code UIResource}, it is set to {@code null}.
     * 
     * @param c
     *            component to uninstall the painter on
     * @throws NullPointerException
     *             if {@code c} is {@code null}
     * @throws IllegalArgumentException
     *             if the component does not contain the "backgroundPainter" property or the
     *             property cannot be set
     */
    public static <T extends JComponent & BackgroundPaintable> void uninstallBackgroundPainter(T c) {
        Painter<?> p = c.getBackgroundPainter();

        if (p == null || p instanceof UIResource) {
            c.setBackgroundPainter(null);
        }
    }
}
