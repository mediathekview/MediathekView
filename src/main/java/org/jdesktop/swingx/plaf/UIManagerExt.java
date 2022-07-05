/*
 * $Id: UIManagerExt.java 4028 2011-06-03 19:32:19Z kschaefe $
 *
 * Copyright 2007 Sun Microsystems, Inc., 4150 Network Circle,
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

import org.jdesktop.swingx.painter.Painter;
import org.jdesktop.swingx.util.Contract;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.*;
import java.awt.*;
import java.util.*;

/**
 * A utility class for obtaining configuration properties from the
 * {@code UIDefaults}. This class handles SwingX-specific L&F needs, such as
 * the installation of painters and shapes. There are several categories of
 * utility methods:
 * <ul>
 * <li>Support for the safe creation of {@code UIResource}s.</li>
 * <li>Support for new {@code UIResource} types, such as
 * {@code PainterUIResource}.</li>
 * <li>Support for the dynamic localization of {@code UIDefaults}.</li>
 * <li>Support for returning non-{@code String} localizations from
 * {@code ResourceBundle}s.</li>
 * </ul>
 * <h3>Safe Methods</h3>
 * <p>
 * The {@code getSafeXXX} methods are designed for use with
 * {@code LookAndFeelAddon}s. Any addon that attempts to obtain a property
 * defined in the defaults (available from {@code UIManager.get}) to set a
 * property that will be added to the defaults for the addon should use the
 * "safe" methods. The methods ensure that a valid value is always returned and
 * that value is a {@code UIResource}.
 * </p>
 * <h3>Support for New Types</h3>
 * <p>
 * {@code UIManagerExt} supports the retrieval of new {@code UIResource} types.
 * There is a {@code getXXX} method for every {@code UIResource} subtype in the
 * {@code org.jdesktop.swingx.plaf} package.
 * </p>
 * <h3>Support for Dynamic Localization</h3>
 * <p>
 * {@code UIManagerExt} enables dynamic localization by supporting
 * {@code ResourceBundle}s. The
 * {@linkplain UIDefaults#addResourceBundle(String)} allows resource bundles to
 * be added to the {@code UIDefaults}. While there is support for this feature
 * in core, there is a bug with the class loader that prevents user added
 * bundles from working correctly when used via Web Start. Therefore,
 * {@code UIManagerExt} defines methods to add and remove resource bundles.
 * These are the only methods that SwingX classes should use when adding
 * resource bundles to the defaults. Since {@code UIManagerExt} is maintaining
 * the bundles, any localized {@code String}s <b>must</b> be retrieved from
 * the {@code getString} methods in this class.
 * </p>
 * <h3>Support for Non-{@code String} Localization Values</h3>
 * <p>
 * All methods work by first determining if the value is present
 * {@code UIDefaults}. If the value is not present, then the installed
 * {@code ResourceBundle}s are queried. {@code UIManagerExt} will attempt to
 * convert any returned value to the appropriate type. For instance,
 * {@code getInt} uses {@code Integer.decode} to convert {@code String}s
 * returned from the bundle into {@code int}s.
 * </p>
 * 
 * @author Karl George Schaefer
 * 
 * @see UIManager
 * @see UIDefaults
 */
@SuppressWarnings("nls")
public class UIManagerExt {
    /**
     * Used to replicate the resource bundle behavior from the
     * {@code UIDefaults}.
     */
    private static class UIDefaultsExt {
        //use vector; we want synchronization
        private Vector<String> resourceBundles;

        /**
         * Maps from a Locale to a cached Map of the ResourceBundle. This is done
         * so as to avoid an exception being thrown when a value is asked for.
         * Access to this should be done while holding a lock on the
         * UIDefaults, eg synchronized(this).
         */
        private Map<Locale, Map<String, String>> resourceCache;
        
        UIDefaultsExt() {
            resourceCache = new HashMap<Locale, Map<String,String>>();
        }
        
        //should this just return String?
        private Object getFromResourceBundle(Object key, Locale l) {

            if( resourceBundles == null ||
                resourceBundles.isEmpty() ||
                !(key instanceof String) ) {
                return null;
            }

            // A null locale means use the default locale.
            if( l == null ) {
                    l = Locale.getDefault();
            }

            synchronized(this) {
                return getResourceCache(l).get(key);
            }
        }

        /**
         * Returns a Map of the known resources for the given locale.
         */
        private Map<String, String> getResourceCache(Locale l) {
            Map<String, String> values = resourceCache.get(l);

            if (values == null) {
                values = new HashMap<String, String>();
                for (int i=resourceBundles.size()-1; i >= 0; i--) {
                    String bundleName = resourceBundles.get(i);
                    
                    try {
                        ResourceBundle b = ResourceBundle.
                            getBundle(bundleName, l, UIManagerExt.class.getClassLoader());
                        Enumeration<String> keys = b.getKeys();

                        while (keys.hasMoreElements()) {
                            String key = keys.nextElement();

                            if (values.get(key) == null) {
                                Object value = b.getObject(key);

                                values.put(key, (String) value);
                            }
                        }
                    } catch( MissingResourceException mre ) {
                        // Keep looking
                    }
                }
                resourceCache.put(l, values);
            }
            return values;
        }

        public synchronized void addResourceBundle(String bundleName) {
            if( bundleName == null ) {
                return;
            }
            if( resourceBundles == null ) {
                resourceBundles = new Vector<String>(5);
            }
            if (!resourceBundles.contains(bundleName)) {
                resourceBundles.add( bundleName );
                resourceCache.clear();
            }
        }
        
        public synchronized void removeResourceBundle( String bundleName ) {
            if( resourceBundles != null ) {
                resourceBundles.remove( bundleName );
            }
            resourceCache.clear();
        }
    }
    
    private static UIDefaultsExt uiDefaultsExt = new UIDefaultsExt();
    
    private UIManagerExt() {
        //does nothing
    }
    
    /**
     * Adds a resource bundle to the list of resource bundles that are searched
     * for localized values. Resource bundles are searched in the reverse order
     * they were added. In other words, the most recently added bundle is
     * searched first.
     * 
     * @param bundleName
     *                the base name of the resource bundle to be added
     * @see ResourceBundle
     * @see #removeResourceBundle
     */
    public static void addResourceBundle(String bundleName) {
        uiDefaultsExt.addResourceBundle(bundleName);
    }
    
    /**
     * Removes a resource bundle from the list of resource bundles that are
     * searched for localized defaults.
     * 
     * @param bundleName
     *                the base name of the resource bundle to be removed
     * @see ResourceBundle
     * @see #addResourceBundle
     */
    public static void removeResourceBundle(String bundleName) {
        uiDefaultsExt.removeResourceBundle(bundleName);
    }

    /**
     * Returns a string from the defaults. If the value for {@code key} is not a
     * {@code String}, {@code null} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the string
     * @return the {@code String} object
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static String getString(Object key) {
        return getString(key, null);
    }
    
    /**
     * Returns a string from the defaults. If the value for {@code key} is not a
     * {@code String}, {@code null} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the string
     * @param l
     *                the {@code Locale} for which the string is desired; refer
     *                to {@code UIDefaults} for details on how a {@code null}
     *                {@code Locale} is handled
     * @return the {@code String} object
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static String getString(Object key, Locale l) {
        Object value = UIManager.get(key, l);
        
        if (value instanceof String) {
            return (String) value;
        }
        
        //only return resource bundle if not in UIDefaults
        if (value == null) {
            value = uiDefaultsExt.getFromResourceBundle(key, l);
            
            if (value instanceof String) {
                return (String) value;
            }
        }
        
        return null;
    }
    
    /**
     * Returns an integer from the defaults. If the value for {@code key} is not
     * an {@code int}, {@code 0} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the integer
     * @return the {@code int}
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static int getInt(Object key) {
        return getInt(key, null);
    }
    
    /**
     * Returns an integer from the defaults. If the value for {@code key} is not
     * an {@code int}, {@code 0} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the integer
     * @param l
     *                the {@code Locale} for which the integer is desired; refer
     *                to {@code UIDefaults} for details on how a {@code null}
     *                {@code Locale} is handled
     * @return the {@code int}
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static int getInt(Object key, Locale l) {
        Object value = UIManager.get(key, l);
        
        if (value instanceof Integer) {
            return (Integer) value;
        }
        
        if (value == null) {
            value = uiDefaultsExt.getFromResourceBundle(key, l);
            
            if (value instanceof Integer) {
                return (Integer) value;
            }
            
            if (value instanceof String) {
                try {
                    return Integer.decode((String) value);
                } catch (NumberFormatException e) {
                    // ignore - the entry was not parseable, can't do anything
                    // JW: should we log it?
                }
            }
        }
        
        return 0;
    }
    
    /**
     * Returns an Boolean from the defaults. If the value for {@code key} is not
     * a {@code boolean}, {@code false} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the Boolean
     * @return the {@code boolean}
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static boolean getBoolean(Object key) {
        return getBoolean(key, null);
    }
    
    /**
     * Returns an Boolean from the defaults. If the value for {@code key} is not
     * a {@code boolean}, {@code false} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the Boolean
     * @param l
     *                the {@code Locale} for which the Boolean is desired; refer
     *                to {@code UIDefaults} for details on how a {@code null}
     *                {@code Locale} is handled
     * @return the {@code boolean}
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static boolean getBoolean(Object key, Locale l) {
        Object value = UIManager.get(key, l);
        
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        
        //only return resource bundle if not in UIDefaults
        if (value == null) {
            value = uiDefaultsExt.getFromResourceBundle(key, l);
            
            if (value instanceof Boolean) {
                return (Boolean) value;
            }
            
            if (value instanceof String) {
                return Boolean.valueOf((String) value);
            }
        }
        
        return false;
    }
    
    /**
     * Returns a color from the defaults. If the value for {@code key} is not
     * a {@code Color}, {@code null} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the color
     * @return the {@code Color} object
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static Color getColor(Object key) {
        return getColor(key, null);
    }
    
    /**
     * Returns a color from the defaults. If the value for {@code key} is not
     * a {@code Color}, {@code null} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the color
     * @param l
     *                the {@code Locale} for which the color is desired; refer
     *                to {@code UIDefaults} for details on how a {@code null}
     *                {@code Locale} is handled
     * @return the {@code Color} object
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static Color getColor(Object key, Locale l) {
        Object value = UIManager.get(key, l);
        
        if (value instanceof Color) {
            return (Color) value;
        }
        
        //only return resource bundle if not in UIDefaults
        if (value == null) {
            value = uiDefaultsExt.getFromResourceBundle(key, l);
            
            if (value instanceof Color) {
                return (Color) value;
            }
            
            if (value instanceof String) {
                try {
                    return Color.decode((String) value);
                } catch (NumberFormatException e) {
                    // incorrect format; does nothing
                }
            }
        }
        
        return null;
    }

    //TODO: Font.decode always returns a valid font.  This is not acceptable for UIManager
//    /**
//     * Returns a font from the defaults. If the value for {@code key} is not
//     * a {@code Font}, {@code null} is returned.
//     * 
//     * @param key
//     *                an {@code Object} specifying the font
//     * @return the {@code Font} object
//     * @throws NullPointerException
//     *                 if {@code key} is {@code null}
//     */
//    public static Font getFont(Object key) {
//        return getFont(key, null);
//    }
//    
//    /**
//     * Returns a font from the defaults. If the value for {@code key} is not
//     * a {@code Font}, {@code null} is returned.
//     * 
//     * @param key
//     *                an {@code Object} specifying the font
//     * @param l
//     *                the {@code Locale} for which the font is desired; refer
//     *                to {@code UIDefaults} for details on how a {@code null}
//     *                {@code Locale} is handled
//     * @return the {@code Font} object
//     * @throws NullPointerException
//     *                 if {@code key} is {@code null}
//     */
//    public static Font getFont(Object key, Locale l) {
//        Object value = UIManager.get(key, l);
//        
//        if (value instanceof Font) {
//            return (Font) value;
//        }
//        
//        //only return resource bundle if not in UIDefaults
//        if (value == null) {
//            value = uiDefaultsExt.getFromResourceBundle(key, l);
//            
//            if (value instanceof Font) {
//                return (Font) value;
//            }
//            
//            if (value instanceof String) {
//                return Font.decode((String) value);
//            }
//        }
//        
//        return null;
//    }
    
    /**
     * Returns a shape from the defaults. If the value for {@code key} is not a
     * {@code Shape}, {@code null} is returned.
     * 
     * @param key an {@code Object} specifying the shape
     * @return the {@code Shape} object
     * @throws NullPointerException if {@code key} is {@code null}
     */
    public static Shape getShape(Object key) {
        Object value = UIManager.getDefaults().get(key);
        return (value instanceof Shape) ? (Shape) value : null;
    }
    
    /**
     * Returns a shape from the defaults that is appropriate for the given
     * locale. If the value for {@code key} is not a {@code Shape},
     * {@code null} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the shape
     * @param l
     *                the {@code Locale} for which the shape is desired; refer
     *                to {@code UIDefaults} for details on how a {@code null}
     *                {@code Locale} is handled
     * @return the {@code Shape} object
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static Shape getShape(Object key, Locale l) {
        Object value = UIManager.getDefaults().get(key, l);
        return (value instanceof Shape) ? (Shape) value : null;
    }
    
    /**
     * Returns a painter from the defaults. If the value for {@code key} is not
     * a {@code Painter}, {@code null} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the painter
     * @return the {@code Painter} object
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static Painter<?> getPainter(Object key) {
        Object value = UIManager.getDefaults().get(key);
        return (value instanceof Painter<?>) ? (Painter<?>) value : null;
    }
    
    /**
     * Returns a painter from the defaults that is appropriate for the given
     * locale. If the value for {@code key} is not a {@code Painter},
     * {@code null} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the painter
     * @param l
     *                the {@code Locale} for which the painter is desired; refer
     *                to {@code UIDefaults} for details on how a {@code null}
     *                {@code Locale} is handled
     * @return the {@code Painter} object
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     */
    public static Painter<?> getPainter(Object key, Locale l) {
        Object value = UIManager.getDefaults().get(key, l);
        return (value instanceof Painter<?>) ? (Painter<?>) value : null;
    }
    
    /**
     * Returns a border from the defaults. If the value for {@code key} is not a
     * {@code Border}, {@code defaultBorder} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the border
     * @param defaultBorder
     *                the border to return if the border specified by
     *                {@code key} does not exist
     * @return the {@code Border} object
     * @throws NullPointerException
     *                 if {@code key} or {@code defaultBorder} is {@code null}
     */
    public static Border getSafeBorder(Object key, Border defaultBorder) {
        Contract.asNotNull(defaultBorder, "defaultBorder cannot be null");
        
        Border safeBorder = UIManager.getBorder(key);
        
        if (safeBorder == null) {
            safeBorder = defaultBorder;
        }
        
        if (!(safeBorder instanceof UIResource)) {
            safeBorder = new BorderUIResource(safeBorder);
        }
        
        return safeBorder;
    }
    
    /**
     * Returns a color from the defaults. If the value for {@code key} is not a
     * {@code Color}, {@code defaultColor} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the color
     * @param defaultColor
     *                the color to return if the color specified by {@code key}
     *                does not exist
     * @return the {@code Color} object
     * @throws NullPointerException
     *                 if {@code key} or {@code defaultColor} is {@code null}
     */
    public static Color getSafeColor(Object key, Color defaultColor) {
        Contract.asNotNull(defaultColor, "defaultColor cannot be null");
        
        Color safeColor = UIManager.getColor(key);
        
        if (safeColor == null) {
            safeColor = defaultColor;
        }
        
        if (!(safeColor instanceof UIResource)) {
            safeColor = new ColorUIResource(safeColor);
        }
        
        return safeColor;
    }
    
    /**
     * Returns a dimension from the defaults. If the value for {@code key} is
     * not a {@code Dimension}, {@code defaultDimension} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the dimension
     * @param defaultDimension
     *                the dimension to return if the dimension specified by
     *                {@code key} does not exist
     * @return the {@code Dimension} object
     * @throws NullPointerException
     *                 if {@code key} or {@code defaultColor} is {@code null}
     */
    public static Dimension getSafeDimension(Object key, Dimension defaultDimension) {
        Contract.asNotNull(defaultDimension, "defaultDimension cannot be null");
        
        Dimension safeDimension = UIManager.getDimension(key);
        
        if (safeDimension == null) {
            safeDimension = defaultDimension;
        }
        
        if (!(safeDimension instanceof UIResource)) {
            safeDimension = new DimensionUIResource(safeDimension.width, safeDimension.height);
        }
        
        return safeDimension;
    }
    
    /**
     * Returns a font from the defaults. If the value for {@code key} is not a
     * {@code Font}, {@code defaultFont} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the font
     * @param defaultFont
     *                the font to return if the font specified by {@code key}
     *                does not exist
     * @return the {@code Font} object
     * @throws NullPointerException
     *                 if {@code key} or {@code defaultFont} is {@code null}
     */
    public static Font getSafeFont(Object key, Font defaultFont) {
        Contract.asNotNull(defaultFont, "defaultFont cannot be null");
        
        Font safeFont = UIManager.getFont(key);
        
        if (safeFont == null) {
            safeFont = defaultFont;
        }
        
        if (!(safeFont instanceof UIResource)) {
            safeFont = new FontUIResource(safeFont);
        }
        
        return safeFont;
    }
    
    /**
     * Returns an icon from the defaults. If the value for {@code key} is not a
     * {@code Icon}, {@code defaultIcon} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the icon
     * @param defaultIcon
     *                the icon to return if the icon specified by {@code key}
     *                does not exist
     * @return the {@code Icon} object
     * @throws NullPointerException
     *                 if {@code key} or {@code defaultIcon} is {@code null}
     */
    public static Icon getSafeIcon(Object key, Icon defaultIcon) {
        Contract.asNotNull(defaultIcon, "defaultIcon cannot be null");
        
        Icon safeIcon = UIManager.getIcon(key);
        
        if (safeIcon == null) {
            safeIcon = defaultIcon;
        }
        
        if (!(safeIcon instanceof UIResource)) {
            safeIcon = new IconUIResource(safeIcon);
        }
        
        return safeIcon;
    }
    
    /**
     * Returns an insets from the defaults. If the value for {@code key} is not
     * a {@code Insets}, {@code defaultInsets} is returned.
     * 
     * @param key
     *                an {@code Object} specifying the insets
     * @param defaultInsets
     *                the insets to return if the insets specified by
     *                {@code key} does not exist
     * @return the {@code Insets} object
     * @throws NullPointerException
     *                 if {@code key} or {@code defaultInsets} is {@code null}
     */
    public static Insets getSafeInsets(Object key, Insets defaultInsets) {
        Contract.asNotNull(defaultInsets, "defaultInsets cannot be null");
        
        Insets safeInsets = UIManager.getInsets(key);
        
        if (safeInsets == null) {
            safeInsets = defaultInsets;
        }
        
        if (!(safeInsets instanceof UIResource)) {
            safeInsets = new InsetsUIResource(safeInsets.top, safeInsets.left,
                    safeInsets.bottom, safeInsets.right);
        }
        
        return safeInsets;
    }
}
