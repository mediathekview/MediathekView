/*
 * @(#)SecurityUtils.java
 *
 * Copyright 2002 - 2004 JIDE Software Inc. All rights reserved.
 */
package com.jidesoft.utils;

import javax.swing.plaf.FontUIResource;
import java.awt.*;
import java.io.IOException;
import java.security.AccessControlException;
import java.util.Locale;
import java.util.MissingResourceException;

/**
 * A class that keeps all the security stuff so that an application can safely run in applet or webstart environment.
 * Please refer to JIDE_Developer_Guide_for_Webstart_Applet.pdf in doc folder for more information.
 */
public class SecurityUtils {
    public static FontUIResource createFontUIResource(String name, int style, int size) {
        Font font = createFont(name, style, size);
        if (font != null) {
            return new FontUIResource(font);
        }
        else {
            return null;
        }
    }

    static class FontStruct {
        String font;
        int style;
    }

    public static final String BOLD = "Bold";
    public static final String ITALIC = "Italic";
    public static final String BOLD_ITALIC = "Bold Italic";

    private static String createFontStrings(String font, int style) {
        String fontString;
        switch (style) {
            case Font.BOLD:
                fontString = font + " " + BOLD;
                break;
            case Font.ITALIC:
                fontString = font + " " + ITALIC;
                break;
            case Font.BOLD | Font.ITALIC:
                fontString = font + " " + BOLD_ITALIC;
                break;
            case Font.PLAIN:
            default:
                fontString = font;
                break;
        }
        return fontString.replace(' ', '_');
    }

    /**
     * Creates font. If there is no permission to access font file, it will try to create the font directly from font
     * file that is bundled as part of jar.
     *
     * @param name  the font name.
     * @param style the font style.
     * @param size  the font size.
     * @return the font.
     */
    public static Font createFont(String name, int style, int size) {
        try {
//            System.out.println("new Font");
            return new Font(name, style, size);
        }
        catch (AccessControlException e) {
//            System.out.println("new Font failed " + createFontStrings(name, style));
            ClassLoader cl = SecurityUtils.class.getClassLoader();
            try {
                String value = null;
                try {
                    value = FontFilesResource.getResourceBundle(Locale.getDefault()).getString(createFontStrings(name, style));
                }
                catch (MissingResourceException me1) {
                    try {
                        value = FontFilesResource.getResourceBundle(Locale.getDefault()).getString(name);
                    }
                    catch (MissingResourceException me2) {
                        // ignore
                    }
                }
                if (value == null) {
                    return null;
                }
                else {
//                    System.out.print("createFont " + value);
                    Font font = Font.createFont(Font.TRUETYPE_FONT, cl.getResourceAsStream(value));
//                    System.out.println("successful " + font);
                    if (font != null) {
                        return font.deriveFont(style, size);
                    }
                }
            }
            catch (FontFormatException e1) {
                e1.printStackTrace();
                throw e;
            }
            catch (IOException e1) {
                e1.printStackTrace();
                throw e;
            }
        }
        return null;
    }


    /**
     * Gets the system property.
     *
     * @param key          the property key
     * @param defaultValue the default value for the property.
     * @return the system property.
     */
    public static String getProperty(String key, String defaultValue) {
        try {
            return System.getProperty(key, defaultValue);
        }
        catch (AccessControlException e) {
            return defaultValue;
        }
    }

    private static boolean _AWTEventListenerDisabled = false;

    /**
     * Checks if AWTEventListener is disabled. This flag can be set by user. If false, JIDE code will read the value and
     * not use AWTEventListener. The reason we need this flag is because AWTEventListener needs a special security
     * permission. If applet, it will throw security if the user policy doesn't have the correct permission.
     *
     * @return true if AWTEventListener is disabled.
     */
    public static boolean isAWTEventListenerDisabled() {
        return _AWTEventListenerDisabled;
    }

    /**
     * Enables or disables the usage of AWTEventListener. If you want to change it, you should change the value at the
     * beginning of your main method.
     *
     * @param AWTEventListenerDisabled true or false.
     */
    public static void setAWTEventListenerDisabled(boolean AWTEventListenerDisabled) {
        _AWTEventListenerDisabled = AWTEventListenerDisabled;
    }

    private static boolean _translucentWindowFeatureDisabled = !SystemInfo.isJdk6u10Above() || !SystemInfo.isWindows();

    /**
     * Checks if the translucent window feature is disabled. It is disabled by default if the JDK version is less than
     * JDK6 u10 or theOS is not Windows.
     *
     * @return true or false.
     */
    public static boolean isTranslucentWindowFeatureDisabled() {
        return _translucentWindowFeatureDisabled;
    }

    /**
     * Disables or enables the usage of the translucent window feature available since JDK6u10. This feature is used in
     * Alert for fading animation.
     *
     * @param translucentWindowFeatureDisabled
     *         true or false.
     */
    public static void setTranslucentWindowFeatureDisabled(boolean translucentWindowFeatureDisabled) {
        _translucentWindowFeatureDisabled = translucentWindowFeatureDisabled;
    }
}
