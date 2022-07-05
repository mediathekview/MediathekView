/*
 * $Id: Utilities.java 4084 2011-11-15 18:53:49Z kschaefe $
 *
 * Copyright 2006 Sun Microsystems, Inc., 4150 Network Circle,
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
package org.jdesktop.swingx.util;


import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.text.BreakIterator;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Contribution from NetBeans: Issue #319-swingx. <p>
 * 
 * PENDING: need to reconcile with OS, JVM... added as-is
 * because needed the shortcut handling to fix #
 *
 * @author apple
 */
public class Utilities {
    private Utilities() {
    }
    
    private static final int CTRL_WILDCARD_MASK = 32768;
    private static final int ALT_WILDCARD_MASK = CTRL_WILDCARD_MASK * 2;
    
    /** Operating system is Windows NT. */
    public static final int OS_WINNT = 1;

    /** Operating system is Windows 95. */
    public static final int OS_WIN95 = OS_WINNT << 1;

    /** Operating system is Windows 98. */
    public static final int OS_WIN98 = OS_WIN95 << 1;

    /** Operating system is Solaris. */
    public static final int OS_SOLARIS = OS_WIN98 << 1;

    /** Operating system is Linux. */
    public static final int OS_LINUX = OS_SOLARIS << 1;

    /** Operating system is HP-UX. */
    public static final int OS_HP = OS_LINUX << 1;

    /** Operating system is IBM AIX. */
    public static final int OS_AIX = OS_HP << 1;

    /** Operating system is SGI IRIX. */
    public static final int OS_IRIX = OS_AIX << 1;

    /** Operating system is Sun OS. */
    public static final int OS_SUNOS = OS_IRIX << 1;

    /** Operating system is Compaq TRU64 Unix */
    public static final int OS_TRU64 = OS_SUNOS << 1;

    /** Operating system is OS/2. */
    public static final int OS_OS2 = OS_TRU64 << 2;

    /** Operating system is Mac. */
    public static final int OS_MAC = OS_OS2 << 1;

    /** Operating system is Windows 2000. */
    public static final int OS_WIN2000 = OS_MAC << 1;

    /** Operating system is Compaq OpenVMS */
    public static final int OS_VMS = OS_WIN2000 << 1;

    /**
     *Operating system is one of the Windows variants but we don't know which
     *one it is
     */
    public static final int OS_WIN_OTHER = OS_VMS << 1;

    /** Operating system is unknown. */
    public static final int OS_OTHER = OS_WIN_OTHER << 1;

    /** Operating system is FreeBSD
     * @since 4.50
     */
    public static final int OS_FREEBSD = OS_OTHER << 1;

    /** A mask for Windows platforms. */
    public static final int OS_WINDOWS_MASK = OS_WINNT | OS_WIN95 | OS_WIN98 | OS_WIN2000 | OS_WIN_OTHER;

    /** A mask for Unix platforms. */
    public static final int OS_UNIX_MASK = OS_SOLARIS | OS_LINUX | OS_HP | OS_AIX | OS_IRIX | OS_SUNOS | OS_TRU64 |
        OS_MAC | OS_FREEBSD;

    /** A height of the windows's taskbar */
    public static final int TYPICAL_WINDOWS_TASKBAR_HEIGHT = 27;

    /** A height of the Mac OS X's menu */
    private static final int TYPICAL_MACOSX_MENU_HEIGHT = 24;
    
    private static int operatingSystem = -1;
    
    /** reference to map that maps allowed key names to their values (String, Integer)
    and reference to map for mapping of values to their names */
    private static Reference<Object> namesAndValues;

    /** Get the operating system on which NetBeans is running.
    * @return one of the <code>OS_*</code> constants (such as {@link #OS_WINNT})
    */
    public static int getOperatingSystem() {
        if (operatingSystem == -1) {
            String osName = System.getProperty("os.name");

            if ("Windows NT".equals(osName)) { // NOI18N
                operatingSystem = OS_WINNT;
            } else if ("Windows 95".equals(osName)) { // NOI18N
                operatingSystem = OS_WIN95;
            } else if ("Windows 98".equals(osName)) { // NOI18N
                operatingSystem = OS_WIN98;
            } else if ("Windows 2000".equals(osName)) { // NOI18N
                operatingSystem = OS_WIN2000;
            } else if (osName.startsWith("Windows ")) { // NOI18N
                operatingSystem = OS_WIN_OTHER;
            } else if ("Solaris".equals(osName)) { // NOI18N
                operatingSystem = OS_SOLARIS;
            } else if (osName.startsWith("SunOS")) { // NOI18N
                operatingSystem = OS_SOLARIS;
            }
            // JDK 1.4 b2 defines os.name for me as "Redhat Linux" -jglick
            else if (osName.endsWith("Linux")) { // NOI18N
                operatingSystem = OS_LINUX;
            } else if ("HP-UX".equals(osName)) { // NOI18N
                operatingSystem = OS_HP;
            } else if ("AIX".equals(osName)) { // NOI18N
                operatingSystem = OS_AIX;
            } else if ("Irix".equals(osName)) { // NOI18N
                operatingSystem = OS_IRIX;
            } else if ("SunOS".equals(osName)) { // NOI18N
                operatingSystem = OS_SUNOS;
            } else if ("Digital UNIX".equals(osName)) { // NOI18N
                operatingSystem = OS_TRU64;
            } else if ("OS/2".equals(osName)) { // NOI18N
                operatingSystem = OS_OS2;
            } else if ("OpenVMS".equals(osName)) { // NOI18N
                operatingSystem = OS_VMS;
            } else if (osName.equals("Mac OS X")) { // NOI18N
                operatingSystem = OS_MAC;
            } else if (osName.startsWith("Darwin")) { // NOI18N
                operatingSystem = OS_MAC;
            } else if (osName.toLowerCase(Locale.US).startsWith("freebsd")) { // NOI18N 
                operatingSystem = OS_FREEBSD;
            } else {
                operatingSystem = OS_OTHER;
            }
        }
        return operatingSystem;
    }
    
    /** Test whether NetBeans is running on some variant of Windows.
    * @return <code>true</code> if Windows, <code>false</code> if some other manner of operating system
    */
    public static boolean isWindows() {
        return (getOperatingSystem() & OS_WINDOWS_MASK) != 0;
    }

    /** Test whether NetBeans is running on some variant of Unix.
    * Linux is included as well as the commercial vendors, and Mac OS X.
    * @return <code>true</code> some sort of Unix, <code>false</code> if some other manner of operating system
    */
    public static boolean isUnix() {
        return (getOperatingSystem() & OS_UNIX_MASK) != 0;
    }
    
    /** Test whether the operating system supports icons on frames (windows).
    * @return <code>true</code> if it does <em>not</em>
    *
    */
    public static boolean isLargeFrameIcons() {
        return (getOperatingSystem() == OS_SOLARIS) || (getOperatingSystem() == OS_HP);
    }

    /**
     * Finds out the monitor where the user currently has the input focus.
     * This method is usually used to help the client code to figure out on
     * which monitor it should place newly created windows/frames/dialogs.
     *
     * @return the GraphicsConfiguration of the monitor which currently has the
     * input focus
     */
    private static GraphicsConfiguration getCurrentGraphicsConfiguration() {
        Component focusOwner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
        if (focusOwner != null) {
            Window w = SwingUtilities.getWindowAncestor(focusOwner);
            if (w != null) {
                return w.getGraphicsConfiguration();
            }
        }

        return GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
    }

    /**
     * Returns the usable area of the screen where applications can place its
     * windows.  The method subtracts from the screen the area of taskbars,
     * system menus and the like.  The screen this method applies to is the one
     * which is considered current, ussually the one where the current input
     * focus is.
     *
     * @return the rectangle of the screen where one can place windows
     *
     * @since 2.5
     */
    public static Rectangle getUsableScreenBounds() {
        return getUsableScreenBounds(getCurrentGraphicsConfiguration());
    }

    /**
     * Returns the usable area of the screen where applications can place its
     * windows.  The method subtracts from the screen the area of taskbars,
     * system menus and the like.
     *
     * @param gconf the GraphicsConfiguration of the monitor
     * @return the rectangle of the screen where one can place windows
     *
     * @since 2.5
     */
    public static Rectangle getUsableScreenBounds(GraphicsConfiguration gconf) {
        if (gconf == null) {
            gconf = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
        }

        Rectangle bounds = new Rectangle(gconf.getBounds());

        String str;

        str = System.getProperty("netbeans.screen.insets"); // NOI18N

        if (str != null) {
            StringTokenizer st = new StringTokenizer(str, ", "); // NOI18N

            if (st.countTokens() == 4) {
                try {
                    bounds.y = Integer.parseInt(st.nextToken());
                    bounds.x = Integer.parseInt(st.nextToken());
                    bounds.height -= (bounds.y + Integer.parseInt(st.nextToken()));
                    bounds.width -= (bounds.x + Integer.parseInt(st.nextToken()));
                } catch (NumberFormatException ex) {
                    Logger.getAnonymousLogger().log(Level.WARNING, null, ex);
                }
            }

            return bounds;
        }

        str = System.getProperty("netbeans.taskbar.height"); // NOI18N

        if (str != null) {
            bounds.height -= Integer.getInteger(str, 0);

            return bounds;
        }

        try {
            Toolkit toolkit = Toolkit.getDefaultToolkit();
            Insets insets = toolkit.getScreenInsets(gconf);
            bounds.y += insets.top;
            bounds.x += insets.left;
            bounds.height -= (insets.top + insets.bottom);
            bounds.width -= (insets.left + insets.right);
        } catch (Exception ex) {
            Logger.getAnonymousLogger().log(Level.WARNING, null, ex);
        }

        return bounds;
    }
    

    /** Initialization of the names and values
    * @return array of two hashmaps first maps
    *   allowed key names to their values (String, Integer)
    *  and second
    * hashtable for mapping of values to their names (Integer, String)
    */
    private static synchronized HashMap[] initNameAndValues() {
        if (namesAndValues != null) {
            HashMap[] arr = (HashMap[]) namesAndValues.get();

            if (arr != null) {
                return arr;
            }
        }

        Field[] fields;
        // JW - fix Issue #353-swingx: play nicer inside sandbox.
        try {
            fields = KeyEvent.class.getDeclaredFields();
//           fields = KeyEvent.class.getFields();
        } catch (SecurityException e) { 
            // JW: need to do better? What are the use-cases where we don't have
            // any access to the fields?
            fields = new Field[0];
        }

        HashMap<String,Integer> names = new HashMap<String,Integer>(((fields.length * 4) / 3) + 5, 0.75f);
        HashMap<Integer,String> values = new HashMap<Integer,String>(((fields.length * 4) / 3) + 5, 0.75f);

        for (int i = 0; i < fields.length; i++) {
            if (Modifier.isStatic(fields[i].getModifiers())) {
                String name = fields[i].getName();

                if (name.startsWith("VK_")) { // NOI18N

                    // exclude VK
                    name = name.substring(3);

                    try {
                        int numb = fields[i].getInt(null);
                        Integer value = numb;
                        names.put(name, value);
                        values.put(value, name);
                    } catch (IllegalArgumentException ex) {
                    } catch (IllegalAccessException ex) {
                    }
                }
            }
        }

        if (names.get("CONTEXT_MENU") == null) { // NOI18N

            Integer n = 0x20C;
            names.put("CONTEXT_MENU", n); // NOI18N
            values.put(n, "CONTEXT_MENU"); // NOI18N

            n = 0x20D;
            names.put("WINDOWS", n); // NOI18N
            values.put(n, "WINDOWS"); // NOI18N
        }

        HashMap[] arr = { names, values };

        namesAndValues = new SoftReference<Object>(arr);

        return arr;
    }

    /** Converts a Swing key stroke descriptor to a familiar Emacs-like name.
    * @param stroke key description
    * @return name of the key (e.g. <code>CS-F1</code> for control-shift-function key one)
    * @see #stringToKey
    */
    public static String keyToString(KeyStroke stroke) {
        StringBuffer sb = new StringBuffer();

        // add modifiers that must be pressed
        if (addModifiers(sb, stroke.getModifiers())) {
            sb.append('-');
        }

        HashMap[] namesAndValues = initNameAndValues();

        String c = (String) namesAndValues[1].get(stroke.getKeyCode());

        if (c == null) {
            sb.append(stroke.getKeyChar());
        } else {
            sb.append(c);
        }

        return sb.toString();
    }

    /** Construct a new key description from a given universal string
    * description.
    * Provides mapping between Emacs-like textual key descriptions and the
    * <code>KeyStroke</code> object used in Swing.
    * <P>
    * This format has following form:
    * <P><code>[C][A][S][M]-<em>identifier</em></code>
    * <p>Where:
    * <UL>
    * <LI> <code>C</code> stands for the Control key
    * <LI> <code>A</code> stands for the Alt key
    * <LI> <code>S</code> stands for the Shift key
    * <LI> <code>M</code> stands for the Meta key
    * </UL>
    * The format also supports two wildcard codes, to support differences in
    * platforms.  These are the preferred choices for registering keystrokes,
    * since platform conflicts will automatically be handled:
    * <UL>
    * <LI> <code>D</code> stands for the default menu accelerator - the Control
    *  key on most platforms, the Command (meta) key on Macintosh</LI>
    * <LI> <code>O</code> stands for the alternate accelerator - the Alt key on
    *  most platforms, the Ctrl key on Macintosh (Macintosh uses Alt as a
    *  secondary shift key for composing international characters - if you bind
    *  Alt-8 to an action, a mac user with a French keyboard will not be able
    *  to type the <code>[</code> character, which is a significant handicap</LI>
    * </UL>
    * If you use the wildcard characters, and specify a key which will conflict
    * with keys the operating system consumes, it will be mapped to whichever
    * choice can work - for example, on Macintosh, Command-Q is always consumed
    * by the operating system, so <code>D-Q</code> will always map to Control-Q.
    * <p>
    * Every modifier before the hyphen must be pressed.
    * <em>identifier</EM> can be any text constant from {@link KeyEvent} but
    * without the leading <code>VK_</code> characters. So {@link KeyEvent#VK_ENTER} is described as
    * <code>ENTER</code>.
    *
    * @param s the string with the description of the key
    * @return key description object, or <code>null</code> if the string does not represent any valid key
    */
    public static KeyStroke stringToKey(String s) {
        StringTokenizer st = new StringTokenizer(s.toUpperCase(Locale.ENGLISH), "-", true); // NOI18N

        int needed = 0;

        HashMap names = initNameAndValues()[0];

        int lastModif = -1;

        try {
            for (;;) {
                String el = st.nextToken();

                // required key
                if (el.equals("-")) { // NOI18N

                    if (lastModif != -1) {
                        needed |= lastModif;
                        lastModif = -1;
                    }

                    continue;
                }

                // if there is more elements
                if (st.hasMoreElements()) {
                    // the text should describe modifiers
                    lastModif = readModifiers(el);
                } else {
                    // last text must be the key code
                    Integer i = (Integer) names.get(el);
                    boolean wildcard = (needed & CTRL_WILDCARD_MASK) != 0;

                    //Strip out the explicit mask - KeyStroke won't know
                    //what to do with it
                    needed = needed & ~CTRL_WILDCARD_MASK;

                    boolean macAlt = (needed & ALT_WILDCARD_MASK) != 0;
                    needed = needed & ~ALT_WILDCARD_MASK;

                    if (i != null) {
                        //#26854 - Default accelerator should be Command on mac
                        if (wildcard) {
                            needed |= getMenuShortCutKeyMask();

                            if ((getOperatingSystem() & OS_MAC) != 0) {
                                if (!usableKeyOnMac(i.intValue(), needed)) {
                                    needed &= ~getMenuShortCutKeyMask();
                                    needed |= KeyEvent.CTRL_MASK;
                                }
                            }
                        }

                        if (macAlt) {
                            if (getOperatingSystem() == OS_MAC) {
                                needed |= KeyEvent.CTRL_MASK;
                            } else {
                                needed |= KeyEvent.ALT_MASK;
                            }
                        }

                        return KeyStroke.getKeyStroke(i.intValue(), needed);
                    } else {
                        return null;
                    }
                }
            }
        } catch (NoSuchElementException ex) {
            return null;
        }
    }
    /**
     * need to guard against headlessExceptions when testing.
     * @return the acceletor mask for shortcuts.
     */
    private static int getMenuShortCutKeyMask() {
        if (GraphicsEnvironment.isHeadless()) {
            return ((getOperatingSystem() & OS_MAC) != 0) ? 
                    KeyEvent.META_MASK : KeyEvent.CTRL_MASK;
        }
 
        return Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
    }

    private static boolean usableKeyOnMac(int key, int mask) {
        //All permutations fail for Q except ctrl
        if (key == KeyEvent.VK_Q) {
            return false;
        }

        boolean isMeta = ((mask & KeyEvent.META_MASK) != 0) || ((mask & KeyEvent.CTRL_DOWN_MASK) != 0);

        boolean isAlt = ((mask & KeyEvent.ALT_MASK) != 0) || ((mask & KeyEvent.ALT_DOWN_MASK) != 0);

        boolean isOnlyMeta = isMeta && ((mask & ~(KeyEvent.META_DOWN_MASK | KeyEvent.META_MASK)) == 0);

        //Mac OS consumes keys Command+ these keys - the app will never see
        //them, so CTRL should not be remapped for these
        if (isOnlyMeta) {
            return (key != KeyEvent.VK_H) && (key != KeyEvent.VK_SPACE) && (key != KeyEvent.VK_TAB);
        } else return !((key == KeyEvent.VK_D) && isMeta && isAlt);
    }

    /** Convert a space-separated list of Emacs-like key binding names to a list of Swing key strokes.
    * @param s the string with keys
    * @return array of key strokes, or <code>null</code> if the string description is not valid
    * @see #stringToKey
    */
    public static KeyStroke[] stringToKeys(String s) {
        StringTokenizer st = new StringTokenizer(s.toUpperCase(Locale.ENGLISH), " "); // NOI18N
        ArrayList<KeyStroke> arr = new ArrayList<KeyStroke>();

        while (st.hasMoreElements()) {
            s = st.nextToken();

            KeyStroke k = stringToKey(s);

            if (k == null) {
                return null;
            }

            arr.add(k);
        }

        return arr.toArray(new KeyStroke[arr.size()]);
    }

    /** Adds characters for modifiers to the buffer.
    * @param buf buffer to add to
    * @param modif modifiers to add (KeyEvent.XXX_MASK)
    * @return true if something has been added
    */
    private static boolean addModifiers(StringBuffer buf, int modif) {
        boolean b = false;

        if ((modif & KeyEvent.CTRL_MASK) != 0) {
            buf.append("C"); // NOI18N
            b = true;
        }

        if ((modif & KeyEvent.ALT_MASK) != 0) {
            buf.append("A"); // NOI18N
            b = true;
        }

        if ((modif & KeyEvent.SHIFT_MASK) != 0) {
            buf.append("S"); // NOI18N
            b = true;
        }

        if ((modif & KeyEvent.META_MASK) != 0) {
            buf.append("M"); // NOI18N
            b = true;
        }

        if ((modif & CTRL_WILDCARD_MASK) != 0) {
            buf.append("D");
            b = true;
        }

        if ((modif & ALT_WILDCARD_MASK) != 0) {
            buf.append("O");
            b = true;
        }

        return b;
    }

    /** Reads for modifiers and creates integer with required mask.
    * @param s string with modifiers
    * @return integer with mask
    * @exception NoSuchElementException if some letter is not modifier
    */
    private static int readModifiers(String s) throws NoSuchElementException {
        int m = 0;

        for (int i = 0; i < s.length(); i++) {
            switch (s.charAt(i)) {
            case 'C':
                m |= KeyEvent.CTRL_MASK;
                break;

            case 'A':
                m |= KeyEvent.ALT_MASK;
                break;

            case 'M':
                m |= KeyEvent.META_MASK;
                break;

            case 'S':
                m |= KeyEvent.SHIFT_MASK;
                break;

            case 'D':
                m |= CTRL_WILDCARD_MASK;
                break;

            case 'O':
                m |= ALT_WILDCARD_MASK;
                break;

            default:
                throw new NoSuchElementException(s);
            }
        }

        return m;
    }
    
    /**
    * Convert an array of objects to an array of primitive types.
    * E.g. an <code>Integer[]</code> would be changed to an <code>int[]</code>.
    * @param array the wrapper array
    * @return a primitive array
    * @throws IllegalArgumentException if the array element type is not a primitive wrapper
    */
    public static Object toPrimitiveArray(Object[] array) {
        if (array instanceof Integer[]) {
            int[] r = new int[array.length];
            int i;
            int k = array.length;

            for (i = 0; i < k; i++)
                r[i] = (array[i] == null) ? 0 : ((Integer) array[i]).intValue();

            return r;
        }

        if (array instanceof Boolean[]) {
            boolean[] r = new boolean[array.length];
            int i;
            int k = array.length;

            for (i = 0; i < k; i++)
                r[i] = (array[i] != null) && ((Boolean) array[i]).booleanValue();

            return r;
        }

        if (array instanceof Byte[]) {
            byte[] r = new byte[array.length];
            int i;
            int k = array.length;

            for (i = 0; i < k; i++)
                r[i] = (array[i] == null) ? 0 : ((Byte) array[i]).byteValue();

            return r;
        }

        if (array instanceof Character[]) {
            char[] r = new char[array.length];
            int i;
            int k = array.length;

            for (i = 0; i < k; i++)
                r[i] = (array[i] == null) ? 0 : ((Character) array[i]).charValue();

            return r;
        }

        if (array instanceof Double[]) {
            double[] r = new double[array.length];
            int i;
            int k = array.length;

            for (i = 0; i < k; i++)
                r[i] = (array[i] == null) ? 0 : ((Double) array[i]).doubleValue();

            return r;
        }

        if (array instanceof Float[]) {
            float[] r = new float[array.length];
            int i;
            int k = array.length;

            for (i = 0; i < k; i++)
                r[i] = (array[i] == null) ? 0 : ((Float) array[i]).floatValue();

            return r;
        }

        if (array instanceof Long[]) {
            long[] r = new long[array.length];
            int i;
            int k = array.length;

            for (i = 0; i < k; i++)
                r[i] = (array[i] == null) ? 0 : ((Long) array[i]).longValue();

            return r;
        }

        if (array instanceof Short[]) {
            short[] r = new short[array.length];
            int i;
            int k = array.length;

            for (i = 0; i < k; i++)
                r[i] = (array[i] == null) ? 0 : ((Short) array[i]).shortValue();

            return r;
        }

        throw new IllegalArgumentException();
    }
    
    /**
    * Convert an array of primitive types to an array of objects.
    * E.g. an <code>int[]</code> would be turned into an <code>Integer[]</code>.
    * @param array the primitive array
    * @return a wrapper array
    * @throws IllegalArgumentException if the array element type is not primitive
    */
    public static Object[] toObjectArray(Object array) {
        if (array instanceof Object[]) {
            return (Object[]) array;
        }

        if (array instanceof int[]) {
            int i;
            int k = ((int[]) array).length;
            Integer[] r = new Integer[k];

            for (i = 0; i < k; i++)
                r[i] = ((int[]) array)[i];

            return r;
        }

        if (array instanceof boolean[]) {
            int i;
            int k = ((boolean[]) array).length;
            Boolean[] r = new Boolean[k];

            for (i = 0; i < k; i++)
                r[i] = ((boolean[]) array)[i] ? Boolean.TRUE : Boolean.FALSE;

            return r;
        }

        if (array instanceof byte[]) {
            int i;
            int k = ((byte[]) array).length;
            Byte[] r = new Byte[k];

            for (i = 0; i < k; i++)
                r[i] = ((byte[]) array)[i];

            return r;
        }

        if (array instanceof char[]) {
            int i;
            int k = ((char[]) array).length;
            Character[] r = new Character[k];

            for (i = 0; i < k; i++)
                r[i] = ((char[]) array)[i];

            return r;
        }

        if (array instanceof double[]) {
            int i;
            int k = ((double[]) array).length;
            Double[] r = new Double[k];

            for (i = 0; i < k; i++)
                r[i] = ((double[]) array)[i];

            return r;
        }

        if (array instanceof float[]) {
            int i;
            int k = ((float[]) array).length;
            Float[] r = new Float[k];

            for (i = 0; i < k; i++)
                r[i] = ((float[]) array)[i];

            return r;
        }

        if (array instanceof long[]) {
            int i;
            int k = ((long[]) array).length;
            Long[] r = new Long[k];

            for (i = 0; i < k; i++)
                r[i] = ((long[]) array)[i];

            return r;
        }

        if (array instanceof short[]) {
            int i;
            int k = ((short[]) array).length;
            Short[] r = new Short[k];

            for (i = 0; i < k; i++)
                r[i] = ((short[]) array)[i];

            return r;
        }

        throw new IllegalArgumentException();
    }

    /** Wrap multi-line strings (and get the individual lines).
    * @param original  the original string to wrap
    * @param width     the maximum width of lines
    * @param breakIterator breaks original to chars, words, sentences, depending on what instance you provide.
    * @param removeNewLines if <code>true</code>, any newlines in the original string are ignored
    * @return the lines after wrapping
    */
    public static String[] wrapStringToArray(
        String original, int width, BreakIterator breakIterator, boolean removeNewLines
    ) {
        if (original.length() == 0) {
            return new String[] { original };
        }

        String[] workingSet;

        // substitute original newlines with spaces,
        // remove newlines from head and tail
        if (removeNewLines) {
            original = trimString(original);
            original = original.replace('\n', ' ');
            workingSet = new String[] { original };
        } else {
            StringTokenizer tokens = new StringTokenizer(original, "\n"); // NOI18N
            int len = tokens.countTokens();
            workingSet = new String[len];

            for (int i = 0; i < len; i++) {
                workingSet[i] = tokens.nextToken();
            }
        }

        if (width < 1) {
            width = 1;
        }

        if (original.length() <= width) {
            return workingSet;
        }

widthcheck:  {
            boolean ok = true;

            for (int i = 0; i < workingSet.length; i++) {
                ok = ok && (workingSet[i].length() < width);

                if (!ok) {
                    break widthcheck;
                }
            }

            return workingSet;
        }

        ArrayList<String> lines = new ArrayList<String>();

        int lineStart = 0; // the position of start of currently processed line in the original string

        for (int i = 0; i < workingSet.length; i++) {
            if (workingSet[i].length() < width) {
                lines.add(workingSet[i]);
            } else {
                breakIterator.setText(workingSet[i]);

                int nextStart = breakIterator.next();
                int prevStart = 0;

                do {
                    while (((nextStart - lineStart) < width) && (nextStart != BreakIterator.DONE)) {
                        prevStart = nextStart;
                        nextStart = breakIterator.next();
                    }

                    if (nextStart == BreakIterator.DONE) {
                        nextStart = prevStart = workingSet[i].length();
                    }

                    if (prevStart == 0) {
                        prevStart = nextStart;
                    }

                    lines.add(workingSet[i].substring(lineStart, prevStart));

                    lineStart = prevStart;
                    prevStart = 0;
                } while (lineStart < workingSet[i].length());

                lineStart = 0;
            }
        }

        String[] s = new String[lines.size()];

        return (String[]) lines.toArray(s);
    }
    
    private static String trimString(String s) {
        int idx = 0;
        char c;
        final int slen = s.length();

        if (slen == 0) {
            return s;
        }

        do {
            c = s.charAt(idx++);
        } while (((c == '\n') || (c == '\r')) && (idx < slen));

        s = s.substring(--idx);
        idx = s.length() - 1;

        if (idx < 0) {
            return s;
        }

        do {
            c = s.charAt(idx--);
        } while (((c == '\n') || (c == '\r')) && (idx >= 0));

        return s.substring(0, idx + 2);
    }    
}
