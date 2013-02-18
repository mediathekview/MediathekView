/*
 * @(#)SystemInfo.java
 *
 * Copyright 2002 JIDE Software Inc. All rights reserved.
 */
package com.jidesoft.utils;

import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * A utility class can detect OS system information.
 */
final public class SystemInfo {

    /**
     * Variable for whether or not we're on Windows.
     */
    private static boolean _isWindows = false;

    /**
     * Variable for whether or not we're on Windows NT or 2000.
     */
    private static boolean _isWindowsNTor2000 = false;

    /**
     * Variable for whether or not we're on Windows XP.
     */
    private static boolean _isWindowsXP = false;

    /**
     * Variable for whether or not we're on Windows Vista.
     */
    private static boolean _isWindowsVista = false;

    /**
     * Variable for whether or not we're on Windows 7.
     */
    private static boolean _isWindows7 = false;

    /**
     * Variable for whether or not we're on Windows 2003.
     */
    private static boolean _isWindows2003 = false;

    /**
     * Flag which indicates that the Win98/Win2k/WinME features should be disabled.
     */
    private static boolean _isClassicWindows = false;

    /**
     * Variable for whether or not we're on Windows 95.
     */
    private static boolean _isWindows95 = false;

    /**
     * Variable for whether or not we're on Windows 98.
     */
    private static boolean _isWindows98 = false;

    /**
     * Variable for whether or not the operating system allows the application to be reduced to the system tray.
     */
    private static boolean _supportsTray = false;

    /**
     * Variable for whether or not we're on Mac 9.1 or below.
     */
    private static boolean _isMacClassic = false;

    /**
     * Variable for whether or not we're on MacOSX.
     */
    private static boolean _isMacOSX = false;

    /**
     * Variable for whether or not we're on Linux.
     */
    private static boolean _isLinux = false;

    /**
     * Variable for whether or not we're on Solaris.
     */
    private static boolean _isSolaris = false;

    private static JavaVersion _currentVersion;

    /**
     * Make sure the constructor can never be called.
     */
    private SystemInfo() {
    }

    /**
     * Initialize the settings statically.
     */
    static {
        // get the operating system
        String os = SecurityUtils.getProperty("os.name", "Windows XP");

        // set the operating system variables
        _isWindows = os.indexOf("Windows") != -1;
        try {
            String osVersion = SecurityUtils.getProperty("os.version", "5.0");
            Float version = Float.valueOf(osVersion);
            _isClassicWindows = version <= 4.0;
        }
        catch (NumberFormatException ex) {
            _isClassicWindows = false;
        }
        if (os.indexOf("Windows XP") != -1 || os.indexOf("Windows NT") != -1 || os.indexOf("Windows 2000") != -1) {
            _isWindowsNTor2000 = true;
        }
        if (os.indexOf("Windows XP") != -1) {
            _isWindowsXP = true;
        }
        if (os.indexOf("Windows Vista") != -1) {
            _isWindowsVista = true;
        }
        if (os.indexOf("Windows 7") != -1) {
            _isWindows7 = true;
        }
        if (os.indexOf("Windows 2003") != -1) {
            _isWindows2003 = true;
            _isWindowsXP = true;
        }
        if (os.indexOf("Windows 95") != -1) {
            _isWindows95 = true;
        }
        if (os.indexOf("Windows 98") != -1) {
            _isWindows98 = true;
        }
        if (_isWindows) _supportsTray = true;
        _isSolaris = (os.indexOf("Solaris") != -1) || (os.indexOf("SunOS") != -1);
        _isLinux = os.indexOf("Linux") != -1;
        if (os.startsWith("Mac OS")) {
            if (os.endsWith("X")) {
                _isMacOSX = true;
            }
            else {
                _isMacClassic = true;
            }
        }
    }

    /**
     * Returns the version of java we're using.
     *
     * @return the java version.
     */
    public static String getJavaVersion() {
        return SecurityUtils.getProperty("java.version", "1.4.2");
    }

    /**
     * Returns the vendor for java we're using.
     *
     * @return the java vendor.
     */
    public static String getJavaVendor() {
        return SecurityUtils.getProperty("java.vendor", "");
    }

    /**
     * Returns the version of the java class we're using.
     *
     * @return the java class version.
     */
    public static String getJavaClassVersion() {
        return SecurityUtils.getProperty("java.class.version", "");
    }

    /**
     * Returns the operating system.
     *
     * @return the os name.
     */
    public static String getOS() {
        return SecurityUtils.getProperty("os.name", "Windows XP");
    }

    /**
     * Returns the operating system version.
     *
     * @return the os version.
     */
    public static String getOSVersion() {
        return SecurityUtils.getProperty("os.version", "");
    }

    /**
     * Returns the operating system architecture.
     *
     * @return the os architecture.
     */
    public static String getOSArchitecture() {
        return SecurityUtils.getProperty("os.arch", "");
    }

    /**
     * Returns the user's home directory.
     *
     * @return the user home .
     */
    public static String getCurrentDirectory() {
        return SecurityUtils.getProperty("user.dir", "");
    }

    /**
     * Returns true if this is Windows NT or Windows 2000 and hence can support a system tray feature.
     *
     * @return true of system tray is supported.
     */
    public static boolean supportsTray() {
        return _supportsTray;
    }

    /**
     * Set supportTray to false in case dll is missing.
     *
     * @param support true or false.
     */
    public static void setSupportsTray(boolean support) {
        _supportsTray = support;
    }

    /**
     * Returns whether or not the os is some version of Windows.
     *
     * @return <tt>true</tt> if the application is running on some Windows version, <tt>false</tt> otherwise.
     */
    public static boolean isWindows() {
        return _isWindows;
    }

    /**
     * Gets the state of the flag which indicates if the old Windows look and feel should be rendered. This flag is used
     * by the component UI delegates as a hint to determine which style the component should be rendered.
     *
     * @return true if Windows 95 and Windows NT 4 look and feel should be rendered.
     */
    public static boolean isClassicWindows() {
        return _isClassicWindows;
    }

    /**
     * Returns whether or not the os is some version of Windows NT.
     *
     * @return <tt>true</tt> if the application is running on Windows NT or 2000, <tt>false</tt> otherwise.
     */
    public static boolean isWindowsNTor2000() {
        return _isWindowsNTor2000;
    }

    /**
     * Returns whether or not the os is some version of Windows XP.
     *
     * @return <tt>true</tt> if the application is running on Windows XP, <tt>false</tt> otherwise.
     */
    public static boolean isWindowsXP() {
        return _isWindowsXP;
    }

    /**
     * Returns whether or not the os is some version of Windows Vista.
     *
     * @return <tt>true</tt> if the application is running on Windows Vista, <tt>false</tt> otherwise.
     */
    public static boolean isWindowsVista() {
        return _isWindowsVista;
    }

    /**
     * Returns whether or not the os is some version of Windows 7.
     *
     * @return <tt>true</tt> if the application is running on Windows 7, <tt>false</tt> otherwise.
     */
    public static boolean isWindows7() {
        return _isWindows7;
    }

    /**
     * Returns whether or not the os is some version of Windows Vista or Windows 7.
     *
     * @return <tt>true</tt> if the application is running on Windows Vista or Windows 7, <tt>false</tt> otherwise.
     */
    public static boolean isWindowsVistaAbove() {
        return _isWindowsVista || _isWindows7;
    }

    /**
     * Returns whether or not the os is some version of Windows 95.
     *
     * @return <tt>true</tt> if the application is running on Windows XP, <tt>false</tt> otherwise.
     */
    public static boolean isWindows95() {
        return _isWindows95;
    }

    /**
     * Returns whether or not the os is some version of Windows 98.
     *
     * @return <tt>true</tt> if the application is running on Windows XP, <tt>false</tt> otherwise.
     */
    public static boolean isWindows98() {
        return _isWindows98;
    }

    /**
     * Returns whether or not the os is some version of Windows 2003.
     *
     * @return <tt>true</tt> if the application is running on Windows 2003, <tt>false</tt> otherwise.
     */
    public static boolean isWindows2003() {
        return _isWindows2003;
    }


    /**
     * Returns whether or not the os is Mac 9.1 or earlier.
     *
     * @return <tt>true</tt> if the application is running on a Mac version prior to OSX, <tt>false</tt> otherwise.
     */
    public static boolean isMacClassic() {
        return _isMacClassic;
    }

    /**
     * Returns whether or not the os is Mac OSX.
     *
     * @return <tt>true</tt> if the application is running on Mac OSX, <tt>false</tt> otherwise.
     */
    public static boolean isMacOSX() {
        return _isMacOSX;
    }

    /**
     * Returns whether or not the os is any Mac os.
     *
     * @return <tt>true</tt> if the application is running on Mac OSX or any previous mac version, <tt>false</tt>
     *         otherwise.
     */
    public static boolean isAnyMac() {
        return _isMacClassic || _isMacOSX;
    }

    /**
     * Returns whether or not the os is Solaris.
     *
     * @return <tt>true</tt> if the application is running on Solaris, <tt>false</tt> otherwise.
     */
    public static boolean isSolaris() {
        return _isSolaris;
    }

    /**
     * Returns whether or not the os is Linux.
     *
     * @return <tt>true</tt> if the application is running on Linux, <tt>false</tt> otherwise.
     */
    public static boolean isLinux() {
        return _isLinux;
    }

    /**
     * Returns whether or not the os is some version of Unix, defined here as only Solaris or Linux.
     *
     * @return <tt>true</tt> if the application is running on a type of UNIX such as Linux or Solaris, <tt>false</tt>
     *         otherwise.
     */
    public static boolean isUnix() {
        return _isLinux || _isSolaris;
    }

    private static void checkJdkVersion() {
        if (_currentVersion == null) {
            _currentVersion = new JavaVersion(getJavaVersion());
        }
    }

    /**
     * Returns whether or no the JDK version is 1.3 and above.
     *
     * @return <tt>true</tt> if the application is running on JDK 1.3 and above, <tt>false</tt> otherwise.
     */
    public static boolean isJdk13Above() {
        checkJdkVersion();
        return _currentVersion.compareVersion(1.3, 0, 0) >= 0;
    }

    /**
     * Returns whether or no the JDK version is 1.4.2 and above.
     *
     * @return <tt>true</tt> if the application is running on JDK 1.4.2 and above, <tt>false</tt> otherwise.
     */
    public static boolean isJdk142Above() {
        checkJdkVersion();
        return _currentVersion.compareVersion(1.4, 2, 0) >= 0;
    }

    /**
     * Returns whether or no the JDK version is 1.4 and above.
     *
     * @return <tt>true</tt> if the application is running on JDK 1.4 and above, <tt>false</tt> otherwise.
     */
    public static boolean isJdk14Above() {
        checkJdkVersion();
        return _currentVersion.compareVersion(1.4, 0, 0) >= 0;
    }

    /**
     * Returns whether or no the JDK version is 1.5 and above.
     *
     * @return <tt>true</tt> if the application is running on JDK 1.5 and above, <tt>false</tt> otherwise.
     */
    public static boolean isJdk15Above() {
        checkJdkVersion();
        return _currentVersion.compareVersion(1.5, 0, 0) >= 0;
    }

    /**
     * Returns whether or no the JDK version is 6 and above.
     *
     * @return <tt>true</tt> if the application is running on JDK 6 and above, <tt>false</tt> otherwise.
     */
    public static boolean isJdk6Above() {
        checkJdkVersion();
        return _currentVersion.compareVersion(1.6, 0, 0) >= 0;
    }

    /**
     * Returns whether or no the JDK version is 6u10 and above.
     *
     * @return <tt>true</tt> if the application is running on JDK 6u10 and above, <tt>false</tt> otherwise.
     */
    public static boolean isJdk6u10Above() {
        checkJdkVersion();
        return _currentVersion.compareVersion(1.6, 0, 10) >= 0;
    }

    /**
     * Returns whether or no the JDK version is 6u14 and above. There are some heavyweight component and lightweight
     * component mixing changes in JDK6u14.
     *
     * @return <tt>true</tt> if the application is running on JDK 6u14 and above, <tt>false</tt> otherwise.
     */
    public static boolean isJdk6u14Above() {
        checkJdkVersion();
        return _currentVersion.compareVersion(1.6, 0, 14) >= 0;
    }

    /**
     * Returns whether or no the JDK version is 6u25 and above. Event firing for JComboBox changes in JDK6u25.
     *
     * @return <tt>true</tt> if the application is running on JDK 6u25 and above, <tt>false</tt> otherwise.
     */
    public static boolean isJdk6u25Above() {
        checkJdkVersion();
        return _currentVersion.compareVersion(1.6, 0, 25) >= 0;
    }

    /**
     * Returns whether or no the JDK version is 1.7 and above.
     *
     * @return <tt>true</tt> if the application is running on JDK 1.7 and above, <tt>false</tt> otherwise.
     */
    public static boolean isJdk7Above() {
        checkJdkVersion();
        return _currentVersion.compareVersion(1.7, 0, 0) >= 0;
    }

    /**
     * Returns whether or not the JDK version is exactly the version you are expecting
     *
     * @param majorVersion your intended major version for JDK6u10, it should be 1.6
     * @param minorVersion your intended major version for JDK6u10, it should be 0
     * @param build        your intended major version for JDK6u10, it should be 10
     * @return <tt>true</tt> if the application is running on the input version, <tt>false</tt> otherwise.
     */
    public static boolean isJdkVersion(double majorVersion, int minorVersion, int build) {
        checkJdkVersion();
        return _currentVersion.compareVersion(majorVersion, minorVersion, build) == 0;
    }

    /**
     * Returns whether or not the JDK version is above the version, including the version, you are expecting
     *
     * @param majorVersion your intended major version for JDK6u10, it should be 1.6
     * @param minorVersion your intended major version for JDK6u10, it should be 0
     * @param build        your intended major version for JDK6u10, it should be 10
     * @return <tt>true</tt> if the application is running on the input version, <tt>false</tt> otherwise.
     */
    public static boolean isJdkVersionAbove(double majorVersion, int minorVersion, int build) {
        checkJdkVersion();
        return _currentVersion.compareVersion(majorVersion, minorVersion, build) >= 0;
    }

    /**
     * Returns whether or not the JDK version is below the version, including the version, you are expecting
     *
     * @param majorVersion your intended major version for JDK6u10, it should be 1.6
     * @param minorVersion your intended major version for JDK6u10, it should be 0
     * @param build        your intended major version for JDK6u10, it should be 10
     * @return <tt>true</tt> if the application is running on the input version, <tt>false</tt> otherwise.
     */
    public static boolean isJdkVersionBelow(double majorVersion, int minorVersion, int build) {
        checkJdkVersion();
        return _currentVersion.compareVersion(majorVersion, minorVersion, build) <= 0;
    }

    /**
     * Returns whether the default locale is one of the three language - Chinese, Japanese or Korean - also known as
     * CJK.
     *
     * @return true if the default locale is in CJK.
     */
    public static boolean isCJKLocale() {
        return isCJKLocale(Locale.getDefault());
    }

    /**
     * Returns whether the locale is one of the three language - Chinese, Japanese or Korean - also known as CJK.
     *
     * @param locale the locale to be checked.
     * @return true if the default locale is in CJK.
     */
    public static boolean isCJKLocale(Locale locale) {
        return locale.equals(Locale.CHINA)
                || locale.equals(Locale.CHINESE)
                || locale.equals(new Locale("zh", "HK"))
                || locale.equals(Locale.TAIWAN)
                || locale.equals(Locale.JAPAN)
                || locale.equals(Locale.JAPANESE)
                || locale.equals(Locale.KOREA)
                || locale.equals(Locale.KOREAN);
    }

    public static class JavaVersion {
        /**
         * For example: 1.6.0_12: Group 1 = major version (1.6) Group 3 = minor version (0) Group 5 = build number (12)
         */
        private static Pattern SUN_JAVA_VERSION = Pattern.compile("(\\d+\\.\\d+)(\\.(\\d+))?(_([^-]+))?(.*)");
        private static Pattern SUN_JAVA_VERSION_SIMPLE = Pattern.compile("(\\d+\\.\\d+)(\\.(\\d+))?(.*)");

        private double _majorVersion;
        private int _minorVersion;
        private int _buildNumber;
        private String _patch;

        public JavaVersion(String version) {
            _majorVersion = 1.4;
            _minorVersion = 0;
            _buildNumber = 0;
            try {
                Matcher matcher = SUN_JAVA_VERSION.matcher(version);
                if (matcher.matches()) {
                    int groups = matcher.groupCount();
                    _majorVersion = Double.parseDouble(matcher.group(1));
                    if (groups >= 3 && matcher.group(3) != null) {
                        _minorVersion = Integer.parseInt(matcher.group(3));
                    }
                    if (groups >= 5 && matcher.group(5) != null) {
                        try {
                            _buildNumber = Integer.parseInt(matcher.group(5));
                        }
                        catch (NumberFormatException e) {
                            _patch = matcher.group(5);
                        }
                    }
                    if (groups >= 6 && matcher.group(6) != null) {
                        String s = matcher.group(6);
                        if (s != null && s.trim().length() > 0) _patch = s;
                    }
                }
            }
            catch (NumberFormatException e) {
                try {
                    Matcher matcher = SUN_JAVA_VERSION_SIMPLE.matcher(version);
                    if (matcher.matches()) {
                        int groups = matcher.groupCount();
                        _majorVersion = Double.parseDouble(matcher.group(1));
                        if (groups >= 3 && matcher.group(3) != null) {
                            _minorVersion = Integer.parseInt(matcher.group(3));
                        }
                    }
                }
                catch (NumberFormatException e1) {
                    System.err.println("Please check the installation of your JDK. The version number " + version + " is not right.");
                }
            }
        }

        public JavaVersion(double major, int minor, int build) {
            _majorVersion = major;
            _minorVersion = minor;
            _buildNumber = build;
        }

        public int compareVersion(double major, int minor, int build) {
            double majorResult = _majorVersion - major;
            if (majorResult != 0) {
                return majorResult < 0 ? -1 : 1;
            }
            int result = _minorVersion - minor;
            if (result != 0) {
                return result;
            }
            return _buildNumber - build;
        }

        public double getMajorVersion() {
            return _majorVersion;
        }

        public int getMinorVersion() {
            return _minorVersion;
        }

        public int getBuildNumber() {
            return _buildNumber;
        }

        public String getPatch() {
            return _patch;
        }
    }
}
