package org.simplericity.macify.eawt;

/*
 * Copyright 2007 Eirik Bjorsnos.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * The Macify Library API interface provides integration with the OS X platform for Java Applications.
 * The API includes a fa�ade to the
 * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/index.html">
 * Apple Java Extensions API
 * </a>.
 * Additionally, it provides access to several useful methods in the Cocoa NSApplication API.
 * <p/>
 * The default implementation of this interface is {@link org.simplericity.macify.eawt.DefaultApplication}.
 */
public interface Application {

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#addAboutMenuItem()">
     * Apple's API
     * </a>.
     */
    void addAboutMenuItem();

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#addApplicationListener(com.apple.eawt.ApplicationListener)">
     * Apple's API
     * </a>.
     */
    void addApplicationListener(ApplicationListener applicationListener);

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#addPreferencesMenuItem()">
     * Apple's API
     * </a>.
     */
    void addPreferencesMenuItem();

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#getEnabledAboutMenu()">
     * Apple's API
     * </a>.
     */
    boolean getEnabledAboutMenu();

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#getEnabledPreferencesMenu()">
     * Apple's API
     * </a>.
     */
    boolean getEnabledPreferencesMenu();

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#isAboutMenuItemPresent()">
     * Apple's API
     * </a>.
     */
    boolean isAboutMenuItemPresent();

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#isPreferencesMenuItemPresent()">
     * Apple's API
     * </a>.
     */
    boolean isPreferencesMenuItemPresent();

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#removeAboutMenuItem()">
     * Apple's API
     * </a>.
     */
    void removeAboutMenuItem();

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#removeApplicationListener(com.apple.eawt.ApplicationListener)">
     * Apple's API
     * </a>.
     */
    void removeApplicationListener(ApplicationListener applicationListener);

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#removePreferencesMenuItem()">
     * Apple's API
     * </a>.
     */
    void removePreferencesMenuItem();

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#getEnabledAboutMenu()">
     * Apple's API
     * </a>.
     */
    void setEnabledAboutMenu(boolean enabled);

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#getEnabledPreferencesMenu()">
     * Apple's API
     * </a>.
     */
    void setEnabledPreferencesMenu(boolean enabled);

    /**
     * See
     * <a href="http://developer.apple.com/documentation/Java/Reference/1.5.0/appledoc/api/com/apple/eawt/Application.html#getMouseLocationOnScreen()">
     * Apple's API
     * </a>.
     */
    Point getMouseLocationOnScreen();

    /**
     * Let the Finder dock icon bounce.
     * @param critical if true, the dock icon will bounce indefinetly, if false it will bounce only once.
     */
    void requestUserAttention(final boolean critical);

    /**
     * Update the application's icon image
     *
     * @param image
     */
    void setApplicationIconImage(BufferedImage image);

    /**
     * Get the application's icon image.
     */
    BufferedImage getApplicationIconImage();

    /**
     * Determines whether the application is running on a Mac AND the Apple Extensions API classes are available.
     *
     * @return
     */
    boolean isMac();

    /**
     * Enables this application to be suddenly terminated.
     *
     * Call this method to indicate your application's state is saved, and requires no notification to be terminated.
     * Letting your application remain terminatable improves the user experience by avoiding re-paging in your application when it's asked to quit.
     *
     * <b>Note: enabling sudden termination will allow your application to be quit without notifying your QuitHandler, or running any shutdown hooks.</b>
     * User initiated Cmd-Q, logout, restart, or shutdown requests will effectively "kill -KILL" your application.
     *
     * This call has no effect on Mac OS X versions prior to 10.6.
     */
    void enableSuddenTermination();
}
