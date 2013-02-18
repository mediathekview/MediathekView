/*
 * @(#)FontFilesResource.java
 *
 * Copyright 2002 - 2004 JIDE Software Inc. All rights reserved.
 */
package com.jidesoft.utils;

import java.util.Locale;
import java.util.ResourceBundle;

class FontFilesResource {
    static final String BASENAME = "fonts.fontfiles";

    static final ResourceBundle RB = ResourceBundle.getBundle(BASENAME);

    public static ResourceBundle getResourceBundle(Locale locale) {
        return ResourceBundle.getBundle(BASENAME, locale);
    }
}
