/*
 * $Id: ErrorLevel.java 1557 2006-11-10 17:02:53Z rbair $
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

package org.jdesktop.swingx.error;

import java.util.logging.Level;

/**
 * <p>Extends {@link Level} adding the <code>FATAL</code> error level.
 * Fatal errors are those unrecoverable errors that must result in the termination
 * of the application.</p>
 *
 * @status REVIEWED
 * @author rbair
 */
public class ErrorLevel extends Level {
    /**
     * FATAL is a message level indicating a catastrophic failure that should
     * result in the immediate termination of the application.
     * <p>
     * In general FATAL messages should describe events that are
     * of considerable critical and which will prevent
     * program execution.   They should be reasonably intelligible
     * to end users and to system administrators.
     * This level is initialized to <CODE>1100</CODE>.
     */    
    public static final ErrorLevel FATAL = new ErrorLevel("FATAL", 1100);
    
    /** Creates a new instance of ErrorLevel */
    protected ErrorLevel(String name, int value) {
        super(name, value);
    }
}
