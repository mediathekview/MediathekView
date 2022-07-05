/*
 * $Id: package-info.java 4146 2012-01-25 19:40:05Z kschaefe $
 *
 * Copyright 2008 Sun Microsystems, Inc., 4150 Network Circle,
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
/**
 * Provides pluggable look-and-feel for SwingX components together with a
 * mechanism to support custom component look-and-feels.
 * <p>
 * The addons are loaded with {@link java.util.ServiceLoader}.  As such we 
 * maintain, a services file for our implementations.  SwingX uses the 
 * <a href="http://metainf-services.kohsuke.org/">MetaInf/services</a>
 * generator API. This add a compile time dependency to the plaf module.
 * The services generator, however, is not required at runtime.
 */
package org.jdesktop.swingx.plaf;

