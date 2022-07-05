/*
 * $Id: TranslucentRepaintManager.java 2476 2007-11-25 15:52:59Z kschaefe $
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

package org.jdesktop.swingx;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * <p>An annotation that can be applied to a {@link javax.swing.RepaintManager} to suggest that
 * the <code>RepaintManager</code> supports translucency. If a <code>JXPanel</code>
 * is made translucent by setting it's alpha property to a value between 0 and 1, 
 * then the <code>JXPanel</code> must ensure that a <code>RepaintManager</code>
 * capable of handling transparency is installed. This annotation tells the
 * <code>JXPanel</code> that the installed <code>RepaintManager</code> does not
 * need to be replaced. This is critical for custom <code>RepaintManager</code>s
 * which are used in applications along with transparent <code>JXPanel</code>s.</p>
 * 
 * <p>A <code>RepaintManager</code> supports translucency if, when a repaint on a
 * child component occurs, it begins painting <em>not</em> on the child component,
 * but on the child component's <code>JXPanel</code> ancestor if: a) there is such
 * an ancestor and b) the ancestor returns an effective alpha of &lt; 1.</p>
 * 
 * @see RepaintManagerX
 * @see JXPanel
 * @author rbair
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface TranslucentRepaintManager {
}
