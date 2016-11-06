/**
 * \cond LICENSE
 * ********************************************************************
 * This is a conditional block for preventing the DoxyGen documentation
 * tool to include this license header within the description of each
 * source code file. If you want to include this block, please define
 * the LICENSE parameter into the provided DoxyFile.
 * ********************************************************************
 *
 * JCarrierPigeon - A notification library
 * Copyright (c) 2010, Paulo Roberto Massa Cereda
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. Neither the name of the project's author nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ********************************************************************
 * End of the LICENSE conditional block
 * ********************************************************************
 * \endcond
 *
 * <b>WindowPosition.java</b>: provides an enumeration for the window position
 * on screen. This enumeration is used to set the parameters referring the
 * window position and to assign the correct mathematical formulas to the
 * animation handlers.
 */

package net.sf.jcarrierpigeon;

/**
 * Provides an enumeration for the window position on screen. This enumeration
 * basically consists on four states, defined later on the documentation.
 * Consider \f$ f: \mathbb{R} \rightarrow \mathbb{Z} \f$ as a function to
 * calculate the current position of a window on the screen. Lets take a
 * parameter \f$ x \in \mathbb{R} \f$ that \f$ 0 \leq x \leq 1 \f$, then
 * \f$ f(x) \in \mathbb{Z} \f$. The animation handler will return a continuum
 * value which must be converted to a screen position through this function
 * \f$ f \f$. Each screen position has its own assignment for this calculation.
 * To enhance the comprehension, consider these symbols:
 *  - \f$ d \f$ refers to the screen dimension. That way, we may also consider
 *    the following notation:
 *    - \f$ d_h \f$ is the X axis, that is, the width of the screen.
 *    - \f$ d_v \f$ is the Y axis, that is, the height of the screen.
 *  - \f$ j \f$ refers to the window acting as a notification. We may also
 *    consider the following notation:
 *    - \f$ j_h \f$ is the X axis, that is, the width of the window.
 *    - \f$ j_v \f$ is the Y axis, that is, the height of the window.
 *  - \f$ b \f$ is the distance between the window bounds to the screen bounds.
 *    We consider the following notation:
 *    - \f$ b_h \f$ is the X axis, that is, the width of the distance.
 *    - \f$ b_v \f$ is the Y axis, that is, the height of the distance.
 *  - \f$ p \f$ is the coordinate for the window acting as a notification. We
 *    consider the following notations:
 *    - \f$ p_h \f$ is the X axis, and by that we really mean the X axis.
 *    - \f$ p_v \f$ is the Y axis, and by that we really mean the Y axis.
 *
 * It is also important to note that the assignments for \f$ f(x) \f$ also
 * depend on <b>net.sf.jcarrierpigeon.AnimationFrame</b>.
 * 
 * @author Paulo Roberto Massa Cereda
 * @version 1.3
 * @since 1.0
 */
public enum WindowPosition {
    /**
     * The window will be displayed on the top left of the screen.
     *  - \f$ p_h = b_h \f$
     *  - \f$ p_v = b_v \f$
     *  - <i>ONSHOW</i>: \f$ f(x) = p_v - ((j_v + b_v) (1 - x)) \f$
     *  - <I>ONDISPLAY</i>: Not applicable.
     *  - <i>ONCLOSE</i>: \f$ f(x) = p_v - ((j_v + b_v) x) \f$
     */
    TOPLEFT,
    /**
     * The window will be displayed on the top right of the screen.
     *  - \f$ p_h = d_h - (j_h + b_h) \f$
     *  - \f$ p_v = b_v \f$
     *  - <i>ONSHOW</i>: \f$ f(x) = p_v - ((j_v + b_v) (1 - x)) \f$
     *  - <I>ONDISPLAY</i>: Not applicable.
     *  - <i>ONCLOSE</i>: \f$ f(x) = p_v - ((j_v + b_v) x) \f$
     */
    TOPRIGHT,
    /**
     * The window will be displayed on the bottom left of the screen.
     *  - \f$ p_h = b_h \f$
     *  - \f$ p_v = d_v - (j_v + b_v) \f$
     *  - <i>ONSHOW</i>: \f$ f(x) = p_v + ((d_v - p_v) (1 - x)) \f$
     *  - <I>ONDISPLAY</i>: Not applicable.
     *  - <i>ONCLOSE</i>: \f$ f(x) = p_v + ((d_v - p_v) x) \f$
     */
    BOTTOMLEFT,
    /**
     * The window will be displayed on the bottom right of the screen.
     *  - \f$ p_h = d_h - (j_h + b_h) \f$
     *  - \f$ p_v = d_v - (j_v + b_v) \f$
     *  - <i>ONSHOW</i>: \f$ f(x) = p_v + ((d_v - p_v) (1 - x)) \f$
     *  - <I>ONDISPLAY</i>: Not applicable.
     *  - <i>ONCLOSE</i>: \f$ f(x) = p_v + ((d_v - p_v) x) \f$
     */
    BOTTOMRIGHT
}
