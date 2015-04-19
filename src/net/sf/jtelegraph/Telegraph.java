/**
 * \cond LICENSE
 * ********************************************************************
 * This is a conditional block for preventing the DoxyGen documentation
 * tool to include this license header within the description of each
 * source code file. If you want to include this block, please define
 * the LICENSE parameter into the provided DoxyFile.
 * ********************************************************************
 *
 * JTelegraph - A message notification library
 * Copyright (c) 2011, Paulo Roberto Massa Cereda
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
 * <b>Telegraph.java</b>: provides a new style for messages notifications.
 * This class aims at making it easy to display messages on a non-modal
 * fashion with predefined icons and colors.
 */

package net.sf.jtelegraph;

// This class relies on the JCarrierPigeon library in order to display
// message animations and indirectly on the Timing Framework library
import net.sf.jcarrierpigeon.Notification;
import net.sf.jcarrierpigeon.WindowPosition;


/**
 * Provides messages notification features. This class encapsulates the
 * animation engine provided by JCarrierPigeon (version 1.3) and the
 * <code>javax.swing.JWindow</code> object referring the message itself.
 * In order to improve your code, make sure to use this class together with the
 * <code>net.sf.jtelegraph.TelegraphQueue</code> class. Check the following example:
 * @code
 * Telegraph telegraph = new Telegraph("Connection status", "The DB connection was established successfully", TelegraphType.NOTIFICATION_DONE, WindowPosition.BOTTOMLEFT, 4000);
 * TelegraphQueue queue = new TelegraphQueue();
 * queue.add(telegraph);
 * @endcode
 * Please note that this class makes use of the <code>net.sf.jcarrierpigeon.WindowPosition</code>
 * enumeration in order to set up the window position. This class is part of the
 * JCarrierPigeon library.
 *
 * @author Paulo Roberto Massa Cereda
 * @version 1.0
 * @since 1.0
 */
public class Telegraph {

    // the notification framework
    private Notification notification;

    // title of the message
    private String title;

    // the message itself
    private String message;

    // type of message
    private TelegraphType type;

    // window position
    private WindowPosition position;

    // duration of message display in milliseconds
    private int duration;

    /**
     * Constructor method, the message structure is basically built in here.
     * @param title Title of the message.
     * @param message The message itself. Virtually there is no limit for this
     * message length, as the window is supposed to automatically resize. Since
     * this message is based on a HTML wrapping, you may use some formatting
     * tags, if you want. In order to display them correctly, you must use a
     * native UI manager somewhere in your code, like the following example:
     * @code
     * UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
     * @endcode
     * The line above throws a lot of exceptions, so make sure to caught all
     * of them. Also note that this procedure has nothing to do with JTelegraph.
     * @param type The type of the message, according to the
     * <code>net.sf.jtelegraph.TelegraphType</code> enumeration.
     * @param position The window position. This parameter expects one of the
     * values in <code>net.sf.jcarrierpigeon.WindowPosition</code>.
     * @param duration The duration of the message display in milliseconds.
     */
    public Telegraph(String title, String message, TelegraphType type, WindowPosition position, int duration) {

        // set the values
        this.title = title;
        this.message = message;
        this.type = type;
        this.position = position;
        this.duration = duration;
    }


    /**
     * Checks if the message notification process is still running. It basically calls the
     * <code>isRunning</code> method from <code>net.sf.jcarrierpigeon.Notification</code>.
     * That method will call another method from <code>org.jdesktop.animation.timing.Animator</code>.
     * @return <code>true</code> if the message notification is still running,
     * or <code>false</code> otherwise.
     */
    public boolean isRunning() {

        // calls the inner method from the notification
        return notification.isRunning();
    }

    /**
     * Performs the animation itself based on the parameters provided in the
     * constructor method. This method will call another method from the
     * <code>net.sf.jcarrierpigeon.Notification</code> class. Keep in mind this
     * method is synchronized. Check the following example:
     * @code
     * JTelegraph telegraph = new Telegraph("Connection status", "The DB connection was established successfully", TelegraphType.NOTIFICATION_DONE, WindowPosition.BOTTOMLEFT, 4000);
     * telegraph.animate();
     * @endcode
     * Wherever possible, please use the notification queue manager from the
     * <code>net.sf.jtelegraph.TelegraphQueue</code> class.
     */
    public synchronized void animate() {

        // create a new telegraph envelope, which is our window
        TelegraphEnvelope window = new TelegraphEnvelope();

        // set the title and body for the message
        window.setMessage(title, message);

        // set the type
        window.setTelegraphType(type);

        // since we are talking about telegraphs and envelopes, time
        // to pack everything
        window.packTelegraph();

        // create a new notification from JCarrierPigeon
        notification = new Notification(window, position, 20, 20, duration);

        // usually the message animation speed needs to be set as shorter as we
        // could define, so the default animation speed is overriden here
        notification.setAnimationSpeed(250);

        // animate the telegraph
        notification.animate();
    }

}
