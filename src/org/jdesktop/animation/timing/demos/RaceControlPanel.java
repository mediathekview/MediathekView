/**
 * Copyright (c) 2006, Sun Microsystems, Inc
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided
 *     with the distribution.
 *   * Neither the name of the TimingFramework project nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.jdesktop.animation.timing.demos;

import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * Go/Stop buttons to control the animation
 *
 * @author Chet
 */
public class RaceControlPanel extends JPanel {
    
	/** Make these static so that outside classes can easily
     *  add themselves as listeners */
    JButton goButton = new JButton("Go");
    JButton stopButton = new JButton("Stop");

    /**
     * Creates a new instance of RaceControlPanel
     */
    public RaceControlPanel() {
        add(goButton);
        add(stopButton);
    }
    
    public JButton getGoButton() {
        return goButton;
    }
    
    public JButton getStopButton() {
        return stopButton;
    }
    
    public void addListener(ActionListener listener) {
        goButton.addActionListener(listener);
        stopButton.addActionListener(listener);
    }
    
    /**
     * For testing purposes only
     */
    public static void main(String args[]) {
        JFrame f = new JFrame();
        //f.add(new RaceControlPanel());
        f.pack();
        f.setVisible(true);
    }
    
	private static final long serialVersionUID = 7406133627780032185L;
}
