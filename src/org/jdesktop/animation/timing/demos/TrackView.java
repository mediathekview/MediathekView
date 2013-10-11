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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.Point;
import javax.swing.JComponent;
import javax.imageio.ImageIO;
import javax.swing.JFrame;

/**
 * This class does the work of rendering the current view of the
 * racetrack.  It holds the car position and rotation and displays
 * the car accordingly.  The track itself is merely a background image
 * that is copied the same on every repaint.
 * Note that carPosition and carRotation are both JavaBean properties, which
 * is exploited in the SetterRace and MultiStepRace variations.
 *
 * @author Chet
 */
public class TrackView extends JComponent {
    
	BufferedImage car;
    BufferedImage track;
    Point carPosition;
    double carRotation = 0;
    int trackW, trackH;
    int carW, carH, carWHalf, carHHalf;

    /** Hard-coded positions of interest on the track */
    static final Point START_POS = new Point(450, 70);
    static final Point FIRST_TURN_START = new Point(130, 70);
    static final Point FIRST_TURN_END = new Point(76, 127);
    static final Point SECOND_TURN_START = new Point(76, 404);
    static final Point SECOND_TURN_END = new Point(130, 461);
    static final Point THIRD_TURN_START = new Point(450, 461);
    static final Point THIRD_TURN_END = new Point(504, 404);
    static final Point FOURTH_TURN_START = new Point(504, 127);
    
    /** Creates a new instance of TrackView */
    public TrackView() {
        try {
            car = ImageIO.read(DemoResources.getResource(DemoResources.BEETLE_RED));
            track = ImageIO.read(DemoResources.getResource(DemoResources.TRACK));
        } catch (Exception e) {
            System.out.println("Problem loading track/car images: " + e);
        }
        carPosition = new Point(START_POS.x, START_POS.y);
        carW = car.getWidth();
        carH = car.getHeight();
        carWHalf = carW / 2;
        carHHalf = carH / 2;
        trackW = track.getWidth();
        trackH = track.getHeight();
    }
    
    public Dimension getPreferredSize() {
        return new Dimension(trackW, trackH);
    }
    
    /**
     * Render the track and car
     */
    public void paintComponent(Graphics g) {
        // First draw the race track
        g.drawImage(track, 0, 0, null);
        
        // Now draw the car.  The translate/rotate/translate settings account
        // for any nonzero carRotation values
        Graphics2D g2d = (Graphics2D)g.create();
        g2d.translate(carPosition.x, carPosition.y);
        g2d.rotate(Math.toRadians(carRotation));
        g2d.translate(-(carPosition.x), -(carPosition.y));
        
        // Now the graphics has been set up appropriately; draw the
        // car in position
        g2d.drawImage(car, carPosition.x - carWHalf, carPosition.y - carHHalf, null);
    }
    
    /**
     * Set the new position and schedule a repaint
     */
    public void setCarPosition(Point newPosition) {
        repaint(0, carPosition.x - carWHalf, carPosition.y - carHHalf,
                carW, carH);
        carPosition.x = newPosition.x;
        carPosition.y = newPosition.y;
        repaint(0, carPosition.x - carWHalf, carPosition.y - carHHalf,
                carW, carH);
    }
    
    /**
     * Set the new rotation and schedule a repaint
     */
    public void setCarRotation(double newDegrees) {
        carRotation = newDegrees;
        // repaint area accounts for larger rectangular are because rotate
        // car will exceed normal rectangular bounds
        repaint(0, carPosition.x - carW, carPosition.y - carH,
                2 * carW, 2 * carH);
    }
    
    /**
     * For testing purposes only
     */
    public static void main(String args[]) {
        JFrame f = new JFrame();
        f.add(new TrackView());
        f.pack();
        f.setVisible(true);
    }
    
	private static final long serialVersionUID = -4488200697426629351L; 
}
