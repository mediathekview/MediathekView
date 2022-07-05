/*
 * $Id: BusyPainter.java 4156 2012-02-02 19:54:38Z kschaefe $
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

package org.jdesktop.swingx.painter;

import org.jdesktop.beans.JavaBean;
import org.jdesktop.swingx.util.PaintUtils;

import java.awt.*;
import java.awt.geom.Ellipse2D;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D.Float;
import java.awt.geom.RoundRectangle2D;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * A specific painter that paints an "infinite progress" like animation.
 */
@JavaBean
@SuppressWarnings("nls")
public class BusyPainter extends AbstractPainter<Object> {

    /**
     * Direction is used to set the initial direction in which the
     * animation starts.
     * 
     * @see BusyPainter#setDirection(Direction)
     */
    public static enum Direction {
        /**
         * cycle proceeds forward
         */
        RIGHT,
        /** cycle proceeds backward */
        LEFT,
    }

    private int frame = -1;

    private int points = 8;

    private Color baseColor = new Color(200, 200, 200);

    private Color highlightColor = Color.BLACK;

    private int trailLength = 4;

    private Shape pointShape;

    private Shape trajectory;

    private Direction direction = Direction.RIGHT;

    private boolean paintCentered;

    /**
     * Creates new busy painter initialized to the shape of circle and bounds size 26x26 points.
     */
    public BusyPainter() {
        this(26);
    }

    /**
     * Creates new painter initialized to the shape of circle and bounds of square of specified height.
     * @param height Painter height.
     */
    public BusyPainter(int height) {
        this(getScaledDefaultPoint(height), getScaledDefaultTrajectory(height));
    }
    
    /**
     * Initializes painter to the specified trajectory and and point shape. Bounds are dynamically calculated to so the specified trajectory fits in.
     * @param point Point shape.
     * @param trajectory Trajectory shape.
     */
    public BusyPainter(Shape point, Shape trajectory) {
        init(point, trajectory, Color.LIGHT_GRAY, Color.BLACK);
    }

    protected static Shape getScaledDefaultTrajectory(int height) {
        return new Ellipse2D.Float(((height * 8) / 26) / 2, ((height * 8) / 26) / 2, height
                - ((height * 8) / 26), height - ((height * 8) / 26));
    }

    protected static Shape getScaledDefaultPoint(int height) {
        return new RoundRectangle2D.Float(0, 0, (height * 8) / 26, 4,
                4, 4);
    }

    /**
     * Initializes painter to provided shapes and default colors.
     * @param point Point shape.
     * @param trajectory Trajectory shape.
     */
    protected void init(Shape point, Shape trajectory, Color baseColor, Color highlightColor) {
        this.baseColor = baseColor;
        this.highlightColor = highlightColor;
        this.pointShape = point;
        this.trajectory = trajectory;
    }

    /**
     * @inheritDoc
     */
    @Override
    protected void doPaint(Graphics2D g, Object t, int width, int height) {
        Rectangle r = getTrajectory().getBounds();
        int tw = width - r.width - 2*r.x;
        int th = height - r.height - 2*r.y;
        if (isPaintCentered()) {
            g.translate(tw/2, th/2);
        }

        PathIterator pi = trajectory.getPathIterator(null);
        float[] coords = new float[6];
        Float cp = new Float();
        Float sp = new Float();
        int ret;
        float totalDist = 0;
        List<float[]> segStack = new ArrayList<float[]>();
        do {
            try {
                ret = pi.currentSegment(coords);
            } catch (NoSuchElementException e) {
                // invalid object definition - one of the bounds is zero or less
                return;
            }
            if (ret == PathIterator.SEG_LINETO || (ret == PathIterator.SEG_CLOSE && (sp.x != cp.x || sp.y != cp.y))) {
                //close by line
                float c = calcLine(coords, cp);
                totalDist += c;
                // move the point to the end (just so it is same for all of them
                segStack.add(new float[] { c, 0, 0, 0, 0, coords[0], coords[1], ret });
                cp.x = coords[0];
                cp.y = coords[1];
            }
            if (ret == PathIterator.SEG_MOVETO) {
                sp.x = cp.x = coords[0];
                sp.y = cp.y = coords[1];

            }
            if (ret == PathIterator.SEG_CUBICTO) {
                float c = calcCube(coords, cp);
                totalDist += c;
                segStack.add(new float[] { c, coords[0], coords[1], coords[2],
                        coords[3], coords[4], coords[5], ret });
                cp.x = coords[4];
                cp.y = coords[5];
            }
            if (ret == PathIterator.SEG_QUADTO) {
                float c = calcLengthOfQuad(coords, cp);
                totalDist += c;
                segStack.add(new float[] { c, coords[0], coords[1], 0 ,0 , coords[2],
                        coords[3], ret });
                cp.x = coords[2];
                cp.y = coords[3];
            }
            // got a starting point, center point on it.
            pi.next();
        } while (!pi.isDone());
        float nxtP = totalDist / getPoints();
        List<Float> pList = new ArrayList<Float>();
        pList.add(new Float(sp.x, sp.y));
        int sgIdx = 0;
        float[] sgmt = segStack.get(sgIdx);
        float len = sgmt[0];
        float travDist = nxtP;
        Float center = new Float(sp.x, sp.y);
        for (int i = 1; i < getPoints(); i++) {
            while (len < nxtP) {
                sgIdx++;
                // Be carefull when messing around with points.
                sp.x = sgmt[5];
                sp.y = sgmt[6];
                sgmt = segStack.get(sgIdx);
                travDist = nxtP - len;
                len += sgmt[0];
            }
            len -= nxtP;
            Float p = calcPoint(travDist, sp, sgmt, width, height);
            pList.add(p);
            center.x += p.x;
            center.y += p.y;
            travDist += nxtP;
        }
        // calculate center
        center.x = ((float) width) / 2;
        center.y = ((float) height) / 2;

        // draw the stuff
        int i = 0;
        g.translate(center.x, center.y);
        for (Float p : pList) {
            drawAt(g, i++, p, center);
        }
        g.translate(-center.x, -center.y);

        if (isPaintCentered()) {
            g.translate(-tw/2, -th/2);
        }
    }

    /**
     * Gets value of centering hint. If true, shape will be positioned in the center of painted area.
     * @return Whether shape will be centered over painting area or not.
     */
    public boolean isPaintCentered() {
        return this.paintCentered;
    }

    /**
     * Centers shape in the area covered by the painter.
     * @param paintCentered Centering hint.
     */
    public void setPaintCentered(boolean paintCentered) {
        boolean old = isPaintCentered();
        this.paintCentered = paintCentered;
        firePropertyChange("paintCentered", old, isPaintCentered());
    }

    private void drawAt(Graphics2D g, int i, Float p, Float c) {
        g.setColor(calcFrameColor(i));
        paintRotatedCenteredShapeAtPoint(p, c, g);
    }

    private void paintRotatedCenteredShapeAtPoint(Float p, Float c, Graphics2D g) {
        Shape s = getPointShape();
        double hh = s.getBounds().getHeight() / 2;
        double wh = s.getBounds().getWidth() / 2;
        double t, x, y;
        double a = c.y - p.y;
        double b = p.x - c.x;
        double sa = Math.signum(a);
        double sb = Math.signum(b);
        sa = sa == 0 ? 1 : sa;
        sb = sb == 0 ? 1 : sb;
        a = Math.abs(a);
        b = Math.abs(b);
        t = Math.atan(a / b);
        t = sa > 0 ? sb > 0 ? -t : -Math.PI + t : sb > 0 ? t : Math.PI - t;
        x = Math.sqrt(a * a + b * b) - wh;
        y = -hh;
        g.rotate(t);
        g.translate(x, y);
        g.fill(s);
        g.translate(-x, -y);
        g.rotate(-t);

    }

    private Float calcPoint(float dist2go, Float startPoint,
            float[] sgmt, int w, int h) {
        Float f = new Float();
        if (sgmt[7] == PathIterator.SEG_LINETO) {
            // linear
            float a = sgmt[5] - startPoint.x;
            float b = sgmt[6] - startPoint.y;
            float pathLen = sgmt[0];
            f.x = startPoint.x + a * dist2go / pathLen;
            f.y = startPoint.y + b * dist2go / pathLen;
        } else if (sgmt[7] == PathIterator.SEG_QUADTO) {
            // quadratic curve
            Float ctrl = new Float(sgmt[1]/w, sgmt[2]/h);
            Float end = new Float(sgmt[5]/w, sgmt[6]/h);
            Float start = new Float(startPoint.x/w, startPoint.y/h);

            // trans coords from abs to rel
            f = getXY(dist2go / sgmt[0], start, ctrl, end);
            f.x *= w;
            f.y *= h;

        } else if (sgmt[7] == PathIterator.SEG_CUBICTO) {
            // bezier curve
            float x = Math.abs(startPoint.x - sgmt[5]);
            float y = Math.abs(startPoint.y - sgmt[6]);

            // trans coords from abs to rel
            float c1rx = Math.abs(startPoint.x - sgmt[1]) / x;
            float c1ry = Math.abs(startPoint.y - sgmt[2]) / y;
            float c2rx = Math.abs(startPoint.x - sgmt[3]) / x;
            float c2ry = Math.abs(startPoint.y - sgmt[4]) / y;
            f = getXY(dist2go / sgmt[0], c1rx, c1ry, c2rx, c2ry);

            float a = startPoint.x - sgmt[5];
            float b = startPoint.y - sgmt[6];

            f.x = startPoint.x - f.x * a;
            f.y = startPoint.y - f.y * b;
        }
        return f;
    }

    
    /**
     * Calculates length of the linear segment.
     * @param coords Segment coordinates.
     * @param cp Start point.
     * @return Length of the segment.
     */
    private float calcLine(float[] coords, Float cp) {
        float a = cp.x - coords[0];
        float b = cp.y - coords[1];
        float c = (float) Math.sqrt(a * a + b * b);
        return c;
    }

    /**
     * Claclulates length of the cubic segment.
     * @param coords Segment coordinates.
     * @param cp Start point.
     * @return Length of the segment.
     */
    private float calcCube(float[] coords, Float cp) {
        float x = Math.abs(cp.x - coords[4]);
        float y = Math.abs(cp.y - coords[5]);

        // trans coords from abs to rel
        float c1rx = Math.abs(cp.x - coords[0]) / x;
        float c1ry = Math.abs(cp.y - coords[1]) / y;
        float c2rx = Math.abs(cp.x - coords[2]) / x;
        float c2ry = Math.abs(cp.y - coords[3]) / y;
        float prevLength = 0, prevX = 0, prevY = 0;
        for (float t = 0.01f; t <= 1.0f; t += .01f) {
            Float xy = getXY(t, c1rx, c1ry, c2rx, c2ry);
            prevLength += (float) Math.sqrt((xy.x - prevX) * (xy.x - prevX)
                    + (xy.y - prevY) * (xy.y - prevY));
            prevX = xy.x;
            prevY = xy.y;
        }
        // prev len is a fraction num of the real path length
        float z = ((Math.abs(x) + Math.abs(y)) / 2) * prevLength;
        return z;
    }

    /**
     * Calculates length of the quadratic segment
     * @param coords Segment coordinates
     * @param cp Start point.
     * @return Length of the segment.
     */
    private float calcLengthOfQuad(float[] coords, Float cp) {
        Float ctrl = new Float(coords[0], coords[1]);
        Float end = new Float(coords[2], coords[3]);
        // get abs values
        // ctrl1
        float c1ax = Math.abs(cp.x - ctrl.x) ;
        float c1ay = Math.abs(cp.y - ctrl.y) ;
        // end1
        float e1ax = Math.abs(cp.x - end.x) ;
        float e1ay = Math.abs(cp.y - end.y) ;
        // get max value on each axis
        float maxX = Math.max(c1ax, e1ax);
        float maxY = Math.max(c1ay, e1ay);

        // trans coords from abs to rel
        // ctrl1
        ctrl.x = c1ax / maxX;
        ctrl.y = c1ay / maxY;
        // end1
        end.x = e1ax / maxX;
        end.y = e1ay / maxY;

        // claculate length
        float prevLength = 0, prevX = 0, prevY = 0;
        for (float t = 0.01f; t <= 1.0f; t += .01f) {
            Float xy = getXY(t, new Float(0,0), ctrl, end);
            prevLength += (float) Math.sqrt((xy.x - prevX) * (xy.x - prevX)
                    + (xy.y - prevY) * (xy.y - prevY));
            prevX = xy.x;
            prevY = xy.y;
        }
        // prev len is a fraction num of the real path length
        float a = Math.abs(coords[2] - cp.x);
        float b = Math.abs(coords[3] - cp.y);
        float dist = (float) Math.sqrt(a*a+b*b);
        return prevLength * dist;
    }

    /**
     * Calculates the XY point for a given t value.
     * 
     * The general spline equation is: x = b0*x0 + b1*x1 + b2*x2 + b3*x3 y =
     * b0*y0 + b1*y1 + b2*y2 + b3*y3 where: b0 = (1-t)^3 b1 = 3 * t * (1-t)^2 b2 =
     * 3 * t^2 * (1-t) b3 = t^3 We know that (x0,y0) == (0,0) and (x1,y1) ==
     * (1,1) for our splines, so this simplifies to: x = b1*x1 + b2*x2 + b3 y =
     * b1*x1 + b2*x2 + b3
     * 
     * @author chet
     * 
     * @param t parametric value for spline calculation
     */
    private Float getXY(float t, float x1, float y1, float x2, float y2) {
        Float xy;
        float invT = (1 - t);
        float b1 = 3 * t * (invT * invT);
        float b2 = 3 * (t * t) * invT;
        float b3 = t * t * t;
        xy = new Float((b1 * x1) + (b2 * x2) + b3, (b1 * y1)
                + (b2 * y2) + b3);
        return xy;
    }

    /**
     * Calculates relative position of the point on the quad curve in time t&lt;0,1&gt;.
     * @param t distance on the curve
     * @param ctrl Control point in rel coords
     * @param end End point in rel coords
     * @return Solution of the quad equation for time T in non complex space in rel coords.
     */
    public static Float getXY(float t, Float begin, Float ctrl, Float end) {
        /*
         *     P1 = (x1, y1) - start point of curve
         *     P2 = (x2, y2) - end point of curve
         *     Pc = (xc, yc) - control point
         *
         *     Pq(t) = P1*(1 - t)^2 + 2*Pc*t*(1 - t) + P2*t^2 =
         *           = (P1 - 2*Pc + P2)*t^2 + 2*(Pc - P1)*t + P1
         *     t = [0:1]
         *     // thx Jim ...
         *     
         *     b0 = (1 -t)^2, b1 = 2*t*(1-t), b2 = t^2
         */
        Float xy;
        float invT = (1 - t);
        float b0 = invT * invT;
        float b1 = 2 * t * invT ;
        float b2 = t * t;
        xy = new Float(b0 * begin.x + (b1 * ctrl.x) + b2* end.x, b0 * begin.y +  (b1 * ctrl.y) + b2* end.y);
        
        return xy;
    }
    
    /**
     * Selects appropriate color for given frame based on the frame position and gradient difference.
     * @param i Frame.
     * @return Frame color.
     */
    private Color calcFrameColor(final int i) {
        if (frame == -1) {
            return getBaseColor();
        }

        for (int t = 0; t < getTrailLength(); t++) {
            if (direction == Direction.RIGHT
                    && i == (frame - t + getPoints()) % getPoints()) {
                float terp = 1 - ((float) (getTrailLength() - t))
                        / (float) getTrailLength();
                return PaintUtils.interpolate(getBaseColor(),
                        getHighlightColor(), terp);
            } else if (direction == Direction.LEFT
                    && i == (frame + t) % getPoints()) {
                float terp = ((float) (t)) / (float) getTrailLength();
                return PaintUtils.interpolate(getBaseColor(),
                        getHighlightColor(), terp);
            }
        }
        return getBaseColor();
    }

    /**
     * Gets current frame.
     * @return Current frame.
     */
    public int getFrame() {
        return frame;
    }

    /**Sets current frame.
     * @param frame Current frame.
     */
    public void setFrame(int frame) {
        int old = getFrame();
        this.frame = frame;
        firePropertyChange("frame", old, getFrame());
    }

    /**
     * Gets base color.
     * @return Base color.
     */
    public Color getBaseColor() {
        return baseColor;
    }

    /**
     * Sets new base color. Bound property.
     * @param baseColor Base color.
     */
    public void setBaseColor(Color baseColor) {
        Color old = getBaseColor();
        this.baseColor = baseColor;
        firePropertyChange("baseColor", old, getBaseColor());
    }

    /**
     * Gets highlight color.
     * @return Current highlight color.
     */
    public Color getHighlightColor() {
        return highlightColor;
    }

    /**
     * Sets new highlight color. Bound property.
     * @param highlightColor New highlight color.
     */
    public void setHighlightColor(Color highlightColor) {
        Color old = getHighlightColor();
        this.highlightColor = highlightColor;
        firePropertyChange("highlightColor", old, getHighlightColor());
    }

    /**
     * Gets total amount of distinct points in spinner.
     * @return Total amount of points.
     */
    public int getPoints() {
        return points;
    }

    /**
     * Sets total amount of points in spinner. Bound property.
     * @param points Total amount of points.
     */
    public void setPoints(int points) {
        int old = getPoints();
        this.points = points;
        firePropertyChange("points", old, getPoints());
    }

    /**
     * Gets length of trail in number of points.
     * @return Trail lenght.
     */
    public int getTrailLength() {
        return trailLength;
    }

    /**
     * Sets length of the trail in points. Bound property.
     * @param trailLength Trail length in points.
     */
    public void setTrailLength(int trailLength) {
        int old = getTrailLength();
        this.trailLength = trailLength;
        firePropertyChange("trailLength", old, getTrailLength());
    }

    /**
     * Gets shape of current point.
     * @return Shape of the point.
     */
    public final Shape getPointShape() {
        return pointShape;
    }

    /**
     * Sets new point shape. Bound property.
     * @param pointShape new Shape.
     */
    public final void setPointShape(Shape pointShape) {
        Shape old = getPointShape();
        this.pointShape = pointShape;
        firePropertyChange("pointShape", old, getPointShape());
    }

    /**
     * Gets current trajectory.
     * @return Current spinner trajectory .
     */
    public final Shape getTrajectory() {
        return trajectory;
    }

    /**
     * Sets new trajectory. Expected trajectory have to be closed shape. Bound property.
     * @param trajectory New trajectory.
     */
    public final void setTrajectory(Shape trajectory) {
        Shape old = getTrajectory();
        this.trajectory = trajectory;
        firePropertyChange("trajectory", old, getTrajectory());
    }
    
    /**
     * Gets current direction of spinning.
     * @return Current spinning direction.
     */
    public Direction getDirection() {
        return direction;
    }

    /**
     * Sets new spinning direction.
     * @param dir Spinning direction.
     */
    public void setDirection(Direction dir) {
        Direction old = getDirection();
        this.direction = dir;
        firePropertyChange("direction", old, getDirection());
    }
}
