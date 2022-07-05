/*
 * $Id: Morphing2D.java 3863 2010-10-26 02:53:32Z kschaefe $
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
package org.jdesktop.swingx.geom;

import java.awt.*;
import java.awt.geom.*;

/**
 * <p>A morphing shape is a shape which geometry is constructed from two
 * other shapes: a start shape and an end shape.</p>
 * <p>The morphing property of a morphing shape defines the amount of
 * transformation applied to the start shape to turn it into the end shape.</p>
 * <p>Both shapes must have the same winding rule.</p>
 *
 * @author Jim Graham
 * @author Romain Guy <romain.guy@mac.com> (Maintainer)
 */
public class Morphing2D implements Shape {
    private double morph;
    private Geometry startGeometry;
    private Geometry endGeometry;

    /**
     * <p>Creates a new morphing shape. A morphing shape can be used to turn
     * one shape into another one. The transformation can be controlled by the
     * morph property.</p>
     *
     * @param startShape the shape to morph from
     * @param endShape   the shape to morph to
     *
     * @throws IllegalPathStateException if the shapes do not have the same
     *                                   winding rule
     * @see #getMorphing()
     * @see #setMorphing(double)
     */
    public Morphing2D(Shape startShape, Shape endShape) {
        startGeometry = new Geometry(startShape);
        endGeometry = new Geometry(endShape);
        if (startGeometry.getWindingRule() != endGeometry.getWindingRule()) {
            throw new IllegalPathStateException("shapes must use same " +
                                                "winding rule");
        }
        double tvals0[] = startGeometry.getTvals();
        double tvals1[] = endGeometry.getTvals();
        double masterTvals[] = mergeTvals(tvals0, tvals1);
        startGeometry.setTvals(masterTvals);
        endGeometry.setTvals(masterTvals);
    }

    /**
     * <p>Returns the morphing value between the two shapes.</p>
     *
     * @return the morphing value between the two shapes
     *
     * @see #setMorphing(double)
     */
    public double getMorphing() {
        return morph;
    }

    /**
     * <p>Sets the morphing value between the two shapes. This value controls
     * the transformation from the start shape to the end shape. A value of 0.0
     * is the start shape. A value of 1.0 is the end shape. A value of 0.5 is a
     * new shape, morphed half way from the start shape to the end shape.</p>
     * <p>The specified value should be between 0.0 and 1.0. If not, the value
     * is clamped in the appropriate range.</p>
     *
     * @param morph the morphing value between the two shapes
     *
     * @see #getMorphing()
     */
    public void setMorphing(double morph) {
        if (morph > 1) {
            morph = 1;
        } else if (morph >= 0) {
            // morphing is finite, not NaN, and in range
        } else {
            // morph is < 0 or NaN
            morph = 0;
        }
        this.morph = morph;
    }

    private static double interp(double v0, double v1, double t) {
        return (v0 + ((v1 - v0) * t));
    }

    private static double[] mergeTvals(double tvals0[], double tvals1[]) {
        int i0 = 0;
        int i1 = 0;
        int numtvals = 0;
        while (i0 < tvals0.length && i1 < tvals1.length) {
            double t0 = tvals0[i0];
            double t1 = tvals1[i1];
            if (t0 <= t1) {
                i0++;
            }
            if (t1 <= t0) {
                i1++;
            }
            numtvals++;
        }
        double newtvals[] = new double[numtvals];
        i0 = 0;
        i1 = 0;
        numtvals = 0;
        while (i0 < tvals0.length && i1 < tvals1.length) {
            double t0 = tvals0[i0];
            double t1 = tvals1[i1];
            if (t0 <= t1) {
                newtvals[numtvals] = t0;
                i0++;
            }
            if (t1 <= t0) {
                newtvals[numtvals] = t1;
                i1++;
            }
            numtvals++;
        }
        return newtvals;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Rectangle getBounds() {
        return getBounds2D().getBounds();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Rectangle2D getBounds2D() {
        int n = startGeometry.getNumCoords();
        double xmin, ymin, xmax, ymax;
        xmin = xmax = interp(startGeometry.getCoord(0), endGeometry.getCoord(0),
                             morph);
        ymin = ymax = interp(startGeometry.getCoord(1), endGeometry.getCoord(1),
                             morph);
        for (int i = 2; i < n; i += 2) {
            double x = interp(startGeometry.getCoord(i),
                              endGeometry.getCoord(i), morph);
            double y = interp(startGeometry.getCoord(i + 1),
                              endGeometry.getCoord(i + 1), morph);
            if (xmin > x) {
                xmin = x;
            }
            if (ymin > y) {
                ymin = y;
            }
            if (xmax < x) {
                xmax = x;
            }
            if (ymax < y) {
                ymax = y;
            }
        }
        return new Rectangle2D.Double(xmin, ymin, xmax - xmin, ymax - ymin);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(double x, double y) {
        throw new InternalError("unimplemented");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(Point2D p) {
        return contains(p.getX(), p.getY());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean intersects(double x, double y, double w, double h) {
        throw new InternalError("unimplemented");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean intersects(Rectangle2D r) {
        return intersects(r.getX(), r.getY(), r.getWidth(), r.getHeight());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(double x, double y, double w, double h) {
        throw new InternalError("unimplemented");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(Rectangle2D r) {
        return contains(r.getX(), r.getY(), r.getWidth(), r.getHeight());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PathIterator getPathIterator(AffineTransform at) {
        return new Iterator(at, startGeometry, endGeometry, morph);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PathIterator getPathIterator(AffineTransform at, double flatness) {
        return new FlatteningPathIterator(getPathIterator(at), flatness);
    }

    private static class Geometry {
        static final double THIRD = (1.0 / 3.0);
        static final double MIN_LEN = 0.001;
        double bezierCoords[];
        int numCoords;
        int windingrule;
        double myTvals[];

        public Geometry(Shape s) {
            // Multiple of 6 plus 2 more for initial moveto
            bezierCoords = new double[20];
            PathIterator pi = s.getPathIterator(null);
            windingrule = pi.getWindingRule();
            if (pi.isDone()) {
                // We will have 1 segment and it will be all zeros
                // It will have 8 coordinates (2 for moveto, 6 for cubic)
                numCoords = 8;
            }
            double coords[] = new double[6];
            int type = pi.currentSegment(coords);
            pi.next();
            if (type != PathIterator.SEG_MOVETO) {
                throw new IllegalPathStateException("missing initial moveto");
            }
            double curx = bezierCoords[0] = coords[0];
            double cury = bezierCoords[1] = coords[1];
            double newx, newy;
            numCoords = 2;
            while (!pi.isDone()) {
                if (numCoords + 6 > bezierCoords.length) {
                    // Keep array size to a multiple of 6 plus 2
                    int newsize = (numCoords - 2) * 2 + 2;
                    double newCoords[] = new double[newsize];
                    System.arraycopy(bezierCoords, 0, newCoords, 0, numCoords);
                    bezierCoords = newCoords;
                }
                switch (pi.currentSegment(coords)) {
                    case PathIterator.SEG_MOVETO:
                        throw new InternalError(
                                "Cannot handle multiple subpaths");
                    case PathIterator.SEG_CLOSE:
                        if (curx == bezierCoords[0] && cury == bezierCoords[1])
                        {
                            break;
                        }
                        coords[0] = bezierCoords[0];
                        coords[1] = bezierCoords[1];
                        /* NO BREAK */
                    case PathIterator.SEG_LINETO:
                        newx = coords[0];
                        newy = coords[1];
                        // A third of the way from curxy to newxy:
                        bezierCoords[numCoords++] = interp(curx, newx, THIRD);
                        bezierCoords[numCoords++] = interp(cury, newy, THIRD);
                        // A third of the way from newxy back to curxy:
                        bezierCoords[numCoords++] = interp(newx, curx, THIRD);
                        bezierCoords[numCoords++] = interp(newy, cury, THIRD);
                        bezierCoords[numCoords++] = curx = newx;
                        bezierCoords[numCoords++] = cury = newy;
                        break;
                    case PathIterator.SEG_QUADTO:
                        double ctrlx = coords[0];
                        double ctrly = coords[1];
                        newx = coords[2];
                        newy = coords[3];
                        // A third of the way from ctrlxy back to curxy:
                        bezierCoords[numCoords++] = interp(ctrlx, curx, THIRD);
                        bezierCoords[numCoords++] = interp(ctrly, cury, THIRD);
                        // A third of the way from ctrlxy to newxy:
                        bezierCoords[numCoords++] = interp(ctrlx, newx, THIRD);
                        bezierCoords[numCoords++] = interp(ctrly, newy, THIRD);
                        bezierCoords[numCoords++] = curx = newx;
                        bezierCoords[numCoords++] = cury = newy;
                        break;
                    case PathIterator.SEG_CUBICTO:
                        bezierCoords[numCoords++] = coords[0];
                        bezierCoords[numCoords++] = coords[1];
                        bezierCoords[numCoords++] = coords[2];
                        bezierCoords[numCoords++] = coords[3];
                        bezierCoords[numCoords++] = curx = coords[4];
                        bezierCoords[numCoords++] = cury = coords[5];
                        break;
                }
                pi.next();
            }
            // Add closing segment if either:
            // - we only have initial moveto - expand it to an empty cubic
            // - or we are not back to the starting point
            if ((numCoords < 8) ||
                curx != bezierCoords[0] ||
                cury != bezierCoords[1]) {
                newx = bezierCoords[0];
                newy = bezierCoords[1];
                // A third of the way from curxy to newxy:
                bezierCoords[numCoords++] = interp(curx, newx, THIRD);
                bezierCoords[numCoords++] = interp(cury, newy, THIRD);
                // A third of the way from newxy back to curxy:
                bezierCoords[numCoords++] = interp(newx, curx, THIRD);
                bezierCoords[numCoords++] = interp(newy, cury, THIRD);
                bezierCoords[numCoords++] = newx;
                bezierCoords[numCoords++] = newy;
            }
            // Now find the segment endpoint with the smallest Y coordinate
            int minPt = 0;
            double minX = bezierCoords[0];
            double minY = bezierCoords[1];
            for (int ci = 6; ci < numCoords; ci += 6) {
                double x = bezierCoords[ci];
                double y = bezierCoords[ci + 1];
                if (y < minY || (y == minY && x < minX)) {
                    minPt = ci;
                    minX = x;
                    minY = y;
                }
            }
            // If the smallest Y coordinate is not the first coordinate,
            // rotate the points so that it is...
            if (minPt > 0) {
                // Keep in mind that first 2 coords == last 2 coords
                double newCoords[] = new double[numCoords];
                // Copy all coordinates from minPt to the end of the
                // array to the beginning of the new array
                System.arraycopy(bezierCoords, minPt,
                                 newCoords, 0,
                                 numCoords - minPt);
                // Now we do not want to copy 0,1 as they are duplicates
                // of the last 2 coordinates which we just copied.  So
                // we start the source copy at index 2, but we still
                // copy a full minPt coordinates which copies the two
                // coordinates that were at minPt to the last two elements
                // of the array, thus ensuring that thew new array starts
                // and ends with the same pair of coordinates...
                System.arraycopy(bezierCoords, 2,
                                 newCoords, numCoords - minPt,
                                 minPt);
                bezierCoords = newCoords;
            }
            /* Clockwise enforcement:
             * - This technique is based on the formula for calculating
             *   the area of a Polygon.  The standard formula is:
             *   Area(Poly) = 1/2 * sum(x[i]*y[i+1] - x[i+1]y[i])
             * - The returned area is negative if the polygon is
             *   "mostly clockwise" and positive if the polygon is
             *   "mostly counter-clockwise".
             * - One failure mode of the Area calculation is if the
             *   Polygon is self-intersecting.  This is due to the
             *   fact that the areas on each side of the self-intersection
             *   are bounded by segments which have opposite winding
             *   direction.  Thus, those areas will have opposite signs
             *   on the accumulation of their area summations and end
             *   up canceling each other out partially.
             * - This failure mode of the algorithm in determining the
             *   exact magnitude of the area is not actually a big problem
             *   for our needs here since we are only using the sign of
             *   the resulting area to figure out the overall winding
             *   direction of the path.  If self-intersections cause
             *   different parts of the path to disagree as to the
             *   local winding direction, that is no matter as we just
             *   wait for the final answer to tell us which winding
             *   direction had greater representation.  If the final
             *   result is zero then the path was equal parts clockwise
             *   and counter-clockwise and we do not care about which
             *   way we order it as either way will require half of the
             *   path to unwind and re-wind itself.
             */
            double area = 0;
            // Note that first and last points are the same so we
            // do not need to process coords[0,1] against coords[n-2,n-1]
            curx = bezierCoords[0];
            cury = bezierCoords[1];
            for (int i = 2; i < numCoords; i += 2) {
                newx = bezierCoords[i];
                newy = bezierCoords[i + 1];
                area += curx * newy - newx * cury;
                curx = newx;
                cury = newy;
            }
            if (area < 0) {
                /* The area is negative so the shape was clockwise
                 * in a Euclidean sense.  But, our screen coordinate
                 * systems have the origin in the upper left so they
                 * are flipped.  Thus, this path "looks" ccw on the
                 * screen so we are flipping it to "look" clockwise.
                 * Note that the first and last points are the same
                 * so we do not need to swap them.
                 * (Not that it matters whether the paths end up cw
                 *  or ccw in the end as long as all of them are the
                 *  same, but above we called this section "Clockwise
                 *  Enforcement", so we do not want to be liars. ;-)
                 */
                // Note that [0,1] do not need to be swapped with [n-2,n-1]
                // So first pair to swap is [2,3] and [n-4,n-3]
                int i = 2;
                int j = numCoords - 4;
                while (i < j) {
                    curx = bezierCoords[i];
                    cury = bezierCoords[i + 1];
                    bezierCoords[i] = bezierCoords[j];
                    bezierCoords[i + 1] = bezierCoords[j + 1];
                    bezierCoords[j] = curx;
                    bezierCoords[j + 1] = cury;
                    i += 2;
                    j -= 2;
                }
            }
        }

        public int getWindingRule() {
            return windingrule;
        }

        public int getNumCoords() {
            return numCoords;
        }

        public double getCoord(int i) {
            return bezierCoords[i];
        }

        public double[] getTvals() {
            if (myTvals != null) {
                return myTvals;
            }

            // assert(numCoords >= 8);
            // assert(((numCoords - 2) % 6) == 0);
            double tvals[] = new double[(numCoords - 2) / 6 + 1];

            // First calculate total "length" of path
            // Length of each segment is averaged between
            // the length between the endpoints (a lower bound for a cubic)
            // and the length of the control polygon (an upper bound)
            double segx = bezierCoords[0];
            double segy = bezierCoords[1];
            double tlen = 0;
            int ci = 2;
            int ti = 0;
            while (ci < numCoords) {
                double prevx, prevy, newx, newy;
                prevx = segx;
                prevy = segy;
                newx = bezierCoords[ci++];
                newy = bezierCoords[ci++];
                prevx -= newx;
                prevy -= newy;
                double len = Math.sqrt(prevx * prevx + prevy * prevy);
                prevx = newx;
                prevy = newy;
                newx = bezierCoords[ci++];
                newy = bezierCoords[ci++];
                prevx -= newx;
                prevy -= newy;
                len += Math.sqrt(prevx * prevx + prevy * prevy);
                prevx = newx;
                prevy = newy;
                newx = bezierCoords[ci++];
                newy = bezierCoords[ci++];
                prevx -= newx;
                prevy -= newy;
                len += Math.sqrt(prevx * prevx + prevy * prevy);
                // len is now the total length of the control polygon
                segx -= newx;
                segy -= newy;
                len += Math.sqrt(segx * segx + segy * segy);
                // len is now sum of linear length and control polygon length
                len /= 2;
                // len is now average of the two lengths

                /* If the result is zero length then we will have problems
                 * below trying to do the math and bookkeeping to split
                 * the segment or pair it against the segments in the
                 * other shape.  Since these lengths are just estimates
                 * to map the segments of the two shapes onto corresponding
                 * segments of "approximately the same length", we will
                 * simply fudge the length of this segment to be at least
                 * a minimum value and it will simply grow from zero or
                 * near zero length to a non-trivial size as it morphs.
                 */
                if (len < MIN_LEN) {
                    len = MIN_LEN;
                }
                tlen += len;
                tvals[ti++] = tlen;
                segx = newx;
                segy = newy;
            }

            // Now set tvals for each segment to its proportional
            // part of the length
            double prevt = tvals[0];
            tvals[0] = 0;
            for (ti = 1; ti < tvals.length - 1; ti++) {
                double nextt = tvals[ti];
                tvals[ti] = prevt / tlen;
                prevt = nextt;
            }
            tvals[ti] = 1;
            return (myTvals = tvals);
        }

        public void setTvals(double newTvals[]) {
            double oldCoords[] = bezierCoords;
            double newCoords[] = new double[2 + (newTvals.length - 1) * 6];
            double oldTvals[] = getTvals();
            int oldci = 0;
            double x0, xc0, xc1, x1;
            double y0, yc0, yc1, y1;
            x0 = xc0 = xc1 = x1 = oldCoords[oldci++];
            y0 = yc0 = yc1 = y1 = oldCoords[oldci++];
            int newci = 0;
            newCoords[newci++] = x0;
            newCoords[newci++] = y0;
            double t0 = 0;
            double t1 = 0;
            int oldti = 1;
            int newti = 1;
            while (newti < newTvals.length) {
                if (t0 >= t1) {
                    x0 = x1;
                    y0 = y1;
                    xc0 = oldCoords[oldci++];
                    yc0 = oldCoords[oldci++];
                    xc1 = oldCoords[oldci++];
                    yc1 = oldCoords[oldci++];
                    x1 = oldCoords[oldci++];
                    y1 = oldCoords[oldci++];
                    t1 = oldTvals[oldti++];
                }
                double nt = newTvals[newti++];
                // assert(nt > t0);
                if (nt < t1) {
                    // Make nt proportional to [t0 => t1] range
                    double relt = (nt - t0) / (t1 - t0);
                    newCoords[newci++] = x0 = interp(x0, xc0, relt);
                    newCoords[newci++] = y0 = interp(y0, yc0, relt);
                    xc0 = interp(xc0, xc1, relt);
                    yc0 = interp(yc0, yc1, relt);
                    xc1 = interp(xc1, x1, relt);
                    yc1 = interp(yc1, y1, relt);
                    newCoords[newci++] = x0 = interp(x0, xc0, relt);
                    newCoords[newci++] = y0 = interp(y0, yc0, relt);
                    xc0 = interp(xc0, xc1, relt);
                    yc0 = interp(yc0, yc1, relt);
                    newCoords[newci++] = x0 = interp(x0, xc0, relt);
                    newCoords[newci++] = y0 = interp(y0, yc0, relt);
                } else {
                    newCoords[newci++] = xc0;
                    newCoords[newci++] = yc0;
                    newCoords[newci++] = xc1;
                    newCoords[newci++] = yc1;
                    newCoords[newci++] = x1;
                    newCoords[newci++] = y1;
                }
                t0 = nt;
            }
            bezierCoords = newCoords;
            numCoords = newCoords.length;
            myTvals = newTvals;
        }
    }

    private static class Iterator implements PathIterator {
        AffineTransform at;
        Geometry g0;
        Geometry g1;
        double t;
        int cindex;

        public Iterator(AffineTransform at,
                        Geometry g0, Geometry g1,
                        double t) {
            this.at = at;
            this.g0 = g0;
            this.g1 = g1;
            this.t = t;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int getWindingRule() {
            return g0.getWindingRule();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean isDone() {
            return (cindex > g0.getNumCoords());
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void next() {
            if (cindex == 0) {
                cindex = 2;
            } else {
                cindex += 6;
            }
        }

        double dcoords[];

        /**
         * {@inheritDoc}
         */
        @Override
        public int currentSegment(float[] coords) {
            if (dcoords == null) {
                dcoords = new double[6];
            }
            int type = currentSegment(dcoords);
            if (type != SEG_CLOSE) {
                coords[0] = (float) dcoords[0];
                coords[1] = (float) dcoords[1];
                if (type != SEG_MOVETO) {
                    coords[2] = (float) dcoords[2];
                    coords[3] = (float) dcoords[3];
                    coords[4] = (float) dcoords[4];
                    coords[5] = (float) dcoords[5];
                }
            }
            return type;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int currentSegment(double[] coords) {
            int type;
            int n;
            if (cindex == 0) {
                type = SEG_MOVETO;
                n = 2;
            } else if (cindex >= g0.getNumCoords()) {
                type = SEG_CLOSE;
                n = 0;
            } else {
                type = SEG_CUBICTO;
                n = 6;
            }
            if (n > 0) {
                for (int i = 0; i < n; i++) {
                    coords[i] = interp(g0.getCoord(cindex + i),
                                       g1.getCoord(cindex + i),
                                       t);
                }
                if (at != null) {
                    at.transform(coords, 0, coords, 0, n / 2);
                }
            }
            return type;
        }
    }
}