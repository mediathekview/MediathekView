/*
 * Copyright (c) 2025-2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.swing

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.font.TextLayout
import java.awt.geom.*
import javax.swing.JComponent
import kotlin.time.Duration.Companion.milliseconds

/**
 * An infinite progress panel displays a rotating figure and a message to notice the user of a long task.
 */
open class InfiniteProgressPanel(
    text: String? = "",
    barsCount: Int = 14,
    shield: Float = 0.70f,
    fps: Float = 15.0f,
    rampDelay: Int = 300,
) : JComponent(), MouseListener {
    protected var ticker: Array<Area> = emptyArray()
    protected var started: Boolean = false
    protected var alphaLevel: Int = 0
    protected var rampDelay: Int = rampDelay.coerceAtLeast(0)
    protected var shield: Float = shield.coerceAtLeast(0.0f)
    protected var barsCount: Int = if (barsCount > 0) barsCount else 14
    protected var fps: Float = if (fps > 0.0f) fps else 15.0f
    protected var hints: RenderingHints = RenderingHints(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY).apply {
        put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        put(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
    }

    private val animationScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var animationJob: Job? = null

    private var messageText: String? = text

    fun setText(text: String?) {
        messageText = text
        repaint()
    }


    fun start() {
        animationJob?.cancel()
        addMouseListener(this)
        isVisible = true
        ticker = buildTicker()
        animationJob = launchAnimator(rampUp = true)
    }

    fun stop() {
        val runningAnimation = animationJob ?: return
        runningAnimation.cancel()
        animationJob = launchAnimator(rampUp = false)
    }

    fun interrupt() {
        animationJob?.cancel()
        animationJob = null
        tidy()
    }

    override fun removeNotify() {
        animationJob?.cancel()
        animationJob = null
        super.removeNotify()
    }

    override fun paintComponent(graphics: Graphics) {
        if (!started) {
            return
        }

        val graphics2D = graphics as Graphics2D
        graphics2D.setRenderingHints(hints)
        graphics2D.color = Color(255, 255, 255, (alphaLevel * shield).toInt())
        graphics2D.fillRect(0, 0, width, height)

        var maxY = 0.0
        ticker.forEachIndexed { index, area ->
            val channel = 224 - 128 / (index + 1)
            graphics2D.color = Color(channel, channel, channel, alphaLevel)
            graphics2D.fill(area)

            val bounds = area.bounds2D
            if (bounds.maxY > maxY) {
                maxY = bounds.maxY
            }
        }

        val displayText = messageText
        if (!displayText.isNullOrEmpty()) {
            val layout = TextLayout(displayText, font, graphics2D.fontRenderContext)
            val bounds = layout.bounds
            graphics2D.color = foreground
            layout.draw(
                graphics2D,
                ((width - bounds.width) / 2).toFloat(),
                (maxY + layout.leading + 2 * layout.ascent).toFloat(),
            )
        }
    }

    private fun tidy() {
        removeMouseListener(this)
        isVisible = false
    }

    private fun buildTicker(): Array<Area> {
        val center = Point2D.Double(width.toDouble() / 2, height.toDouble() / 2)
        val fixedAngle = 2.0 * Math.PI / barsCount

        return Array(barsCount) { index ->
            buildPrimitive().apply {
                val toCenter = AffineTransform.getTranslateInstance(center.x, center.y)
                val toBorder = AffineTransform.getTranslateInstance(45.0, -6.0)
                val toCircle = AffineTransform.getRotateInstance(-index * fixedAngle, center.x, center.y)

                val toWheel = AffineTransform()
                toWheel.concatenate(toCenter)
                toWheel.concatenate(toBorder)

                transform(toWheel)
                transform(toCircle)
            }
        }
    }

    private fun buildPrimitive(): Area {
        val body = Rectangle2D.Double(6.0, 0.0, 30.0, 12.0)
        val head = Ellipse2D.Double(0.0, 0.0, 12.0, 12.0)
        val tail = Ellipse2D.Double(30.0, 0.0, 12.0, 12.0)

        return Area(body).apply {
            add(Area(head))
            add(Area(tail))
        }
    }

    private fun launchAnimator(rampUp: Boolean): Job = animationScope.launch {
        val center = Point2D.Double(width.toDouble() / 2, height.toDouble() / 2)
        val fixedIncrement = 2.0 * Math.PI / barsCount
        val toCircle = AffineTransform.getRotateInstance(fixedIncrement, center.x, center.y)

        val start = System.currentTimeMillis()
        if (rampDelay == 0) {
            alphaLevel = if (rampUp) 255 else 0
        }

        started = true
        var inRamp = rampUp

        while (isActive) {
            if (!inRamp) {
                ticker.forEach { it.transform(toCircle) }
            }

            repaint()

            if (rampUp) {
                if (alphaLevel < 255) {
                    alphaLevel = (255 * (System.currentTimeMillis() - start) / rampDelay).toInt()
                    if (alphaLevel >= 255) {
                        alphaLevel = 255
                        inRamp = false
                    }
                }
            } else if (alphaLevel > 0) {
                alphaLevel = (255 - (255 * (System.currentTimeMillis() - start) / rampDelay)).toInt()
                if (alphaLevel <= 0) {
                    alphaLevel = 0
                    break
                }
            } else {
                break
            }

            delay(if (inRamp) 10.milliseconds else (1000 / fps).toLong().coerceAtLeast(1).milliseconds)
        }

        if (!rampUp) {
            started = false
            repaint()
            tidy()
            animationJob = null
        }
    }

    override fun mouseClicked(event: MouseEvent) = Unit

    override fun mousePressed(event: MouseEvent) = Unit

    override fun mouseReleased(event: MouseEvent) = Unit

    override fun mouseEntered(event: MouseEvent) = Unit

    override fun mouseExited(event: MouseEvent) = Unit
}
