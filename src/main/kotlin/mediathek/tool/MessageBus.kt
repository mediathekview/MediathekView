package mediathek.tool

import mediathek.gui.messages.BaseEvent
import net.engio.mbassy.bus.MBassador
import net.engio.mbassy.bus.config.BusConfiguration
import net.engio.mbassy.bus.config.Feature
import net.engio.mbassy.bus.config.IBusConfiguration
import net.engio.mbassy.bus.error.PublicationError
import org.apache.logging.log4j.LogManager

/**
 * The central message bus to dispatch notifications throughout the program
 */
object MessageBus {
    @JvmStatic
    val messageBus: MBassador<BaseEvent>
    private val logger = LogManager.getLogger()

    init {
        messageBus = MBassador(BusConfiguration()
            .addFeature(Feature.SyncPubSub.Default())
            .addFeature(Feature.AsynchronousHandlerInvocation.Default())
            .addFeature(Feature.AsynchronousMessageDispatch.Default())
            .addPublicationErrorHandler { error: PublicationError ->
                logger.error(error.message, error.cause)
            }
            .setProperty(IBusConfiguration.Properties.BusId, "global bus"))
    }
}