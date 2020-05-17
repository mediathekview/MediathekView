package mediathek.tool.notification;

import javafx.application.Platform;
import mediathek.config.Konstanten;
import mediathek.tool.javafx.FXErrorDialog;
import mediathek.tool.notification.thrift.NotificationMessage;
import mediathek.tool.notification.thrift.ThriftNotificationCenter;
import net.posick.mDNS.Browse;
import net.posick.mDNS.DNSSDListener;
import net.posick.mDNS.MulticastDNSService;
import net.posick.mDNS.ServiceInstance;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TMultiplexedProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.jetbrains.annotations.NotNull;
import org.xbill.DNS.Message;

import java.io.Closeable;
import java.io.IOException;
import java.net.Inet4Address;
import java.util.Arrays;

public class MacNotificationCenter implements INotificationCenter, Closeable {
    private static final Logger logger = LogManager.getLogger();
    private static final String NOTIFICATION_SERVICE_NAME = "NotificationService";
    private static final String MV_NOTIFICATION_MDNS_NAME = "_mv-notification._tcp.local.";
    private TSocket transport;
    private TProtocol protocol;
    private TMultiplexedProtocol mp;
    private ThriftNotificationCenter.Client client;
    private MulticastDNSService service;

    public MacNotificationCenter() {
        setupJmdnsListener();
    }

    private void setupJmdnsListener() {
        try {
            service = new MulticastDNSService();
            service.startServiceDiscovery(new Browse(MV_NOTIFICATION_MDNS_NAME), new DNSSDListener() {
                public void serviceDiscovered(Object id, ServiceInstance service) {
                    logger.trace("Notification Service Discovered");
                    Arrays.stream(service.getAddresses())
                            .filter(a -> a instanceof Inet4Address).findAny()
                            .ifPresent(a -> {
                                cleanupThrift();
                                createThriftClient(a.getHostAddress(), service.getPort());
                            });
                }


                public void serviceRemoved(Object id, ServiceInstance service) {
                    logger.trace("Notification Service Removed");
                    cleanupThrift();
                }


                public void handleException(Object id, Exception e) {
                }


                public void receiveMessage(Object id, Message message) {
                }
            });
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private void cleanupThrift() {
        if (transport != null) {
            if (transport.isOpen())
                transport.close();
        }
        transport = null;
        mp = null;
        client = null;
        protocol = null;
    }

    @Override
    public void displayNotification(NotificationMessage msg) {
        try {
            logger.trace("Sending native notification to {} on serverPort {}", transport.getSocket().getInetAddress(), transport.getSocket().getPort());
            client.displayNotification(msg);
        } catch (Exception e) {
            showErrorDialog(e);
        }
    }

    private void showErrorDialog(Exception ex) {
        Platform.runLater(() -> FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, "Native Benachrichtigungen können nicht angezeigt werden",
                "Bitte stellen Sie sicher das das Hilfsprogramm gestartet ist.", ex));
    }

    private void createThriftClient(@NotNull String addr, int port) {
        try {
            logger.trace("creating thrift client");
            transport = new TSocket(addr, port);
            transport.open();
            protocol = new TBinaryProtocol(transport);
            mp = new TMultiplexedProtocol(protocol, NOTIFICATION_SERVICE_NAME);

            client = new ThriftNotificationCenter.Client(mp);
        } catch (TException e) {
            logger.error("thrift client creation failed!", e);
            transport = null;
            mp = null;
            client = null;
            protocol = null;
        }
    }

    @Override
    public void close() throws IOException {
        if (service != null)
            service.close();

        if (transport != null) {
            if (transport.isOpen())
                transport.close();
        }
    }
}
