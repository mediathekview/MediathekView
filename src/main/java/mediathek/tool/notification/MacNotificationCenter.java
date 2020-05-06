package mediathek.tool.notification;

import javafx.application.Platform;
import mediathek.config.Konstanten;
import mediathek.tool.javafx.FXErrorDialog;
import mediathek.tool.notification.thrift.NotificationMessage;
import mediathek.tool.notification.thrift.ThriftNotificationCenter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TMultiplexedProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.jetbrains.annotations.NotNull;

import javax.jmdns.JmDNS;
import javax.jmdns.ServiceEvent;
import javax.jmdns.ServiceListener;
import javax.jmdns.impl.JmDNSImpl;
import java.io.Closeable;
import java.io.IOException;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.util.Arrays;

public class MacNotificationCenter implements INotificationCenter, ServiceListener, Closeable {
    private static final Logger logger = LogManager.getLogger();
    private static final String NOTIFICATION_SERVICE_NAME = "NotificationService";
    private JmDNS jmdns;
    private TSocket transport;
    private TProtocol protocol;
    private TMultiplexedProtocol mp;
    private ThriftNotificationCenter.Client client;

    public MacNotificationCenter() {
        setupJmdnsListener();
    }

    private void setupJmdnsListener() {
        try {
            jmdns = JmDNS.create(InetAddress.getLocalHost());
            jmdns.addServiceListener("_mv-notification._tcp.local.", this);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    @Override
    public void displayNotification(NotificationMessage msg) {
            try {
                logger.trace("Sending native notification to {} on serverPort {}", transport.getSocket().getInetAddress(), transport.getSocket().getPort());
                client.displayNotification(msg);
            }
            catch (Exception e) {
                showErrorDialog(e);
            }
    }

    private void showErrorDialog(Exception ex) {
        Platform.runLater(() -> FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, "Native Benachrichtigungen können nicht angezeigt werden",
                "Bitte stellen Sie sicher das das Hilfsprogramm gestartet ist.", ex));
    }

    @Override
    public void serviceAdded(ServiceEvent serviceEvent) {
        // do nothing as we don´t have useful information in here
    }

    @Override
    public void serviceRemoved(ServiceEvent serviceEvent) {
        System.out.println("REMOVING SERVICE DELETING TRANSPORT");
        if (transport != null) {
            if (transport.isOpen())
                transport.close();
        }
        transport = null;
        mp = null;
        client = null;
        protocol = null;

        //invalidate cache...maybe this prevents exceptions
        JmDNSImpl impl = (JmDNSImpl)jmdns;
        impl.getDns().cleanCache();
        impl.getCache().clear();
        impl.cleanCache();
    }

    @Override
    public void serviceResolved(ServiceEvent serviceEvent) {
        var info = serviceEvent.getInfo();
        System.out.println("SERVICE RESOLVED: " + serviceEvent.getType());

        var clientAddress = Arrays.stream(info.getInetAddresses()).filter(addr -> addr instanceof Inet4Address).findFirst();
        clientAddress.ifPresent(addr -> {
            System.out.println("SERVICE RESOLVED IPV4 TRANSPORT");
            System.out.println("Address: " + addr.getHostAddress());
            System.out.println("Port: " + info.getPort());

            if (transport == null) {
                System.out.println("TRANSPORT IS NULL");
                createThriftClient(addr.getHostAddress(),info.getPort());
            }
            else {
                //we already have a transport
                var socket = transport.getSocket();
                var boundAddr = socket.getInetAddress();
                var boundPort = socket.getPort();

                //check if transport is bound to resolved entry
                if (boundPort == info.getPort() && boundAddr.equals(addr)) {
                    // this transport is already assigned to the advertisement -> no action
                    System.out.println("PORT AN ADDR EQUAL -> NO ACTION");
                }
                else {
                    System.out.println("PORT AND ADDR NOTEQUAL -> RECREATE TRANSPORT");
                    //reassign transport
                    if (transport.isOpen())
                        transport.close();

                    createThriftClient(addr.getHostAddress(),info.getPort());
                }
            }
        });

    }

    private void createThriftClient(@NotNull String addr, int port) {
        try {
            System.out.println("CREATING THRIFT CLIENT");
            transport = new TSocket(addr, port);
            transport.open();
            protocol = new TBinaryProtocol(transport);
            mp = new TMultiplexedProtocol(protocol, NOTIFICATION_SERVICE_NAME);

            client = new ThriftNotificationCenter.Client(mp);
        } catch (TException e) {
            logger.error("Client creation failed!",e);
            transport = null;
            mp = null;
            client = null;
            protocol = null;
        }
    }

    @Override
    public void close() throws IOException {
        if (transport != null) {
            if (transport.isOpen())
                transport.close();
        }
        if (jmdns != null)
            jmdns.close();
    }
}
