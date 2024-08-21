package mediathek.tool.http;

import mediathek.config.Config;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.dns.DnsSelector;
import okhttp3.Authenticator;
import okhttp3.ConnectionSpec;
import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import org.apache.commons.configuration2.Configuration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.concurrent.TimeUnit;

public class MVHttpClient {
    private static final MVHttpClient ourInstance = new MVHttpClient();
    private final Logger logger = LogManager.getLogger(MVHttpClient.class);
    private final ByteCounter byteCounter = new ByteCounter();
    private final DnsSelector dnsSelector = new DnsSelector();
    private OkHttpClient httpClient;

    private MVHttpClient() {
        String proxyHost = System.getProperty("http.proxyHost");
        String proxyPort = System.getProperty("http.proxyPort");

        try {
            if (proxyHost != null && proxyPort != null && !proxyHost.isEmpty() && !proxyPort.isEmpty()) {
                //we are configuring the proxy from environment variables...
                final Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, Integer.parseInt(proxyPort)));
                setupProxyClients(proxy);
                logger.info("MVHttpClient: Proxy configured from environment variables: ({})", proxyHost);
            } else {
                //environment variables were not set, use application settings...
                try {
                    Configuration config = ApplicationConfiguration.getConfiguration();
                    proxyHost = config.getString(ApplicationConfiguration.HttpProxy.HOST);
                    proxyPort = config.getString(ApplicationConfiguration.HttpProxy.PORT);
                    if (!proxyHost.isEmpty() && !proxyPort.isEmpty()) {
                        final Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, Integer.parseInt(proxyPort)));
                        setupProxyClients(proxy);
                        logger.info("MVHttpClient: Proxy configured from application config: ({})", proxyHost);
                    } else {
                        //no proxy setup specified...
                        setupNonProxyClients();
                    }
                } catch (NoSuchElementException e) {
                    setupNonProxyClients();
                }
            }
        } catch (NumberFormatException ex) {
            setupNonProxyClients();
            logger.error("PROXY config failed. Creating non proxy config", ex);
        }
    }

    public static MVHttpClient getInstance() {
        return ourInstance;
    }

    /**
     * Build the client builder with default settings.
     *
     * @return A Builder with default settings.
     */
    private OkHttpClient.Builder getDefaultClientBuilder() {
        var builder = new OkHttpClient.Builder();

        if (Config.isHttpTrafficDebuggingEnabled()) {
            var interceptor = new HttpLoggingInterceptor(logger::trace);
            HttpLoggingInterceptor.Level level = HttpLoggingInterceptor.Level.BASIC;
            try {
                var levelName = ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_DEBUG_HTTP_TRAFFIC_TRACE_LEVEL);
                level = HttpLoggingInterceptor.Level.valueOf(levelName);
            } catch (Exception ignored) {
                logger.error("Error reading http traffic debug trace level, using BASIC");
            }
            interceptor.level(level);
            builder.addInterceptor(interceptor);
            builder.connectionSpecs(Arrays.asList(ConnectionSpec.MODERN_TLS, ConnectionSpec.COMPATIBLE_TLS));
        }

        builder.connectTimeout(NETWORK_TIMEOUT, TimeUnit.SECONDS)
                .writeTimeout(NETWORK_TIMEOUT, TimeUnit.SECONDS)
                .readTimeout(NETWORK_TIMEOUT, TimeUnit.SECONDS)
                .socketFactory(byteCounter.socketFactory())
                .followRedirects(true)
                .followSslRedirects(true)
                .dns(dnsSelector);
        return builder;
    }

    //TODO make configurable network timeout
    private static final long NETWORK_TIMEOUT = 10;

    public ByteCounter getByteCounter() {
        return byteCounter;
    }


    /**
     * Set the proxy parameters on the shared HTTP clients.
     *
     * @param proxy The proxy settings to be used.
     */
    private void setupProxyClients(Proxy proxy) {
        var prxManager = new OkHttpProxyAuthenticator();
        final Authenticator proxyAuthenticator = prxManager.getProxyAuthenticator();

        OkHttpClient.Builder tmpBuilder;
        tmpBuilder = getDefaultClientBuilder().proxy(proxy);

        if (proxyAuthenticator != null)
            tmpBuilder.proxyAuthenticator(proxyAuthenticator);
        httpClient = tmpBuilder.build();
    }

    /**
     * Setup HTTP client without proxy settings
     */
    private void setupNonProxyClients() {
        httpClient = getDefaultClientBuilder().build();
        logger.info("MVHttpClient: Proxy not configured");
    }

    public OkHttpClient getHttpClient() {
        return httpClient;
    }
}
