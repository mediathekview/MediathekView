package mediathek.tool.http

import mediathek.config.CommandLineOptions
import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.dns.DnsSelector
import okhttp3.ConnectionSpec
import okhttp3.OkHttpClient
import okhttp3.logging.HttpLoggingInterceptor
import org.apache.logging.log4j.LogManager
import java.net.InetSocketAddress
import java.net.Proxy
import java.util.concurrent.atomic.AtomicReference
import kotlin.time.Duration.Companion.seconds
import kotlin.time.toJavaDuration

object MVHttpClient {
    private val NETWORK_TIMEOUT = 10.seconds

    private val logger = LogManager.getLogger(MVHttpClient::class.java)
    val byteCounter = ByteCounter()
    private val dnsSelector = DnsSelector()
    private val httpClientReference = AtomicReference(createHttpClient())
    val httpClient: OkHttpClient
        get() = httpClientReference.get()

    fun reloadProxySettings() {
        val oldClient = httpClientReference.getAndSet(createHttpClient())
        oldClient.dispatcher.executorService.shutdown()
        oldClient.connectionPool.evictAll()
    }

    private fun createHttpClient(): OkHttpClient {
        var proxyHost = System.getProperty("http.proxyHost")
        var proxyPort = System.getProperty("http.proxyPort")

        return try {
            if (!proxyHost.isNullOrEmpty() && !proxyPort.isNullOrEmpty()) {
                val proxy = Proxy(Proxy.Type.HTTP, InetSocketAddress(proxyHost, proxyPort.toInt()))
                logger.info("MVHttpClient: Proxy configured from environment variables: ({})", proxyHost)
                createProxyClient(proxy)
            } else {
                try {
                    val config = ApplicationConfiguration.getInstance()
                    proxyHost = config.httpProxyHost
                    proxyPort = config.httpProxyPort
                    if (proxyHost.isNotEmpty() && proxyPort.isNotEmpty()) {
                        val proxy = Proxy(Proxy.Type.HTTP, InetSocketAddress(proxyHost, proxyPort.toInt()))
                        logger.info("MVHttpClient: Proxy configured from application config: ({})", proxyHost)
                        createProxyClient(proxy)
                    } else {
                        createNonProxyClient()
                    }
                } catch (_: NoSuchElementException) {
                    createNonProxyClient()
                }
            }
        } catch (ex: NumberFormatException) {
            logger.error("PROXY config failed. Creating non proxy config", ex)
            createNonProxyClient()
        }
    }

    private fun defaultClientBuilder(): OkHttpClient.Builder =
        OkHttpClient.Builder().apply {
            if (CommandLineOptions.isHttpTrafficDebuggingEnabled()) {
                val interceptor = HttpLoggingInterceptor(logger::trace)
                val level = try {
                    val levelName = ApplicationConfiguration.getInstance().httpTrafficTraceLevel
                    HttpLoggingInterceptor.Level.valueOf(levelName)
                } catch (_: Exception) {
                    logger.error("Error reading http traffic debug trace level, using BASIC")
                    HttpLoggingInterceptor.Level.BASIC
                }
                interceptor.level = level
                addInterceptor(interceptor)
                connectionSpecs(listOf(ConnectionSpec.MODERN_TLS, ConnectionSpec.COMPATIBLE_TLS))
            }

            connectTimeout(NETWORK_TIMEOUT.toJavaDuration())
            writeTimeout(NETWORK_TIMEOUT.toJavaDuration())
            readTimeout(NETWORK_TIMEOUT.toJavaDuration())
            socketFactory(byteCounter.socketFactory())
            followRedirects(true)
            followSslRedirects(true)
            dns(dnsSelector)
        }

    private fun createProxyClient(proxy: Proxy): OkHttpClient {
        val proxyAuthenticator = OkHttpProxyAuthenticator().proxyAuthenticator
        return defaultClientBuilder()
            .proxy(proxy)
            .apply {
                if (proxyAuthenticator != null) {
                    this.proxyAuthenticator(proxyAuthenticator)
                }
            }
            .build()
    }

    private fun createNonProxyClient(): OkHttpClient {
        logger.info("MVHttpClient: Proxy not configured")
        return defaultClientBuilder().build()
    }
}
