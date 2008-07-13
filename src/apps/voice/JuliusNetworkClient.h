
#ifndef VE_VOICE_JULIUS_NETWORK_CLIENT_H_
#define VE_VOICE_JULIUS_NETWORK_CLIENT_H_

#include "apps/voice/JuliusXMLParser.h"

#include <ace/SOCK_Connector.h>

#include <string>

/**
 * Connects to a Julius daemon listening on a remote socket and retrieves the
 * XML results that Julius sends it.  It forwards these results to the
 * JuliusXMLParser.
 */
class JuliusNetworkClient
{
public:

    JuliusNetworkClient();

   ~JuliusNetworkClient();

    /**
     * Connects to the Julius Network daemon on the specified host and port.
     *
     * @param   host     the network host that the daemon is running on.
     * @param   port     the network port that the daemon is running on.
     *
     * @return     true if successful, false otherwise.
     */
    bool connect(const std::string& host, const unsigned short port=10500);

    /**
     * Disconnects from the Julius Network daemon; this is a no-op if
     * connect() hasn't been called yet.
     */
    void disconnect();

    /**
     * Queries the client to see if it's connected.
     */
    bool isConnected() const
    {
        return mConnected;
    }

    /**
     * Sets the parser that this client will use to parse the results 
     * received from Julius.
     *
     * @param   parser   the parser to use.
     */
    void setParser(const JuliusXMLParserPtr& parser)
    {
        mParser = parser;
    }

    /**
     * Starts the data loop; this will continuously sit and receive data from
     * the Julius daemon until the data loop is stopped.
     *
     * @note    This call will block indefinitely, so spinning it off in a new
     *          thread is required.  Call stopDataLoop() to make this function
     *          return.
     *
     * @return     true if no errors occurred, false if there was a problem
     *             setting it up.
     */
    bool startDataLoop();

    /**
     * Stops the data loop if it is already running.
     */
    void stopDataLoop()
    {
        mStop = true;
    }

private:

    /// The Julius XML Parser used to parse XML results received from the
    /// network peer.
    JuliusXMLParserPtr                              mParser;

    /// Are we connected?
    bool                                            mConnected;

    /// The peer stream used to receive data.
    ACE_SOCK_Connector::PEER_STREAM                 mDataStream;

    /// The socket connector used to connect the data stream and remote host.
    ACE_SOCK_Connector                              mConnector;

    /// Is it time to stop the data loop?
    bool                                            mStop;
};

#endif
// vim:ts=4:sw=4:et:tw=0
