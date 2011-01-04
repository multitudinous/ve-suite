/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

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
