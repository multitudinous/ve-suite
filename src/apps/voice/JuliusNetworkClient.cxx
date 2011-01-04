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

#include "apps/voice/JuliusNetworkClient.h"

#include <ace/INET_Addr.h>
#include <ace/Time_Value.h>
#include <ace/os_include/sys/os_uio.h>

#include <iostream>

JuliusNetworkClient::JuliusNetworkClient()
   : mParser(NULL), mConnected(false), mStop(false)
{
}

JuliusNetworkClient::~JuliusNetworkClient()
{
    if (mConnected)
    {
        disconnect();
    }
}

bool
JuliusNetworkClient::connect(const std::string& host, const unsigned short port)
{
    if (!mConnected)
    {
        ACE_INET_Addr addr(port, host.c_str(), AF_INET);
        ACE_Time_Value timeout(120);
        if (mConnector.connect(mDataStream, addr, &timeout) == -1)
        {
            return false;
        }
        mConnected = true;
    }
    return true;
}

void
JuliusNetworkClient::disconnect()
{
    if (mConnected)
    {
        /// The ACE documentation claims that close_writer() must be called
        /// first on Win32 platforms.
        mDataStream.close_writer();
        mDataStream.close();
        mConnected = false;
    }
}

bool
JuliusNetworkClient::startDataLoop()
{
    if (!mParser)
    {
        std::cerr << "[ERR] Attempted to start the network client data loop "
                << "without a setting a parser." << std::endl;
        return false;
    }
    if (!mConnected)
    {
        std::cerr << "[ERR] Attempted to start the network client data loop "
                << "without connecting to Julius." << std::endl;
        return false;
    }
    while (mConnected && !mStop)
    {
        iovec data = {0, 0};
        ACE_Time_Value timeout(30);
        ssize_t read = mDataStream.recvv(&data, &timeout);
        if (read > 0 && data.iov_len > 0)
        {
            std::string data_to_parse = reinterpret_cast<char*>(data.iov_base);
            mParser->parse(data_to_parse);
            delete[] reinterpret_cast<char*>(data.iov_base);
        }
    }
    return true;
}

// vim:ts=4:sw=4:et:tw=0
