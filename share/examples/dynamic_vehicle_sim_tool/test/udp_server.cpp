/****************** <VPR heading BEGIN do not edit this line> *****************
 *
 * VR Juggler Portable Runtime
 *
 * Original Authors:
 *   Allen Bierbaum, Patrick Hartling, Kevin Meinert, Carolina Cruz-Neira
 *
 ****************** <VPR heading END do not edit this line> ******************/

/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VR Juggler is (C) Copyright 1998-2010 by Iowa State University
 *
 * Original Authors:
 *   Allen Bierbaum, Christopher Just,
 *   Patrick Hartling, Kevin Meinert,
 *   Carolina Cruz-Neira, Albert Baker
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
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <iostream>
#include <fstream>

#include <stdlib.h>

#include <vpr/vpr.h>
#include <vpr/IO/Socket/SocketDatagram.h>
#include <vpr/IO/Socket/InetAddr.h>

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string.hpp>

#include <boost/lexical_cast.hpp>

int main (int argc, char* argv[])
{
   int status;
   vpr::Uint16 port(12345);     // Default listening port

   // If a command-line argument was given, use it as the port value instead
   // of the default.
   if ( argc == 2 )
   {
      port = (unsigned short) atoi(argv[1]);
   }

   try
   {
        // Create a datagram socket that will be bound to port.
        vpr::InetAddr local;
        local.setPort(port);

        vpr::SocketDatagram sock(local, vpr::InetAddr::AnyAddr);

        // Bind the socket to the port.
        sock.open();
        sock.bind();

        //Now lets connet to the multicast group
        // Create a socket that is sending to a remote host named in the first
        // argument listening on the port named in the second argument.
        vpr::InetAddr remote_addr;
        remote_addr.setAddress("225.0.0.37", port);
        //vpr::SocketDatagram sock(vpr::InetAddr::AnyAddr, remote_addr);
        //vpr::SocketOptions::Types option = vpr::SocketOptions::AddMember;
        //vpr::SocketOptions::Data data;
        //data.mcast_add_member = vpr::McastReq( remote_addr, vpr::InetAddr::AnyAddr);
        vpr::McastReq data = vpr::McastReq( remote_addr, vpr::InetAddr::AnyAddr);
        sock.addMcastMember( data );
        typedef std::vector< std::string > split_vector_type;

        char recv_buf[2048];
        memset(recv_buf, '\0', sizeof(recv_buf));

        std::ofstream outputFile( "test_data_out.txt" );
        // Loop forever reading messages from clients.
        while ( true )
      {
         vpr::InetAddr addr;

         try
         {
            // Read a message from a client.
            const vpr::Uint32 bytes = sock.recvfrom(recv_buf, sizeof(recv_buf),
                                                    addr);
            std::string tempbuff;
            for( size_t i = 0; i < 2048; ++i )
            {
                if( recv_buf[ i ] == '\0' )
                {
                    tempbuff.push_back( ' ' );
                    continue;
                }
                std::cout << recv_buf[ i ] << std::endl;
                tempbuff.push_back( recv_buf[ i ] );
            }
            
            boost::algorithm::trim( tempbuff );
            
            // If we read anything, print it and send a response.
            std::cout << "Read '" << tempbuff << "' (" << bytes
                      << " bytes) from " << addr.getAddressString()
                      << std::endl;
             
             outputFile << tempbuff << std::endl;
             
             split_vector_type splitVec;
             boost::split( splitVec, tempbuff, boost::is_any_of(" "), boost::token_compress_on );
             double tempDouble = 0;
             for( size_t i = 0; i < splitVec.size(); ++i )
             {
                 std::cout << "<" << splitVec.at( i ) << "> ";
                 try
                 {
                     tempDouble = boost::lexical_cast<double>( splitVec.at( i ) );
                     //positionData.push_back( tempDouble );
                 }
                 catch( boost::bad_lexical_cast& ex )
                 {
                     std::cout << "cannot cast data " << ex.what() << std::endl;
                 }
             }
             std::cout << std::flush << std::endl;

         }
         catch (vpr::IOException& ex)
         {
            std::cerr << "Caught an I/O exception while communicating "
                      << "with client at " << addr.getAddressString()
                      << ":\n" << ex.what() << std::endl;
         }
      }

      sock.close();
       outputFile.close();
    
      status = EXIT_SUCCESS;
   }
   catch (vpr::SocketException& ex)
   {
      std::cerr << "Caught a socket exception:\n" << ex.what() << std::endl;
      status = EXIT_FAILURE;
   }
   catch (vpr::IOException& ex)
   {
      std::cerr << "Caught an I/O exception:\n" << ex.what() << std::endl;
      status = EXIT_FAILURE;
   }

   return status;
}
