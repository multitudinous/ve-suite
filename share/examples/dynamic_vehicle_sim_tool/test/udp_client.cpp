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
#include <cstdlib>
#include <string>
#include <fstream>

#include <vpr/vpr.h>
#include <vpr/IO/Socket/SocketDatagram.h>

#include <boost/lexical_cast.hpp>

#define TEST_DVST 1
///IP Port and ip address ranges for multicast
///http://agenda.ictp.trieste.it/agenda_links/smr1335/sockets/node18.html
///http://ntrg.cs.tcd.ie/undergrad/4ba2/multicast/antony/example.html

//#define HELLO_PORT 12345
//#define HELLO_GROUP "225.0.0.37"

int main (int argc, char* argv[])
{
   /*if ( argc != 3 )
   {
      std::cerr << "Usage: " << argv[0] << " <address> <port>" << std::endl;
      return EXIT_FAILURE;
   }*/

   // Create a socket that is sending to a remote host named in the first
   // argument listening on the port named in the second argument.
   vpr::InetAddr remote_addr;
   remote_addr.setAddress("225.0.0.37", 6339);
   vpr::SocketDatagram sock(vpr::InetAddr::AnyAddr, remote_addr);
#ifndef TEST_DVST
   try
   {
      sock.open();

      try
      {
         // We only send to one host, so call connect().
         sock.connect();
          std::string buffer2;
         char buffer[40];
         memset(buffer, '\0', sizeof(buffer));
         //strcpy(buffer, "Hi,\0I'm\0a\0client");

         buffer[ 0 ] = 'H';buffer[ 1 ] = 'i';buffer[ 2 ] = ',';buffer[ 3 ] = '\0';
          buffer[ 4 ] = 'I';buffer[ 5 ] = '\'';buffer[ 6 ] = 'm';buffer[ 7 ] = ' ';buffer[ 8 ] = '\0';
          buffer[ 9 ] = 'a';buffer[ 10 ] = '\0';buffer[ 11 ] = ' ';buffer[ 12 ] = 'c';buffer[ 13 ] = 'l';buffer[ 14 ] = 'i';
          buffer[ 15 ] = 'e';buffer[ 16 ] = 'n';buffer[ 17 ] = 't';buffer[ 18 ] = '\0';
          int counter = 0;
          while( true )
          {
              // Write to the server.
              sock.write(buffer, 40);
              //vpr::System::msleep( 10 );  // thenth-second delay
              buffer2 = boost::lexical_cast<std::string>( counter );
              sock.write(buffer2.c_str(), buffer2.size());
              counter += 1;
          }

         // Read from the server.
         //const vpr::Uint32 bytes = sock.read(buffer, 40);

         // If the server reasponded, print the result.
         //std::cout << "Read " << bytes << " from server\n"
         //          << "    Got '" << buffer << "'" << std::endl;
      }
      catch (vpr::SocketException& ex)
      {
         std::cerr << "Caught a socket exception:\n" << ex.what()
                   << std::endl;
      }

      sock.close();
   }
   catch (vpr::IOException& ex)
   {
      std::cerr << "Caught an I/O exception:\n" << ex.what() << std::endl;
   }
#else
    try
    {
        sock.open();
        
        try
        {
            std::ifstream sampleFile( "test_data_out6.txt" );
            
            // We only send to one host, so call connect().
            sock.connect();
            //std::string buffer2;
            char buffer[2048];
            memset(buffer, '\0', sizeof(buffer));
            
            while( true )
            {
                sampleFile.getline( buffer, 2047 );
                
                //int counter = 0;
                do
                {
                    // Write to the server.
                    sock.write(buffer, 2047);
                    sampleFile.getline( buffer, 2047 );
                    //std::cout << sampleFile.good() << std::endl;
                }
                while( !sampleFile.eof() );
                sampleFile.clear();
                sampleFile.seekg(0, std::ios::beg);
            }
        }
        catch (vpr::SocketException& ex)
        {
            std::cerr << "Caught a socket exception:\n" << ex.what()
            << std::endl;
        }
        
        sock.close();
    }
    catch (vpr::IOException& ex)
    {
        std::cerr << "Caught an I/O exception:\n" << ex.what() << std::endl;
    }
#endif
   return EXIT_SUCCESS;
}
