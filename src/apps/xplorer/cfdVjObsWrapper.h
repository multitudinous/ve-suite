/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VJOBSWRAPPER_H
#define CFD_VJOBSWRAPPER_H
/*!\file cfdVjObsWrapper.h
cfdVjObsWrapper API
*/

/*!\class VE_Xplorer::cfdVjObsWrapper
*
*/

namespace VE_Xplorer
{
   class VjObs_i;
   class cfdCommandArray;
   
}

class Body_VEXplorer_i;

namespace CosNaming{ class NamingContext; }
namespace CORBA{ class ORB; }
namespace PortableServer{ class POA; }
#include <vector>
#include <string>

namespace VE_XML
{
   class Command;
}
namespace VE_Xplorer
{
class cfdVjObsWrapper
{
public:
   ///Constructor
   cfdVjObsWrapper( void );
   ///Destructor
   ~cfdVjObsWrapper( void );
   ///init function to pass corba pointers arounf for registration purposes
   void init( CosNaming::NamingContext*, CORBA::ORB*, PortableServer::POA*, PortableServer::POA*, int, char** );
   ///get command array
   ///shoudl be removed
   cfdCommandArray* GetCommandArray( void );
   ///get xml command data
   VE_XML::Command* GetXMLCommand( void );
   ///Get short array
   ///shoudl be removed
   double GetShortArray( int );
   ///get cfd state variables to be called by cfd app
   void GetCfdStateVariables( void );
   ///Called every frame
   void PreFrameUpdate( void );
   ///parse some command line input not sure what for
   int getStringTokens( const char* buffer, char* delim, std::vector<std::string> &toks); // YANG, a string parsing utility, it is a not thread safe call.
   ///Initialize the cluster data
   void InitCluster( void );
   ///Get the clsuter data in preframe
   void GetUpdateClusterStateVariables( void );
   ///Get/set the app time
   float GetSetAppTime( float );
   ///Set the frame number for the app
   long GetSetFrameNumber( long );
   ///This should be removed as soon as the quat cam code is fixed
   bool IsMaster( void );

   CosNaming::NamingContext* naming_context;///< holds the naming context for tao
   PortableServer::POA* child_poa;///< holds the poa server for tao
   PortableServer::POA* poa;///< holds the poa server for tao
   VjObs_i* _vjObs;///< holds the vjobs pointer for tao
   Body_VEXplorer_i* m_xplorer;///< holds the xplorer pointer for tao
private:
   CORBA::ORB* _orbPtr;///<holds the orb pointer for tao
   bool isMaster;///is the master should be removed
};
}
#endif
