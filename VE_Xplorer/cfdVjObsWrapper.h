/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VJOBSWRAPPER_H
#define CFD_VJOBSWRAPPER_H

namespace VE_Xplorer
{
   class VjObs_i;
   class cfdCommandArray;
}
#ifdef _TAO
namespace CosNaming{ class NamingContext; }
namespace CORBA{ class ORB; }
namespace PortableServer{ class POA; }
#else
#include <omniORB4/CORBA.h>
#endif
#include <vector>
#include <string>

namespace VE_Xplorer
{
   class cfdVjObsWrapper
   {
      public:
         cfdVjObsWrapper( void );
         ~cfdVjObsWrapper( void );
      #ifdef _TAO
         void init( CosNaming::NamingContext*, CORBA::ORB*, PortableServer::POA*, PortableServer::POA*, int, char** );
      #else
         void init( CosNaming::NamingContext_ptr, CORBA::ORB_ptr, int, char** );
      #endif
         cfdCommandArray* GetCommandArray( void );
         double GetShortArray( int );
         void GetCfdStateVariables( void );
         void PreFrameUpdate( void );
   
         int getStringTokens(char* buffer, char* delim, std::vector<std::string> &toks); // YANG, a string parsing utility, it is a not thread safe call.
         //cfdCosNaming* GetCosNaming( void );
         void InitCluster( void );
         void GetUpdateClusterStateVariables( void );
         float GetSetAppTime( float );
         long GetSetFrameNumber( long );
      #ifdef _TAO
         CosNaming::NamingContext* naming_context;
         CORBA::ORB* _orbPtr;
         PortableServer::POA* child_poa;
         PortableServer::POA* poa;
      #else
         CosNaming::NamingContext_ptr naming_context;
         CORBA::ORB_ptr _orbPtr;
      #endif
         VjObs_i* _vjObs;
   };
}
#endif
