/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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

class VjObs_i;
class cfdCommandArray;
class cfdSteadyStateVizHandler;
class cfdEnvironmentHandler;
class cfdModelHandler;
#ifdef _TAO
namespace CosNaming
{
   class NamingContext;
}
#else
#include <omniORB4/CORBA.h>
#endif
#include <vector>
#include <string>

class cfdVjObsWrapper
{
   public:
      cfdVjObsWrapper( void );
      ~cfdVjObsWrapper( void );
#ifdef _TAO
      void init( CosNaming::NamingContext* );
#else
      void init( CosNaming::NamingContext_ptr );
#endif
      cfdCommandArray* GetCommandArray( void );
      double GetShortArray( int );
      void GetCfdStateVariables( void );
      void PreFrameUpdate( void );
      void SetHandlers( cfdSteadyStateVizHandler*, 
                           cfdEnvironmentHandler*, 
                           cfdModelHandler* );
      int getStringTokens(char* buffer, char* delim, std::vector<std::string> &toks); // YANG, a string parsing utility, it is a not thread safe call.
      //cfdCosNaming* GetCosNaming( void );
      void InitCluster( void );
      void GetUpdateClusterStateVariables( void );
#ifdef _TAO
      CosNaming::NamingContext* naming_context;
#else
      CosNaming::NamingContext_ptr naming_context;
#endif
      VjObs_i* _vjObs;
};
#endif
