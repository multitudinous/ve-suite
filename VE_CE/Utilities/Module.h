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
#ifndef CE_UTILITIES_MODULE_H
#define CE_UTILITIES_MODULE_H
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/skel/moduleS.h"

#include "VE_Open/XML/Model/ModelStrongPtr.h"
#include "VE_Open/XML/Model/ModelWeakPtr.h"

#include <vector>
#include <string>
namespace VE_XML
{
namespace VE_Model
{
   class Model;
   class Port;
}
}

namespace VE_XML
{
   class Command;
   class XMLObject;
}

namespace VE_CE
{
namespace Utilities
{
class OPort;
class IPort;
class Network;
class Connection;

///??
class VE_CE_UTILS_EXPORTS Module 
{
public:
   Module();
   Module( const Module& );
   ~Module();

   void copy( const Module& );

   ///return number of output ports
   size_t numOPorts( void );
   ///return number of input ports
   size_t numIPorts( void );

   ///Get the vector index for the specific input port id 
   int iportIdx( int idx );
   ///Get the vector index for the specific output port id 
   int oportIdx( int idx );

   ///Add input port
   void addIPort (int, Connection*);
   ///Add output port
   void addOPort (int, Connection*);

   ///Get the ith output port
   OPort* getOPort( int i );
   ///Get the ith input port
   IPort* getIPort( int i );
  
   ///Get Feedback Port
   IPort* getFBPort ();

   ///Get output port data for the specific port
   int getPortData( int, VE_XML::Command& );
   ///Set output port data for the specific port
   int setPortData( int, VE_XML::Command* );

   int getPortProfile( int, Types::Profile_out& );
   int setPortProfile( int, const Types::Profile* );

   ///Accessors for input data
   std::vector< VE_XML::Command* > GetInputData( void );
   void SetInputData( std::vector< VE_XML::XMLObject* > inputData );

   ///Accessors for input data
   std::vector< VE_XML::Command* > GetResultsData( void );
   void SetResultsData( std::vector< VE_XML::XMLObject* > resultsData );

   ///Get the ID for the module
   ///\return The module id
   int get_id();
   ///Get the modules name
   ///\return The module name
   std::string GetModuleName( void );
   ///Get the VE_Model for this module
   ///\return The model for this module
   VE_XML::VE_Model::ModelWeakPtr GetVEModel( void );
   ///Set the VE_Model for this module
   ///\param mod The model to add
   void SetVEModel( VE_XML::VE_Model::ModelWeakPtr mod );

   int _need_execute;
   int _return_state;
   int _is_feedback;

   //int _type;
   //int _category;

private:
   //Input ports for the module
   std::vector<IPort*> _iports;
   //Output ports for the module
   std::vector<OPort*> _oports;

   ///ID for the particular module
   int _id;
   ///Module name
   std::string _name;
   
   // The holder of the raw data for this class
   // This class is responsible for the memory management here
   VE_XML::VE_Model::ModelStrongPtr veModel;
   //Container for input data
   std::vector< VE_XML::Command* > inputs;
   //Container for results data
   std::vector< VE_XML::Command* > results;
   //Container for port data
   std::vector< VE_XML::VE_Model::Port* > ports;
   ///Do we need to keep track of messages?
};
}
}
#endif
