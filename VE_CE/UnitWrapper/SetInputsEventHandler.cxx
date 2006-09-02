/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2006-07-08 22:04:36 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4905 $
 * Author:        $Author: mccdo $
 * Id:            $Id: EventHandler.h 4905 2006-07-09 03:04:36Z mccdo $
 * -----------------------------------------------------------------
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_CE/UnitWrapper/SetInputsEventHandler.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Model/Model.h"

using namespace VE_CE;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
SetInputsEventHandler::SetInputsEventHandler()
:VE_CE::EventHandler()
{
   baseModel = 0;
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
SetInputsEventHandler::~SetInputsEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void SetInputsEventHandler::SetBaseObject( VE_XML::XMLObject* model)
{
   try
   {
      if ( model )
      {
         baseModel = dynamic_cast< VE_Model::Model* >( model );
      }
   }
   catch(...)
   {
      baseModel = 0;
      std::cout<<"Invalid object passed to SetInputsEventHandler::SetGlobalBaseObject!"<<std::endl;
   }
}
////////////////////////////////////////////////////////////////////////////////
std::string SetInputsEventHandler::Execute( VE_XML::XMLObject* objectToProcess )
{
   if ( !baseModel )
   {
      std::cerr << "Must call SetInputsEventHandler::SetBaseObject first" << std::endl;
      return std::string();
   }
   
   try
   {
      VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( objectToProcess );
      size_t numInputs = command->GetNumberOfDataValuePairs();
      for ( size_t i = 0; i < numInputs; ++i )
      {
         VE_XML::DataValuePair* veModelDVP = command->GetDataValuePair( i );
         std::string dataName = veModelDVP->GetDataName();
         VE_XML::Command* tempCommandInput = 
               dynamic_cast< VE_XML::Command* >( veModelDVP->GetDataXMLObject() );
         VE_XML::Command* tempInput = baseModel->GetInput( dataName );
         if ( tempInput )
         {
            (*tempInput) = (*tempCommandInput);
         }
         else
         {
            *(baseModel->GetInput()) = (*tempCommandInput);
         }
      }
   }
   catch (...)
   {
      std::cerr << " ERROR : SetInputsEventHandler::Execute " << std::endl;
   }
   return std::string();
}
