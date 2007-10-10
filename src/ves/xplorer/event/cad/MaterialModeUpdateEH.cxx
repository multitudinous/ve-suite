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
#include "VE_Xplorer/XplorerHandlers/MaterialModeUpdateEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/ModelCADHandler.h"
#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/FloatArray.h"

#include "VE_Open/XML/CAD/CADMaterial.h"
using namespace VE_EVENTS;
////////////////////////////////////////////////////////
//Constructor                                         //
////////////////////////////////////////////////////////
MaterialModeUpdateEventHandler::MaterialModeUpdateEventHandler()
:VE_EVENTS::AttributeEventHandler()
{
}
/////////////////////////////////////////////////////////////////////////////////////////////
//Copy Constructor                                                                         //
/////////////////////////////////////////////////////////////////////////////////////////////
MaterialModeUpdateEventHandler::MaterialModeUpdateEventHandler(const MaterialModeUpdateEventHandler& ceh)
:VE_EVENTS::AttributeEventHandler(ceh)
{
}
/////////////////////////////////////////////////////////
MaterialModeUpdateEventHandler::~MaterialModeUpdateEventHandler()
{
}
////////////////////////////////////////////////////////////////////////////////////////////////////////
MaterialModeUpdateEventHandler& MaterialModeUpdateEventHandler::operator=(const MaterialModeUpdateEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::AttributeEventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////////////   
void MaterialModeUpdateEventHandler::_operateOnNode(VE_XML::XMLObject* veXMLObject) 
{
   try
   {
      VE_XML::Command* componentUpdate = dynamic_cast<VE_XML::Command*>(veXMLObject);
      VE_XML::DataValuePairWeakPtr nodeId = componentUpdate->GetDataValuePair("Node ID");
      VE_XML::DataValuePairWeakPtr material = componentUpdate->GetDataValuePair("Material");
      VE_XML::DataValuePairWeakPtr updateMode = componentUpdate->GetDataValuePair("Mode");
      
      VE_XML::VE_CAD::CADMaterial* rawMaterial = dynamic_cast<VE_XML::VE_CAD::CADMaterial*>(material->GetDataXMLObject());
      std::string newMode = updateMode->GetDataString();
      std::string value = "";
    
      if(newMode == "Face")
      {
         value = rawMaterial->GetFace();
      }
      else if(newMode == "Color")
      {
         value = rawMaterial->GetColorMode();
      }
      _activeModel->GetModelCADHandler()->UpdateMaterialMode(nodeId->GetDataString(),
                                       rawMaterial->GetMaterialName(),
                                       newMode,value);
      
   }
   catch(...)
   {
      std::cout<<"Unable to update material!!"<<std::endl;
      std::cout<<"===MaterialUpdateEventHandler==="<<std::endl;
   }
}
