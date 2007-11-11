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
#include <ves/xplorer/event/cad/MaterialUpdateEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>

#include <ves/open/xml/cad/CADMaterial.h>
using namespace ves::xplorer::event;
using namespace ves::open::xml;
using namespace ves::open::xml::cad;

////////////////////////////////////////////////////////
//Constructor                                         //
////////////////////////////////////////////////////////
MaterialUpdateEventHandler::MaterialUpdateEventHandler()
:ves::xplorer::event::AttributeEventHandler()
{
}
/////////////////////////////////////////////////////////////////////////////////////////////
//Copy Constructor                                                                         //
/////////////////////////////////////////////////////////////////////////////////////////////
MaterialUpdateEventHandler::MaterialUpdateEventHandler(const MaterialUpdateEventHandler& ceh)
:ves::xplorer::event::AttributeEventHandler(ceh)
{
}
/////////////////////////////////////////////////////////
MaterialUpdateEventHandler::~MaterialUpdateEventHandler()
{
}
////////////////////////////////////////////////////////////////////////////////////////////////////////
MaterialUpdateEventHandler& MaterialUpdateEventHandler::operator=(const MaterialUpdateEventHandler& rhs)
{
   if(this != &rhs)
   {
      ves::xplorer::event::AttributeEventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////////////   
void MaterialUpdateEventHandler::_operateOnNode(XMLObject* veXMLObject) 
{
   std::cout<<"Material update event handler"<<std::endl;
   try
   {
      Command* componentUpdate = dynamic_cast<Command*>(veXMLObject);
      DataValuePairWeakPtr nodeId = componentUpdate->GetDataValuePair("Node ID");
      DataValuePairWeakPtr material = componentUpdate->GetDataValuePair("Material");
      DataValuePairWeakPtr component = componentUpdate->GetDataValuePair("Material Component");
      
      CADMaterial* rawMaterial = dynamic_cast<CADMaterial*>(material->GetDataXMLObject());
      std::string rawComponent = component->GetDataString();
      std::vector<double> values;
      if(rawComponent == "Diffuse")
      {
         values = rawMaterial->GetDiffuse()->GetArray();
      }
      else if(rawComponent == "Ambient")
      {
         values = rawMaterial->GetAmbient()->GetArray();
      }
      else if(rawComponent == "Emissive")
      {
         values = rawMaterial->GetEmissive()->GetArray();
      }
      else if(rawComponent == "Specular")
      {
         values = rawMaterial->GetSpecular()->GetArray();
      }
      else if(rawComponent == "Opacity")
      {
         //std::cout<<"Update opacity"<<std::endl;
         values.push_back(rawMaterial->GetOpacity());
      }

       values.push_back(rawMaterial->GetOpacity());
      _activeModel->GetModelCADHandler()->
		  UpdateMaterialComponent(nodeId->GetDataString(),
		                          rawMaterial->GetMaterialName(),
								  rawComponent,
								  rawMaterial->GetFace(),
								  values);
      
   }
   catch(...)
   {
      std::cout<<"Unable to update material!!"<<std::endl;
      std::cout<<"===MaterialUpdateEventHandler==="<<std::endl;
   }
}
