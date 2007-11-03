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
#include <ves/xplorer/event/data/DataTransformEH.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/cfdDataSet.h>
#include <ves/xplorer/cfdEnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/event/data/SeedPoints.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/Clone.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/open/xml/Transform.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/cad/CADNode.h>
#include <iostream>
using namespace ves::xplorer::event;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
DataTransformEventHandler::DataTransformEventHandler()
:ves::xplorer::event::EventHandler()
{
   _activeModel = 0;
}
///////////////////////////////////////////////////////////////////////////////////////
DataTransformEventHandler::DataTransformEventHandler(const DataTransformEventHandler& rhs)
:ves::xplorer::event::EventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
DataTransformEventHandler::~DataTransformEventHandler()
{
}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
DataTransformEventHandler& DataTransformEventHandler::operator=(const DataTransformEventHandler& rhs)
{
   if(this != &rhs)
   {
      ves::xplorer::event::EventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////
void DataTransformEventHandler::Execute(XMLObject* xmlObject)
{
    try
    {
        if( _activeModel )
        {
            Command* command = dynamic_cast<Command*>(xmlObject);
            DataValuePairWeakPtr datasetName = command->GetDataValuePair("Parameter Block ID");     
            DataValuePairWeakPtr newTransform = command->GetDataValuePair("Transform");     
		    ves::xplorer::scenegraph::DCS* transform = 0;
            transform = _activeModel->GetActiveDataSet()->GetDCS();
      
            if( transform )
            {
                Transform* dataTransform = dynamic_cast<Transform*>(newTransform->GetDataXMLObject());
                transform->SetTranslationArray( dataTransform->GetTranslationArray()->GetArray() );
                transform->SetRotationArray( dataTransform->GetRotationArray()->GetArray() );
                transform->SetScaleArray( dataTransform->GetScaleArray()->GetArray() );
		        VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPointsDCS()->SetTranslationArray( dataTransform->GetTranslationArray()->GetArray() );
                VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPointsDCS()->SetRotationArray( dataTransform->GetRotationArray()->GetArray() );
                VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPointsDCS()->SetScaleArray( dataTransform->GetScaleArray()->GetArray() );
            }
        }
    }
    catch(...)
    {
        std::cout<<"Error!!!Invalid command passed to DataTransformEH!!"<<std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void DataTransformEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
   try
   {
      if ( model )
      {
         _activeModel = dynamic_cast< VE_Xplorer::cfdModel* >( model );
      }
      else
      {
         _activeModel = VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to DataTransformEventHandler::SetGlobalBaseObject!"<<std::endl;
   }
}
