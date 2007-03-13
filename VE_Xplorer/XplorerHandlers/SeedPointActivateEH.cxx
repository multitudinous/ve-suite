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
 * Date modified: $Date: 2007-02-22 18:56:39 -0600 (Thu, 22 Feb 2007) $
 * Version:       $Rev: 6981 $
 * Author:        $Author: biv $
 * Id:            $Id: TBActivateEH.cxx 6981 2007-02-23 00:56:39Z biv $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/XplorerHandlers/SeedPointActivateEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/XplorerHandlers/SeedPoints.h"

#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

using namespace VE_EVENTS;
using namespace VE_Xplorer;
#ifdef VE_PATENTED
////////////////////////////////////////////////////////////////////
SeedPointActivateEventHandler::SeedPointActivateEventHandler()
{
	_activeModel = 0;
}
///////////////////////////////////////////////////////////////////
SeedPointActivateEventHandler
::SeedPointActivateEventHandler(const SeedPointActivateEventHandler& ceh)
{
	_activeModel = ceh._activeModel;
}
/////////////////////////////////////////////////////////////////////
SeedPointActivateEventHandler::~SeedPointActivateEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
SeedPointActivateEventHandler& 
SeedPointActivateEventHandler::operator=(const SeedPointActivateEventHandler& rhs)
{
   if(&rhs != this)
   {
	  _activeModel = rhs._activeModel;
   }
   return *this;
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void SeedPointActivateEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject)
{
   ///is this overkill????
   try
   {
      if(baseObject)
      {
         _activeModel = dynamic_cast<VE_Xplorer::cfdModel*>(baseObject);
      }
      else
      {
         _activeModel = VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to TextureBasedEventHandler!"<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////////////////////////   
void SeedPointActivateEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   try
   {
      if(_activeModel)
      {
         //make the CAD transparent
         _activeModel->MakeCADRootTransparent();
         ///what happens if texture is somehow added first? Is that possible?
         VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
         VE_XML::DataValuePair* seedPointsFlag = command->GetDataValuePair( "OnOff" );
         //check to see if the seed points exist
		 if(!VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->SearchChild(VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPointsDCS()))
		 {
			 VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->addChild(VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPointsDCS());
          
          VE_SceneGraph::DCS* tempDCS = _activeModel->GetActiveDataSet()->GetDCS();
          VE_SceneGraph::DCS* seedPointDCS = VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPointsDCS();
          seedPointDCS->SetTranslationArray( tempDCS->GetVETranslationArray() );
          seedPointDCS->SetRotationArray( tempDCS->GetRotationArray() );
          seedPointDCS->SetScaleArray( tempDCS->GetScaleArray() );
		 }
		 VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPoints()->Toggle((seedPointsFlag->GetUIntData()==1)?true:false);
      } 
   }
   catch(...)
   {
      std::cout<<"Invalid Model!!"<<std::endl;
      std::cout<<"SeedPointActivateEventHandler::_operateOnNode()"<<std::endl;
   }

}
#endif
