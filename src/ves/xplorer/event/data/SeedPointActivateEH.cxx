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

#include <ves/xplorer/event/data/SeedPointActivateEH.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/cfdDataSet.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/cfdEnvironmentHandler.h>
#include <ves/xplorer/event/SeedPoints.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace VE_EVENTS;
using namespace VE_Xplorer;
using namespace ves::open::xml;

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
void SeedPointActivateEventHandler::Execute(XMLObject* veXMLObject)
{
   try
   {
      if(_activeModel)
      {
         //make the CAD transparent
         _activeModel->GetModelCADHandler()->MakeCADRootTransparent();
         ///what happens if texture is somehow added first? Is that possible?
         Command* command = dynamic_cast< Command* >( veXMLObject );
         DataValuePairWeakPtr seedPointsFlag = command->GetDataValuePair( "OnOff" );
		   DataValuePairWeakPtr activeDataset = command->GetDataValuePair( "Active Dataset" );
		   std::string datasetname;
		   activeDataset->GetData(datasetname);
		   //check to see if the seed points exist
		   if(!VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->SearchChild(VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPointsDCS()))
		   {
            VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->addChild(VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPointsDCS());   
		   }
		 
            //this seems to be a bad sequence of calls but we need to set the
            //active dataset otherwise this set of calls goes in every seed pointEH
            //as well as all the commands have to lug this extra info.
            _activeModel->SetActiveDataSet(_activeModel->GetCfdDataSet(_activeModel->GetIndexOfDataSet(datasetname)));
            VE_SceneGraph::DCS* tempDCS = _activeModel->GetActiveDataSet()->GetDCS();
            VE_SceneGraph::DCS* seedPointDCS = VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPointsDCS();

            seedPointDCS->SetTranslationArray( tempDCS->GetVETranslationArray() );
            seedPointDCS->SetRotationArray( tempDCS->GetRotationArray() );
            seedPointDCS->SetScaleArray( tempDCS->GetScaleArray() );
            VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPoints()->Toggle((seedPointsFlag->GetUIntData()==1)?true:false);
        } 
   }
   catch(...)
   {
      std::cout<<"Invalid Model!!"<<std::endl;
      std::cout<<"SeedPointActivateEventHandler::_operateOnNode()"<<std::endl;
   }

}
