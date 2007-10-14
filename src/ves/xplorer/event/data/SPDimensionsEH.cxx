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

#include <ves/xplorer/event/data/SPDimensionsEH.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/cfdEnvironmentHandler.h>
#include <ves/xplorer/event/data/SeedPoints.h>
#include <ves/xplorer/cfdDataSet.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <vtkDataSet.h>
using namespace VE_EVENTS;
using namespace VE_Xplorer;
////////////////////////////////////////////////////////////////////
SeedPointDimensionsEventHandler::SeedPointDimensionsEventHandler()
{
   _activeModel = 0;
}
///////////////////////////////////////////////////////////////////
SeedPointDimensionsEventHandler
::SeedPointDimensionsEventHandler(const SeedPointDimensionsEventHandler& ceh)
{
   _activeModel = ceh._activeModel;
}
/////////////////////////////////////////////////////////////////////
SeedPointDimensionsEventHandler::~SeedPointDimensionsEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
SeedPointDimensionsEventHandler& 
SeedPointDimensionsEventHandler::operator=(const SeedPointDimensionsEventHandler& rhs)
{
   if(&rhs != this)
   {
      _activeModel = rhs._activeModel;
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////////
void SeedPointDimensionsEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
   try
   {
      if(model)
      {
         _activeModel = dynamic_cast<VE_Xplorer::cfdModel*>(model);
      }
      else
      {
         _activeModel = VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to SeedPointDimensionsEventHandler!"<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////////////////////////   
void SeedPointDimensionsEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   if(!_activeModel)
      throw;
   try
   {
      VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
     
     std::vector<long> allDimensions;
     VE_XML::DataValuePairWeakPtr dimensions = command->GetDataValuePair("Dimensions");
     dimensions->GetData(allDimensions);
     VE_Xplorer::cfdEnvironmentHandler::instance()->GetSeedPoints()->SetDimensions(allDimensions[0],
                                                                                   allDimensions[1],
                                                                                   allDimensions[2]);
   }
   catch(...)
   {
      std::cout<<"Invalid Bounds!!"<<std::endl;
      std::cout<<"SeedPointDimensionsEventHandler::Execute()"<<std::endl;
   }
}
