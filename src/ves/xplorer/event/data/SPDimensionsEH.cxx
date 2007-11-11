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
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/event/data/SeedPoints.h>
#include <ves/xplorer/DataSet.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <vtkDataSet.h>
using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

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
void SeedPointDimensionsEventHandler::SetGlobalBaseObject(ves::xplorer::GlobalBase* model)
{
   try
   {
      if(model)
      {
         _activeModel = dynamic_cast<ves::xplorer::Model*>(model);
      }
      else
      {
         _activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to SeedPointDimensionsEventHandler!"<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////////////////////////   
void SeedPointDimensionsEventHandler::Execute(XMLObject* veXMLObject)
{
   if(!_activeModel)
      throw;
   try
   {
      Command* command = dynamic_cast< Command* >( veXMLObject );
     
     std::vector<long> allDimensions;
     DataValuePairWeakPtr dimensions = command->GetDataValuePair("Dimensions");
     dimensions->GetData(allDimensions);
     ves::xplorer::EnvironmentHandler::instance()->GetSeedPoints()->SetDimensions(allDimensions[0],
                                                                                   allDimensions[1],
                                                                                   allDimensions[2]);
   }
   catch(...)
   {
      std::cout<<"Invalid Bounds!!"<<std::endl;
      std::cout<<"SeedPointDimensionsEventHandler::Execute()"<<std::endl;
   }
}
