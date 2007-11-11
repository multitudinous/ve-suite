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

#include <ves/xplorer/event/volume/TBUpdateSolutionEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/event/data/DataSetScalarBar.h>
#include <ves/xplorer/cfdTextureBasedVizHandler.h>

#include <ves/xplorer/volume/cfdTextureDataSet.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
TextureBasedUpdateSolutionEventHandler::TextureBasedUpdateSolutionEventHandler()
{
}
///////////////////////////////////////////////////////////////////
TextureBasedUpdateSolutionEventHandler
::TextureBasedUpdateSolutionEventHandler(const TextureBasedUpdateSolutionEventHandler& ceh)
{
}
/////////////////////////////////////////////////////////////////////
TextureBasedUpdateSolutionEventHandler::~TextureBasedUpdateSolutionEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedUpdateSolutionEventHandler& 
TextureBasedUpdateSolutionEventHandler::operator=(const TextureBasedUpdateSolutionEventHandler& rhs)
{
   if(&rhs != this)
   {
      TextureBasedEventHandler::operator=(rhs);
   }
   return *this;
}
/////////////////////////////////////////////////////////////////////////////////////   
void TextureBasedUpdateSolutionEventHandler::_operateOnNode(XMLObject* veXMLObject)
{
   try
   {
      Command* command = dynamic_cast< Command* >( veXMLObject );
      DataValuePairWeakPtr activeDataset = command->GetDataValuePair( "Active Dataset" );
      std::string dataName;
      activeDataset->GetData( dataName );

      DataValuePairWeakPtr type = command->GetDataValuePair( "Data Type" );
      std::string dataType;
      type->GetData( dataType );

      if(_activeTDSet)
      {
         if(dataType == "Scalar")
         {
            _activeTDSet->SetActiveScalar( dataName );
            
            ves::xplorer::volume::cfdTextureBasedVizHandler::instance()->UpdateActiveTextureManager();
            
            double scalarRange[2] = {0.f,100.f};

            DataValuePairWeakPtr minScalarRange = command->GetDataValuePair( "Mininum Scalar Range" );
            minScalarRange->GetData( scalarRange[0] );
            
            DataValuePairWeakPtr maxScalarRange = command->GetDataValuePair( "Maximum Scalar Range" );
            maxScalarRange->GetData( scalarRange[1] );
            //this is overkill
            float floatRange[2];
            floatRange[0] = scalarRange[0];
            floatRange[1] = scalarRange[1];
            ves::xplorer::volume::cfdTextureBasedVizHandler::instance()->UpdateScalarRange(floatRange);
            //need to pass the scalar range command to update it
            
            //if ( _activeModel )
            {
               DataSet dataSet = ModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
               //if ( dataSet )
               {
                  DataSetScalarBar* scalarBar = dataSet->GetDataSetScalarBar();
                  if ( scalarBar )
                  {
                     dataSet->SetActiveScalar( dataName );
                     scalarBar->AddScalarBarToGroup();
                  }
               }
            }            
         }
	       else if(dataType == "Vector")
          {
             _activeTDSet->SetActiveVector( dataName );
          }
      }
   }
   catch(...)
   {
      std::cout<<"Invalid TextureDataSet!!"<<std::endl;
      std::cout<<"TextureBasedUpdateSolutionEventHandler::_operateOnNode()"<<std::endl;
   }
}
