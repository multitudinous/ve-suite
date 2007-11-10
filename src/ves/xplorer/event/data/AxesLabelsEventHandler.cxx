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
#include <ves/xplorer/event/data/AxesLabelsEventHandler.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/cfdDataSet.h>
#include <ves/xplorer/event/data/DataSetAxis.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/cfdEnvironmentHandler.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/device/cfdCursor.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/cfdDebug.h>

#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
AxesLabelsEventHandler::AxesLabelsEventHandler()
:ves::xplorer::event::EventHandler()
{
   _activeModel = 0;
}
////////////////////////////////////////////////////////////////////////////////
AxesLabelsEventHandler::AxesLabelsEventHandler(const AxesLabelsEventHandler& rhs)
:ves::xplorer::event::EventHandler(rhs)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
AxesLabelsEventHandler::~AxesLabelsEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
AxesLabelsEventHandler& AxesLabelsEventHandler::operator=(const AxesLabelsEventHandler& rhs)
{
   if(this != &rhs)
   {
      ves::xplorer::event::EventHandler::operator=(rhs);
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void AxesLabelsEventHandler::SetGlobalBaseObject(ves::xplorer::cfdGlobalBase* model)
{
   try
   {
      if ( model )
      {
         _activeModel = dynamic_cast< ves::xplorer::cfdModel* >( model );
      }
      else
      {
         _activeModel = ves::xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to BBoxEventHandler::SetGlobalBaseObject!"<<std::endl;
   }
}
//////////////////////////////////////////////////////////////////////////
void AxesLabelsEventHandler::Execute( XMLObject* xmlObject )
{
   Command* command = dynamic_cast< Command* >( xmlObject );
   DataValuePairWeakPtr activeModelDVP = command->GetDataValuePair( "Axes Labels" );
   std::vector< std::string > labels;
   activeModelDVP->GetData( labels );
   
   if ( _activeModel && !labels.empty() )
   {
      cfdDataSet* dataSet = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
      if ( dataSet )
      {
         dataSet->GetDataSetAxes()->SetAxisLabels( labels.at( 0 ), labels.at( 1 ), labels.at( 2 ) );
         dataSet->GetDataSetAxes()->CreateAxis();
      }
   }
   
}
