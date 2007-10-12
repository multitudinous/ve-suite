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
#include <ves/xplorer/event/ScalarBarEventHandler.h>
#include <ves/xplorer/event/viz/cfdModel.h>
#include <ves/xplorer/event/viz/cfdDataSet.h>
#include <ves/xplorer/event/viz/cfdModelHandler.h>
#include <ves/xplorer/event/viz/cfdEnvironmentHandler.h>
#include <ves/xplorer/event/viz/cfdEnum.h>
#include <ves/xplorer/event/viz/cfdCursor.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/event/viz/cfdDebug.h>

#include <iostream>

using namespace VE_EVENTS;
using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
ScalarBarEventHandler::ScalarBarEventHandler()
:VE_EVENTS::EventHandler()
{
   _activeModel = 0;
}
////////////////////////////////////////////////////////////////////////////////
ScalarBarEventHandler::ScalarBarEventHandler(const ScalarBarEventHandler& rhs)
:VE_EVENTS::EventHandler(rhs)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
ScalarBarEventHandler::~ScalarBarEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
ScalarBarEventHandler& ScalarBarEventHandler::operator=(const ScalarBarEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::EventHandler::operator=(rhs);
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void ScalarBarEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
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
      std::cout<<"Invalid object passed to BBoxEventHandler::SetGlobalBaseObject!"<<std::endl;
   }
}
//////////////////////////////////////////////////////////////////////////
void ScalarBarEventHandler::Execute( VE_XML::XMLObject* xmlObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );
   VE_XML::DataValuePairWeakPtr activeModelDVP = command->GetDataValuePair( "Scalar Bar State" );
   
   unsigned int state = 0;
   activeModelDVP->GetData( state );
   if ( _activeModel )
   {
      cfdDataSet* dataSet = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
      if ( dataSet )
      {
         dataSet->SetDataSetScalarState( state );
      }
   }
   
}
