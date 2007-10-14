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
#include <ves/xplorer/event/viz/ClearVisObjectsEventHandler.h>
#include <ves/xplorer/cfdSteadyStateVizHandler.h>
#include <ves/xplorer/cfdTextureBasedVizHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/cfdDataSet.h>
#include <ves/xplorer/event/data/DataSetAxis.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/cfdDebug.h>

#include <iostream>

using namespace VE_EVENTS;
using namespace VE_Xplorer;
using namespace VE_TextureBased;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
ClearVisObjectsEventHandler::ClearVisObjectsEventHandler()
:VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
ClearVisObjectsEventHandler::ClearVisObjectsEventHandler(const ClearVisObjectsEventHandler& rhs)
:VE_EVENTS::EventHandler(rhs)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                                                 //
////////////////////////////////////////////////////////////////////////////////
ClearVisObjectsEventHandler::~ClearVisObjectsEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
ClearVisObjectsEventHandler& ClearVisObjectsEventHandler::operator=(const ClearVisObjectsEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::ClearVisObjectsEventHandler::operator=(rhs);
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void ClearVisObjectsEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
   ;
}
//////////////////////////////////////////////////////////////////////////
void ClearVisObjectsEventHandler::Execute( ves::open::xml::XMLObject* xmlObject )
{
   //call back over to ssvishandler to clear the vis objects
   cfdSteadyStateVizHandler::instance()->ClearVisObjects();
   cfdTextureBasedVizHandler::instance()->ClearAll();
   if ( cfdModelHandler::instance()->GetActiveModel() )
   {
      cfdModelHandler::instance()->GetActiveModel()->GetModelCADHandler()->MakeCADRootOpaque();

      unsigned int state = 0;
      cfdDataSet* dataSet = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
      if ( dataSet )
      {
         dataSet->SetBoundingBoxState( state );
         dataSet->SetDataSetScalarState( state );
         dataSet->SetWireframeState( state );
         dataSet->SetAxesState( state );
      }
   }
}
