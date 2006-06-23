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
 * File:          $RCSfile: ClearVisObjectsEventHandler.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/ClearVisObjectsEventHandler.h"
#include "VE_Xplorer/cfdSteadyStateVizHandler.h"
#include "VE_Xplorer/cfdModelHandler.h"
#include "VE_Xplorer/cfdModel.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/Model/Model.h"

#include "VE_Xplorer/cfdDebug.h"

#include <iostream>

using namespace VE_EVENTS;
using namespace VE_Xplorer;

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
void ClearVisObjectsEventHandler::Execute( VE_XML::XMLObject* xmlObject )
{
   //call back over to ssvishandler to clear the vis objects
   cfdSteadyStateVizHandler::instance()->ClearVisObjects();
   cfdModelHandler::instance()->GetActiveModel()->MakeCADRootOpaque();
}
