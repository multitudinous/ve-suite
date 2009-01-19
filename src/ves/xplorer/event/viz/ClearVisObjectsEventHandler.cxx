/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/event/viz/ClearVisObjectsEventHandler.h>
#include <ves/xplorer/SteadyStateVizHandler.h>
#include <ves/xplorer/TextureBasedVizHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/event/data/DataSetAxis.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/Debug.h>

#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::xplorer::volume;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
ClearVisObjectsEventHandler::ClearVisObjectsEventHandler()
        : ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ClearVisObjectsEventHandler::ClearVisObjectsEventHandler( const ClearVisObjectsEventHandler& rhs )
        : ves::xplorer::event::EventHandler( rhs )
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
ClearVisObjectsEventHandler& ClearVisObjectsEventHandler::operator=( const ClearVisObjectsEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::ClearVisObjectsEventHandler::operator=( rhs );
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void ClearVisObjectsEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    ;
}
//////////////////////////////////////////////////////////////////////////
void ClearVisObjectsEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    //call back over to ssvishandler to clear the vis objects
    SteadyStateVizHandler::instance()->ClearVisObjects();
    TextureBasedVizHandler::instance()->ClearAll();
    
    // I believe this guard is a hack and should be removed in the future
    // I was unable to figure out what was going wrong with loading 
    // and reloading ves files and default plugins. Something appears to 
    // be incorrect in how we are setting activemodel in deleting plugins.
    std::string tempCommandName = boost::dynamic_pointer_cast< ves::open::xml::Command >( xmlObject )->GetCommandName();
    if( "DELETE_OBJECT_FROM_NETWORK" == tempCommandName )
    {
        return;
    }
    
    // Only process dataset related events on the clear vis command
    if( !ModelHandler::instance()->GetActiveModel() )
    {
        return;
    }

    ModelHandler::instance()->GetActiveModel()->
        GetModelCADHandler()->MakeCADRootOpaque();

    unsigned int state = 0;
    DataSet* dataSet = 
        ModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
    if( dataSet )
    {
        dataSet->SetBoundingBoxState( state );
        dataSet->SetDataSetScalarState( state );
        dataSet->SetWireframeState( state );
        dataSet->SetAxesState( state );
    }
}
