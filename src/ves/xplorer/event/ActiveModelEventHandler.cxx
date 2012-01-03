/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/event/ActiveModelEventHandler.h>

#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>

#include <iostream>

#include <boost/concept_check.hpp>

using namespace ves::xplorer::event;
//////////////////////////////////////////////////////////
///Constructor                                          //
////////////////////////////////////////////////////////////////////////////////
ActiveModelEventHandler::ActiveModelEventHandler()
    : 
    ves::xplorer::event::EventHandler()
{
    CONNECTSIGNALS_1( "%ActiveModel",
                     void ( const std::string& activModelID ),
                     &ActiveModelEventHandler::SetActiveModel,
                     m_connections, any_SignalType, normal_Priority );    
}
////////////////////////////////////////////////////////////////////////////////
ActiveModelEventHandler::ActiveModelEventHandler( const ActiveModelEventHandler& rhs )
    : 
    ves::xplorer::event::EventHandler( rhs )
{
    ;
}
////////////////////////////////////
///Destructor                     //
////////////////////////////////////////////////////////////////////////////////
ActiveModelEventHandler::~ActiveModelEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ActiveModelEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler )
{
    boost::ignore_unused_variable_warning( modelHandler );
}
///////////////////////////////////////////////////////
///Exectute the event                                //
////////////////////////////////////////////////////////////////////////////////
void ActiveModelEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    ves::open::xml::CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject ) );
    ves::open::xml::DataValuePairPtr activeModelDVP = command->GetDataValuePair( "CHANGE_ACTIVE_MODEL" );
    if( activeModelDVP )
    {
        std::string newModel;
        activeModelDVP->GetData( newModel );
        SetActiveModel( newModel );
        return;
    }

    activeModelDVP = command->GetDataValuePair( "Optimize CAD" );
    if( activeModelDVP )
    {
        ves::xplorer::ModelHandler::instance()->GetActiveModel()->GetModelCADHandler()->OptimizeAllCAD();
    }
}
////////////////////////////////////////////////////////////////////////////////
ActiveModelEventHandler& ActiveModelEventHandler::operator=( const ActiveModelEventHandler& rhs )
{
    if( this != &rhs )
    {
        ;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void ActiveModelEventHandler::SetActiveModel( const std::string& activModelID )
{
    ves::xplorer::ModelHandler::instance()->SetActiveModel( activModelID );
}
////////////////////////////////////////////////////////////////////////////////
