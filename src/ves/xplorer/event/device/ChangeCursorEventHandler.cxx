/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/xplorer/event/device/ChangeCursorEventHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/device/cfdCursor.h>

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
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
ChangeCursorEventHandler::ChangeCursorEventHandler()
        : ves::xplorer::event::EventHandler()
{
    _activeModel = 0;
}
////////////////////////////////////////////////////////////////////////////////
ChangeCursorEventHandler::ChangeCursorEventHandler( const ChangeCursorEventHandler& rhs )
        : ves::xplorer::event::EventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
ChangeCursorEventHandler::~ChangeCursorEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
ChangeCursorEventHandler& ChangeCursorEventHandler::operator=( const ChangeCursorEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::ChangeCursorEventHandler::operator=( rhs );
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void ChangeCursorEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    try
    {
        if( model )
        {
            _activeModel = dynamic_cast< ves::xplorer::Model* >( model );
        }
        else
        {
            _activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        }
    }
    catch ( ... )
    {
        _activeModel = 0;
        std::cout << "Invalid object passed to ChangeCursorEventHandler::SetGlobalBaseObject!" << std::endl;
    }
}
//////////////////////////////////////////////////////////////////////////
void ChangeCursorEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    //Grab the subdialog settings from streamlines to adjust cursor settings
    CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
    DataValuePairPtr activeModelDVP = command->GetDataValuePair( "Sub-Dialog Settings" );
    CommandPtr objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>(  activeModelDVP->GetDataXMLObject() );
    if( objectCommand->GetCommandName() != "UPDATE_STREAMLINE_SETTINGS" )
    {
        return;
    }

    std::string direction;
    DataValuePairPtr directionDVP = objectCommand->GetDataValuePair( "Cursor Direction" );
    if( directionDVP )
    {
        directionDVP->GetData( direction );
    }

    std::string planes;
    DataValuePairPtr planesDVP = objectCommand->GetDataValuePair( "Cursor Type" );
    if( planesDVP )
    {
        planesDVP->GetData( planes );
    }

    unsigned int numPointsPerPlane = 2;
    DataValuePairPtr pointsDVP = objectCommand->GetDataValuePair( "Number Of Points Per Plane" );
    if( pointsDVP )
    {
        pointsDVP->GetData( numPointsPerPlane );
        vprDEBUG( vesDBG, 2 ) << "Number of seed points " 
            << numPointsPerPlane << std::endl << vprDEBUG_FLUSH;
        EnvironmentHandler::instance()->GetCursor()->SetPlaneReso( static_cast< int >( numPointsPerPlane ) );
    }

    double planeSize = 1;
    DataValuePairPtr sizeDVP = objectCommand->GetDataValuePair( "Size" );
    if( sizeDVP )
    {
        sizeDVP->GetData( planeSize );
        vprDEBUG( vesDBG, 2 ) << "|\tStreamline planesize " 
            << planeSize << std::endl << vprDEBUG_FLUSH;
        EnvironmentHandler::instance()->GetCursor()->SetPlaneSize( static_cast< int >( planeSize ) );
    }


    if( planes == "plane" )
    {
        if( direction == "x" )
        {
            EnvironmentHandler::instance()->GetCursor()->SetCursorType( XPLANE );
        }
        else if( direction == "y" )
        {
            EnvironmentHandler::instance()->GetCursor()->SetCursorType( YPLANE );
        }
        else if( direction == "z" )
        {
            EnvironmentHandler::instance()->GetCursor()->SetCursorType( ZPLANE );
        }
    }
    else if( planes == "none" )
    {
        EnvironmentHandler::instance()->GetCursor()->SetCursorType( NONE );
    }
    else if( planes == "point" )
    {
        EnvironmentHandler::instance()->GetCursor()->SetCursorType( SPHERE );
    }
    else if( planes == "line" )
    {
        if( direction == "x" )
        {
            EnvironmentHandler::instance()->GetCursor()->SetCursorType( XLINE );
        }
        else if( direction == "y" )
        {
            EnvironmentHandler::instance()->GetCursor()->SetCursorType( YLINE );
        }
        else if( direction == "z" )
        {
            EnvironmentHandler::instance()->GetCursor()->SetCursorType( ZLINE );
        }
    }
}
