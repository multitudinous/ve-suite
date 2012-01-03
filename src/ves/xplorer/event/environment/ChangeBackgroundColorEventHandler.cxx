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
#include <ves/xplorer/event/environment/ChangeBackgroundColorEventHandler.h>

#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/scenegraph/HeadsUpDisplay.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>

#include <osgText/Text>

#include <string>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace ves::xplorer::event;
using namespace ves::open::xml;

//////////////////////////////////////////////////////////
///Constructor                                          //
////////////////////////////////////////////////////////////////////////////////
ChangeBackgroundColorEventHandler::ChangeBackgroundColorEventHandler()
        : ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ChangeBackgroundColorEventHandler::ChangeBackgroundColorEventHandler( const ChangeBackgroundColorEventHandler& rhs )
        : 
        ves::xplorer::event::EventHandler( rhs )
{}
////////////////////////////////////////////////////////////////////////////////
///Destructor                     //
////////////////////////////////////////////////////////////////////////////////
ChangeBackgroundColorEventHandler::~ChangeBackgroundColorEventHandler()
{}
////////////////////////////////////////////////////////////////////////////////
void ChangeBackgroundColorEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* )
{}
////////////////////////////////////////////////////////////////////////////////
///Exectute the event                                                        //
////////////////////////////////////////////////////////////////////////////////
void ChangeBackgroundColorEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    CommandPtr command = boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject );
    DataValuePairPtr activeModelDVP = command->GetDataValuePair( "Background Color" );
    std::vector< double > color;
    activeModelDVP->GetData( color );
    if( !color.empty() )
    {
        UpdateBackgroundColor( true, color );
    }
}
////////////////////////////////////////////////////////////////////////////////
ChangeBackgroundColorEventHandler& ChangeBackgroundColorEventHandler::operator=( const ChangeBackgroundColorEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void ChangeBackgroundColorEventHandler::UpdateBackgroundColor( bool const, std::vector< double > const& color )
{
    ves::xplorer::scenegraph::SceneManager::instance()->SetBackgroundColor( color );
    ves::xplorer::EnvironmentHandler::instance()->
        GetHeadsUpDisplay()->SetTextColor( color );
}
////////////////////////////////////////////////////////////////////////////////
