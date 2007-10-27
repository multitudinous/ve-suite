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
// --- VE-Suite Includes --- //
#include <ves/xplorer/network/SwitchXplorerViewEventHandler.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/network/cfdExecutive.h>
#include <ves/xplorer/network/NetworkSystemView.h>

#include <ves/xplorer/plugin/cfdVEBaseClass.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/cfdDebug.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/Group>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace VE_EVENTS;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace ves::open::xml;
using namespace ves::xplorer::network;

////////////////////////////////////////////////////////////////////////////////
SwitchXplorerViewEventHandler::SwitchXplorerViewEventHandler()
:
VE_EVENTS::EventHandler()
{
    ;
}

////////////////////////////////////////////////////////////////////////////////
SwitchXplorerViewEventHandler::SwitchXplorerViewEventHandler( const SwitchXplorerViewEventHandler& rhs )
:
VE_EVENTS::EventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SwitchXplorerViewEventHandler::~SwitchXplorerViewEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SwitchXplorerViewEventHandler& SwitchXplorerViewEventHandler::operator=( const SwitchXplorerViewEventHandler& rhs )
{
    if( this != &rhs )
    {
        SwitchXplorerViewEventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void SwitchXplorerViewEventHandler::SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* model )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SwitchXplorerViewEventHandler::Execute( XMLObject* xmlObject )
{
    Command* command = dynamic_cast< Command* >( xmlObject );
    DataValuePairWeakPtr activeModelDVP = 
        command->GetDataValuePair( "CHANGE_XPLORER_VIEW" );

    std::string viewData;
    activeModelDVP->GetData( viewData );
    if( viewData == "CHANGE_XPLORER_VIEW_NETWORK" )
    {
        SceneManager::instance()->SetActiveSwitchNode( 2 );
        osg::ref_ptr< VE_SceneGraph::DCS > tempDCS = SceneManager::instance()->GetNetworkDCS();
        NetworkSystemView networkLayout( cfdExecutive::instance()->GetCurrentNetwork() );
        if( tempDCS->GetNumChildren() >= 1 )
        {
            tempDCS->removeChildren( 0, tempDCS->GetNumChildren() );
        }

        osg::ref_ptr< osg::Group > tempGroup = networkLayout.DrawNetwork();
        if( tempGroup.valid() )
        {
            tempDCS->addChild( tempGroup.get() );
        }
    }
    else if( viewData == "CHANGE_XPLORER_VIEW_CAD" )
    {
        SceneManager::instance()->SetActiveSwitchNode( 0 );
    }
    else if( viewData == "CHANGE_XPLORER_VIEW_LOGO" )
    {
        SceneManager::instance()->SetActiveSwitchNode( 1 );
    }
    else
    {
        SceneManager::instance()->SetActiveSwitchNode( 0 );
    }
   
    VE_Xplorer::DeviceHandler::instance()->GetActiveDevice()->SetActiveDCS( 
        VE_SceneGraph::SceneManager::instance()->GetActiveSwitchNode() );
}
////////////////////////////////////////////////////////////////////////////////
