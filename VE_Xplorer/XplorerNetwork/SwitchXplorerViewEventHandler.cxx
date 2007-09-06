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
#include "VE_Xplorer/XplorerNetwork/SwitchXplorerViewEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerNetwork/cfdExecutive.h"
#include "VE_Xplorer/XplorerNetwork/NetworkSystemView.h"
#include "VE_Xplorer/GraphicalPlugin/cfdVEBaseClass.h"
#include "VE_Xplorer/SceneGraph/SceneManager.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/Model/Model.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/Group>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace VE_EVENTS;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

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
        VE_EVENTS::SwitchXplorerViewEventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void SwitchXplorerViewEventHandler::SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* model )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SwitchXplorerViewEventHandler::Execute( VE_XML::XMLObject* xmlObject )
{
    VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );
    VE_XML::DataValuePairWeakPtr activeModelDVP = 
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
}
////////////////////////////////////////////////////////////////////////////////
