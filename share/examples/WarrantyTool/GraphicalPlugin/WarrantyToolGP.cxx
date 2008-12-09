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

// --- My Includes --- //
#include "WarrantyToolGP.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <ves/xplorer/scenegraph/util/OpacityVisitor.h>
#include <ves/xplorer/scenegraph/util/MaterialInitializer.h>
#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

using namespace ves::xplorer::scenegraph;
using namespace warrantytool;

////////////////////////////////////////////////////////////////////////////////
WarrantyToolGP::WarrantyToolGP()
:
PluginBase(),
mAddingParts( false )
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "WarrantyToolUI";

    mEventHandlerMap[ "CAMERA_GEOMETRY_ON_OFF" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolGP::~WarrantyToolGP()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
    //Load model
    /*cadEntity = new CADEntity( "Models/test2_head_osg26-share.ive",
              mDCS.get(), false, false, NULL );
    osg::Node::DescriptionList descriptorsList;
    descriptorsList = cadEntity->GetDCS()->getDescriptions();
    descriptorsList.push_back( "Part" );
    cadEntity->GetDCS()->setDescriptions( descriptorsList );*/
    //Make transparent
    //ves::xplorer::scenegraph::util::MaterialInitializer 
    //    material_initializer( cadEntity->GetDCS() );
   /* ves::xplorer::scenegraph::util::OpacityVisitor 
        opVisitor( cadEntity->GetDCS(), true, true, 1.0f );
    ves::xplorer::scenegraph::util::OpacityVisitor 
        opVisitor1( cadEntity->GetDCS(), false, true, 0.4f );
    //Highlight part
    ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
        highlight( cadEntity->GetDCS(), "AN220959", mDCS.get() );*/

    //ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
    //    highlight2( cadEntity->GetDCS(), "AN220959", mDCS.get(), false );
    //ves::xplorer::scenegraph::util::OpacityVisitor 
    //    opVisitor2( cadEntity->GetDCS(), false, true, 1.0f );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }
    const std::string commandName = command->GetCommandName();
    ves::open::xml::DataValuePairPtr dvp = command->GetDataValuePair( 0 );
    //Before anything else remove the glow if there is glow
    
    if( dvp->GetDataName() == "RESET" )
    {
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight2( mDCS.get(), "", false );
        //Make everything opaque
        ves::xplorer::scenegraph::util::OpacityVisitor 
            opVisitor( mDCS.get(), false, false, 1.0f );
        mAddingParts = false;
    }
    else if( dvp->GetDataName() == "ADD" )
    {
        //Highlight the respective node
        //Make a user specified part glow
        if( !mAddingParts )
        {
            ves::xplorer::scenegraph::util::OpacityVisitor 
                opVisitor1( mDCS.get(), false, true, 0.3f );
            mAddingParts = true;
        }
        //Highlight part
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight( mDCS.get(), dvp->GetDataString() );
    }
    else if( dvp->GetDataName() == "CLEAR" )
    {
        ves::xplorer::scenegraph::util::OpacityVisitor 
            opVisitor1( mDCS.get(), false, true, 0.3f );

        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight2( mDCS.get(), "", false );
    }
}
////////////////////////////////////////////////////////////////////////////////
