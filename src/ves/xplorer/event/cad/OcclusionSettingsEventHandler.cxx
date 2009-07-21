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
#include <ves/xplorer/event/cad/OcclusionSettingsEventHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

#include <ves/xplorer/scenegraph/util/OcclusionQueryVisitor.h>

#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADNode.h>

#include <osg/OcclusionQueryNode>

#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::xplorer::event::cad;
using namespace ves::open::xml;
using namespace ves::open::xml::cad;

///////////////////////////////////////////////////////////////////////////////////////
OcclusionSettingsEventHandler::OcclusionSettingsEventHandler()
        :
        ves::xplorer::event::CADEventHandler()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
OcclusionSettingsEventHandler::OcclusionSettingsEventHandler( const OcclusionSettingsEventHandler& rhs )
        :
        ves::xplorer::event::CADEventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
OcclusionSettingsEventHandler::~OcclusionSettingsEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
OcclusionSettingsEventHandler& OcclusionSettingsEventHandler::operator=( const OcclusionSettingsEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::CADEventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void OcclusionSettingsEventHandler::_operateOnNode( XMLObjectPtr xmlObject )
{
    try
    {
        CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
        DataValuePairPtr nodeID = command->GetDataValuePair( "Node ID" );
        DataValuePairPtr nodeType = command->GetDataValuePair( "OQ Settings" );

        if( !m_cadHandler->PartExists( nodeID->GetDataString() ) )
        {
            return;
        }
        
        osg::ref_ptr< osg::Node > activePart = 
          m_cadHandler->GetPart( nodeID->GetDataString() )->
          GetNode()->GetNode();
        std::string oqSettings = nodeType->GetDataString();
        
        osg::ref_ptr< osg::Group > rootPartNode = 
            static_cast< osg::Group* >( activePart.get() );
        
        osgOQ::StatisticsVisitor statsVisitor;
        rootPartNode->accept( statsVisitor );
        unsigned int numOQNs = statsVisitor.getNumOQNs();
        bool oqPresent = false;
        std::cout << numOQNs << std::endl;
        vprDEBUG( vesDBG, 1 ) << "|\t\tThere are currently " << numOQNs <<
            << "oq nodes." << std::endl
            << vprDEBUG_FLUSH;
        
        if( numOQNs > 0 )
        {
            oqPresent = true;
        }
        unsigned int occlusionThreshold = 1000;
        unsigned int visibilityThreshold = 100;
        bool occlude = false;
        
        if( oqSettings == "Off" )
        {
            occlude = false;
        }
        else if( oqSettings == "Low" )
        {
            occlude = true;
            occlusionThreshold = 10000;
            visibilityThreshold = 100;
        }
        else if( oqSettings == "Medium" )
        {
            occlude = true;
            occlusionThreshold = 5000;
            visibilityThreshold = 250;
        }
        else if( oqSettings == "High" )
        {
            occlude = true;
            occlusionThreshold = 2500;
            visibilityThreshold = 500;
        }

        if( !oqPresent && occlude )
        {
            vprDEBUG( vesDBG, 1 ) << "|\t\tCreating new oq nodes " << std::endl
                << vprDEBUG_FLUSH;
            
            osgOQ::OcclusionQueryNonFlatVisitor oqv;
            //Specify the vertex count threshold for performing 
            // occlusion query tests.
            //Settings others use are:
            //Fairly lax culling
            //occlusionThreshold = 5000
            //visibilityThreshold = 250
            //Fairly aggressive culling
            //occlusionThreshold = 2500
            //visibilityThreshold = 500
            // If the child geometry has less than the specified number
            //   of vertices, don't perform occlusion query testing (it's
            //   an occluder). Otherwise, perform occlusion query testing
            //   (it's an occludee).
            oqv.setOccluderThreshold( occlusionThreshold );
            rootPartNode->accept( oqv );
            //Setup the number frames to skip
            osgOQ::QueryFrameCountVisitor queryFrameVisitor( 2 );
            rootPartNode->accept( queryFrameVisitor );
            // If the occlusion query test indicates that the number of
            //   visible pixels is greater than this value, render the
            //   child geometry. Otherwise, don't render and continue to
            //   test for visibility in future frames.
            osgOQ::VisibilityThresholdVisitor visibilityThresholdVisitor( visibilityThreshold );
            rootPartNode->accept( visibilityThresholdVisitor );
        }
        else if( oqPresent && !occlude )
        {
            vprDEBUG( vesDBG, 1 ) << "|\t\tRemoving oq nodes " << std::endl
                << vprDEBUG_FLUSH;

            //remove oq nodes
            osgOQ::RemoveOcclusionQueryVisitor removeOQV;
            rootPartNode->accept( removeOQV );
            
            osgOQ::StatisticsVisitor statsVisitor;
            rootPartNode->accept( statsVisitor );
            unsigned int numOQNs = statsVisitor.getNumOQNs();
            std::cout << numOQNs << std::endl;
            
        }
        else if( oqPresent )
        {
            vprDEBUG( vesDBG, 1 ) << "|\t\tUpdating oq nodes " << std::endl
                << vprDEBUG_FLUSH;
            //remove oq nodes
            osgOQ::RemoveOcclusionQueryVisitor removeOQV;
            rootPartNode->accept( removeOQV );
            
            //Now create the oq nodes with new settings...
            osgOQ::OcclusionQueryNonFlatVisitor oqv;
            oqv.setOccluderThreshold( occlusionThreshold );
            rootPartNode->accept( oqv );

            osgOQ::QueryFrameCountVisitor queryFrameVisitor( 2 );
            rootPartNode->accept( queryFrameVisitor );
            
            osgOQ::VisibilityThresholdVisitor visibilityThresholdVisitor( visibilityThreshold );
            rootPartNode->accept( visibilityThresholdVisitor );
        }
        vprDEBUG( vesDBG, 1 ) << "|\t\tThere are now " << numOQNs <<
            << "oq nodes." << std::endl
            << vprDEBUG_FLUSH;
        
    }
    catch( ... )
    {
        std::cout << "Error!!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
