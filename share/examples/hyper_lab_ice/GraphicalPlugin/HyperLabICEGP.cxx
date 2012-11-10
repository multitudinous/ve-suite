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
#include <ves/xplorer/communication/CommunicationHandler.h>

#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>

#include <ves/xplorer/scenegraph/util/MaterialPresent.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/EnvironmentHandler.h>

#include <osgDB/ReadFile>
#include <osgUtil/LineSegmentIntersector>
#include <osgUtil/Optimizer>

#include <osg/Depth>

#include <sstream>
#include <iostream>
#include <fstream>
#include <algorithm>

// pick this up from RTTScene for now
extern osg::ref_ptr<osg::Texture2D> RTTtex;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//#define NETL_DEMO

#include "HyperLabICEGP.h"
#ifdef NETL_DEMO
#include "RenderPrep.h"
#endif

using namespace Poco::Data;
using namespace ves::xplorer::scenegraph;
using namespace warrantytool;

#define CM2FEET 0.0328

////////////////////////////////////////////////////////////////////////////////
HyperLabICEGP::HyperLabICEGP()
    :
    PluginBase()
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "HyperLabICEPlugin";
}
////////////////////////////////////////////////////////////////////////////////
HyperLabICEGP::~HyperLabICEGP()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
    
    InitializeLabModels();
}
////////////////////////////////////////////////////////////////////////////////
int HyperLabICEGP::InitializeLabModels()
{    
    osg::Group* root = new osg::Group();

    mDCS->addChild( root );
    
    osg::ref_ptr< osgDB::ReaderWriter::Options > options = new osgDB::ReaderWriter::Options;
    options->setOptionString( "dds_flip" );
    
    osg::ref_ptr< osg::Group > renderRoot = new osg::Group;
    osg::ref_ptr< osg::Node > models = osgDB::readNodeFile( "Models/ControlRoom_v9.osg", options.get() );
    if( !( models.valid() ) )
    {
        osg::notify( osg::FATAL ) << "Can't open model file(s)." << std::endl;
    }
    renderRoot->addChild( models.get() );

    models = osgDB::readNodeFile( "Models/HyperLab_Facility_v6.osg", options.get() );
    if( !( models.valid() ) )
    {
        osg::notify( osg::FATAL ) << "Can't open model file(s)." << std::endl;
    }
    renderRoot->addChild( models.get() );

    models = osgDB::readNodeFile( "Models/HyperSystem_v6.osg", options.get() );
    if( !( models.valid() ) )
    {
        osg::notify( osg::FATAL ) << "Can't open model file(s)." << std::endl;
    }
    renderRoot->addChild( models.get() );

    {
        // Main prep work for rendering.
#ifdef NETL_DEMO
        float textSize( 0.f );
        bool parallaxMap( false );
        RenderPrep renderPrep( renderRoot.get(), textSize, parallaxMap );
#endif
    }

    // Scale to feet.
    {
        osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform(
            osg::Matrix::scale( osg::Vec3( CM2FEET, CM2FEET, CM2FEET ) ) );
        mt->setDataVariance( osg::Object::STATIC );
        mt->addChild( renderRoot.get() );
        
        osgUtil::Optimizer optimizer;
        optimizer.optimize( mt.get(), osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS );
    }
    
    root->addChild( renderRoot.get() );

    std::cout << "Loaded all of the models and physics for the Gate." << std::endl;
    return( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::PreFrameUpdate()
{
    //Update the Pressure Indicators
    {
        //Traverse to find the node
        //PI019, 411, 413
        //Get the first child
        //Rotate the gauge accordingly
    }
    //Update the HV gauges
    {
        //Traverse to find the nodes
        //HV408,414,430
        //Rotate the child
        //Spin it with a given velocity
    }
    //Update the FI015 gauges
    {
        //Traverse to find the nodes
        //Move the ball gauge accordingly
    }
    
    return;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    std::cout << "HyperLabICEGP::SetCurrentCommand" << std::endl << std::flush;
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::RemoveSelfFromSG()
{
    PluginBase::RemoveSelfFromSG();
}
////////////////////////////////////////////////////////////////////////////////
