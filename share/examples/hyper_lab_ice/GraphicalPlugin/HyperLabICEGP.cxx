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
#include <ves/xplorer/scenegraph/util/FindChildWithNameVisitor.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/EnvironmentHandler.h>

#include <osgDB/ReadFile>
#include <osgUtil/LineSegmentIntersector>
#include <osgUtil/Optimizer>

#include <osg/Depth>
#include <osg/MatrixTransform>
#include <osg/Matrix>

#include <sstream>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <cstdlib>

// pick this up from RTTScene for now
extern osg::ref_ptr<osg::Texture2D> RTTtex;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
#define NETL_DEMO

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
    PluginBase(),
    m_threeSecond( boost::posix_time::microsec_clock::local_time() ),
    m_lastSend( boost::posix_time::microsec_clock::local_time() )
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "HyperLabICEPlugin";
    
    std::srand( 111978 );
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
    
    InitializeLiveSensorObjects();
    
    m_threeSecond = boost::posix_time::microsec_clock::local_time();
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
    //renderRoot->addChild( models.get() );

    models = osgDB::readNodeFile( "Models/HyperLab_Facility_v6.osg", options.get() );
    if( !( models.valid() ) )
    {
        osg::notify( osg::FATAL ) << "Can't open model file(s)." << std::endl;
    }
    //renderRoot->addChild( models.get() );

    models = osgDB::readNodeFile( "Models/HyperSystem_v7.osg", options.get() );
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
    //{
        osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform(
            osg::Matrix::scale( osg::Vec3( CM2FEET, CM2FEET, CM2FEET ) ) );
        mt->setDataVariance( osg::Object::STATIC );
        mt->addChild( renderRoot.get() );
        
        //osgUtil::Optimizer optimizer;
        //optimizer.optimize( mt.get(), osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS );
    //}
    
    root->addChild( mt.get() );

    std::cout << "Loaded all of the models for the HYPER plant." << std::endl;
    return( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::PreFrameUpdate()
{
    if( !OneSecondCheck( m_lastSend, 500000 ) )
    {
        return;
    }

    {
        static double counter = 90;
        int coinFlip = std::rand() % 11 - 5;
        counter += coinFlip;

        if( counter > 180 )
        {
            counter = 180;
        }
        if( counter < 0 )
        {
            counter = 0;
        }
        size_t gaugeIndex = std::rand() % 3;
        SensorGaugeContainer::iterator iter = m_pressureIndicators.begin();
        std::advance( iter, gaugeIndex );
        //iter = iter + gaugeIndex;
        //for( SensorGaugeContainer::const_iterator iter = m_pressureIndicators.begin(); iter != m_pressureIndicators.end(); ++iter )
        {
            osg::Matrix tempMat = iter->second->getMatrix();
            tempMat.setRotate( osg::Matrix::rotate( osg::DegreesToRadians( counter ), osg::Vec3( 0, 1, 0 ) ).getRotate() );
            iter->second->setMatrix( tempMat );
        }
    }

    //Updates for the valve
    {
        static bool updateValve = false;
        static double valveCounter = 0;
        bool timeStatus = OneSecondCheck( m_threeSecond, 20000000 );

        if( !timeStatus && !updateValve )
        {
            //std::cout << " here " << std::endl;
            return;
        }
        updateValve = true;
        valveCounter+=5;
        
        for( SensorGaugeContainer::const_iterator iter = m_hvIndicators.begin(); iter != m_hvIndicators.end(); ++iter )
        {
            osg::Vec3 rotAxis( 0, 1, 0 );
            if( iter->first == "HV430" )
            {
                rotAxis.set( 0, 0, 1 );
            }
            osg::Matrix tempMat = iter->second->getMatrix();
            //tempMat.setRotate( osg::Matrix::rotate( osg::DegreesToRadians( 90.0 ), rotAxis ).getRotate() );
            //tempMat.preMultRotate( osg::Matrix::rotate( osg::DegreesToRadians( valveCounter ), rotAxis ).getRotate() );
            tempMat.setRotate( osg::Matrix::rotate( osg::DegreesToRadians( valveCounter ), rotAxis ).getRotate() );
           iter->second->setMatrix( tempMat );
        }
        m_threeSecond = boost::posix_time::microsec_clock::local_time();

        if( ( valveCounter == 90 ) || ( valveCounter == 0 ) )
        {
            valveCounter *= -1;
            updateValve = false;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::InitializeLiveSensorObjects()
{
    //Update the Pressure Indicators
    {
        //Traverse to find the node
        //PI019, 411, 413
        //Get the first child
        //Rotate the gauge accordingly
        std::vector< std::string > loadedPartNumbers;
        loadedPartNumbers.push_back( "PI019" );
        loadedPartNumbers.push_back( "PI411" );
        loadedPartNumbers.push_back( "PI413" );
        
        for( size_t i = 0; i < loadedPartNumbers.size(); ++i )
        {
            ves::xplorer::scenegraph::util::FindChildWithNameVisitor 
                childVisitor( mDCS.get(), loadedPartNumbers.at( i ), true, false );
            if( childVisitor.FoundChild() )
            {
                std::cout << "Found graphics node match for " << loadedPartNumbers.at( i ) << std::endl;
                osg::ref_ptr< osg::Group > tempGroup = childVisitor.GetFoundNode()->asGroup();
                osg::ref_ptr< osg::MatrixTransform > child = tempGroup->getChild( 0 )->asTransform()->asMatrixTransform();
                //tempGroup->removeChild( child.get() );
                
                //osg::ref_ptr< osg::PositionAttitudeTransform > pat = new osg::PositionAttitudeTransform();
                m_pressureIndicators[ loadedPartNumbers.at( i ) ] = child.get();
                
                //pat->addChild( child.get() );
                //tempGroup->addChild( pat.get() );
            }
            else
            {
                std::cout << "Did not find graphics node for " << loadedPartNumbers.at( i ) << std::endl;
            }
        }
    }
    //Update the HV gauges
    {
        //Traverse to find the nodes
        //HV408,414,430
        //Rotate the child
        //Spin it with a given velocity
        std::vector< std::string > loadedPartNumbers;
        loadedPartNumbers.push_back( "HV408" );
        loadedPartNumbers.push_back( "HV414" );
        loadedPartNumbers.push_back( "HV430" );
        
        for( size_t i = 0; i < loadedPartNumbers.size(); ++i )
        {
            ves::xplorer::scenegraph::util::FindChildWithNameVisitor 
            childVisitor( mDCS.get(), loadedPartNumbers.at( i ), true, false );
            if( childVisitor.FoundChild() )
            {
                std::cout << "Found graphics node match for " << loadedPartNumbers.at( i ) << std::endl;
                osg::ref_ptr< osg::Group > tempGroup = childVisitor.GetFoundNode()->asGroup();
                osg::ref_ptr< osg::MatrixTransform > child = tempGroup->getChild( 0 )->asTransform()->asMatrixTransform();
                //tempGroup->removeChild( child.get() );
                
                //m_hvIndicators[ loadedPartNumbers.at( i ) ] = new osg::PositionAttitudeTransform();
                m_hvIndicators[ loadedPartNumbers.at( i ) ] = child.get();
                
                //pat->addChild( child.get() );
                //tempGroup->addChild( pat.get() );
            }
            else
            {
                std::cout << "Did not find graphics node for " << loadedPartNumbers.at( i ) << std::endl;
            }
        }
    }
    //Update the FI015 gauges
    {
        //Traverse to find the nodes
        //Move the ball gauge accordingly
    }
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
bool HyperLabICEGP::OneSecondCheck( boost::posix_time::ptime& last_send, const int timeDelta )
{
    bool second_elapsed = false;
    //const int microsecs_in_second = 1000000;
    boost::posix_time::ptime current_time( boost::posix_time::microsec_clock::local_time() );
    boost::posix_time::time_duration diff = current_time - last_send;
    if( diff.total_microseconds() > timeDelta )
    {
        last_send = current_time;
        second_elapsed = true;
    }
    return second_elapsed;
}
////////////////////////////////////////////////////////////////////////////////
