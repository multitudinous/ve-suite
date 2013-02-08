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

#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/classification.hpp>

#include <vpr/vpr.h>
#include <vpr/Thread/Thread.h>

#include <sstream>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <cstdlib>

// pick this up from RTTScene for now
extern osg::ref_ptr<osg::Texture2D> RTTtex;

#ifdef OPC_CLIENT_CONNECT
#include "zmq.hpp"
#endif

#include <boost/config.hpp>
#ifdef BOOST_WINDOWS
# pragma warning(disable: 4275)
#else
#include <ves/util/GNUCompilerGuards.h>
DIAG_OFF( unused-parameter )
#endif

#include <boost/program_options.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

#ifdef BOOST_WINDOWS
# pragma warning(default: 4275)
#else
DIAG_ON( unused-parameter )
#endif

namespace pt = boost::property_tree;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//#define NETL_DEMO
#define TEST_GAUGES

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
    
    //Start thread for sampling OPC data
#ifdef OPC_CLIENT_CONNECT
    m_sampleThread = new vpr::Thread( boost::bind( &HyperLabICEGP::SetupOPCClient, this ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
int HyperLabICEGP::InitializeLabModels()
{    
    osg::Group* root = new osg::Group();

    mDCS->addChild( root );
    
    osg::ref_ptr< osgDB::ReaderWriter::Options > options = new osgDB::ReaderWriter::Options;
    options->setOptionString( "dds_flip" );
    
    osg::ref_ptr< osg::Group > renderRoot = new osg::Group;
#ifndef TEST_GAUGES
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

    models = osgDB::readNodeFile( "Models/HyperSystem_v8.osg", options.get() );
    if( !( models.valid() ) )
    {
        osg::notify( osg::FATAL ) << "Can't open model file(s)." << std::endl;
    }
    renderRoot->addChild( models.get() );
#else
    //osg::ref_ptr< osg::Node > models = osgDB::readNodeFile( "Models/Hyper_StandardizedIndicatorTest.osg", options.get() );
    osg::ref_ptr< osg::Node > models = osgDB::readNodeFile( "Models/Hyper_DigitalReadoutTest_v1.osg", options.get() );
    if( !( models.valid() ) )
    {
        osg::notify( osg::FATAL ) << "Can't open model file(s)." << std::endl;
    }
    renderRoot->addChild( models.get() );
#endif

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
    if( !OneSecondCheck( m_lastSend, 250000 ) )
    {
        return;
    }

    //Update the pressure indicators
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
        if( !m_pressureIndicators.empty() )
        {
            size_t gaugeIndex = std::rand() % m_pressureIndicators.size();
            SensorGaugeContainer::iterator iter = m_pressureIndicators.begin();
            std::advance( iter, gaugeIndex );
            //iter = iter + gaugeIndex;
            //for( SensorGaugeContainer::const_iterator iter = m_pressureIndicators.begin(); iter != m_pressureIndicators.end(); ++iter )
            {
                osg::Matrix tempMat = iter->second->getMatrix();
                osg::Quat tempQuat = tempMat.getRotate();
                tempQuat *= osg::Matrix::rotate( osg::DegreesToRadians( double( coinFlip ) ), osg::Vec3( 0, 1, 0 ) ).getRotate();
                tempMat.setRotate( tempQuat );
                iter->second->setMatrix( tempMat );
            }
        }
    }

    //Update the pressure transducers
    {
        if( !m_pressureTransducers.empty() )
        {
            size_t indicatorIndex = std::rand() % m_pressureTransducers.size();
            GuageTextContainer::iterator iter = m_pressureTransducers.begin();
            std::advance( iter, indicatorIndex );
            //iter = iter + gaugeIndex;
            //for( GuageTextContainer::const_iterator iter = m_pressureTransducers.begin(); iter != m_pressureTransducers.end(); ++iter )
            {
                //double testValue = ( std::rand() % 1000 ) * 0.01;
                double testValue = 10.0 * sin( 2. * 3.14 * m_lastSend.time_of_day().total_milliseconds() * 0.001 + 0.0 ) + 10.0;
                std::ostringstream streamData;
                streamData << std::fixed << std::setprecision( 3 ) << testValue;
                iter->second->setText( streamData.str() );
            }
        }
    }
    
    //Update the flow indicators
    {
        //double ballHeight = m_ballHeight;
        int coinFlip = std::rand() % 3 - 1;

        for( SensorGaugeContainer::const_iterator iter = m_fiIndicators.begin(); iter != m_fiIndicators.end(); ++iter )
        {
            osg::Matrix tempMat = iter->second->getMatrix();
            osg::Vec3 currentTrans = tempMat.getTrans();
            currentTrans[ 2 ] += (coinFlip * 0.25);
            if( currentTrans[ 2 ] < m_ballHeight )
            {
                currentTrans[ 2 ] = m_ballHeight;
            }
            if( currentTrans[ 2 ] > m_ballHeight + 12.0 )
            {
                currentTrans[ 2 ] = m_ballHeight + 12.0;
            }
            tempMat.setTrans( currentTrans );
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
            /*if( iter->first == "HV430" )
            {
                rotAxis.set( 0, 0, 1 );
            }*/
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
        //ConfigurePressureIndicators();
    }
    //Update the HV gauges
    {
        //ConfigureHandValves();
    }
    //Update the FI015 gauges
    {
        //ConfigureFlowIndicators();
    }
    //Update the Pressure Transducers
    {
        ConfigurePressureTransducers();
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
void HyperLabICEGP::ConfigurePressureIndicators()
{
    //Traverse to find the node
    //PI019, 411, 413
    //Get the first child
    //Rotate the gauge accordingly
    std::vector< std::string > loadedPartNumbers;
#ifndef TEST_GAUGES
    loadedPartNumbers.push_back( "PI019" );
    loadedPartNumbers.push_back( "PI026" );
    loadedPartNumbers.push_back( "PI403" );
    loadedPartNumbers.push_back( "PI406B" );
    loadedPartNumbers.push_back( "PI407" );
    loadedPartNumbers.push_back( "PI411" );
    loadedPartNumbers.push_back( "PI413" );
    loadedPartNumbers.push_back( "PI013" );
#else
    loadedPartNumbers.push_back( "PI000" );
    loadedPartNumbers.push_back( "PI001" );
    loadedPartNumbers.push_back( "PI002" );
    loadedPartNumbers.push_back( "PI003" );
#endif
    for( size_t i = 0; i < loadedPartNumbers.size(); ++i )
    {
        ves::xplorer::scenegraph::util::FindChildWithNameVisitor
        childVisitor( mDCS.get(), loadedPartNumbers.at( i ), true, false );
        if( childVisitor.FoundChild() )
        {
            std::cout << "Found graphics node match for " << loadedPartNumbers.at( i ) << std::endl;
            osg::ref_ptr< osg::Group > tempGroup = childVisitor.GetFoundNode()->asGroup();
            osg::ref_ptr< osg::MatrixTransform > child = tempGroup->getChild( 0 )->asTransform()->asMatrixTransform();
            
            osg::ref_ptr< osg::MatrixTransform > newTrans = new osg::MatrixTransform();
            osg::ref_ptr< osg::Node > dofNode = child->getChild( 0 );//->asGroup()->getChild( 0 );
            std::string nodeName = dofNode->getName();
            
            std::vector<std::string> strs;
            boost::split(strs,nodeName,boost::is_any_of("_"));
            //for( size_t j = 0; j < strs.size(); ++j )
            //{
            //    std::cout << strs.at( j ) << std::endl;
            //}
            unsigned int childNum = 0;
            child->removeChild( childNum );
            child->addChild( newTrans.get() );
            newTrans->addChild( dofNode.get() );
            
            //osg::Matrix tempMat = child->getMatrix();
            osg::Matrix tempMat = newTrans->getMatrix();
            tempMat.setRotate( osg::Matrix::rotate( osg::DegreesToRadians( 90.0 ), osg::Vec3( 0, 1, 0 ) ).getRotate() );
            //child->setMatrix( tempMat );
            newTrans->setMatrix( tempMat );
            
            //m_pressureIndicators[ loadedPartNumbers.at( i ) ] = child.get();
            m_pressureIndicators[ loadedPartNumbers.at( i ) ] = newTrans.get();
        }
        else
        {
            std::cout << "Did not find graphics node for " << loadedPartNumbers.at( i ) << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::ConfigurePressureTransducers()
{
    std::vector< std::string > loadedPartNumbers;
#ifndef TEST_GAUGES
    /*
    loadedPartNumbers.push_back( "PT003" );
    loadedPartNumbers.push_back( "PT012" );
    loadedPartNumbers.push_back( "PT104" );
    loadedPartNumbers.push_back( "PT116" );
    loadedPartNumbers.push_back( "PT140" );
    loadedPartNumbers.push_back( "PT148" );
    loadedPartNumbers.push_back( "PT151" );
    loadedPartNumbers.push_back( "PT180" );
    loadedPartNumbers.push_back( "PT200" );
    loadedPartNumbers.push_back( "PT236" );
    loadedPartNumbers.push_back( "PT305" );
    loadedPartNumbers.push_back( "PT342" );
    
    loadedPartNumbers.push_back( "PT406" );
    loadedPartNumbers.push_back( "PT411" );
    loadedPartNumbers.push_back( "PT420" );
    loadedPartNumbers.push_back( "PT436" );
    loadedPartNumbers.push_back( "PT610" );
    */
    
    loadedPartNumbers.push_back( "PT003" );
    loadedPartNumbers.push_back( "PT116" );
    loadedPartNumbers.push_back( "AT001" );
    loadedPartNumbers.push_back( "AT002" );
    loadedPartNumbers.push_back( "FT110" );
    loadedPartNumbers.push_back( "FT162" );
    loadedPartNumbers.push_back( "FT380" );
    loadedPartNumbers.push_back( "FT432" );
    loadedPartNumbers.push_back( "PDT172" );
#else
    loadedPartNumbers.push_back( "PT003" );
#endif
    for( size_t i = 0; i < loadedPartNumbers.size(); ++i )
    {
        const std::string childName = "Screen_Locator_" + loadedPartNumbers.at( i );
        ves::xplorer::scenegraph::util::FindChildWithNameVisitor
        childVisitor( mDCS.get(), childName, true, false );
        if( childVisitor.FoundChild() )
        {
            std::cout << "Found graphics node match for " << loadedPartNumbers.at( i ) << std::endl;
            osg::ref_ptr< osg::Geode > tempGroup = childVisitor.GetFoundNode()->asGroup()->getChild( 0 )->asGeode();
            
            osg::ref_ptr<osgText::Font> font = osgText::readFontFile( "fonts/arial.ttf" );
            osg::ref_ptr<osgText::Text> text = new osgText::Text;
            text->setDataVariance( osg::Object::DYNAMIC );
            text->setFont( font.get() );
            text->setCharacterSize( 0.90 );
            text->setPosition( osg::Vec3d( 0.0, -0.3, 0.0 ) );
            text->setAlignment( osgText::TextBase::LEFT_BOTTOM ); //osgText::TextBase::BASE_LINE );
            text->setAxisAlignment( osgText::TextBase::YZ_PLANE );
            text->setColor( osg::Vec4( 0, 0, 0, 1.0f ) );
            text->setText( "12.5" );
            
            tempGroup->addDrawable( text.get() );
            
            text->getOrCreateStateSet()->addUniform( new osg::Uniform( "isOsgText", true ) );
            
            m_pressureTransducers[ loadedPartNumbers.at( i ) ] = text.get();
        }
        else
        {
            std::cout << "Did not find graphics node for " << loadedPartNumbers.at( i ) << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::ConfigureHandValves()
{
    //Traverse to find the nodes
    //HV408,414,430
    //Rotate the child
    //Spin it with a given velocity
    std::vector< std::string > loadedPartNumbers;
#ifndef TEST_GAUGES
    loadedPartNumbers.push_back( "HV408" );
    loadedPartNumbers.push_back( "HV414" );
    loadedPartNumbers.push_back( "HV430" );
#else
    loadedPartNumbers.push_back( "HV000" );
    loadedPartNumbers.push_back( "HV001" );
#endif
    for( size_t i = 0; i < loadedPartNumbers.size(); ++i )
    {
        ves::xplorer::scenegraph::util::FindChildWithNameVisitor
        childVisitor( mDCS.get(), loadedPartNumbers.at( i ), true, false );
        if( childVisitor.FoundChild() )
        {
            std::cout << "Found graphics node match for " << loadedPartNumbers.at( i ) << std::endl;
            osg::ref_ptr< osg::Group > tempGroup = childVisitor.GetFoundNode()->asGroup();
            osg::ref_ptr< osg::MatrixTransform > child = tempGroup->getChild( 0 )->asTransform()->asMatrixTransform();
            osg::ref_ptr< osg::MatrixTransform > newTrans = new osg::MatrixTransform();
            osg::ref_ptr< osg::Node > dofNode = child->getChild( 0 );//->asGroup()->getChild( 0 );
            std::string nodeName = dofNode->getName();
            
            std::vector<std::string> strs;
            boost::split(strs,nodeName,boost::is_any_of("_"));
            //for( size_t j = 0; j < strs.size(); ++j )
            //{
            //    std::cout << strs.at( j ) << std::endl;
            //}
            unsigned int childNum = 0;
            child->removeChild( childNum );
            child->addChild( newTrans.get() );
            newTrans->addChild( dofNode.get() );
            
            m_hvIndicators[ loadedPartNumbers.at( i ) ] = newTrans;
        }
        else
        {
            std::cout << "Did not find graphics node for " << loadedPartNumbers.at( i ) << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::ConfigureFlowIndicators()
{
    //Traverse to find the nodes
    //Move the ball gauge accordingly
    std::vector< std::string > loadedPartNumbers;
    loadedPartNumbers.push_back( "FI015" );
    
    for( size_t i = 0; i < loadedPartNumbers.size(); ++i )
    {
        ves::xplorer::scenegraph::util::FindChildWithNameVisitor
        childVisitor( mDCS.get(), loadedPartNumbers.at( i ), true, false );
        if( childVisitor.FoundChild() )
        {
            std::cout << "Found graphics node match for " << loadedPartNumbers.at( i ) << std::endl;
            /*osg::ref_ptr< osg::Group > tempGroup = childVisitor.GetFoundNode()->asGroup();
             osg::ref_ptr< osg::MatrixTransform > child = tempGroup->getChild( 0 )->asTransform()->asMatrixTransform();
             m_fiIndicators[ loadedPartNumbers.at( i ) ] = child.get();
             osg::Matrix tempMat = child->getMatrix();
             osg::Vec3 currentTrans = tempMat.getTrans();
             m_ballHeight = currentTrans[ 2 ];
             currentTrans[ 2 ] += 6.0;
             tempMat.setTrans( currentTrans );
             child->setMatrix( tempMat );*/
            
            osg::ref_ptr< osg::Group > tempGroup = childVisitor.GetFoundNode()->asGroup();
            osg::ref_ptr< osg::MatrixTransform > child = tempGroup->getChild( 0 )->asTransform()->asMatrixTransform();
            osg::ref_ptr< osg::MatrixTransform > newTrans = new osg::MatrixTransform();
            osg::ref_ptr< osg::Node > dofNode = child->getChild( 0 );
            std::string nodeName = dofNode->getName();
            
            std::vector<std::string> strs;
            boost::split(strs,nodeName,boost::is_any_of("_"));
            
            unsigned int childNum = 0;
            child->removeChild( childNum );
            child->addChild( newTrans.get() );
            newTrans->addChild( dofNode.get() );
            
            m_fiIndicators[ loadedPartNumbers.at( i ) ] = newTrans.get();
            
            osg::Matrix tempMat = newTrans->getMatrix();
            osg::Vec3 currentTrans = tempMat.getTrans();
            currentTrans[ 2 ] += 6.0;
            tempMat.setTrans( currentTrans );
            newTrans->setMatrix( tempMat );
        }
        else
        {
            std::cout << "Did not find graphics node for " << loadedPartNumbers.at( i ) << std::endl;
        }
    }
}
#ifdef OPC_CLIENT_CONNECT
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::SetupOPCClient()
{
    //Init zeromq context
    zmq::context_t context( 1 );
    
    //Socket to receive messages on
    //zmq::socket_t receiver( context, ZMQ_PULL );
    //receiver.bind( "tcp://*:3097" );
    
    //Socket for control input
    zmq::socket_t controller( context, ZMQ_SUB );
    controller.connect( "tcp://localhost:3098" );
    controller.setsockopt( ZMQ_SUBSCRIBE, "", 0 );
    
    //Process messages from receiver and controller
    //zmq::pollitem_t items [] = {
    //    { receiver, 0, ZMQ_POLLIN, 0 },
    //    { controller, 0, ZMQ_POLLIN, 0 } };
    zmq::pollitem_t items [] = {
        { controller, 0, ZMQ_POLLIN, 0 } };
    
    //
    for( ; ; )
    {
        zmq::poll( &items [ 0 ], 1, -1 );
        
        if( items [ 0 ].revents & ZMQ_POLLIN )
        {
            zmq::message_t zmq_msg;
            controller.recv( &zmq_msg );
            std::string str( static_cast< char* >( zmq_msg.data() ), zmq_msg.size() );
            
            std::stringstream sstm;
            sstm << str;
            pt::ptree tree;
            boost::property_tree::json_parser::read_json( sstm, tree );
            const std::string message_type = tree.get<std::string>( "message_type" );

            
            std::cout << message_type << std::endl;
        }
        
        //Any waiting controller command acts as 'KILL'
        //if( items[ 0 ].revents & ZMQ_POLLIN ) break;
    }
}
////////////////////////////////////////////////////////////////////////////////
#endif