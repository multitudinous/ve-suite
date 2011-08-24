/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/xplorer/network/VE_i.h>
#include "ValveGraphicalPlugin.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/device/KeyboardMouse.h>
#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>
#include <gadget/Type/KeyboardMouseInterface.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/AnimationPath>
#include <osg/ShapeDrawable>
#include <osg/Sequence>

#include <osgText/Text>

#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

#include <osgSim/ColorRange>
#include <osg/Vec3d>

#include <boost/lexical_cast.hpp>

using namespace opcgp;

////////////////////////////////////////////////////////////////////////////////
ValveGraphicalPlugin::ValveGraphicalPlugin()
    :
    ves::xplorer::plugin::PluginBase(),
    m_keyboard( 0 )
{
    m_valveHeight = 0;
    m_valveOnOff = true;

    //DYNSIM
    mObjectName = "DWPlugin"; //name of the sheet
    ///Set the name of the commands we want to capture from the dynsim unit
    mEventHandlerMap[ "OPCData" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
ValveGraphicalPlugin::~ValveGraphicalPlugin()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ValveGraphicalPlugin::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    m_keyboard = 
        dynamic_cast< ves::xplorer::device::KeyboardMouse* >( mDevice );

    //valve
    m_handwheelGeometry = osgDB::readNodeFile( "Valve/handwheel.ive" );
    m_stemGeometry = osgDB::readNodeFile( "Valve/stem.ive" );
    m_valveGeometry = osgDB::readNodeFile( "Valve/valve.ive" );
    
    m_rotationDCS = new ves::xplorer::scenegraph::DCS();
    m_stemTransDCS = new ves::xplorer::scenegraph::DCS();
    m_valveDCS = new ves::xplorer::scenegraph::DCS();
    
    m_rotationDCS->addChild( m_handwheelGeometry.get() );
    m_stemTransDCS->addChild( m_stemGeometry.get() );
    m_rotationDCS->addChild( m_stemTransDCS.get() );
    
    m_valveDCS->addChild( m_valveGeometry.get() );
    m_valveDCS->addChild( m_rotationDCS.get() );
    
    double rot[3] = { 0.0, 0.0, 90.0 };
    double scale[3] = { 6.56, 6.56, 6.56 };
    double pos[3] = { 20.75, -52.5, 6.7 };
    m_valveDCS->SetTranslationArray( pos );
    m_valveDCS->SetRotationArray( rot );
    
    m_valveDCS->SetScaleArray( scale );
    mDCS->addChild( m_valveDCS.get() );

    //Setting up the pump or something to change colors with a scalar
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > meterDCS = new ves::xplorer::scenegraph::DCS();     
    m_pumpGeometry = osgDB::readNodeFile( "FlowMeter/flowmeter.ive" );
      
    meterDCS->addChild( m_pumpGeometry.get() );
    //double scale = { 3.28, 3.28, 3.28 };
    scale[0] = scale[1] = scale[2] = 3.28;
    //double pos = { 36, -28.67, 1.0 };
    pos[0] = 36;
    pos[1] = -28.67;
    pos[2] = 1.0;
    meterDCS->SetTranslationArray( pos );
    meterDCS->SetScaleArray( scale );

    mDCS->addChild( meterDCS.get() );

    //Initialize shaders
    const std::string shaderName = 
        osgDB::findDataFile( "color_texture_part.fs" );
    osg::ref_ptr< osg::Shader > fragShader = 
        osg::Shader::readShaderFile( osg::Shader::FRAGMENT, shaderName );
    
    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( fragShader.get() );
    
    osg::ref_ptr< osg::StateSet > stateSet = 
        m_pumpGeometry->getOrCreateStateSet();
    stateSet->setAttributeAndModes( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    
    m_highlightColor = new osg::Uniform( "partColor", osg::Vec4( 1.0, 1.0, 1.0, 0.0 ) );
    stateSet->addUniform( m_highlightColor );
    stateSet->addUniform( new osg::Uniform( "tex", 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void ValveGraphicalPlugin::PreFrameUpdate()
{
    //Valve
    double rotRate = 5;
    double transRate = 0.0039;

    double* tempTrans = m_stemTransDCS->GetVETranslationArray();

    if ( gmtl::Math::abs( tempTrans[1] -  m_valveHeight ) > ( transRate) )
    {
        double* tempRot = m_rotationDCS->GetRotationArray();
        if( m_valveHeight > tempTrans[1])
        {
            tempRot[2] -= rotRate;
        }
        else
        {
            tempRot[2] += rotRate;
        }
        m_rotationDCS->SetRotationArray( tempRot );

    }
	
    tempTrans[1] = m_valveHeight;
    m_stemTransDCS->SetTranslationArray( tempTrans );
      
    //Process key board event
    /*if( m_keyboard )
    {
        //If the mouse made a pick event
        if( !m_keyboard->GetMousePickEvent() )
        {
            return;
        }

        //If we had keyboard input then try and highlight the cad
        bool pickedParts = false;
        //if( m_mouseSelection )
        {
            FindPartNodeAndHighlightNode();
        }
    }*/

    //Process key board event
    if( m_keyboard )
    {
        gadget::KeyboardMousePtr tempKeys = 
            m_keyboard->GetKeyboardMouseVRJDevice();

        //Get the event queue
        gadget::KeyboardMouse::EventQueue evt_queue =
            tempKeys->getEventQueue();

        //Return if no events occurred
        if( evt_queue.empty() )
        {
            return;
        }

        bool pickedParts = false;

        //Iterate over the keyboard and mouse events
        gadget::KeyboardMouse::EventQueue::iterator i;
        for( i = evt_queue.begin(); i != evt_queue.end(); ++i )
        {
            const gadget::EventType type = ( *i )->type();

            switch( type )
            {
            case gadget::MouseButtonPressEvent:
            {
                gadget::MouseEventPtr mouse_evt =
                    boost::dynamic_pointer_cast< gadget::MouseEvent >( *i );

                mButton = mouse_evt->getButton();

                if( mButton == gadget::MBUTTON1)
                {
                    //m_valueAnimation->setSingleChildOn( 1 );
                }
                else if( mButton == gadget::MBUTTON3)
                {
                    //m_valueAnimation->setSingleChildOn( 2 );
                }
                break;
            }
            case gadget::MouseButtonReleaseEvent:
            {
                //to avoid multiple events being registered
                gadget::MouseEventPtr mouse_evt =
                    boost::dynamic_pointer_cast< gadget::MouseEvent >( *i );
                
                mButton = mouse_evt->getButton();
                
                if( mButton == gadget::MBUTTON1)
                {
                    pickedParts = true;
                }
                break;
            }
            }
        }

        //If we had keyboard input then try and highlight the cad
        if( pickedParts )
        {
            FindPartNodeAndHighlightNode();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ValveGraphicalPlugin::SetCurrentCommand(
    ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }
    
    //Valve
    if( command->GetDataValuePair("MY_VALVE") )
    {
        std::string percent;
        command->GetDataValuePair("MY_VALVE")->GetData( percent );
        double test = boost::lexical_cast<double>( percent );
        std::cout << "valve position " << test << std::endl;
        m_valveHeight = -0.125 * test;

        m_highlightColor->set( osg::Vec4( 1.0, 1.0-test, 1.0-test, 0.0) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ValveGraphicalPlugin::SetCurrentCommands(
    std::vector< ves::open::xml::CommandPtr > const& commands )
{
    if( commands.empty() )
    {
        return;
    }
    ves::open::xml::CommandPtr tempPtr = commands.at( commands.size() - 1 );
    SetCurrentCommand( tempPtr );
}
////////////////////////////////////////////////////////////////////////////////
void ValveGraphicalPlugin::FindPartNodeAndHighlightNode()
{

    osg::ref_ptr< osgUtil::LineSegmentIntersector > intersectorSegment = 
        m_keyboard->GetLineSegmentIntersector();

    osgUtil::IntersectionVisitor intersectionVisitor( intersectorSegment.get() );

    //Add the IntersectVisitor to the root Node so that all geometry will be
    //checked and no transforms are done to the line segement
    mDCS->accept( intersectionVisitor );

    osgUtil::LineSegmentIntersector::Intersections& intersections =
        intersectorSegment->getIntersections();
    if( intersections.empty() )
    {
        return;
    }
    
    //Find the part numbers of the nodes we hit
    osg::Node* objectHit = 0;
    osg::Node* tempParent = 0;
    //for( osgUtil::LineSegmentIntersector::Intersections::iterator itr =
    //    intersections.begin(); itr != intersections.end(); ++itr )
    {
        //objectHit = itr->nodePath[3];
        //grab the first item that is intersected
        //avoids sending multiple commands or activating an object in the
        //background
        objectHit = intersections.begin()->nodePath[3];
        std::string nodeName;
        
        if( objectHit == m_handwheelGeometry )
        {            
            ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
            //input variables;
            params->SetCommandName( "setOPCValues" );

            //add list to DVP
            ves::open::xml::DataValuePairPtr
                inpParams( new ves::open::xml::DataValuePair() );
            inpParams->SetDataName( "MY_VALVE.OP" );

            if( m_valveOnOff )
            {
                inpParams->SetDataString( "0" );
                m_valveOnOff = false;
            }
            else
            {
                inpParams->SetDataString( "1" );
                m_valveOnOff = true;
            }
            params->AddDataValuePair( inpParams );

            std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
                nodes;
            nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, 
            std::string >( params, "vecommand" ) );

            ves::open::xml::XMLReaderWriter commandWriter;
            std::string status="returnString";
            commandWriter.UseStandaloneDOMDocumentManager();
            commandWriter.WriteXMLDocument( nodes, status, "Command" );
            std::string temp = m_graphicalPluginManager->GetCORBAInterface()->QueryCE( status );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
