/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include "SwitchGraphicalPlugin.h"

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
SwitchGraphicalPlugin::SwitchGraphicalPlugin()
    :
    ves::xplorer::plugin::PluginBase(),
    m_keyboard( 0 )
{
    //DYNSIM
    //mObjectName = "DSPlugin"; //name of the sheet
    mObjectName = "switch"; //name of the sheet
    ///Set the name of the commands we want to capture from the dynsim unit
    mEventHandlerMap[ "OPCData" ] = this;
    mEventHandlerMap[ "SWITCH_CAD" ] = this;

    m_switchDCS = 0;
}
////////////////////////////////////////////////////////////////////////////////
SwitchGraphicalPlugin::~SwitchGraphicalPlugin()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SwitchGraphicalPlugin::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    m_keyboard = 
        dynamic_cast< ves::xplorer::device::KeyboardMouse* >( mDevice );

    //default CAD
    //SWITCH
    m_startTransDCS = new ves::xplorer::scenegraph::DCS();
    m_stopTransDCS = new ves::xplorer::scenegraph::DCS();
    m_switchDCS = new ves::xplorer::scenegraph::DCS();
    
    //m_panelGeometry = osgDB::readNodeFile( "Switch/panel.ive" );
    m_startButtonGeometry = osgDB::readNodeFile( "Switch/go_button.ive" );
    m_stopButtonGeometry = osgDB::readNodeFile( "Switch/stop_button.ive" );

    m_startTransDCS->addChild( m_startButtonGeometry.get() );
    m_stopTransDCS->addChild( m_stopButtonGeometry.get() );
    
    //m_switchDCS->addChild( m_panelGeometry.get() );
    m_switchDCS->addChild( m_startTransDCS.get() );
    m_switchDCS->addChild( m_stopTransDCS.get() );
    
    double scale2[3] = { 3.28, 3.28, 3.28 };
    //double scale2[3] = { 1.0, 1.0, 1.0 };
    m_switchDCS->SetScaleArray( scale2 );
    
    double pos2[3] = { -66.313964, 16.33586, 5.349864 };
    //double pos2[3] = { -30.315, -12.336, 6.35 };
    m_switchDCS->SetTranslationArray( pos2 );

    double rot2[3] = { 90.0, 0.0, 0.0 };
    m_switchDCS->SetRotationArray( rot2 );
    
    mDCS->addChild( m_switchDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
void SwitchGraphicalPlugin::PreFrameUpdate()
{
    if( m_switchDCS != 0 )
    {
        //switch
        double* tempTrans = m_startTransDCS->GetVETranslationArray();
        double* tempTrans2 = m_stopTransDCS->GetVETranslationArray();

        //switch on
        if( m_switchOnOff > 0)
        {
            tempTrans[0] = -0.015;
            m_startTransDCS->SetTranslationArray( tempTrans );
            tempTrans2[0] = 0;
            m_stopTransDCS->SetTranslationArray( tempTrans2 );
        }
        //switch off
        else
        {
            tempTrans[0] = 0;
            m_startTransDCS->SetTranslationArray( tempTrans );
            tempTrans2[0] = -0.015;
            m_stopTransDCS->SetTranslationArray( tempTrans2 );
        }
    }

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
void SwitchGraphicalPlugin::SetCurrentCommand(
    ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }

    //switch value
    if( command->GetDataValuePair("MY_SWITCH") )
    {
        std::string percent;
        command->GetDataValuePair("MY_SWITCH")->GetData( percent );
        m_switchOnOff = boost::lexical_cast<double>( percent );
    }

    //switch cad
    if( command->GetCommandName( ).compare( "SWITCH_CAD" ) == 0 )
    {
        if( m_switchDCS != 0 )
        {
            mDCS->removeChild( m_switchDCS.get() );
        }

        m_startTransDCS = new ves::xplorer::scenegraph::DCS();
        m_stopTransDCS = new ves::xplorer::scenegraph::DCS();
        m_switchDCS = new ves::xplorer::scenegraph::DCS();

        //body
        std::string CAD;
        command->GetDataValuePair("SWITCH_BODY")->GetData( CAD );
        //m_panelGeometry = osgDB::readNodeFile( CAD.c_str() );

        //on button
        command->GetDataValuePair("ON_BUTTON")->GetData( CAD );
        m_startButtonGeometry = osgDB::readNodeFile( CAD.c_str() );

        //offbutton
        command->GetDataValuePair("OFF_BUTTON")->GetData( CAD );
        m_stopButtonGeometry = osgDB::readNodeFile( CAD.c_str() );

        //add to scene and placement
        m_startTransDCS = new ves::xplorer::scenegraph::DCS();
        m_stopTransDCS = new ves::xplorer::scenegraph::DCS();
        m_switchDCS = new ves::xplorer::scenegraph::DCS();

        m_startTransDCS->addChild( m_startButtonGeometry.get() );
        m_stopTransDCS->addChild( m_stopButtonGeometry.get() );

        //m_switchDCS->addChild( m_panelGeometry.get() );
        m_switchDCS->addChild( m_startTransDCS.get() );
        m_switchDCS->addChild( m_stopTransDCS.get() );

        double rot2[3] = { 90.0, 0.0, 0.0 };
        double scale2[3] = { 3.28, 3.28, 3.28 };
        double pos2[3] = { -30.315, -12.336, 6.35 };
        m_switchDCS->SetTranslationArray( pos2 );
        m_switchDCS->SetRotationArray( rot2 );
        m_switchDCS->SetScaleArray( scale2 );

        mDCS->addChild( m_switchDCS.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SwitchGraphicalPlugin::SetCurrentCommands(
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
void SwitchGraphicalPlugin::FindPartNodeAndHighlightNode()
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
        
        //start button
        if( objectHit == m_stopButtonGeometry )
        {    
            ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
            //input variables;
            params->SetCommandName( "setOPCValues" );

            //add list to DVP
            ves::open::xml::DataValuePairPtr
                inpParams( new ves::open::xml::DataValuePair() );
            inpParams->SetDataName( "MY_SWITCH.POS" );
            inpParams->SetDataString( "0" );
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
        //stop button
        else if( objectHit == m_startButtonGeometry )
        {
            ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
            //input variables;
            params->SetCommandName( "setOPCValues" );

            //add list to DVP
            ves::open::xml::DataValuePairPtr
                inpParams( new ves::open::xml::DataValuePair() );
            inpParams->SetDataName( "MY_SWITCH.POS" );
            inpParams->SetDataString( "1" );
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
