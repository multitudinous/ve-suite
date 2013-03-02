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
#include <ves/xplorer/event/cad/CADAnimationEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/Clone.h>
#include <ves/xplorer/scenegraph/util/AnimationNonInterpolatedPath.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/open/xml/cad/CADNodeAnimation.h>
#include <ves/open/xml/TransformPtr.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/cad/CADNode.h>

#include <ves/xplorer/Debug.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <osg/Node>
#include <osg/AnimationPath>

// --- C/C++ Libraries --- //
#include <sstream>
#include <fstream>
#include <istream>
#include <string>
#include <iostream>

// --- VR Juggler Includes --- //
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/version.hpp>
using namespace ves::xplorer::event;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADAnimationEventHandler::CADAnimationEventHandler()
    : ves::xplorer::event::CADEventHandler()
{
    CONNECTSIGNALS_3( "%CADAnimation",
                      void( std::string const&, std::string const&, std::string const& ),
                      &CADAnimationEventHandler::CreateAnimatedCAD,
                      m_connections, any_SignalType, normal_Priority );
    offDirx.clear();
    offDirx.push_back( 1 );
    offDirx.push_back( 1 );
    offDirx.push_back( 1 );
}
///////////////////////////////////////////////////////////////////////////////////////
CADAnimationEventHandler::CADAnimationEventHandler( const CADAnimationEventHandler& rhs )
    : ves::xplorer::event::CADEventHandler( rhs )
{}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADAnimationEventHandler::~CADAnimationEventHandler()
{}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADAnimationEventHandler& CADAnimationEventHandler::operator=( const CADAnimationEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::CADEventHandler::operator=( rhs );
    }
    return *this;
}
///////////////////////////////////////////////////////////////////////
void CADAnimationEventHandler::_operateOnNode( XMLObjectPtr xmlObject )
{
    try
    {
        CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );

        DataValuePairPtr nodeID = command->GetDataValuePair( "Node ID" );
        DataValuePairPtr nodeType = command->GetDataValuePair( "Node Type" );
        DataValuePairPtr cadAnim = command->GetDataValuePair( "Animation Info" );

        DataValuePairPtr dirSel = command->GetDataValuePair( "Direction Info" );
        offDirx.clear();
        dirSel->GetData( offDirx );

        const CADNodeAnimationPtr newAnim = boost::static_pointer_cast<CADNodeAnimation>( cadAnim->GetDataXMLObject() );

        CreateAnimatedCAD( nodeType->GetDataString(),
                           newAnim->GetAnimationFileName(),
                           nodeID->GetDataString() );
    }
    catch( ... )
    {
        std::cout << "Error!!!Invalid command passed to CADAnimationEH!!" << std::endl;
    }
}
//////////////////////////////////////////////////////////////////////////////////////////////////
bool CADAnimationEventHandler::ReadData( std::string const& animFile )
{
    std::ifstream inputFile( animFile.c_str(), std::ios::in );

    if( !inputFile )
    {
        std::cout << "|\tModel reference file " << animFile << " could not be opened." << std::endl;
        return false;
    }
    else
    {
        std::cout << "|\tSuccessfully opened " << animFile << " file." << std::endl;
    }

    //char temp[1024];
    float scale = 1.0f;
    float roationConv = 1.0f;

    std::vector< float > time;
    std::vector< float > seatX;
    std::vector< float > seatY;
    std::vector< float > seatZ;
    std::vector< float > seatRoll;
    std::vector< float > seatPitch;
    std::vector< float > seatYaw;

    float tempData[7];
    std::string testString;
    while( !inputFile.eof() )
    {
        std::getline( inputFile, testString );
        std::istringstream inputStream( testString );

        inputStream >> tempData[0] >> tempData[1] >> tempData[2] >> tempData[3] >> tempData[4] >> tempData[5] >>
                    tempData[6];

        time.push_back( tempData[0] );

        seatX.push_back( tempData[1] * scale * offDirx.at( 0 ) );
        seatY.push_back( tempData[2] * scale * offDirx.at( 1 ) );
        seatZ.push_back( tempData[3] * scale * offDirx.at( 2 ) );
        seatRoll.push_back( tempData[4] * roationConv );
        seatPitch.push_back( tempData[5] * roationConv );
        seatYaw.push_back( tempData[6] * roationConv );
    }

    objectOne[ "time" ] = time;
    //position and orientation Data
    objectOne[ "seatX" ] = seatX;
    objectOne[ "seatY" ] = seatY;
    objectOne[ "seatZ" ] = seatZ;
    objectOne[ "seatRoll" ] = seatRoll;
    objectOne[ "seatPitch" ] = seatPitch;
    objectOne[ "seatYaw" ] = seatYaw;

    inputFile.close();

    return true;
}
////////////////////////////////////////////////////////////////////////////////
osg::AnimationPath* CADAnimationEventHandler::createAnimationPath( std::string component )
{
    osg::AnimationPath* animationPath =
        new ves::xplorer::scenegraph::util::AnimationNonInterpolatedPath();
    animationPath->setLoopMode( osg::AnimationPath::NO_LOOPING );

    std::string x = component + "X";
    std::string y = component + "Y";
    std::string z = component + "Z";
    std::string roll = component + "Roll";
    std::string pitch = component + "Pitch";
    std::string yaw = component + "Yaw";

    osg::Vec3 scale( 1, 1, 1 );
    osg::Vec3 xaxis( 1, 0, 0 );
    osg::Vec3 yaxis( 0, 1, 0 );
    osg::Vec3 zaxis( 0, 0, 1 );
    osg::Quat quat;

    osg::Vec3 trans;

    std::map< std::string, std::vector< float > > activeObj;
    activeObj = objectOne;

    unsigned int numTimeSteps = activeObj[ "time" ].size();
    for( unsigned int i = 0; i < numTimeSteps; ++i )
    {
        trans = osg::Vec3( activeObj[ x ].at( i ),
                           activeObj[ y ].at( i ),
                           activeObj[ z ].at( i ) );

        quat = osg::Quat( osg::DegreesToRadians( activeObj[ roll ].at( i ) ),  xaxis,
                          osg::DegreesToRadians( activeObj[ pitch ].at( i ) ), yaxis,
                          osg::DegreesToRadians( activeObj[ yaw ].at( i ) ),   zaxis );
        float time = activeObj[ "time" ].at( i );
        animationPath->insert( time, osg::AnimationPath::ControlPoint( trans, quat, scale ) );
    }
    std::cout << "|\tRead " << numTimeSteps << " timesteps." << std::endl;

    return animationPath;
}
////////////////////////////////////////////////////////////////////////////////
void CADAnimationEventHandler::CreateAnimatedCAD( std::string const& nodeType,
        std::string const& filename, std::string const& nodeID )
{
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
    boost::filesystem::path correctedPath( filename );
    vprDEBUG( vesDBG, 1 ) << "|\t---" << filename << "---"
                          << correctedPath.string()
                          << std::endl << vprDEBUG_FLUSH;
    const std::string animationFile = correctedPath.string();
#else
    boost::filesystem::path correctedPath( filename, boost::filesystem::no_check );
    vprDEBUG( vesDBG, 1 ) << "|\t---" << filename << "---"
                          << correctedPath.native_file_string()
                          << std::endl << vprDEBUG_FLUSH;
    const std::string animationFile = correctedPath.native_file_string();
#endif
    if( !ReadData( animationFile ) )
    {
        return;
    }

    m_activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
    if( m_activeModel )
    {
        m_cadHandler = m_activeModel->GetModelCADHandler();
    }
    else
    {
        return;
    }

    ves::xplorer::scenegraph::DCS* animPart = 0;
    ves::xplorer::scenegraph::CADEntity* cadPart = 0;
    if( nodeType == std::string( "Part" ) )
    {
        if( m_cadHandler->PartExists( nodeID ) )
        {
            animPart = m_cadHandler->GetPart( nodeID )->GetDCS();
            cadPart = m_cadHandler->GetPart( nodeID );
            ModelHandler::instance()->RegisterAnimatedCADFile( cadPart );
        }
    }
    else if( nodeType == std::string( "Assembly" ) )
    {
        if( m_cadHandler->AssemblyExists( nodeID ) )
        {
            animPart = m_cadHandler->GetAssembly( nodeID );
            //cadPart = m_cadHandler->GetAssembly( nodeID );
        }
    }


    if( cadPart )
    {
        osg::ref_ptr< osg::AnimationPathCallback > callBack_obj = new osg::AnimationPathCallback();
        callBack_obj->setAnimationPath( createAnimationPath( "seat" ) );
        animPart->setUpdateCallback( callBack_obj.get() );
        callBack_obj->setPause( true );
        std::cout << "|\tRegister animated cad." << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
