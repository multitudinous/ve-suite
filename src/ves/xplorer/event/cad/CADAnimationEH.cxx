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
#include <ves/xplorer/event/cad/CADAnimationEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/Clone.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/open/xml/cad/CADNodeAnimation.h>
#include <ves/open/xml/TransformPtr.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/cad/CADNode.h>

#include <ves/xplorer/util/fileIO.h>
#include <ves/xplorer/Debug.h>

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
using namespace ves::xplorer::event;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADAnimationEventHandler::CADAnimationEventHandler()
        : ves::xplorer::event::CADEventHandler()
{}
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
        dirSel->GetData( offDirx );

        CADNodeAnimationPtr newAnim;
        std::string animationFile;
        newAnim = boost::dynamic_pointer_cast<CADNodeAnimation>( cadAnim->GetDataXMLObject() );
        std::string tempFilename = newAnim->GetAnimationFileName();
        boost::filesystem::path correctedPath( newAnim->GetAnimationFileName(), boost::filesystem::no_check );
        vprDEBUG( vesDBG, 1 ) << "|\t---" << tempFilename << "---"
            << correctedPath.native_file_string()
            << std::endl << vprDEBUG_FLUSH;
        animationFile = correctedPath.native_file_string();

        _readData( animationFile );
        
        ves::xplorer::scenegraph::DCS* animPart = 0;

        if( nodeType->GetDataString() == std::string( "Part" ) )
        {
            if( m_cadHandler->PartExists( nodeID->GetDataString() ) )
            {
                animPart = m_cadHandler->GetPart( nodeID->GetDataString() )->GetDCS();
            }
        }
        else if( nodeType->GetDataString() == std::string( "Assembly" ) )
        {
            if( m_cadHandler->AssemblyExists( nodeID->GetDataString() ) )
            {
                animPart = m_cadHandler->GetAssembly( nodeID->GetDataString() );
            }
        }

	    osg::ref_ptr< osg::AnimationPathCallback > callBack_obj = new osg::AnimationPathCallback();
	    callBack_obj->setAnimationPath( createAnimationPath( "seat" ).get() );
	    animPart->setUpdateCallback( callBack_obj.get() );

    }
    catch ( ... )
    {
        std::cout << "Error!!!Invalid command passed to CADAnimationEH!!" << std::endl;
    }
}
//////////////////////////////////////////////////////////////////////////////////////////////////
void CADAnimationEventHandler::_readData( std::string animFile )
{
	std::ifstream inputFile( animFile.c_str(), std::ios::in ); 

    if (!inputFile)
    {
        std::cout<<"Model reference file "<<animFile.c_str()<<" could not be opened\n"<<std::endl;
        return;
    }
    else
    {
        std::cout << "\nSuccessfully opened "<<animFile.c_str()<<" file\n" <<std::endl;
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

    std::vector< float > chassisX;
    std::vector< float > chassisY;
    std::vector< float > chassisZ;
    std::vector< float > chassisRoll;
    std::vector< float > chassisPitch;
    std::vector< float > chassisYaw;

    std::vector< float > cabX;
    std::vector< float > cabY;
    std::vector< float > cabZ;
    std::vector< float > cabRoll;
    std::vector< float > cabPitch;
    std::vector< float > cabYaw;


    float tempData[19];
    std::string testString;
    while ( !inputFile.eof() )
    {
	    std::getline(inputFile, testString);
        std::istringstream inputStream(testString);
        
        inputStream>>   tempData[0]>>tempData[1]>>tempData[2]>>tempData[3]>>tempData[4]>>tempData[5]>>
                        tempData[6]>>tempData[7]>>tempData[8]>>tempData[9]>>tempData[10]>>tempData[11]>>
                        tempData[12]>>tempData[13]>>tempData[14]>>tempData[15]>>tempData[16]>>tempData[17]>>
                        tempData[18];

	    time.push_back( tempData[0] );

	    seatX.push_back( tempData[1] * scale * offDirx.at(0) );
	    seatY.push_back( tempData[2] * scale * offDirx.at(1) );
	    seatZ.push_back( tempData[3] * scale * offDirx.at(2) );
	    seatRoll.push_back( tempData[4] * roationConv );
	    seatPitch.push_back( tempData[5] * roationConv );
	    seatYaw.push_back( tempData[6] * roationConv );

	    cabX.push_back( tempData[7] * scale * offDirx.at(0) );
	    cabY.push_back( tempData[8] * scale * offDirx.at(1) );
	    cabZ.push_back( tempData[9] * scale * offDirx.at(2) );
	    cabRoll.push_back( tempData[10] * roationConv );
	    cabPitch.push_back( tempData[11] * roationConv );
	    cabYaw.push_back( tempData[12] * roationConv );

	    chassisX.push_back( tempData[13] * scale * offDirx.at(0) );
	    chassisY.push_back( tempData[14] * scale * offDirx.at(1) );
	    chassisZ.push_back( tempData[15] * scale * offDirx.at(2) );
	    chassisRoll.push_back( tempData[16] * roationConv );
	    chassisPitch.push_back( tempData[17] * roationConv );
	    chassisYaw.push_back( tempData[18] * roationConv );

    }

	objectOne[ "time" ] = time;
	//position and orientation Data
	objectOne[ "seatX" ] = seatX;
	objectOne[ "seatY" ] = seatY;
	objectOne[ "seatZ" ] = seatZ;
	objectOne[ "seatRoll" ] = seatRoll;
	objectOne[ "seatPitch" ] = seatPitch;
	objectOne[ "seatYaw" ] = seatYaw;

}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::AnimationPath > CADAnimationEventHandler::createAnimationPath(std::string component)
{
   osg::ref_ptr<osg::AnimationPath> animationPath=new osg::AnimationPath;
   animationPath->setLoopMode(osg::AnimationPath::NO_LOOPING);

   std::string x = component + "X";
   std::string y = component + "Y";
   std::string z = component + "Z";
   std::string roll = component + "Roll";
   std::string pitch = component + "Pitch";
   std::string yaw = component + "Yaw";

   osg::Vec3 scale(1,1,1);
   osg::Vec3 xaxis(1,0,0);
   osg::Vec3 yaxis(0,1,0);
   osg::Vec3 zaxis(0,0,1);
   osg::Quat quat;

   osg::Vec3 trans;

   std::map< std::string, std::vector< float > > activeObj;
   activeObj = objectOne;

   unsigned int numTimeSteps = activeObj[ "time" ].size();
   for(unsigned int i=0;i< numTimeSteps;i++)
   {
      trans = osg::Vec3( -activeObj[ x ].at( i ),
                         activeObj[ y ].at( i ),
                         activeObj[ z ].at( i ));
      
      quat = osg::Quat( -activeObj[ roll ].at( i ),  xaxis,
                        -activeObj[ pitch ].at( i ), yaxis,
                        activeObj[ yaw ].at( i ),   zaxis );
      float time = activeObj[ "time" ].at( i );
      animationPath->insert( time, osg::AnimationPath::ControlPoint(trans,quat,scale) );
   }

   return animationPath;
}
