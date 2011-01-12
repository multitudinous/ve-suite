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

#include <ves/xplorer/communication/CommunicationHandler.h>

// --- My Includes --- //
#include "DynamicVehicleSimToolGP.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>

#include <ves/xplorer/scenegraph/util/OpacityVisitor.h>
#include <ves/xplorer/scenegraph/util/MaterialInitializer.h>
#include <ves/xplorer/scenegraph/util/FindChildWithNameVisitor.h>
#include <ves/xplorer/scenegraph/util/ToggleNodesVisitor.h>

#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>
#include <ves/xplorer/scenegraph/FindParentWithNameVisitor.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/TextTexture.h>
#include <ves/xplorer/scenegraph/GroupedTextTextures.h>
#include <ves/xplorer/scenegraph/HeadPositionCallback.h>
#include <ves/xplorer/scenegraph/HeadsUpDisplay.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>

#include <ves/xplorer/environment/TextTextureCallback.h>

#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/device/KeyboardMouse.h>

#include <osgUtil/LineSegmentIntersector>
#include <osg/Depth>

#include <sstream>
#include <iostream>
#include <fstream>
#include <algorithm>

using namespace ves::xplorer::scenegraph;
using namespace warrantytool;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

#include <vector>

#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/find.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <boost/algorithm/string.hpp>
#include <boost/bind.hpp>

#include <vpr/vpr.h>
#include <vpr/IO/Socket/SocketDatagram.h>
#include <vpr/IO/Socket/InetAddr.h>

#include <gmtl/VecOps.h>
#include <gmtl/gmtl.h>
#include <gmtl/Point.h>
#include <gmtl/Misc/MatrixConvert.h>

//Define to test any of the dvst code
#define DVST_TEST 1

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimToolGP::DynamicVehicleSimToolGP()
    :
    PluginBase(),
    m_sampleThread( 0 ),
    m_simState( "Pause" ),
    m_computerName( "" ),
    m_computerPort( "" ),
    m_runSampleThread( false ),
    cm2ft( 0.032808399 )
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "DynamicVehicleSimToolUI";
    
    mEventHandlerMap[ "Tool Info" ] = this;
    mEventHandlerMap[ "Geometry Data Map" ] = this;
    mEventHandlerMap[ "Geometry Map Update" ] = this;
    mEventHandlerMap[ "Simulator Update" ] = this;
    mEventHandlerMap[ "Computer Info Update" ] = this;
    mEventHandlerMap[ "DVST Registration Update" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimToolGP::~DynamicVehicleSimToolGP()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
    
    m_sampleThread = 
        new vpr::Thread( boost::bind( &DynamicVehicleSimToolGP::SimulatorCaptureThread, this ) );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::PreFrameUpdate()
{
    std::string simState;
    GetSimState( simState );

    if( simState != "Start" )
    {
        return;
    }

    UpdateSelectedGeometryPositions();
    bool syncedData = true;
    if( m_positionStack.size() < m_animationedNodes.size() )
    {
        syncedData = false;
        vprDEBUG( vesDBG, 2 ) 
            << "\t\tDVST There is not enough position data from the simulator." 
            << std::endl << vprDEBUG_FLUSH;
    }

    //Update the animated parts
    bool hasConstrainedData = false;
    for( size_t i = 0; i < m_animationedNodes.size(); ++i )
    {
        //std::string cadID = m_animationedNodes.at( i ).first;
        osg::ref_ptr< ves::xplorer::scenegraph::DCS > tempNode = 
            m_animationedNodes.at( i ).second;
        if( syncedData )
        {
            if( tempNode.get() == m_constrainedGeom.get() )
            {
                hasConstrainedData = true;
            }
            
            tempNode->SetMat( m_positionStack.at( i  ) );
        }
    }

    //Now update the constrained geom
    if( hasConstrainedData && m_constrainedGeom.valid() )
    {
        osg::ref_ptr< DCS > navDCS = mSceneManager->GetNavDCS();
        
        //Create the matrix stack for the constrained geom so that we get the 
        //fully transformed sim data to be applied to the nav matrix
        const osg::Matrixd localToWorldMatrix = 
            osg::computeLocalToWorld( m_constrainedGeomPath );
        gmtl::Matrix44d tempMat;
        tempMat.set( localToWorldMatrix.ptr() );
        //Invert the view matrix because we want to negate the motion of the
        //cosntrained geometry
        gmtl::invert( tempMat );

        gmtl::Vec3d scaleXVec( tempMat[ 0 ][ 0 ], tempMat[ 1 ][ 0 ], tempMat[ 2 ][ 0 ] );
        double scaleVal = gmtl::length( scaleXVec );
        //std::cout << scaleVal << std::endl;
        const gmtl::Matrix44d invertScale = gmtl::makeScale< gmtl::Matrix44d >( 1.0/scaleVal );
        //std::cout << invertScale << std::endl;
        //The sim data is rotated 90 -- I do not understand why this is needed
        //gmtl::AxisAngled viewCorrection( gmtl::Math::deg2Rad( 90.0 ), 0, 0, 1 );
        //gmtl::Matrix44d myMat = 
        //    gmtl::makeRot< gmtl::Matrix44d >( viewCorrection );
        //std::cout << "nave update" << std::endl;
        //std::cout << myMat << std::endl;
        //tempMat = myMat * tempMat;
        //Get the current nav position and add the constrained matrix into the 
        //current nav position
        //gmtl::Matrix44d navMat = navDCS->GetMat();
        //std::cout << tempMat << std::endl;
        tempMat = m_initialNavMatrix * invertScale * tempMat;
        navDCS->SetMat( tempMat );
    }
    
    //Reset everything on a per frame basis
    m_positionStack.resize(0);
    m_navStack.resize(0);
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }
    m_currentCommand = command;
    
    const std::string commandName = m_currentCommand->GetCommandName();

    if( commandName == "Tool Info" )
    {
        ves::open::xml::DataValuePairPtr dvp1 = 
            m_currentCommand->GetDataValuePair( "ComputerName" );
        std::string computerName;
        dvp1->GetData( computerName );
        
        ves::open::xml::DataValuePairPtr dvp2 = 
            m_currentCommand->GetDataValuePair( "ComputerPort" );
        std::string computerPort;
        dvp2->GetData( computerPort );
        SetComputerData( computerName, computerPort );
        
        ves::open::xml::DataValuePairPtr dvp3 = 
            m_currentCommand->GetDataValuePair( "Constrained Geometry" );
        std::string constrainedGeom;
        dvp3->GetData( constrainedGeom );

        if( !constrainedGeom.empty() && (constrainedGeom != "None") )
        {
            if( mModelHandler->GetActiveModel()->
                GetModelCADHandler()->PartExists( constrainedGeom ) )
            {
                m_constrainedGeom = 
                    mModelHandler->GetActiveModel()->
                    GetModelCADHandler()->GetPart( constrainedGeom )->GetDCS();
            }
            else if( mModelHandler->GetActiveModel()->
                GetModelCADHandler()->AssemblyExists( constrainedGeom ) )
            {
                m_constrainedGeom = 
                    mModelHandler->GetActiveModel()->
                    GetModelCADHandler()->GetAssembly( constrainedGeom );
            }
            else
            {
                m_constrainedGeom = 0;
            }
        }
        else
        {
            m_constrainedGeom = 0;
        }
        
        if( m_constrainedGeom.valid() )
        {
            ves::xplorer::scenegraph::LocalToWorldNodePath npVisitor( 
                m_constrainedGeom.get(), mSceneManager->GetModelRoot() );
            m_constrainedGeomPath = 
                npVisitor.GetLocalToWorldNodePath( true ).at( 0 ).second;
        }
        return;
    }
    
    if( commandName == "Geometry Data Map" )
    {
        ResetScene();
        m_animationedNodes.clear();
        m_initialPositionStack.clear();
        m_initialPositionAccumulatedStack.clear();
        size_t numDVPs = m_currentCommand->GetNumberOfDataValuePairs();
        std::string nodeName;
        //bool noGeom = false;
        for( size_t i = 0; i < numDVPs; ++i )
        {
            ves::open::xml::DataValuePairPtr geomDVP = 
                m_currentCommand->GetDataValuePair( i );
            geomDVP->GetData( nodeName );
            //std::cout << nodeName << std::endl;
            if( nodeName == "No Geom" )
            {
                break;
            }

            osg::ref_ptr< ves::xplorer::scenegraph::DCS > tempNode;
            if( mModelHandler->GetActiveModel()->
                GetModelCADHandler()->PartExists( nodeName ) )
            {
                tempNode = 
                    mModelHandler->GetActiveModel()->
                    GetModelCADHandler()->GetPart( nodeName )->GetDCS();
            }
            else if( mModelHandler->GetActiveModel()->
                GetModelCADHandler()->AssemblyExists( nodeName ) )
            {
                tempNode = 
                    mModelHandler->GetActiveModel()->
                    GetModelCADHandler()->GetAssembly( nodeName );
            }

            if( tempNode.valid() )
            {
                ves::xplorer::scenegraph::LocalToWorldNodePath npVisitor( 
                    tempNode.get(), mSceneManager->GetModelRoot() );
                
                const osg::NodePath tempPath = 
                    npVisitor.GetLocalToWorldNodePath( true ).at( 0 ).second;
                
                const osg::Matrixd localToWorldMatrix = osg::computeLocalToWorld( tempPath );
                gmtl::Matrix44d tempMat;
                tempMat.set( localToWorldMatrix.ptr() );
                gmtl::Vec3d scaleXVec( tempMat[ 0 ][ 0 ], tempMat[ 1 ][ 0 ], tempMat[ 2 ][ 0 ] );
                const double scaleFactor = gmtl::length( scaleXVec );


                m_animationedNodes.push_back( std::make_pair< double, 
                    osg::ref_ptr< ves::xplorer::scenegraph::DCS > >( scaleFactor, tempNode.get() ) );
                
                m_initialPositionAccumulatedStack.push_back( tempMat );

                m_initialPositionStack.push_back( tempNode->GetMat() );
            }
        }
        
        return;
    }

    if( commandName == "Geometry Map Update" )
    {
        ves::open::xml::DataValuePairPtr dvp = 
            m_currentCommand->GetDataValuePair( "Constrained Geometry" );
        std::string constrainedGeom;
        dvp->GetData( constrainedGeom );

        if( !constrainedGeom.empty() && (constrainedGeom != "None") )
        {
            if( mModelHandler->GetActiveModel()->
                GetModelCADHandler()->PartExists( constrainedGeom ) )
            {
                m_constrainedGeom = 
                    mModelHandler->GetActiveModel()->
                    GetModelCADHandler()->GetPart( constrainedGeom )->GetDCS();
            }
            else if( mModelHandler->GetActiveModel()->
                GetModelCADHandler()->AssemblyExists( constrainedGeom ) )
            {
                m_constrainedGeom = 
                    mModelHandler->GetActiveModel()->
                    GetModelCADHandler()->GetAssembly( constrainedGeom );
            }
            else
            {
                m_constrainedGeom = 0;
            }
        }
        else
        {
            m_constrainedGeom = 0;
        }

        if( m_constrainedGeom.valid() )
        {
            ves::xplorer::scenegraph::LocalToWorldNodePath npVisitor( 
                m_constrainedGeom.get(), mSceneManager->GetModelRoot() );
            m_constrainedGeomPath = 
                npVisitor.GetLocalToWorldNodePath( true ).at( 0 ).second;
        }
        return;
    }

    if( commandName == "Simulator Update" )
    {
        ves::open::xml::DataValuePairPtr dvp = 
            m_currentCommand->GetDataValuePair( "Simulator State" );
        std::string simState;
        dvp->GetData( simState );
        SetSimState( simState );

        if( simState == "Start" )
        {
            ;
        }
        else if( simState == "Reset" )
        {
            ResetScene();
        }
        return;
    }

    if( commandName == "DVST Registration Update" )
    {
        ves::open::xml::DataValuePairPtr dvp = 
            m_currentCommand->GetDataValuePair( "Mode" );
        std::string simState;
        dvp->GetData( simState );
        
        if( simState == "Manual" )
        {
            m_initialNavMatrix = mSceneManager->GetNavDCS()->GetMat();
        }
        else
        {
            ves::open::xml::DataValuePairPtr dvp1 = 
                m_currentCommand->GetDataValuePair( "Filename" );
            dvp1->GetData( m_birdFilename );

            ReadBirdRegistrationFile();
            
            CalculateRegistrationVariables();
        }

        return;
    }

    if( commandName == "Computer Info Update" )
    {
        ves::open::xml::DataValuePairPtr dvp1 = 
            m_currentCommand->GetDataValuePair( "ComputerName" );
        std::string computerName;
        dvp1->GetData( computerName );
        
        ves::open::xml::DataValuePairPtr dvp2 = 
            m_currentCommand->GetDataValuePair( "ComputerPort" );
        std::string computerPort;
        dvp2->GetData( computerPort );
        SetComputerData( computerName, computerPort );
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::SimulatorCaptureThread()
{
    std::string simState;
    GetSimState( simState );
    std::string computerName, computerPort;
    
    while( simState != "Start" )
    {
        GetSimState( simState );
        vpr::System::msleep( 100 );  // thenth-second delay
        if( simState == "Exit" )
        {
            return;
        }
    }

    int status;
    vpr::Uint16 port(12345);     // Default listening port
    
    try
    {
        GetComputerData( computerName, computerPort );
        try
        {
            port = boost::lexical_cast<unsigned int>( computerPort );
        }
        catch( boost::bad_lexical_cast& ex )
        {
            std::cout << "cannot cast port data. defaulting to port " << port << std::endl;
        }
        
        m_runSampleThread = true;

        // Create a datagram socket that will be bound to port.
        vpr::InetAddr local;
        local.setPort(port);
        
        vpr::SocketDatagram sock(local, vpr::InetAddr::AnyAddr);
        
        // Bind the socket to the port.
        sock.open();
        sock.bind();
        
        //Now lets connet to the multicast group
        // Create a socket that is sending to a remote host named in the first
        // argument listening on the port named in the second argument.
        vpr::InetAddr remote_addr;
        remote_addr.setAddress( computerName, port);
        //vpr::SocketDatagram sock(vpr::InetAddr::AnyAddr, remote_addr);
        //vpr::SocketOptions::Types option = vpr::SocketOptions::AddMember;
        //vpr::SocketOptions::Data data;
        //data.mcast_add_member = vpr::McastReq( remote_addr, vpr::InetAddr::AnyAddr);
        vpr::McastReq data = vpr::McastReq( remote_addr, vpr::InetAddr::AnyAddr);
        sock.addMcastMember( data );
        
        const vpr::Uint32 bufferSize = 1200;
        char* recv_buf = new char[bufferSize];
        memset(recv_buf, '\0', bufferSize );//sizeof(recv_buf));

        typedef std::vector< std::string > split_vector_type;
        std::vector< double > positionData;
        std::string bufferData;
        vpr::InetAddr addr;
        split_vector_type splitVec;

        // Loop forever reading messages from clients.
        while( m_runSampleThread )
        {
            GetSimState( simState );

            while( simState == "Start" )
            {
                try
                {
                    // Read a message from a client.
                    const vpr::Uint32 bytes = sock.recvfrom(recv_buf, bufferSize,
                                                            addr);

                    // If we read anything, print it and send a response.
                    vprDEBUG( vesDBG, 2 ) << "\t\tDVST read (" << bytes
                        << " bytes) from " << addr.getAddressString()
                        << std::endl << vprDEBUG_FLUSH;

                    bufferData.resize( 0 );
                    for( size_t i = 0; i < bytes; ++i )
                    {
                        if( recv_buf[ i ] == '\0' )
                        {
                            bufferData.push_back( ' ' );
                            continue;
                        }
                        bufferData.push_back( recv_buf[ i ] );
                    }
                    
                    boost::algorithm::trim( bufferData );
                    /*typedef boost::tokenizer< boost::escaped_list_separator<char> > Tok;
                     boost::escaped_list_separator<char> sep( "", "\n", "");
                     Tok tok( bufferData, sep );
                     std::string tempTok;
                     double tempDouble = 0;
                     size_t columnCount1 = 0;
                     std::vector< std::string > columnNames;
                     Tok::iterator firstDouble;
                     for(Tok::iterator tok_iter = tok.begin(); tok_iter != tok.end(); ++tok_iter)
                     {
                     tempTok = *tok_iter;
                     if( tempTok.empty() )
                     {
                     continue;
                     }
                     std::cout << "<" << tempTok << "> ";
                     
                     try
                     {
                     tempDouble = boost::lexical_cast<double>( tempTok );
                     //firstDouble = tok_iter;
                     //break;
                     //std::cout << tempDouble << " "; 
                     }
                     catch( boost::bad_lexical_cast& ex )
                     {
                     std::cout << "cannot cast data" << std::endl;
                     //columnNames.push_back( tempTok );
                     //columnCount1 +=1;
                     }
                     }
                     //std::cout << "Column Count " << columnCount1 << std::endl;*/
                    vprDEBUG( vesDBG, 2 ) << "\t\tDVST buffer data " 
                        << bufferData << std::endl << vprDEBUG_FLUSH;

                    boost::split( splitVec, bufferData, boost::is_any_of(" "), boost::token_compress_on );
                    double tempDouble = 0;
                    for( size_t i = 0; i < splitVec.size(); ++i )
                    {
                        try
                        {
                            tempDouble = boost::lexical_cast<double>( splitVec.at( i ) );
                            positionData.push_back( tempDouble );
                        }
                        catch( boost::bad_lexical_cast& ex )
                        {
                            std::cout << "cannot cast data " << ex.what() 
                                << " data is " << splitVec.at( i ) << std::endl;
                        }
                    }
                    SetPositionData( positionData );
                    positionData.resize( 0 );
                    splitVec.resize( 0 );
                }
                catch (vpr::IOException& ex)
                {
                    std::cerr << "Caught an I/O exception while communicating "
                    << "with client at " << addr.getAddressString()
                    << ":\n" << ex.what() << std::endl;
                }
            }
        }
        
        sock.close();
        
        status = EXIT_SUCCESS;
    }
    catch (vpr::SocketException& ex)
    {
        std::cerr << "Caught a socket exception:\n" << ex.what() << std::endl;
        status = EXIT_FAILURE;
    }
    catch (vpr::IOException& ex)
    {
        std::cerr << "Caught an I/O exception:\n" << ex.what() << std::endl;
        status = EXIT_FAILURE;
    }
    
    std::cout << "Thread exiting." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::UpdateSelectedGeometryPositions()
{
    std::string simState;
    GetSimState( simState );
    if( simState == "Pause" )
    {
        return;
    }

    const size_t numEntries = 9;
    gmtl::Point3d posData;
    gmtl::Vec3d xVec, yVec, zVec;
    gmtl::Matrix44d transMat;
    gmtl::Matrix44d hackTransMat;

    std::vector< double > positionData;
    GetPositionData( positionData );
    if( positionData.size() == 0 )
    {
        positionData.resize( numEntries, 0.0 );
        positionData.at( 3 ) = 1.0;
        positionData.at( 7 ) = 1.0;
    }
    unsigned int numObjects = positionData.size() / numEntries;

    vprDEBUG( vesDBG, 2 ) << "|\t\tDVST Number of objects from the simulator "
        << numObjects << std::endl << vprDEBUG_FLUSH;

    for( size_t i = 0; i < numObjects; ++i )
    {
        if( i == m_animationedNodes.size() )
        {
            ///If we are not trying to map data to nodes then do not process the data
            break;
        }

        const size_t indexOffset = i*numEntries;
        posData.set( positionData.at( 0 + indexOffset ), -positionData.at( 2 + indexOffset ), positionData.at( 1 + indexOffset ) );
        xVec.set( positionData.at( 3 + indexOffset ), -positionData.at( 5 + indexOffset ), positionData.at( 4 + indexOffset ) );
        zVec.set( positionData.at( 6 + indexOffset ), -positionData.at( 8 + indexOffset ), positionData.at( 7 + indexOffset ) );

        yVec.set( (zVec[1]*xVec[2]) - (zVec[2]*xVec[1]),
                  (zVec[2]*xVec[0]) - (zVec[0]*xVec[2]),
                  (zVec[0]*xVec[1]) - (zVec[1]*xVec[0]) );

        //All sim data is coming in as centimeters so scale it.
        double scaleFactor = 0.0328;
        scaleFactor = scaleFactor / m_animationedNodes.at( i ).first;

        //GMTL is columan major order so this is why the data is laid out in columns
        //http://www.fastgraph.com/makegames/3drotation/
        transMat.set( xVec[ 0 ], yVec[ 0 ], zVec[ 0 ], posData[ 0 ]*scaleFactor,
                      xVec[ 1 ], yVec[ 1 ], zVec[ 1 ], posData[ 1 ]*scaleFactor,
                      xVec[ 2 ], yVec[ 2 ], zVec[ 2 ], posData[ 2 ]*scaleFactor,
                             0.,        0.,        0.,           1. );
    
        //This is a hack to record the body position
        if( i == 0 )
        {
            hackTransMat = transMat;
        }

        //We can grab the ith matrix because the indices of the position stack
        //correspond to the position of the data coming back from the simulator
        if( i > 1 )
        {
            //This is a hack to make the wheels spin relative to the machine
            transMat = m_initialPositionStack.at( i ) * hackTransMat * transMat;            
        }
        else
        {
            transMat = m_initialPositionStack.at( i ) * transMat;            
        }

        //Now we push back the whole new matrix
        m_positionStack.push_back( transMat );
    }
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::SetupGeometryDataMaps()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::SimulatorControlUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::SetPositionData( std::vector< double >& temp )
{
    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
    m_positionBuffer = temp;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::GetPositionData( std::vector< double >& temp )
{
    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
    temp = m_positionBuffer;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::SetSimState( std::string& temp )
{
    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
    m_simState = temp;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::GetSimState( std::string& temp )
{
    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
    temp = m_simState;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::SetComputerData( std::string& computerName, std::string& computerPort )
{
    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
    m_computerName = computerName;
    m_computerPort = computerPort;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::GetComputerData( std::string& computerName, std::string& computerPort )
{
    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
    computerName = m_computerName;
    computerPort = m_computerPort;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::RemoveSelfFromSG()
{
    mOnSceneGraph = false;
    mWorldDCS->removeChild( mDCS.get() );

    m_runSampleThread = false;
    if( m_sampleThread )
    {
        try
        {
            std::string exitStr( "Exit" );
            SetSimState( exitStr );
            //vpr::System::msleep( 300 );
            m_sampleThread->kill();
            m_sampleThread->join();
            delete m_sampleThread;
        }
        catch( ... )
        {
            ;//do nothing
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::ResetScene()
{
    for( size_t i = 0; i < m_initialPositionStack.size(); ++i )
    {
        m_animationedNodes.at( i ).second->SetMat( m_initialPositionStack.at( i ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::CalculateRegistrationVariables()
{
    if( m_birdData.size() != 9 )
    {
        std::cout << "There are not enough birds in the bird config file." << std::endl;
        return;
    }
    gadget::PositionInterface  headposDevice;
    headposDevice.init( m_frontBird );
    gadget::PositionInterface  wandposDevice;
    wandposDevice.init( m_lrBird );
    gadget::PositionInterface  pointerposDevice;
    pointerposDevice.init( m_rrBird );
    
    //Get the bird data from
    //VJHead
    gmtl::Matrix44d headMat = gmtl::convertTo< double >( headposDevice->getData() );
#ifndef DVST_TEST
    gmtl::Point4d headPoint = gmtl::makeTrans< gmtl::Point4d >( headMat );
#else
    gmtl::Point4d headPoint;
    headPoint.set( 0.00165595, 4.06479, -2.38933, 1.0 );
#endif

    //VJWand - left rear
    gmtl::Matrix44d wandMat = gmtl::convertTo< double >( wandposDevice->getData() );
#ifndef DVST_TEST
    gmtl::Point4d wandPoint = gmtl::makeTrans< gmtl::Point4d >( wandMat );
#else
    gmtl::Point4d wandPoint;
    wandPoint.set( -1.36624, 4.40733, 2.72343, 1.0 );
#endif

    //VJPointer - right rear
    gmtl::Matrix44d pointerMat = gmtl::convertTo< double >( pointerposDevice->getData() );
#ifndef DVST_TEST
    gmtl::Point4d pointerPoint = gmtl::makeTrans< gmtl::Point4d >( pointerMat );
#else
    gmtl::Point4d pointerPoint;
    pointerPoint.set( 1.30108, 4.23967, 2.6748, 1.0 );
#endif

    //Create the centroid for the triangle
    //centroid = ((pt1[0]+pt2[0]+pt3[0])/3.0,
    //            (pt1[1]+pt2[1]+pt3[1])/3.0,
    //            (pt1[2]+pt2[2]+pt3[2])/3)
    gmtl::Point3d centroid;
    centroid.set( 
        (headPoint[ 0 ] + wandPoint[ 0 ] + pointerPoint[ 0 ])/3.0, 
        (headPoint[ 1 ] + wandPoint[ 1 ] + pointerPoint[ 1 ])/3.0,
        (headPoint[ 2 ] + wandPoint[ 2 ] + pointerPoint[ 2 ])/3.0 );

    //Create vector from the origin of the triangle to the fron bird
    gmtl::Vec3d forwardVec;
    forwardVec.set( headPoint[ 0 ] - centroid[ 0 ], 
                   -(headPoint[ 2 ] - centroid[ 2 ]), 
                   headPoint[ 1 ] - centroid[ 1 ] );
    gmtl::normalize( forwardVec );

    //Cross the front vector with the vector from one of the rear bird corners
    gmtl::Vec3d rearVec;
    rearVec.set( pointerPoint[ 0 ] - centroid[ 0 ], 
                -(pointerPoint[ 2 ] - centroid[ 2 ]), 
                pointerPoint[ 1 ] - centroid[ 1 ] );
    gmtl::normalize( rearVec );

    //Create the up vector
    gmtl::Vec3d upVec;
    upVec.set( (rearVec[1]*forwardVec[2]) - (rearVec[2]*forwardVec[1]),
             (rearVec[2]*forwardVec[0]) - (rearVec[0]*forwardVec[2]),
             (rearVec[0]*forwardVec[1]) - (rearVec[1]*forwardVec[0]) );
    gmtl::normalize( upVec );

    gmtl::Vec3d rightVec;
    rightVec.set( (forwardVec[1]*upVec[2]) - (forwardVec[2]*upVec[1]),
                 (forwardVec[2]*upVec[0]) - (forwardVec[0]*upVec[2]),
                 (forwardVec[0]*upVec[1]) - (forwardVec[1]*upVec[0]) );
    gmtl::normalize( rightVec );
    
    //GMTL is columan major order so this is why the data is laid out in columns
    //http://www.fastgraph.com/makegames/3drotation/
    gmtl::Matrix44d transMat;
    transMat.set( rightVec[ 0 ], forwardVec[ 0 ], upVec[ 0 ],  centroid[ 0 ],
                  rightVec[ 1 ], forwardVec[ 1 ], upVec[ 1 ], -centroid[ 2 ],
                  rightVec[ 2 ], forwardVec[ 2 ], upVec[ 2 ],  centroid[ 1 ],
                             0.,         0.,              0.,             1. );
    
    //Get the SIP offsets from the birds to the centroid
    //X is to the rear, y is up, z is to the left
    //fbirdd = (sip[0]*u.mm-1048.1*u.mm,sip[1]*u.mm+686.8*u.mm,sip[2]*u.mm+13.3*u.mm)
	//lrbirdd = (sip[0]*u.mm+597.8*u.mm,sip[1]*u.mm+792.5*u.mm,sip[2]*u.mm+421.4*u.mm)
	//rrbirdd = (sip[0]*u.mm+600.9*u.mm,sip[1]*u.mm+792.4*u.mm,sip[2]*u.mm-421.4*u.mm)
    double mm2ft = 0.0032808;

    /*double frontBirdX = -1048.1 * mm2ft;
    double frontBirdY = 686.8 * mm2ft;
    double frontBirdZ = 13.3 * mm2ft;

    double leftRearBirdX = 597.8 * mm2ft;
    double leftRearBirdY = 792.5 * mm2ft;
    double leftRearBirdZ = 421.4 * mm2ft;

    double rightRearBirdX = 600.9 * mm2ft;
    double rightRearBirdY = 792.4 * mm2ft;
    double rightRearBirdZ = -421.4 * mm2ft;*/
    
    double frontBirdX = m_birdData.at( 0 ) * mm2ft;
    double frontBirdY = m_birdData.at( 1 ) * mm2ft;
    double frontBirdZ = m_birdData.at( 2 ) * mm2ft;
    
    double leftRearBirdX = m_birdData.at( 3 ) * mm2ft;
    double leftRearBirdY = m_birdData.at( 4 ) * mm2ft;
    double leftRearBirdZ = m_birdData.at( 5 ) * mm2ft;
    
    double rightRearBirdX = m_birdData.at( 6 ) * mm2ft;
    double rightRearBirdY = m_birdData.at( 7 ) * mm2ft;
    double rightRearBirdZ = m_birdData.at( 8 ) * mm2ft;
    
    //These coords are transformed into ves coord space
    //x = -z
    //y = -x
    //z = y
    gmtl::Point3d sipOffSetFrontBird;
    sipOffSetFrontBird.set( -frontBirdZ, -frontBirdX, frontBirdY );

    gmtl::Point3d sipOffSetLeftRearBird;
    sipOffSetLeftRearBird.set( -leftRearBirdZ, -leftRearBirdX, leftRearBirdY );
    
    gmtl::Point3d sipOffSetRightRearBird;
    sipOffSetRightRearBird.set( -rightRearBirdZ, -rightRearBirdX, rightRearBirdY );
    
    gmtl::Point3d measuredSIPCentroid;
    measuredSIPCentroid.set( 
        (sipOffSetFrontBird[ 0 ] + sipOffSetLeftRearBird[ 0 ] + sipOffSetRightRearBird[ 0 ])/3.0, 
        (sipOffSetFrontBird[ 1 ] + sipOffSetLeftRearBird[ 1 ] + sipOffSetRightRearBird[ 1 ])/3.0,
        (sipOffSetFrontBird[ 2 ] + sipOffSetLeftRearBird[ 2 ] + sipOffSetRightRearBird[ 2 ])/3.0 );

    gmtl::Matrix44d measuredSIPCentroidMat = 
        gmtl::makeTrans< gmtl::Matrix44d >( measuredSIPCentroid );
    
    //Now we convert the sip matrix back through the transform mat to move it 
    //to the VR Juggler coord
#ifndef DVST_TEST
    gmtl::Matrix44d registerMat = transMat * measuredSIPCentroidMat;
    gmtl::invert( registerMat );

    m_initialNavMatrix = registerMat;
#else
    gmtl::AxisAngled viewCorrection( gmtl::Math::deg2Rad( 90.0 ), 0, 0, 1 );
    gmtl::Matrix44d myMat = gmtl::makeRot< gmtl::Matrix44d >( viewCorrection );
    gmtl::Matrix44d registerMat = transMat * measuredSIPCentroidMat;
    gmtl::invert( registerMat );

    m_initialNavMatrix = registerMat * myMat;
#endif
    //std::cout << m_initialNavMatrix << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolGP::ReadBirdRegistrationFile()
{
    m_birdData.resize( 0 );
    std::ifstream birdFile( m_birdFilename.c_str() );
    std::string bufferData;
    typedef std::vector< std::string > split_vector_type;
    split_vector_type splitVec;
    size_t count = 0;
    do
    {
        std::getline( birdFile, bufferData );
        boost::algorithm::trim( bufferData );
        if( bufferData[ 0 ] == '#' )
        {
            //test for a comment
        }
        else
        {
            boost::split( splitVec, bufferData, boost::is_any_of(" "), boost::token_compress_on );
            if( count == 0 )
            {
                m_frontBird = boost::lexical_cast<std::string>( splitVec.at( 0 ) );
                count += 1;
            }
            else if( count == 1 )
            {
                m_lrBird = boost::lexical_cast<std::string>( splitVec.at( 0 ) );
                count += 1;
            }
            else if( count == 2 )
            {
                m_rrBird = boost::lexical_cast<std::string>( splitVec.at( 0 ) );
                count += 1;
            }

            for( size_t i = 1; i < splitVec.size(); ++i )
            {
                try
                {
                    double tempDouble = boost::lexical_cast<double>( splitVec.at( i ) );
                    m_birdData.push_back( tempDouble );
                }
                catch( boost::bad_lexical_cast& ex )
                {
                    std::cout << "cannot cast data " << ex.what() 
                    << " data is " << splitVec.at( i ) << std::endl;
                }
            }          
        }
    }
    while( !birdFile.eof() );
}
////////////////////////////////////////////////////////////////////////////////
