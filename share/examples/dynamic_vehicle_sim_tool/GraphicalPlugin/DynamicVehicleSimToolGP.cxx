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
    UpdateSelectedGeometryPositions();
    bool syncedData = true;
    if( m_positionStack.size() < m_animationedNodes.size() )
    {
        syncedData = false;
        //std::cout << "we have a problem with the position stack size." << std::endl;
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
    //std::cout << syncedData << " " << hasConstrainedData << " " << m_constrainedGeom.valid() << std::endl;
    //Now update the constrained geom
    if( hasConstrainedData && m_constrainedGeom.valid() )
    {
        osg::ref_ptr< DCS > navDCS = mSceneManager->GetNavDCS();
        
        //Create the matrix stack for the constrained geom so that we get the 
        //fully transformed sim data to be applied to the nav matrix
        osg::Matrixd localToWorldMatrix = 
            osg::computeLocalToWorld( m_constrainedGeomPath );
        gmtl::Matrix44d tempMat;
        tempMat.set( localToWorldMatrix.ptr() );
        //Invert the view matrix because we want to negate the motion of the
        //cosntrained geometry
        gmtl::invert( tempMat );
        //The sim data is rotated 90 -- I do not understand why this is needed
        gmtl::AxisAngled viewCorrection( gmtl::Math::deg2Rad( 90.0 ), 0, 0, 1 );
        gmtl::Matrix44d myMat = 
            gmtl::makeRot< gmtl::Matrix44d >( viewCorrection );
        tempMat = myMat * tempMat;
        //Get the current nav position and add the constrained matrix into the 
        //current nav position
        //gmtl::Matrix44d navMat = navDCS->GetMat();
        //tempMat = m_initialNavMatrix * tempMat;
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

        if( !constrainedGeom.empty() )
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
        
        std::cout << computerName << " " << computerPort << " " << constrainedGeom << std::endl;
        return;
    }
    
    if( commandName == "Geometry Data Map" )
    {
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
            std::cout << nodeName << std::endl;
            if( nodeName == "No Geom" )
            {
                //noGeom = true;
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
                
                osg::NodePath tempPath = 
                    npVisitor.GetLocalToWorldNodePath( true ).at( 0 ).second;
                
                osg::Matrixd localToWorldMatrix = osg::computeLocalToWorld( tempPath );
                gmtl::Matrix44d tempMat;
                tempMat.set( localToWorldMatrix.ptr() );
        double scaleFactor = 1;
        //std::cout << i << " " <<  m_animationedNodes.size() << std::endl;
        //Need matrix stack for data
        //if( i < m_animationedNodes.size() )
        {
            //osg::Matrixd localToWorldMatrix = 
            //    osg::computeLocalToWorld( m_animationedNodes.at( i ).first );
            //gmtl::Matrix44d tempMat = m_initialPositionAccumulatedStack.at( i );
            //tempMat.set( localToWorldMatrix.ptr() );
            //std::cout << tempMat << std::endl;
            //double tempScale = 1.0;
            //Get the scale from the matrix. We are assuming a unifor scale.
            //osg::Vec3d scaleVec = localToWorldMatrix.getScale();
            gmtl::Vec3d scaleXVec( tempMat[ 0 ][ 0 ], tempMat[ 1 ][ 0 ], tempMat[ 2 ][ 0 ] );

            /*std::cout << " test 1 " << scaleVec[ 0 ] << std::endl;
            //tempMat
            osg::Vec3d translation;
            osg::Quat rotation;
            osg::Vec3d scale;
            osg::Quat so;
            localToWorldMatrix.decompose( translation, rotation, scale, so );
            std::cout << scale[ 0 ] << std::endl;*/
            scaleFactor = gmtl::length( scaleXVec );
        }

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
        //if( dvp )
        dvp->GetData( constrainedGeom );

        if( !constrainedGeom.empty() ) //&& (constrainedGeom != "No Geom") )
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
            std::cout << " constrained set " << std::endl;
            ves::xplorer::scenegraph::LocalToWorldNodePath npVisitor( 
                m_constrainedGeom.get(), mSceneManager->GetModelRoot() );
            m_constrainedGeomPath = 
                npVisitor.GetLocalToWorldNodePath( true ).at( 0 ).second;
        }

        std::cout << "geom map update " << constrainedGeom << std::endl;
        return;
    }

    if( commandName == "Simulator Update" )
    {
        ves::open::xml::DataValuePairPtr dvp = 
            m_currentCommand->GetDataValuePair( "Simulator State" );
        std::string simState;
        dvp->GetData( simState );
        SetSimState( simState );
        std::cout << "SimState " << simState << std::endl;
        if( simState == "Start" )
        {
            m_initialNavMatrix = mSceneManager->GetNavDCS()->GetMat();
            //gmtl::invert( m_initialNavMatrix );
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

        std::cout << computerName << " " << computerPort << std::endl;

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
                    //std::cout << "Read '" << " " << "' (" << bytes
                    //    << " bytes) from " << addr.getAddressString()
                    //    << std::endl;
                    bufferData.resize( 0 );
                    for( size_t i = 0; i < bytes; ++i )//bufferSize; ++i )
                    {
                        if( recv_buf[ i ] == '\0' )
                        {
                            bufferData.push_back( ' ' );
                            continue;
                        }
                        //std::cout << recv_buf[ i ] << std::endl;
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
                    
                    boost::split( splitVec, bufferData, boost::is_any_of(" "), boost::token_compress_on );
                    double tempDouble = 0;
                    for( size_t i = 0; i < splitVec.size(); ++i )
                    {
                        //std::cout << "<" << splitVec.at( i ) << "> ";
                        try
                        {
                            tempDouble = boost::lexical_cast<double>( splitVec.at( i ) );
                            positionData.push_back( tempDouble );
                        }
                        catch( boost::bad_lexical_cast& ex )
                        {
                            std::cout << "cannot cast data " << ex.what() << std::endl;
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

    gmtl::Point3d posData;
    gmtl::Vec3d xVec, yVec, zVec;
    gmtl::Matrix44d transMat;
    std::vector< double > positionData;
    GetPositionData( positionData );
    if( positionData.size() == 0 )
    {
        positionData.resize( 9, 0.0 );
        positionData.at( 3 ) = 1.0;
        positionData.at( 7 ) = 1.0;
    }
    unsigned int numObjects = positionData.size() / 9;

    if( simState == "Reset" )
    {
        for( size_t i = 0; i < positionData.size(); ++i )
        {
            positionData.at( i ) = 0.0;
        }
    }
    //std::cout << numObjects << std::endl;
    for( size_t i = 0; i < numObjects; ++i )
    {
        //GetPositionData( positionData );
        posData.set( positionData.at( 0 ), -positionData.at( 2 ), positionData.at( 1 ) );
        //std::cout << posData << std::endl;
        //std::cout << positionData.at( 3 ) << ", " << positionData.at( 4 ) << ", " << positionData.at( 5 )<< std::endl;
        //std::cout << positionData.at( 6 ) << ", " << positionData.at( 7 ) << ", " << positionData.at( 8 )<< std::endl;
        //std::cout << positionData.at( 3 ) << ", " << positionData.at( 4 ) << ", " << positionData.at( 5 )<< std::endl;
        xVec.set( positionData.at( 3 ), -positionData.at( 5 ), positionData.at( 4 ) );
        zVec.set( positionData.at( 6 ), -positionData.at( 8 ), positionData.at( 7 ) );

        yVec.set( (zVec[1]*xVec[2]) - (zVec[2]*xVec[1]),
                  (zVec[2]*xVec[0]) - (zVec[0]*xVec[2]),
                  (zVec[0]*xVec[1]) - (zVec[1]*xVec[0]) );
        //I do not know what this does not work since the math above is cut out
        //of the gmtl::cross function
        //yVec = gmtl::cross( yVec, xVec, zVec );
        //std::cout << xVec << std::endl << yVec << std::endl << zVec << std::endl;
        // 0.00328 mikes data
        //If we know we have an animated node for this data then figure out the 
        //scale factor
        double scaleFactor = 0.0328;
        //std::cout << i << " " << m_animationedNodes.size() << std::endl;

        if( i < m_animationedNodes.size() )
        {
            //std::cout << m_animationedNodes.at( i ).first << std::endl;
            scaleFactor = scaleFactor / m_animationedNodes.at( i ).first;
        }

        //std::cout << scaleFactor << std::endl;
        //scaleFactor = 0.01;
        //I do not know why the data is stored in columns but it works
        //http://www.fastgraph.com/makegames/3drotation/
        transMat.set( xVec[ 0 ], yVec[ 0 ], zVec[ 0 ], posData[ 0 ]*scaleFactor,
                      xVec[ 1 ], yVec[ 1 ], zVec[ 1 ], posData[ 1 ]*scaleFactor,
                      xVec[ 2 ], yVec[ 2 ], zVec[ 2 ], posData[ 2 ]*scaleFactor,
                             0.,        0.,        0.,           1. );
        
        //We can grab the ith matrix because the indices of the position stack
        //correspond to the position of the data coming back from the simulator
        transMat = m_initialPositionStack.at( i ) * transMat;
            //gmtl::Vec3d scaleXVec( transMat[ 0 ][ 0 ], transMat[ 1 ][ 0 ], transMat[ 2 ][ 0 ] );
            //scaleFactor = gmtl::length( scaleXVec );
            //std::cout << scaleFactor << std::endl;
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