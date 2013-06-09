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

#include <ves/xplorer/scenegraph/util/MaterialInitializer.h>
#include <ves/xplorer/scenegraph/util/FindChildWithNameVisitor.h>
#include <ves/xplorer/scenegraph/util/ToggleNodesVisitor.h>

#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>
#include <ves/xplorer/scenegraph/FindParentWithNameVisitor.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/TextTexture.h>
#include <ves/xplorer/scenegraph/GroupedTextTextures.h>
#include <ves/xplorer/scenegraph/HeadPositionCallback.h>
#include <ves/xplorer/scenegraph/HeadsUpDisplay.h>
#include <ves/xplorer/scenegraph/Geode.h>

#include <ves/xplorer/environment/TextTextureCallback.h>

#include <ves/xplorer/EnvironmentHandler.h>

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkPolyDataMapper.h>
#include <vtkPolyData.h>
#include <vtkCellDataToPointData.h>
#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkCellArray.h>
#include <vtkDoubleArray.h>
#include <vtkPointData.h>

#include <osgUtil/LineSegmentIntersector>
#include <osg/Depth>

#include <sstream>
#include <iostream>
#include <fstream>
#include <algorithm>

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

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
#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/IO/TimeoutException.h>
#include <vpr/Util/Interval.h>

#include "SensorDemoPluginGP.h"

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/Debug.h>

using namespace Poco::Data;
using namespace ves::xplorer::scenegraph;
using namespace warrantytool;
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
SensorDemoPluginGP::SensorDemoPluginGP()
    :
    PluginBase(),
    mAddingParts( false ),
    m_groupedTextTextures( 0 ),
    m_cadRootNode( 0 ),
    m_hasPromiseDate( false ),
    m_mouseSelection( false ),
m_sampleThread( 0 ),
m_computerName( "" ),
m_computerPort( "" ),
m_runSampleThread( false )

{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "SensorDemoPlugin";
    m_dbFilename = "sample.db";
}
////////////////////////////////////////////////////////////////////////////////
SensorDemoPluginGP::~SensorDemoPluginGP()
{
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
    
    {
        m_textTrans = new ves::xplorer::scenegraph::DCS();
        std::vector< double > data;
        data.push_back( -1.43 );
        data.push_back(   6.5 );
        data.push_back( -0.70 );
        m_textTrans->SetTranslationArray( data );
        
        mDCS->addChild( m_textTrans.get() );
    }

    CONNECTSIGNAL_2( "%ConnectToSensorServer",
                    void( std::string const&, std::string const& ),
                    &SensorDemoPluginGP::SetComputerInfo,
                    m_connections, normal_Priority );

    CreateSensorGrid();
    
    LoadModels();
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::PreFrameUpdate()
{
    CreateSensorGrid();

}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    std::cout << "SensorDemoPluginGP::SetCurrentCommand" << std::endl << std::flush;
    if( !command )
    {
        return;
    }
    m_currentCommand = command;
    
    const std::string commandName = m_currentCommand->GetCommandName();
    ves::open::xml::DataValuePairPtr dvp = 
        m_currentCommand->GetDataValuePair( 0 );
    std::cout << commandName << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::SetComputerInfo( std::string const& ip, std::string const& port )
{
    std::cout << " computer ip # " << ip << " " << port << std::endl;
    m_computerName = ip;
    m_computerPort = port;

    if( !m_sampleThread )
    {
        m_sampleThread = 
            new vpr::Thread( boost::bind( &SensorDemoPluginGP::SimulatorCaptureThread, this ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::RemoveSelfFromSG()
{
    PluginBase::RemoveSelfFromSG();

    m_runSampleThread = false;
    if( m_sampleThread )
    {
        try
        {
            //std::string exitStr( "Exit" );
            //SetSimState( exitStr );
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
void SensorDemoPluginGP::StripDollarCharacters( std::string& data )
{
    char firstChar = data[ 0 ];
    char dollarChar( '$' );
    if( firstChar != dollarChar )
    {
        return;
    }

    boost::algorithm::replace_all( data, "$", "" );
    /*for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( '$', index );
        if ( index != std::string::npos )
        {
            //data.replace( index, 1, "" );
            data.erase( index, 1 );
        }
    }*/

    boost::algorithm::replace_all( data, ",", "" );
    /*for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( ',', index );
        if ( index != std::string::npos )
        {
            //data.replace( index, 1, "" );
            data.erase( index, 1 );
        }
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::ReplaceSpacesCharacters( std::string& data )
{
    boost::algorithm::trim( data );
    boost::algorithm::replace_all( data, " ", "_" );

/*
    for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( ' ', index );
        if ( index != std::string::npos )
        {
            data.replace( index, 1, "_" );
            //data.erase( index, 1 );
        }
    }
*/
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::SimulatorCaptureThread()
{
    //std::string simState;
    //GetSimState( simState );
    std::string computerName, computerPort;
    computerName = m_computerName;
    computerPort = m_computerPort;
    /*while( simState != "Start" )
    {
        GetSimState( simState );
        vpr::System::msleep( 100 );  // thenth-second delay
        if( simState == "Exit" )
        {
            return;
        }
    }*/
#if 1
    int status;
    vpr::Uint16 port(12345);     // Default listening port
    
    try
    {
        //GetComputerData( computerName, computerPort );
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
            //GetSimState( simState );
            
            //while( simState == "Start" )
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
#else
    vpr::Uint16 port(12345);     // Default listening port
    
    try
    {
        port = boost::lexical_cast<unsigned int>( computerPort );
    }
    catch( boost::bad_lexical_cast& ex )
    {
        std::cout << "cannot cast port data. defaulting to port " << port << std::endl;
    }
    
    try
    {
        m_runSampleThread = true;
        
        vpr::InetAddr remote_addr;
        remote_addr.setAddress(computerName, port);
    
        try
        {
            // The socket is a stack variable, so it will be deallocated if an
            // exception gets thrown. The vpr::SocketStream destructor closes
            // the socket if it is still open.
            vpr::SocketStream sock(vpr::InetAddr::AnyAddr, remote_addr);
            
            sock.open();
            
            // Connect to the server.
            sock.connect();
            
            const vpr::Uint32 bufferSize = 1200;
            char* recv_buf = new char[bufferSize];
            memset(recv_buf, '\0', bufferSize );//sizeof(recv_buf));
            
            typedef std::vector< std::string > split_vector_type;
            std::vector< double > positionData;
            std::string bufferData;
            vpr::InetAddr addr;
            split_vector_type splitVec;
            
            while( m_runSampleThread )
            {
                // Read from the server.
                try
                {
                    const vpr::Uint32 bytes =
                        sock.read(recv_buf, 1200, vpr::Interval(10, vpr::Interval::Sec));
                    
                    // If we read anything, print it and send a response.
                    vprDEBUG( vesDBG, 2 ) << "\t\tDVST read (" << bytes
                        << " bytes) from " << remote_addr.getAddressString()
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
                catch (vpr::TimeoutException&)
                {
                    std::cerr << "No resposne from server within timeout period!\n";
                }
            }
            
            sock.close();
        }
        catch (vpr::SocketException& ex)
        {
            std::cerr << "Caught a socket exception:\n" << ex.what()
            << std::endl;
        }
        catch (vpr::IOException& ex)
        {
            std::cerr << "Caught an I/O exception:\n" << ex.what() << std::endl;
        }
    }
    catch (vpr::UnknownHostException& ex)
    {
        std::cerr << "Failed to set server address:\n" << ex.what()
        << std::endl;
        return;
    }
    catch (vpr::Exception& ex)
    {
        std::cerr << "Caught an exception:\n" << ex.what() << std::endl;
    }
#endif    
    std::cout << "Thread exiting." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::SetPositionData( std::vector< double >& temp )
{
    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
    m_positionBuffer = temp;
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::GetPositionData( std::vector< double >& temp )
{
    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
    temp = m_positionBuffer;
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::CreateSensorGrid()
{
    vtkPolyData* pd = vtkPolyData::New();
    
    // temp grids
    vtkIdType face0[4] = { 0,  1,  5,  4};
    vtkIdType face1[4] = { 1,  2,  6,  5};
    vtkIdType face2[4] = { 2,  3,  7,  6};
    vtkIdType face3[4] = { 4,  5,  9,  8};
    vtkIdType face4[4] = { 5,  6, 10,  9};
    vtkIdType face5[4] = { 6,  7, 11, 10};
    vtkIdType face6[4] = { 8,  9, 13, 12};
    vtkIdType face7[4] = { 9, 10, 14, 13};
    vtkIdType face8[4] = {10, 11, 15, 14};

    vtkCellArray* cells = vtkCellArray::New();
    cells->InsertNextCell( 4, face0);
    cells->InsertNextCell( 4, face1);
    cells->InsertNextCell( 4, face2);
    cells->InsertNextCell( 4, face3);
    cells->InsertNextCell( 4, face4);
    cells->InsertNextCell( 4, face5);
    cells->InsertNextCell( 4, face6);
    cells->InsertNextCell( 4, face7);
    cells->InsertNextCell( 4, face8);

    vtkIdType  face9[4] = { 16, 17,  5,  4};
    vtkIdType face10[4] = { 17, 18,  6,  5};
    vtkIdType face11[4] = { 18, 19,  7,  6};
    vtkIdType face12[4] = { 20, 21, 17, 16};
    vtkIdType face13[4] = { 21, 22, 18, 17};
    vtkIdType face14[4] = { 22, 23, 19, 18};
    vtkIdType face15[4] = { 24, 25, 21, 20};
    vtkIdType face16[4] = { 25, 26, 22, 21};
    vtkIdType face17[4] = { 26, 27, 23, 22};
    
    cells->InsertNextCell( 4,  face9);
    cells->InsertNextCell( 4, face10);
    cells->InsertNextCell( 4, face11);
    cells->InsertNextCell( 4, face12);
    cells->InsertNextCell( 4, face13);
    cells->InsertNextCell( 4, face14);
    cells->InsertNextCell( 4, face15);
    cells->InsertNextCell( 4, face16);
    cells->InsertNextCell( 4, face17);
    
    pd->SetPolys( cells );

    vtkPoints* points = vtkPoints::New();
    //vertical
    //1st row
    points->InsertNextPoint(   0.0, -0.292, 1.25 );
    points->InsertNextPoint( 0.167, -0.292, 1.25 );
    points->InsertNextPoint(   0.5, -0.292, 1.25 );
    points->InsertNextPoint( 0.667, -0.292, 1.25 );
    //2nd row
    points->InsertNextPoint(   0.0, 0.0, 1.25 );
    points->InsertNextPoint( 0.167, 0.0, 1.25 );
    points->InsertNextPoint(   0.5, 0.0, 1.25 );
    points->InsertNextPoint( 0.667, 0.0, 1.25 );
    //3rd row
    points->InsertNextPoint(   0.0, 0.25,  1.25 );
    points->InsertNextPoint( 0.167, 0.25,  1.25 );
    points->InsertNextPoint(   0.5, 0.25,  1.25 );
    points->InsertNextPoint( 0.667, 0.25,  1.25 );
    //4th row
    points->InsertNextPoint(   0.0, 0.542, 1.25 );
    points->InsertNextPoint( 0.167, 0.542, 1.25 );
    points->InsertNextPoint(   0.5, 0.542, 1.25 );
    points->InsertNextPoint( 0.667, 0.542, 1.25 );
    
    //horizontal
    //1st row
    points->InsertNextPoint(   0.0, 0.0, 0.875 );
    points->InsertNextPoint( 0.167, 0.0, 0.875 );
    points->InsertNextPoint(   0.5, 0.0, 0.875 );
    points->InsertNextPoint( 0.667, 0.0, 0.875 );
    //2nd row
    points->InsertNextPoint(   0.0, 0.0, 0.583 );
    points->InsertNextPoint( 0.167, 0.0, 0.583 );
    points->InsertNextPoint(   0.5, 0.0, 0.583 );
    points->InsertNextPoint( 0.667, 0.0, 0.583 );
    //3rd row
    points->InsertNextPoint(   0.0, 0.0, 0.292 );
    points->InsertNextPoint( 0.167, 0.0, 0.292 );
    points->InsertNextPoint(   0.5, 0.0, 0.292 );
    points->InsertNextPoint( 0.667, 0.0, 0.292 );
    
    pd->SetPoints( points );
    
    vtkDoubleArray* tempScalars = vtkDoubleArray::New();
    tempScalars->SetNumberOfComponents( 1 );
    tempScalars->SetName( "Temperature" );
    //tempScalars->SetNumberOfTuples( 28 );
    std::vector< double > tempData;
    GetPositionData( tempData );
    if( tempData.size() == 0 )
    {
        for( size_t i = 0; i < 28; ++i )
        {
            tempScalars->InsertNextTuple1( 0.0f );
        }
    }
    else
    {
        for( size_t i = 0; i < tempData.size(); ++i )
        {
            tempScalars->InsertNextTuple1( tempData.at( i ) );
        }
    }
    
    pd->GetPointData()->AddArray( tempScalars );
    
    vtkLookupTable* lut = vtkLookupTable::New();
    lut->SetNumberOfTableValues( 256 );
    lut->SetHueRange( 0.6667, 0 );
    lut->SetSaturationRange( 1, 1 );
    lut->SetValueRange( 1, 1 );
    //lut->SetVectorComponent( 0 );
    //lut->SetVectorMode( 1 );
    
    vtkPolyDataMapper* mapper = vtkPolyDataMapper::New();
    mapper->SetInput( pd );
    mapper->SetScalarModeToUsePointFieldData();
    mapper->ColorByArrayComponent( "Temperature", 0 );
    mapper->SetScalarRange( 28.0f, 60.0f );
    mapper->SetLookupTable( lut );
    
    vtkActor* contActor = vtkActor::New();
    contActor->SetMapper(mapper);
    
    if( m_contourGeode.valid() )
    {
        m_textTrans->removeChild( m_contourGeode.get() );
    }
    
    osg::ref_ptr< ves::xplorer::scenegraph::Geode > tempGeode = 
        new ves::xplorer::scenegraph::Geode();
    tempGeode->TranslateToGeode( contActor );
    m_contourGeode = tempGeode.get();
    m_textTrans->addChild( m_contourGeode.get() );

    pd->Delete();
    points->Delete();
    cells->Delete();
    mapper->Delete();
    contActor->Delete();
    tempScalars->Delete();
    lut->Delete();
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPluginGP::LoadModels()
{
    m_sensorRack = new ves::xplorer::scenegraph::CADEntity(
                                            "sensor_rack.ive",
                                            mDCS.get(),
                                            false,
                                            "Off",
                                            mPhysicsSimulator );
    std::vector< double > data;
    //data.push_back( 2.6 );
    //data.push_back( 8.2 );
    //data.push_back( 3.38 );
    data.push_back( 7.5 );
    data.push_back( 22.3 );
    data.push_back( 11.1 );
    m_sensorRack->GetDCS()->SetTranslationArray( data );
    data[ 0 ] =   0.0;
    data[ 1 ] =   0.0;
    data[ 2 ] = 180.0;
    m_sensorRack->GetDCS()->SetRotationArray( data );
    data[ 0 ] = 3.28;
    data[ 1 ] = 3.28;
    data[ 2 ] = 3.28;
    m_sensorRack->GetDCS()->SetScaleArray( data );
    
    m_stovesLab = new ves::xplorer::scenegraph::CADEntity(
                                                          "StoveLab_v1.ive",
                                                          mDCS.get(),
                                                          false,
                                                          "Off",
                                                          mPhysicsSimulator );

    /*m_stovesLab = new ves::xplorer::scenegraph::CADEntity(
                                            "stoves_2.ive",
                                            mDCS.get(),
                                            false,
                                            "Off",
                                            mPhysicsSimulator );*/
    //data[ 0 ] = 0.5;
    //data[ 1 ] = 2.0;
    //data[ 2 ] = 0.0;
    data[ 0 ] = 0.5;
    data[ 1 ] = 2.0;
    data[ 2 ] = 0.0;    
    m_stovesLab->GetDCS()->SetTranslationArray( data );
    data[ 0 ] = -90.0;
    data[ 1 ] =  90.0;
    data[ 2 ] =   0.0;
    m_stovesLab->GetDCS()->SetRotationArray( data );
    data[ 0 ] = 3.28;
    data[ 1 ] = 3.28;
    data[ 2 ] = 3.28;
    m_stovesLab->GetDCS()->SetScaleArray( data );
}
////////////////////////////////////////////////////////////////////////////////
