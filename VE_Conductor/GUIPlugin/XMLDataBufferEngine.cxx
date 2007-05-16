/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2007-02-27 22:54:03 -0600 (Tue, 27 Feb 2007) $
 * Version:       $Rev: 7013 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/GUIPlugin/XMLDataBufferEngine.h"

#include "VE_Open/XML/DataValuePair.h"

#include <sstream>

using namespace VE_XML;
using namespace VE_Conductor;

vprSingletonImp( XMLDataBufferEngine );
////////////////////////////////////////////////////////////////////////////////
XMLDataBufferEngine::XMLDataBufferEngine( void )
{ 
   VE_XML::Command nullCommand;
   nullCommand.SetCommandName( "NULL" );
   m_commandMap[ "NULL" ] = nullCommand;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::CleanUp( void )
{
   m_commandMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::Command& XMLDataBufferEngine::GetCommand( std::string commandKey )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    std::map< std::string, VE_XML::Command >::iterator iter;
    iter = m_commandMap.find( commandKey );
    if ( iter == m_commandMap.end() )
    {
        return m_commandMap[ "NULL" ];
    }
    return iter->second;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::SetCommand( std::string commandKey, 
                                      VE_XML::Command& command )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap[ commandKey ] = command;
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, VE_XML::Command >& 
XMLDataBufferEngine::GetCommandMap( void )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    return m_commandMap;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::SetCommandMap( std::map< std::string, 
                                         VE_XML::Command >& tempMap )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap = tempMap;
}
////////////////////////////////////////////////////////////////////////////////




/*
////////////////////////////////////////////////////////
void Network::CreateNetwork( std::string xmlNetwork )
{
    if ( xmlNetwork.empty() )
    {
        return;
    }
    
    // Just clear the design canvas
    //while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR) { ; }
    // Start the busy cursor
    // Load from the nt file loaded through wx
    // Get a list of all the command elements   
    _fileProgress->Update( 10, _("start loading") );
    VE_XML::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    
    if ( xmlNetwork.size() < 512 )
    {
        networkWriter.ReadFromFile();
    }
    else
    {
        networkWriter.ReadFromString();
    }
    _fileProgress->Update( 15, _("start loading") );
    networkWriter.ReadXMLData( xmlNetwork, "Model", "veNetwork" );
    std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();
    _fileProgress->Update( 25, _("start loading") );
    
    // do this for network
    if ( veNetwork )
        delete veNetwork;
    
    // we are expecting that a network will be found
    if ( !objectVector.empty() )
    {
        veNetwork = dynamic_cast< VE_XML::VE_Model::Network* >( objectVector.at( 0 ) );
    }
    else
    {
        wxMessageBox( _("Improperly formated ves file."), 
                      _("VES File Read Error"), wxOK | wxICON_INFORMATION );
    }
    
    _fileProgress->Update( 30, _("start loading") );
    long int tempScaleInfo;
    veNetwork->GetDataValuePair( 0 )->GetData( (userScale.first)  );
    veNetwork->GetDataValuePair( 1 )->GetData( (userScale.second) );
    veNetwork->GetDataValuePair( 2 )->GetData( tempScaleInfo );
    numPix.first = tempScaleInfo;
    veNetwork->GetDataValuePair( 3 )->GetData( tempScaleInfo );
    numPix.second = tempScaleInfo;
    veNetwork->GetDataValuePair( 4 )->GetData( tempScaleInfo );
    numUnit.first = tempScaleInfo;
    veNetwork->GetDataValuePair( 5 )->GetData( tempScaleInfo );
    numUnit.second = tempScaleInfo;
    
    links.clear();
    _fileProgress->Update( 35, _("start loading") );
    
    for ( size_t i = 0; i < veNetwork->GetNumberOfLinks(); ++i )
    {
        links.push_back( VE_Conductor::GUI_Utilities::Link( this ) );
        
        links.at( i ).SetFromPort( *(veNetwork->GetLink( i )->GetFromPort()) );
        links.at( i ).SetToPort( *(veNetwork->GetLink( i )->GetToPort()) );
        
        long moduleID;
        veNetwork->GetLink( i )->GetFromModule()->GetData( moduleID );
        links.at( i ).SetFromModule( moduleID );
        veNetwork->GetLink( i )->GetToModule()->GetData( moduleID );
        links.at( i ).SetToModule( moduleID );
        
        size_t numberOfPoints = veNetwork->GetLink( i )->GetNumberOfLinkPoints();
        for ( size_t j = 0; j < numberOfPoints; ++j )
        {
            std::pair< unsigned int, unsigned int > rawPoint = veNetwork->GetLink( i )->GetLinkPoint( j )->GetPoint();
            wxPoint point;
            point.x = rawPoint.first;
            point.y = rawPoint.second;
            links.at( i ).SetPoint( &point );
        }
        // Create the polygon for links
        links.at( i ).CalcLinkPoly();
    }
    _fileProgress->Update( 50, _("create models") );
    // do this for models
    networkWriter.ReadXMLData( xmlNetwork, "Model", "veModel" );
    objectVector = networkWriter.GetLoadedXMLObjects();
    
    _fileProgress->Update( 75, _("done create models") );
    // now lets create a list of them
    int timeCalc = 25/objectVector.size();
    for ( size_t i = 0; i < objectVector.size(); ++i )
    {
        _fileProgress->Update( 75 + (i*timeCalc), _("Loading data") );
        VE_XML::VE_Model::Model* model = dynamic_cast< VE_XML::VE_Model::Model* >( objectVector.at( i ) );
        
        wxClassInfo* cls = wxClassInfo::FindClass( wxString(model->GetModelName().c_str(),wxConvUTF8) );
        // If the class has not had a custom module been created
        REI_Plugin* tempPlugin = 0;
        if ( cls == 0 )
        {
            tempPlugin = new DefaultPlugin();
        }
        else
        {
            tempPlugin = dynamic_cast< REI_Plugin* >( cls->CreateObject() );
        }
        tempPlugin->SetNetworkFrame( this );
        PushEventHandler( tempPlugin );
        tempPlugin->SetName( wxString(model->GetModelName().c_str(),wxConvUTF8) );
        tempPlugin->SetCORBAService( VE_Conductor::CORBAServiceList::instance() );
        if ( model->GetIconFilename() != "DefaultPlugin" )
        {   
            tempPlugin->SetImageIcon( model->GetIconFilename(), 
                                      model->GetIconRotation(), 
                                      model->GetIconMirror(), 
                                      model->GetIconScale() );
        }
        
        Module temp_mod;
        unsigned int num = model->GetModelID();
        modules[ num ] = temp_mod;
        modules[ num ].SetPlugin( tempPlugin );
        modules[ num ].GetPlugin()->SetID( num );
        modules[ num ].SetClassName( model->GetModelName() );
        modules[ num ].GetPlugin()->SetVEModel( model );
        //Second, calculate the polyes
        wxRect bbox = modules[ num ].GetPlugin()->GetBBox();
        int polynum = modules[ num ].GetPlugin()->GetNumPoly();
        POLY tmpPoly;
        tmpPoly.resize( polynum );
        modules[ num ].GetPlugin()->GetPoly(tmpPoly);
        VE_Conductor::GUI_Utilities::Polygon tempPoly;
        *(tempPoly.GetPolygon()) = tmpPoly;
        tempPoly.TransPoly( bbox.x, bbox.y, *(modules[ num ].GetPolygon()) ); //Make the network recognize its polygon 
    }
    
    networkWriter.ReadXMLData( xmlNetwork, "XML", "User" );
    objectVector = networkWriter.GetLoadedXMLObjects();
    if ( !objectVector.empty() )
    {
        backgroundColor.clear();
        VE_XML::User* userColor = dynamic_cast< VE_XML::User* >( objectVector.at( 0 ) );
        //Set user preferences
        std::vector< VE_XML::Command* > tempStates = userColor->GetUserStateInfo()->GetStateVector();
        std::map< std::string, VE_XML::Command > tempMap;
        for ( size_t i = 0; i < tempStates.size(); ++i )
        {
            VE_XML::Command* tempCommand = tempStates.at( i );
            tempMap[ tempCommand->GetCommandName() ] = (*tempCommand); 
        }
        UserPreferencesDataBuffer::instance()->SetCommandMap( tempMap );
    }
    else
    {
        //Set the user preferences to nothing since this 
        //ves file does not have anything
        std::map< std::string, VE_XML::Command > tempMap;
        UserPreferencesDataBuffer::instance()->SetCommandMap( tempMap );
        
        backgroundColor.clear();
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 1.0f );
        
        VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair( );
        dataValuePair->SetData(std::string("Background Color"),backgroundColor);
        VE_XML::Command* veCommand = new VE_XML::Command();
        veCommand->SetCommandName(std::string("CHANGE_BACKGROUND_COLOR"));
        veCommand->AddDataValuePair(dataValuePair);
        UserPreferencesDataBuffer::instance()->SetCommand( std::string("CHANGE_BACKGROUND_COLOR"), *veCommand );
        delete veCommand;
    }
    // Create the command and data value pairs
    
    VE_XML::Command colorCommand = UserPreferencesDataBuffer::instance()->
        GetCommand( "CHANGE_BACKGROUND_COLOR" );
    
    VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( &colorCommand );
    */
    /*
     // do this for tags
     DOMNodeList* subElements = doc->getDocumentElement()->getElementsByTagName( xercesString("veTag") );
     unsigned int numTags = subElements->getLength();
     // now lets create a list of them
     for ( unsigned int i = 0; i < numCommands; ++i )
     {
         VE_Model::Tag* temp = new VE_Model::Tag( doc );
         temp->SetObjectFromXMLData( dynamic_cast< DOMElement* >( subElements->item(i) ) );
         veTagVector.push_back( temp );
         tags.push_back( TAG );
         tags.back().text = wxString( veTagVector.back()->GetTagText().c_str() );
         tags.back().cons[0].x = veTagVector.back()->GetTagPoint( 0 )->GetPoint().first;
         tags.back().cons[0].y = veTagVector.back()->GetTagPoint( 0 )->GetPoint().second;
         tags.back().cons[1].x = veTagVector.back()->GetTagPoint( 1 )->GetPoint().first;
         tags.back().cons[1].y = veTagVector.back()->GetTagPoint( 1 )->GetPoint().second;
         tags.back().box.x = veTagVector.back()->GetTagPoint( 2 )->GetPoint().first;
         tags.back().box.x = veTagVector.back()->GetTagPoint( 2 )->GetPoint().second;
         // Create the polygon for tags
         tags.back().CalcTagPoly();
     }
     */
     /*
    m_selMod = -1;
    m_selFrPort = -1; 
    m_selToPort = -1; 
    m_selLink = -1; 
    m_selLinkCon = -1; 
    m_selTag = -1; 
    m_selTagCon = -1; 
    xold = yold =0;
    _fileProgress->Update( 100, _("Done") );
    //while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR){ ; }
    Refresh();
}

std::string Network::Save( std::string fileName )
{
    // Here we wshould loop over all of the following
    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    //  Newtork
    if ( veNetwork )
        delete veNetwork;
    
    veNetwork = new VE_XML::VE_Model::Network();
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( veNetwork, "veNetwork" ) );
    
    veNetwork->GetDataValuePair( -1 )->SetData( "m_xUserScale", userScale.first );
    veNetwork->GetDataValuePair( -1 )->SetData( "m_yUserScale", userScale.second );
    veNetwork->GetDataValuePair( -1 )->SetData( "nPixX", static_cast< long int >( numPix.first ) );
    veNetwork->GetDataValuePair( -1 )->SetData( "nPixY", static_cast< long int >( numPix.second ) );
    veNetwork->GetDataValuePair( -1 )->SetData( "nUnitX", static_cast< long int >( numUnit.first ) );
    veNetwork->GetDataValuePair( -1 )->SetData( "nUnitY", static_cast< long int >( numUnit.second ) );
    
    for ( size_t i = 0; i < links.size(); ++i )
    {
        VE_XML::VE_Model::Link* xmlLink = veNetwork->GetLink( -1 );
        //xmlLink->GetFromPort()->SetData( modules[ links[i].GetFromModule() ].GetPlugin()->GetModelName(), links[i].GetFromPort() );
        //xmlLink->GetToPort()->SetData( modules[ links[i].GetToModule() ].pl_mod->GetModelName(), links[i].GetToPort() );
        xmlLink->GetFromModule()->SetData( modules[ links[i].GetFromModule() ].GetClassName(), static_cast< long int >( links[i].GetFromModule() ) );
        xmlLink->GetToModule()->SetData( modules[ links[i].GetToModule() ].GetClassName(), static_cast< long int >( links[i].GetToModule() ) );
        *(xmlLink->GetFromPort()) = static_cast< long int >( links[i].GetFromPort() );
        *(xmlLink->GetToPort()) = static_cast< long int >( links[i].GetToPort() );
        
        //Try to store link cons,
        //link cons are (x,y) wxpoint
        //here I store x in one vector and y in the other
        for ( size_t j = 0; j < links[ i ].GetNumberOfPoints(); ++j )
        {
            xmlLink->GetLinkPoint( j )->SetPoint( std::pair< unsigned int, unsigned int >( links[ i ].GetPoint( j )->x, links[ i ].GetPoint( j )->y ) );
        }
    }
    
    //  Models
    std::map< int, Module >::iterator iter;
    for ( iter=modules.begin(); iter!=modules.end(); ++iter )
    {
        iter->second.GetPlugin()->SetID( iter->first );
        nodes.push_back( 
                         std::pair< VE_XML::XMLObject*, std::string >( 
                                                                       iter->second.GetPlugin()->GetVEModel(), "veModel" ) 
                         );
        //dynamic_cast< VE_Model::Model* >( nodes.back().first )->SetModelName( modules[ iter->first ].GetClassName() );
    }
    */
    //  tags
    /*for ( size_t i = 0; i < veTagVector.size(); ++i )
    {
        delete veTagVector.at( i );
    }
    veTagVector.clear();
    
    for ( size_t i = 0; i < tags.size(); ++i )
    {
        std::pair< unsigned int, unsigned int > pointCoords;
        
        veTagVector.push_back( new VE_Model::Tag( doc ) );
        
        veTagVector.back()->SetTagText( tags.back().text.c_str() );
        
        pointCoords.first = tags.back().cons[0].x;
        pointCoords.second = tags.back().cons[0].y;
        veTagVector.back()->GetTagPoint( 0 )->SetPoint( pointCoords );
        
        pointCoords.first = tags.back().cons[1].x;
        pointCoords.second = tags.back().cons[1].y;
        veTagVector.back()->GetTagPoint( 1 )->SetPoint( pointCoords );
        
        pointCoords.first = tags.back().box.x;
        pointCoords.second = tags.back().box.y;
        veTagVector.back()->GetTagPoint( 2 )->SetPoint( pointCoords );
    }
    
    for ( size_t i = 0; i < tags.size(); ++i )
    {
        doc->getDocumentElement()->appendChild
        ( 
          veTagVector.at( i )->GetXMLData( "veTag" )
          );
    }*/
   /* 
    //Write out the veUser info for the local user
    VE_XML::User userInfo;
    userInfo.SetUserId( "jaredabo" );
    userInfo.SetControlStatus( VE_XML::User::VEControlStatus( "MASTER" ) );
    VE_XML::StateInfo* colorState = new VE_XML::StateInfo();
    ///Load the current preferences from the data buffer
    std::map< std::string, VE_XML::Command > tempMap = UserPreferencesDataBuffer::instance()->GetCommandMap();
    std::map< std::string, VE_XML::Command >::iterator prefIter;
    for ( prefIter = tempMap.begin(); prefIter != tempMap.end(); ++prefIter )
    {
        colorState->AddState( new VE_XML::Command( prefIter->second ) );
    }
    userInfo.SetStateInfo( colorState );
    
    nodes.push_back( 
                     std::pair< VE_XML::XMLObject*, std::string >( 
                                                                   &userInfo, "User" ) 
                     );
    
    VE_XML::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
    
    return fileName;
}
////////////////////////////////////////////////////////
void Network::New( bool promptClearXplorer )
{
    // Just clear the design canvas
    while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
    
    int answer = wxID_NO;
    if ( !promptClearXplorer )
    {
        wxMessageDialog promptDlg( this, 
                                   _("Do you want to reset Xplorer?"), 
                                   _("Reset Xplorer Warning"), 
                                   wxYES_NO|wxNO_DEFAULT|wxICON_QUESTION, 
                                   wxDefaultPosition);
        answer = promptDlg.ShowModal();
    }
    
    if ( ( answer == wxID_OK ) || ( promptClearXplorer ) )
    {
        std::map<int, Module>::iterator iter;
        for ( iter=modules.begin(); iter!=modules.end(); ++iter )
        {
            VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair(  std::string("UNSIGNED INT") );
            dataValuePair->SetDataName( "Object ID" );
            dataValuePair->SetDataValue( static_cast< unsigned int >( iter->first ) );
            VE_XML::Command* veCommand = new VE_XML::Command();
            veCommand->SetCommandName( std::string("DELETE_OBJECT_FROM_NETWORK") );
            veCommand->AddDataValuePair( dataValuePair );
            bool connected = VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
            //Clean up memory
            delete veCommand;
        }
    }
    
    links.clear();
    
    std::map< int, Module >::iterator iter;
    for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
        PopEventHandler( false );
    }
    modules.clear();
    
    tags.clear();
    ///Reset the canvas available spaces
    sbboxes.clear();
    
    while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
    
    Refresh();
    
    //reset the state of dataset check boxes on a new network load
    if(vistab)
    {
        vistab->ResetAllDatasetDependentCheckBoxes();
    }
    
    if(cadDialog)
    {
        cadDialog->ClearLoadedCADFiles();
    }
}
////////////////////////////////////////////////////////
*/
