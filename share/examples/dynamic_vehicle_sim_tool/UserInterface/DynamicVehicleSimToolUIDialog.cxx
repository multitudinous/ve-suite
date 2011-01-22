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

// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "DynamicVehicleSimToolUIDialog.h"
#include "CADListCreator.h"

// --- VE-Suite Includes --- //
#include <ves/conductor/util/spinctld.h>
#include <ves/conductor/UIPluginBase.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/OneDStringArray.h>

#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>

#include <ves/open/xml/model/Model.h>

// --- wxWidgets Includes --- //
#include <wx/statline.h>
#include <wx/sizer.h>
#include <wx/radiobox.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/statbox.h>
#include <wx/frame.h>
#include <wx/textctrl.h>
#include <wx/notebook.h>
#include <wx/combobox.h>

#include <wx/filename.h>
#include <wx/choicdlg.h>

#include <wx/filedlg.h>
#include <wx/textdlg.h>

#include <fstream>

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>

#include <boost/lexical_cast.hpp>

#include <Poco/SharedPtr.h>
#include <Poco/Tuple.h>
#include <Poco/Data/SessionFactory.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/SQLite/Connector.h>

using namespace Poco::Data;


using namespace dynamicvehicletool;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimToolUIDialog::DynamicVehicleSimToolUIDialog()
    :
    dvst::DynamicVehicleSimToolBase( 0 ),
    mServiceList( 0 )
{
    m_computerTextCtrl->SetValue( wxString( "225.0.0.37", wxConvUTF8 ) );
}
////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimToolUIDialog::DynamicVehicleSimToolUIDialog( 
    wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
    :
    dvst::DynamicVehicleSimToolBase( parent, id ),
    mServiceList( service )
{
    SetSizeHints( wxSize( 500, -1 ), wxDefaultSize );

    CenterOnParent();
    //SetTitle( _("Deere Analytics") );
    m_computerTextCtrl->SetValue( wxString( "225.0.0.37", wxConvUTF8 ) );
}
////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimToolUIDialog::~DynamicVehicleSimToolUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool DynamicVehicleSimToolUIDialog::TransferDataToWindow()
{
    PopulateDialogs();
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool DynamicVehicleSimToolUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnComputerNameEnter( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::DataValuePairPtr computerNameText( new ves::open::xml::DataValuePair() );
    computerNameText->SetData( "ComputerName", ConvertUnicode( m_computerTextCtrl->GetValue().c_str() ) );
    ves::open::xml::DataValuePairPtr computerPortText( new ves::open::xml::DataValuePair() );
    computerPortText->SetData( "ComputerPort", ConvertUnicode( m_portTextCtrl->GetValue().c_str() ) );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( computerNameText );
    command->AddDataValuePair( computerPortText );
    const std::string commandName = "Computer Info Update";
    command->SetCommandName( commandName );
    mServiceList->SendCommandStringToXplorer( command );
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnPortNumberEnter( wxCommandEvent& event )
{
    OnComputerNameEnter( event );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnStartStopButton( wxCommandEvent& event )
{
    bool state = m_toggleBtn1->GetValue();
    std::string simState = "Stop";
    if( state )
    {
        OnComputerNameEnter( event );
        simState = "Start";
    }
    ves::open::xml::DataValuePairPtr simText( new ves::open::xml::DataValuePair() );
    simText->SetData( "Simulator State", simState );
    
    double scaleValue = 1.0;
    //wxT("m -> ft"), wxT("cm -> ft"), wxT("mm -> ft"), wxT("in -> ft")
    int choice = m_simScale->GetSelection();
    if( choice == 0 )
    {
        scaleValue = 3.2808399;
    }
    else if( choice == 1 )
    {
        scaleValue = 0.032808399;
    }
    else if( choice == 2 )
    {
        scaleValue = 0.0032808399;
    }
    else if( choice == 3 )
    {
        scaleValue = 0.0833333;
    }

    ves::open::xml::DataValuePairPtr simScale( new ves::open::xml::DataValuePair() );
    simScale->SetData( "Simulator Scale", scaleValue );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( simText );
    command->AddDataValuePair( simScale );
    const std::string commandName = "Simulator Update";
    command->SetCommandName( commandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnResetSimulation( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::DataValuePairPtr simText( new ves::open::xml::DataValuePair() );
    simText->SetData( "Simulator State", "Reset" );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( simText );
    const std::string commandName = "Simulator Update";
    command->SetCommandName( commandName );
    mServiceList->SendCommandStringToXplorer( command );

    //Reset it back to start
    bool state = m_toggleBtn1->GetValue();
    std::string simState = "Stop";
    if( state )
    {
        simState = "Start";
    }    
    simText->SetData( "Simulator State", simState );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnGeometryDataMapping( wxCommandEvent& WXUNUSED( event ) )
{
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnAddGeometryGroupButton( wxCommandEvent& WXUNUSED( event ) )
{
    size_t newNum = m_geomChoiceList.size();
	wxBoxSizer* bSizer;
	bSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* staticText = new wxStaticText( m_scrolledWindow1, wxID_ANY, wxString::Format( _("%d"), newNum), wxDefaultPosition, wxDefaultSize, 0 );
	staticText->Wrap( -1 );
	bSizer->Add( staticText, 0, wxALIGN_CENTER, 5 );
	
    ves::open::xml::cad::CADNodePtr rootNode = mUIPluginBase->GetVEModel()->GetGeometry();
    dynamicvehicletool::CADListCreator nodeListCreator( rootNode );
    m_nodeList = nodeListCreator.GetNodeList();
    std::vector< std::string > nodeListNames = 
        nodeListCreator.GetNodeNameList();
	wxArrayString m_choice11Choices;
    if( nodeListNames.size() != m_nodeList.size() )
    {
        std::cout << " something is wrong with name generation." << std::endl;
    }

    for( size_t i = 0; i < nodeListNames.size(); ++i )
    {
        //m_choice11Choices.Add( wxString( nodeList.at( i )->GetNodeName().c_str(), wxConvUTF8 ) );
        m_choice11Choices.Add( wxString( nodeListNames.at( i ).c_str(), wxConvUTF8 ) );
    }
	wxChoice* choice = new wxChoice( m_scrolledWindow1, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_choice11Choices );
	choice->SetSelection( 0 );
	bSizer->Add( choice, 0, wxALIGN_CENTER, 5 );
	
	m_scrolledWindowSizer->Add( bSizer, 0, 0, 5 );
    m_scrolledWindowSizer->FitInside( m_scrolledWindow1 );
    m_geomChoiceList.push_back( choice );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnRemoveGeometryGroupButton( wxCommandEvent& WXUNUSED( event ) )
{
    wxSizerItemList& list = m_scrolledWindowSizer->GetChildren();
    size_t num = list.size();
    if( num > 0 )
    {
        ///Remove the last item;
        list.back()->DeleteWindows();
        m_scrolledWindowSizer->Remove( num - 1 );
        //m_geomChoiceList.back()->Destroy();
        m_geomChoiceList.pop_back();
    }

    m_scrolledWindowSizer->FitInside( m_scrolledWindow1 );
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnConstrainedGeometrySelection( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::DataValuePairPtr constrainedText( new ves::open::xml::DataValuePair() );
    if( m_constrainedGeomChoice->GetSelection() == 0 )
    {
        constrainedText->SetData( "Constrained Geometry", "None" );
    }
    else
    {
        ves::open::xml::cad::CADNodePtr tempCADNode = m_nodeList.at( m_constrainedGeomChoice->GetSelection() - 1 );
        //geomDVP->SetData( tempCADNode->GetNodeName(), tempCADNode->GetID() );
        constrainedText->SetData( "Constrained Geometry", tempCADNode->GetID() );
        //constrainedText->SetData( "Contrainted Geometry", ConvertUnicode( m_choice3->GetStringSelection().c_str() ) );
    }
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( constrainedText );
    const std::string commandName = "Geometry Map Update";
    command->SetCommandName( commandName );
    mServiceList->SendCommandStringToXplorer( command );
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnApplyButton( wxCommandEvent& event )
{
    OnConstrainedGeometrySelection( event );
    
    ves::open::xml::CommandPtr geomCommand( new ves::open::xml::Command() );
    geomCommand->SetCommandName( "Geometry Data Map" );
    if( m_nodeList.size() > 0 )
    {
        for( size_t i = 0; i < m_geomChoiceList.size(); ++i )
        {
            //std::string dvpName = "Geometry_" + boost::lexical_cast<std::string>( i );

            ves::open::xml::DataValuePairPtr geomDVP( new ves::open::xml::DataValuePair() );
            //std::string nodeName = ConvertUnicode( m_geomChoiceList.at( i )->GetStringSelection().c_str() );
            //nodeIter = std::find( 
            ves::open::xml::cad::CADNodePtr tempCADNode = 
                m_nodeList.at( m_geomChoiceList.at( i )->GetSelection() );
            geomDVP->SetData( tempCADNode->GetNodeName(), tempCADNode->GetID() );
            geomCommand->AddDataValuePair( geomDVP );
        }
    }
    if( (m_geomChoiceList.size() == 0) || (m_nodeList.size() == 0) )
    {
        ves::open::xml::DataValuePairPtr geomDVP( new ves::open::xml::DataValuePair() );
        geomDVP->SetData( "No Geometry Selected", "No Geom" );
        geomCommand->AddDataValuePair( geomDVP );
    }
    mServiceList->SendCommandStringToXplorer( geomCommand );
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnOKButton( wxCommandEvent& WXUNUSED( event ) )
{
    //OnApplyButton( event );
    //Do not do anything and close the dialog
    Close();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::UpdateModelData()
{
    ves::open::xml::model::ModelPtr tempModel = mUIPluginBase->GetVEModel();

    ves::open::xml::CommandPtr toolCommand( new ves::open::xml::Command() );
    toolCommand->SetCommandName( "Tool Info" );

    {
        ves::open::xml::DataValuePairPtr constrainedText( new ves::open::xml::DataValuePair() );
        if( m_constrainedGeomChoice->GetSelection() == 0 )
        {
            constrainedText->SetData( "Constrained Geometry", "None" );
        }
        else
        {
            ves::open::xml::cad::CADNodePtr tempCADNode = m_nodeList.at( m_constrainedGeomChoice->GetSelection() - 1 );
            //geomDVP->SetData( tempCADNode->GetNodeName(), tempCADNode->GetID() );
            constrainedText->SetData( "Constrained Geometry", tempCADNode->GetID() );
            //constrainedText->SetData( "Contrainted Geometry", ConvertUnicode( m_choice3->GetStringSelection().c_str() ) );
        }
        
        //ves::open::xml::cad::CADNodePtr tempCADNode = 
        //        m_nodeList.at( m_constrainedGeomChoice->GetSelection() );
        //geomDVP->SetData( tempCADNode->GetNodeName(), tempCADNode->GetID() );
        //constrainedText->SetData( "Constrained Geometry", tempCADNode->GetID() );
        toolCommand->AddDataValuePair( constrainedText );
    }
    
    ves::open::xml::DataValuePairPtr computerNameText( new ves::open::xml::DataValuePair() );
    computerNameText->SetData( "ComputerName", ConvertUnicode( m_computerTextCtrl->GetValue().c_str() ) );
    toolCommand->AddDataValuePair( computerNameText );

    ves::open::xml::DataValuePairPtr computerPortText( new ves::open::xml::DataValuePair() );
    computerPortText->SetData( "ComputerPort", ConvertUnicode( m_portTextCtrl->GetValue().c_str() ) );
    toolCommand->AddDataValuePair( computerPortText );
    //tempModel->SetInput( toolCommand );

    double scaleValue = 1.0;
    //wxT("m -> ft"), wxT("cm -> ft"), wxT("mm -> ft"), wxT("in -> ft")
    int choice = m_simScale->GetSelection();
    if( choice == 0 )
    {
        scaleValue = 3.2808399;
    }
    else if( choice == 1 )
    {
        scaleValue = 0.032808399;
    }
    else if( choice == 2 )
    {
        scaleValue = 0.0032808399;
    }
    else if( choice == 3 )
    {
        scaleValue = 0.0833333;
    }

    ves::open::xml::DataValuePairPtr simScale( new ves::open::xml::DataValuePair() );
    simScale->SetData( "Simulator Scale", scaleValue );
    toolCommand->AddDataValuePair( simScale );
    tempModel->SetInput( toolCommand );

    ves::open::xml::CommandPtr regCommand( new ves::open::xml::Command() );
    regCommand->SetCommandName( "DVST Registration Update" );
    double sipVal = 0.0;
    ves::open::xml::DataValuePairPtr sipValX( new ves::open::xml::DataValuePair() );
    m_sipLocX->GetValue().ToDouble( &sipVal );
    sipValX->SetData( "SIP X", sipVal );
    regCommand->AddDataValuePair( sipValX );

    ves::open::xml::DataValuePairPtr sipValY( new ves::open::xml::DataValuePair() );
    m_sipLocY->GetValue().ToDouble( &sipVal );
    sipValY->SetData( "SIP Y", sipVal );
    regCommand->AddDataValuePair( sipValY );

    ves::open::xml::DataValuePairPtr sipValZ( new ves::open::xml::DataValuePair() );
    m_sipLocZ->GetValue().ToDouble( &sipVal );
    sipValZ->SetData( "SIP Z", sipVal );
    regCommand->AddDataValuePair( sipValZ );
    tempModel->SetInput( regCommand );
    //mServiceList->SendCommandStringToXplorer( toolCommand );

    /*
    ves::open::xml::cad::CADNodePtr rootNode = mUIPluginBase->GetVEModel()->GetGeometry();
    dynamicvehicletool::CADListCreator nodeListCreator( rootNode );
    std::vector< ves::open::xml::cad::CADNodePtr > nodeList = 
        nodeListCreator.GetNodeList();
    */
    //std::vector< ves::open::xml::cad::CADNodePtr >::iterator nodeIter;
    
    ves::open::xml::CommandPtr geomCommand( new ves::open::xml::Command() );
    geomCommand->SetCommandName( "Geometry Data Map" );
    if( m_nodeList.size() > 0 )
    {
        for( size_t i = 0; i < m_geomChoiceList.size(); ++i )
        {
            //std::string dvpName = "Geometry_" + boost::lexical_cast<std::string>( i );

            ves::open::xml::DataValuePairPtr geomDVP( new ves::open::xml::DataValuePair() );
            //std::string nodeName = ConvertUnicode( m_geomChoiceList.at( i )->GetStringSelection().c_str() );
            //nodeIter = std::find( 
            ves::open::xml::cad::CADNodePtr tempCADNode = 
                m_nodeList.at( m_geomChoiceList.at( i )->GetSelection() );
            geomDVP->SetData( tempCADNode->GetNodeName(), tempCADNode->GetID() );
            geomCommand->AddDataValuePair( geomDVP );
        }
    }
    if( (m_geomChoiceList.size() == 0) || (m_nodeList.size() == 0) )
    {
        ves::open::xml::DataValuePairPtr geomDVP( new ves::open::xml::DataValuePair() );
        geomDVP->SetData( "No Geometry Selected", "No Geom" );
        geomCommand->AddDataValuePair( geomDVP );
    }
    tempModel->SetInput( geomCommand );
    //mServiceList->SendCommandStringToXplorer( geomCommand );
 }
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::PopulateDialogs()
{
    ves::open::xml::model::ModelPtr tempModel = mUIPluginBase->GetVEModel();
    
    ves::open::xml::CommandPtr toolCommand = tempModel->GetInput( "Tool Info" );
    std::string constrainedGeom;
    if( toolCommand )
    {
        ves::open::xml::DataValuePairPtr geomDVP = toolCommand->GetDataValuePair( "Constrained Geometry" );
        if( geomDVP )
        {
            geomDVP->GetData( constrainedGeom );
        }
    }
    
    ves::open::xml::cad::CADNodePtr rootNode = tempModel->GetGeometry();
    //if( rootNode )
    //{
    dynamicvehicletool::CADListCreator nodeListCreator( rootNode );
    //std::vector< ves::open::xml::cad::CADNodePtr > nodeList = 
    //    nodeListCreator.GetNodeList();
    m_nodeList = nodeListCreator.GetNodeList();
    std::vector< std::string > nodeListNames = 
        nodeListCreator.GetNodeNameList();
    
	wxArrayString m_choice11Choices;
    m_choice11Choices.Add( wxString( "None", wxConvUTF8 ) );

    for( size_t i = 0; i < nodeListNames.size(); ++i )
    {
        m_choice11Choices.Add( wxString( nodeListNames.at( i ).c_str(), wxConvUTF8 ) );
    }

    //need to clear choice 3
    m_constrainedGeomChoice->Clear();
    
	m_constrainedGeomChoice->Append( m_choice11Choices );
    m_constrainedGeomChoice->SetSelection( 0 );
    //m_choice3->SetStringSelection( wxString( constrainedGeom.c_str(), wxConvUTF8 ) );
    size_t nodeIndex1 = 0;
    std::string selectedNode( "None" );
    for( size_t j = 0; j < m_nodeList.size(); ++j )
    {
        std::string nodeID = m_nodeList.at( j )->GetID();
        if( nodeID == constrainedGeom )
        {
            nodeIndex1 = j;
            selectedNode = nodeListNames.at( nodeIndex1 );
            break;
        }
    }
    m_constrainedGeomChoice->SetStringSelection( wxString( selectedNode.c_str(), wxConvUTF8 ) );
    
    //Setup computer info
    std::string computerName;
    if( toolCommand )
    {
        toolCommand->GetDataValuePair( "ComputerName" )->GetData( computerName );
    }
    if( computerName.empty() )
    {
        computerName = "225.0.0.37";
    }
    m_computerTextCtrl->ChangeValue( wxString( computerName.c_str(), wxConvUTF8 ) );

    std::string computerPort;
    if( toolCommand )
    {
        toolCommand->GetDataValuePair( "ComputerPort" )->GetData( computerPort );
    }
    if( computerPort.empty() )
    {
        computerPort = "12345";
    }
    m_portTextCtrl->ChangeValue( wxString( computerPort.c_str(), wxConvUTF8 ) );

    double scaleValue = 1.0;
    if( toolCommand )
    {
        ves::open::xml::DataValuePairPtr simScale = toolCommand->GetDataValuePair( "Simulator Scale" );
        if( simScale )
        {
            simScale->GetData( scaleValue );
        }
    }

    //wxT("m -> ft"), wxT("cm -> ft"), wxT("mm -> ft"), wxT("in -> ft")
    int choice = 1;
    if( scaleValue == 3.2808399 )
    {
        choice = 0;
    }
    else if( scaleValue == 0.032808399 )
    {
        choice = 1;
    }
    else if( scaleValue == 0.0032808399 )
    {
        choice = 2;
    }
    else if( scaleValue == 0.0833333 )
    {
        choice = 3;
    }
    m_simScale->SetSelection( choice );

    //Initialize the registration data
    //Get bird info from VR Juggler
    toolCommand = tempModel->GetInput( "DVST Registration Update" );
    if( toolCommand )
    {
        double sipVal = 0.0;
        ves::open::xml::DataValuePairPtr sipValDVP = toolCommand->GetDataValuePair( "SIP X" );
        sipValDVP->GetData( sipVal );
        m_sipLocX->SetValue( wxString::Format( "%d", sipVal ) );

        sipValDVP = toolCommand->GetDataValuePair( "SIP Y" );
        sipValDVP->GetData( sipVal );
        m_sipLocY->SetValue( wxString::Format( "%d", sipVal ) );
        
        sipValDVP = toolCommand->GetDataValuePair( "SIP Z" );
        sipValDVP->GetData( sipVal );
        m_sipLocZ->SetValue( wxString::Format( "%d", sipVal ) );
    }

    toolCommand = tempModel->GetInput( "Geometry Data Map" );
    if( !toolCommand )
    {
        return;
    }

    ///Need to clear any array choice selections
    if( m_geomChoiceList.size() > 0 )
    {
        //we already have the choices loaded up
        return;
    }

    size_t numDVPs = toolCommand->GetNumberOfDataValuePairs();
    wxCommandEvent event;
    std::string nodeName;
    for( size_t i = 0; i < numDVPs; ++i )
    {
        ves::open::xml::DataValuePairPtr geomDVP = 
            toolCommand->GetDataValuePair( i );
        geomDVP->GetData( nodeName );
        if( nodeName == "No Geom" )
        {
            break;
        }
        
        OnAddGeometryGroupButton( event );
        size_t nodeIndex = 0;
        for( size_t j = 0; j < m_nodeList.size(); ++j )
        {
            std::string nodeID = m_nodeList.at( j )->GetID();
            if( nodeID == nodeName )
            {
                nodeIndex = j;
                break;
            }
        }
        m_geomChoiceList.at( i )->SetStringSelection( wxString( nodeListNames.at( nodeIndex ).c_str(), wxConvUTF8 ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnRegisterButton( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 

    ves::open::xml::DataValuePairPtr simText( new ves::open::xml::DataValuePair() );
    simText->SetData( "Mode", ConvertUnicode( m_registrationChoice->GetStringSelection().c_str() ) );
    if( m_registrationChoice->GetSelection() == 1 )
    {
        ves::open::xml::DataValuePairPtr fileText( new ves::open::xml::DataValuePair() );
        fileText->SetData( "Filename", m_fileName );
        command->AddDataValuePair( fileText );
    }
    command->AddDataValuePair( simText );

    double sipVal = 0.0;
    ves::open::xml::DataValuePairPtr sipValX( new ves::open::xml::DataValuePair() );
    m_sipLocX->GetValue().ToDouble( &sipVal );
    sipValX->SetData( "SIP X", sipVal );
    command->AddDataValuePair( sipValX );

    ves::open::xml::DataValuePairPtr sipValY( new ves::open::xml::DataValuePair() );
    m_sipLocY->GetValue().ToDouble( &sipVal );
    sipValY->SetData( "SIP Y", sipVal );
    command->AddDataValuePair( sipValY );

    ves::open::xml::DataValuePairPtr sipValZ( new ves::open::xml::DataValuePair() );
    m_sipLocZ->GetValue().ToDouble( &sipVal );
    sipValZ->SetData( "SIP Z", sipVal );
    command->AddDataValuePair( sipValZ );

    const std::string commandName = "DVST Registration Update";
    command->SetCommandName( commandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnRegistrationFileChoice( wxCommandEvent& WXUNUSED( event ) ) 
{ 
    if( m_registrationChoice->GetSelection() == 1 )
    {
        wxFileDialog dialog
        (
         this,
         _T( "Select File" ),
         ::wxGetCwd(),
         wxT( "" ),
         _T( "Files (*.*)|*.*" ),
         wxFD_OPEN | wxFD_FILE_MUST_EXIST
         );
        
        if( dialog.ShowModal() != wxID_OK )
        {
            return;
        }
        
        //wxFileName vesFileName( dialog.GetPath() );
        m_fileName = ConvertUnicode( dialog.GetPath().c_str() );
    }
}
////////////////////////////////////////////////////////////////////////////////
