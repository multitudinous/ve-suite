/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/conductor/ViewLocPane.h>

#include <ves/conductor/ConductorLibEnums.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <ves/util/icons/ve_icon32x32.xpm>

#include <iostream>
#include <string>
#include <sstream>

using namespace ves::open::xml;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( ViewLocPane, wxDialog )
    EVT_BUTTON( VIEWLOCPANE_LOAD_BUTTON, ViewLocPane::_onLoad )
    EVT_BUTTON( VIEWLOCPANE_REMOVE_VIEW_PT_BUTTON, ViewLocPane::_onRemoveVP )
    EVT_BUTTON( VIEWLOCPANE_ACCEPTNEWVPNAME_BUTTON, ViewLocPane::_onAcceptNewVPName )
    EVT_BUTTON( VIEWLOCPANE_CANCELNEWVPNAME_BUTTON, ViewLocPane::_onCancelNewVPName )
    EVT_COMBOBOX( VIEWLOCPANE_MOVETOVP_COMBOBOX, ViewLocPane::_onMoveToVP )
    EVT_BUTTON( VIEWLOCPANE_NEWFLY_BUTTON, ViewLocPane::_onBuildNewFlyButton )
    EVT_BUTTON( VIEWLOCPANE_ACCEPTNEWFLYNAME_BUTTON, ViewLocPane::_onAcceptNewFlyName )
    EVT_BUTTON( VIEWLOCPANE_CANCELNEWFLYNAME_BUTTON, ViewLocPane::_onCancelNewFlyName )
    EVT_COMBOBOX( VIEWLOCPANE_ACTIVEFLYSEL_COMBOBOX, ViewLocPane::_onActiveFlySel )
    EVT_COMBOBOX( VIEWLOCPANE_ADDVPTOFLYSEL_COMBOBOX, ViewLocPane::_onAddVPtoFlySel )
    EVT_COMBOBOX( VIEWLOCPANE_INSERTVPINFLYSEL_COMBOBOX, ViewLocPane::_onInsertVPinFlySel )
    EVT_COMBOBOX( VIEWLOCPANE_REMOVEVPFROMFLYSEL_COMBOBOX, ViewLocPane::_onRemoveVPfromFlySel )
    EVT_COMBOBOX( VIEWLOCPANE_DELETEFLYSEL_COMBOBOX, ViewLocPane::_onDeleteFlySel )
    EVT_BUTTON( VIEWLOCPANE_RUNFLY_BUTTON, ViewLocPane::_onStartActiveFly )
    EVT_BUTTON( VIEWLOCPANE_LOAD_FILE, ViewLocPane::_onLoadStoredPointsFile )
    EVT_BUTTON( VIEWLOCPANE_SAVE_FILE, ViewLocPane::_onSaveStoredPointsFile )
    EVT_BUTTON( VIEWLOCPANE_STOPFLY_BUTTON, ViewLocPane::_onStopFly )
    EVT_LISTBOX( VIEWLOCPANE_FLYBUILDER_LISTBOX, ViewLocPane::_onFlyBuilderListBox )
    EVT_SPINCTRL( VIEWLOCPANE_SPEED_CONTROL_SPIN, ViewLocPane::_onSpeedChange )
    EVT_IDLE( ViewLocPane::_refreshGUIFromXplorerData )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ViewLocPane::ViewLocPane( wxWindow* parent )
        : wxDialog( parent, -1, _( "Viewpoints Pane" ),
                    wxDefaultPosition, wxDefaultSize,
                    ( wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX | wxMINIMIZE_BOX ) & ~ wxSTAY_ON_TOP )
{
    _numStoredLocations = 0;
    _numStoredFlythroughs = 0;
    _vwptsInActiveFly = 0;
    _numViewLocLocal = 0;
    _vwptsInActiveFlyLocal = 0;
    _numStoredFlythroughsLocal = 0;
    _commandName = "";
    _numStoredLocations = 0;
    _numStoredFlythroughs = 0;
    _vwptsInActiveFly = 0;
    _locationName = 0;
    _flythroughName = 0;
    _activeFlyNames = 0;
    _numView_LocsGlobal = 0;
    _locNamesLocal = 0;
    _activeFlyNamesLocal = 0;
    _flythroughNamesLocal = 0;
    _numView_LocsGlobal = 0;
    _vwptsInActiveFlyLocal = 0;
    _removevwptSel = 0;
    _movetovwptSel = 0;

    flyThroughList.clear();
    _buildPage();

    GetSizer()->Fit( this );
    GetSizer()->SetSizeHints( this );
    Centre();
    
    SetIcon( ve_icon32x32_xpm );
}
////////////////////////////////////////////////////////////////////////////////
ViewLocPane::~ViewLocPane()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onLoadStoredPointsFile( wxCommandEvent& event )
{
    ///this is Waaaaaaaaaaaaaaay hacked... --biv
    wxFileDialog dialog( this,
                         _T( "Open File" ),
                         ::wxGetCwd(),
                         _T( "" ),
                         _T( "View Location files (*.vel;*.dat)|*.vel;*.dat;" ),
                         wxOPEN | wxFILE_MUST_EXIST | wxFD_PREVIEW,
                         wxDefaultPosition );
    dialog.CentreOnParent();
    
    if( dialog.ShowModal() == wxID_OK )
    {
        wxFileName viewPtsFilename( dialog.GetPath() );
        viewPtsFilename.MakeRelativeTo( ::wxGetCwd(), wxPATH_NATIVE );
        wxString relativeViewLocationsPath( wxString( "./", wxConvUTF8 ) + viewPtsFilename.GetFullPath() );

        DataValuePairPtr velFileName( new DataValuePair() );
        velFileName->SetData( "View Locations file", ConvertUnicode( relativeViewLocationsPath.c_str() ) );
        _dataValuePairList.push_back( velFileName );

        _commandName = "QC_LOAD_STORED_POINTS";
        SendCommandsToXplorer();
    }
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onSaveStoredPointsFile( wxCommandEvent& event )
{
    wxFileName velFileName;
    do
    {
        wxTextEntryDialog viewLocationsFileDlg( this,
                                                _( "Enter the prefix for *.vel filename:" ),
                                                _( "Save VEL file as..." ),
                                                _( "locations" ), wxOK | wxCANCEL );

        if( viewLocationsFileDlg.ShowModal() == wxID_OK )
        {
            velFileName.ClearExt();
            velFileName.SetName( viewLocationsFileDlg.GetValue() );
            velFileName.SetExt( _( "vel" ) );
        }
        else
        {
            break;
        }
    }
    while( velFileName.FileExists() );

    if( velFileName.HasName() )
    {
        _dataValuePairList.clear();
        DataValuePairPtr velFile( new DataValuePair() );
        velFile->SetData( "View Points file", ConvertUnicode( velFileName.GetFullPath( wxPATH_NATIVE ).c_str() ) );
        _dataValuePairList.push_back( velFile );

        _commandName = "VL_SAVE_STORED_POINTS";
        SendCommandsToXplorer();
    }
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_buildPage()
{
//*******Setting up the widgets for making and naming a new view point
    wxBoxSizer* mainSizer = new wxBoxSizer( wxVERTICAL );

    wxString choices[] = { _( "Choose a View Point" )};
    //scrollWindow = new wxScrolledWindow( this, -1, wxDefaultPosition, wxDefaultSize, wxHSCROLL | wxVSCROLL);
    int nUnitX = 20;
    int nUnitY = 10;
    int nPixX = 5;
    int nPixY = 10;
    //scrollWindow->SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );

    wxStaticBox* _ViewPointsControls = new wxStaticBox( this, -1, _( "Viewpoint Controls" ), wxDefaultPosition, wxDefaultSize, wxCAPTION );
    wxStaticBoxSizer* _allVPCtrlsGroup = new wxStaticBoxSizer( _ViewPointsControls, wxVERTICAL );


    wxStaticText* _loadLabel = new wxStaticText( this, -1, wxT( "Load Viewpoints from File" ) );
    wxButton* _loadViewLocationButton = new wxButton( this, VIEWLOCPANE_LOAD_FILE, _( "Load" ) );


    wxStaticText* _addvwptLabel = new wxStaticText( this, -1, wxT( "Add Current Location as Viewpoint" ) );
    wxButton* _addnewviewptButton = new wxButton( this, VIEWLOCPANE_LOAD_BUTTON, wxT( "Add Location" ) );

    wxStaticText* _removevwptLabel = new wxStaticText( this, -1, wxT( "Select Viewpoint(s) for Removal" ) );
    wxButton* _removeViewPointButton = new wxButton( this, VIEWLOCPANE_REMOVE_VIEW_PT_BUTTON, wxT( "Delete Location(s)" ) );

    wxBoxSizer* _viewpointLoadControlsSizer = new wxBoxSizer( wxVERTICAL );
    _viewpointLoadControlsSizer->Add( _loadLabel, 0, wxALIGN_LEFT );
    _viewpointLoadControlsSizer->Add( _loadViewLocationButton, 0, wxALIGN_CENTER_HORIZONTAL | wxBOTTOM | wxTOP, 10 );

    wxBoxSizer* _viewpointAddControlsSizer = new wxBoxSizer( wxVERTICAL );
    _viewpointAddControlsSizer->Add( _addvwptLabel, 0, wxALIGN_LEFT );//2
    _viewpointAddControlsSizer->Add( _addnewviewptButton, 0, wxALIGN_CENTER_HORIZONTAL | wxBOTTOM | wxTOP, 10 );

    wxBoxSizer* _viewpointRemoveControlsSizer = new wxBoxSizer( wxVERTICAL );
    _viewpointRemoveControlsSizer->Add( _removevwptLabel, 0, wxALIGN_LEFT );
    _viewpointRemoveControlsSizer->Add( _removeViewPointButton, 0, wxALIGN_CENTER_HORIZONTAL | wxBOTTOM | wxTOP, 10 );


    _allVPCtrlsGroup->Add( _viewpointLoadControlsSizer, 1, wxALIGN_CENTER_HORIZONTAL | wxEXPAND );
    _allVPCtrlsGroup->Add( _viewpointAddControlsSizer, 1, wxALIGN_CENTER_HORIZONTAL | wxEXPAND );
    _allVPCtrlsGroup->Add( _viewpointRemoveControlsSizer, 1, wxALIGN_CENTER_HORIZONTAL | wxEXPAND );

    wxStaticBox* _FlythroughControls = new wxStaticBox( this, -1, _( "Flythrough Control" ),
                                                        wxDefaultPosition, wxDefaultSize, wxCAPTION );
    wxStaticBoxSizer* _FlythroughGroup = new wxStaticBoxSizer( _FlythroughControls, wxVERTICAL );

    wxBoxSizer* _moveToViewpointSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* _movetovwptLabel = new wxStaticText( this, -1, wxT( "Move to a Viewpoint " ) );
    _movetovwptSel = new wxComboBox( this, VIEWLOCPANE_MOVETOVP_COMBOBOX, wxT( "Select a Viewpoint" ), wxDefaultPosition,
                                     wxDefaultSize, 1,
                                     choices, wxCB_READONLY );

    _moveToViewpointSizer->Add( _movetovwptSel, 0, wxALIGN_CENTER );


    wxBoxSizer* _speedGroup = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* _speedButtonsSizer = new wxBoxSizer( wxVERTICAL );

    wxButton* _startFlythrough = new wxButton( this, VIEWLOCPANE_RUNFLY_BUTTON, wxT( "Start" ) );
    wxButton* _stopFlythrough = new wxButton( this, VIEWLOCPANE_STOPFLY_BUTTON, wxT( "Stop" ) );


    _speedButtonsSizer->Add( _startFlythrough, 1, wxALIGN_LEFT | wxBOTTOM, 2 );
    _speedButtonsSizer->Add( _stopFlythrough, 1, wxALIGN_LEFT | wxTOP, 2 );

    wxBoxSizer* _spinControlsSizer = new wxBoxSizer( wxVERTICAL );
    wxStaticText* _spinLabel = new wxStaticText( this, -1, wxT( "Speed (ft/s)" ) );


    _spinSpeedControls = new wxSpinCtrlDbl();
    _spinSpeedControls->Create( this,
                                VIEWLOCPANE_SPEED_CONTROL_SPIN,
                                wxEmptyString,
                                wxDefaultPosition,
                                wxDefaultSize,
                                wxSP_ARROW_KEYS,
                                0.0,
                                100000.0,
                                10.0,
                                0.1,
                                wxSPINCTRLDBL_AUTODIGITS,
                                wxEmptyString );

    _spinControlsSizer->Add( _spinSpeedControls, 1, wxALIGN_CENTER | wxALIGN_TOP | wxALL, 2 );
    _spinControlsSizer->Add( _spinLabel, 1, wxALIGN_CENTER );


    _speedGroup->Add( _speedButtonsSizer, 1, wxALIGN_LEFT | wxRIGHT, 5 );
    _speedGroup->Add( _spinControlsSizer, 1, wxALIGN_RIGHT | wxLEFT, 5 );

    wxStaticText* _FlythroughLabel = new wxStaticText( this, -1, wxT( "Automate Flythrough" ) );

    _FlythroughGroup->Add( _movetovwptLabel, 0, wxALIGN_LEFT );
    _FlythroughGroup->Add( _moveToViewpointSizer, 1, wxALIGN_CENTER_HORIZONTAL | wxTOP, 10 );
    _FlythroughGroup->Add( _FlythroughLabel, 0, wxALIGN_LEFT );
    _FlythroughGroup->Add( _speedGroup, 2, wxALIGN_CENTER_HORIZONTAL | wxTOP, 10 );

    mainSizer->Add( _allVPCtrlsGroup , 1, wxEXPAND | wxALIGN_CENTER );
    mainSizer->Add( _FlythroughGroup, 1, wxEXPAND | wxALIGN_CENTER );


    wxButton* _closeButton = new wxButton( this, wxID_OK, _T( "Close" ), wxDefaultPosition, wxDefaultSize, 0 );
    mainSizer->Add( _closeButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    SetAutoLayout( true );
    SetSizer( mainSizer );

    //Complete Hack needed to get the page to refresh properly
    // Hack because Refresh and SetSize(GetSize() ) don't work on win32 platform
    static bool test = false;
    int flag = 0;
    if( test )
    {
        flag = 1;
        test = false;
    }
    else
    {
        flag = -1;
        test = true;
    }

    wxSize temp = GetSize();
    temp.SetHeight( temp.GetHeight() + flag );
    temp.SetWidth( temp.GetWidth() + flag );
    SetSize( temp );

    _rebuildPage();
    _resetSelections();
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_rebuildNameArrays()
{
    //This will get called every time there's a change so all
    //dynamic memory allocations have to be cleaned up

    _locationName.Clear();
    _flythroughName.Clear();
    //Now the wxString arrays can be filled
    if( _numStoredLocations > 0 )
    {

        for( unsigned int i = 0; i < _numStoredLocations; i++ )
        {
            std::ostringstream vwptstream;
            vwptstream << "View Location " << i ;
            _locationName.Add( wxString( vwptstream.str().c_str(), wxConvUTF8 ) );
        }
    }
    else
    {
        _numStoredLocations = 1;
        _locationName.Add( wxT( "No Stored Locations" ) );
    }

    if( !flyThroughList.empty() )
    {
        _numStoredFlythroughs = flyThroughList.size();

        for( unsigned int i = 0; i < _numStoredFlythroughs; i++ )
        {
            std::ostringstream flynamestream;
            flynamestream << "Flythrough " << i ;
            _flythroughName.Add( wxString( flynamestream.str().c_str(), wxConvUTF8 ) );
        }

    }
    else
    {
        _numStoredFlythroughs = 1;
        _flythroughName.Add( _( "No Flythroughs Built" ) );
        _vwptsInActiveFly = 1;
        _activeFlyNames.Add( _( "No Flythroughs Built" ) );
    }

}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_setUpActiveFlyThroughNames( int index )
{
    _activeFlyNames.Clear();

    _vwptsInActiveFly = flyThroughList.at( index ).size();

    for( unsigned int i = 0; i < _vwptsInActiveFly; i++ )
    {
        std::ostringstream activeflynamestream;
        activeflynamestream << "View Location " << i;//flyThroughList.at( index ).at( i ) ;
        _activeFlyNames.Add( wxString( activeflynamestream.str().c_str(), wxConvUTF8 ) );
    }

}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_refreshGUIFromXplorerData( wxIdleEvent& WXUNUSED( event ) )
{
    /*
    if( !IsShown() )
    {
        return;
    }
    */

    CommandPtr viewPointData =
        CORBAServiceList::instance()->GetGUIUpdateCommands(
            "VIEWPOINT_GUI_DATA" );

    //Hasn't updated yet
    if( viewPointData->GetCommandName() == "NULL" )
    {
        return;
    }

    _numStoredLocations = viewPointData->GetNumberOfDataValuePairs();
    _numView_LocsGlobal = _numStoredLocations;

    _rebuildNameArrays();

    if( flyThroughList.size() > 0 )
    {
        _setUpActiveFlyThroughNames( 0 );
    }
    else if( flyThroughList.size() != 0 )
    {
        _setUpActiveFlyThroughNames( 0 );
    }

    _rebuildPage();
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onLoad( wxCommandEvent& WXUNUSED( event ) )
{
    dataValueName = "LOAD_NEW_VIEWPT";
    commandInputs.push_back( 0 );
    commandInputs.push_back( 0 );
    SendCommandsToXplorer();
    _resetSelections();
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onAcceptNewVPName( wxCommandEvent& WXUNUSED( event ) )
{
    //This will be used once user defined names are implemented

    //we can handle this now with the new schema. All we have to do is send back
    //a string name for each view point. ---biv
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onCancelNewVPName( wxCommandEvent& WXUNUSED( event ) )
{
    //This will be used once user defined names are implemented

    //we can handle this now with the new schema. All we have to do is send back
    //a string name for each view point. ---biv
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onRemoveVP( wxCommandEvent& WXUNUSED( event ) )
{
    if( _numView_LocsGlobal > 0 )
    {
        wxSingleChoiceDialog viewPointSelector( this, _T( "Select View Point to Delete." ),
                                                _T( "Remove View Point" ),
                                                _locationName );

        viewPointSelector.SetSize( GetRect() );
        unsigned int selectionIndex = 0;
        if( viewPointSelector.ShowModal() == wxID_OK )
        {
            for( size_t i = 0; i < _numStoredLocations; i++ )
            {
                if( !_locationName[i].Cmp( viewPointSelector.GetStringSelection() ) )
                {
                    selectionIndex = i;
                    break;
                }
            }
            dataValueName = "REMOVE_SELECTED_VIEWPT";
            commandInputs.push_back( selectionIndex );
            SendCommandsToXplorer();

        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onMoveToVP( wxCommandEvent& WXUNUSED( event ) )
{
    {
        for( unsigned int i = 0;i < _numStoredLocations;i++ )
        {
            if( _movetovwptSel->GetValue() == _locationName[i] )
            {
                dataValueName = "MOVE_TO_SELECTED_LOCATION";
                commandInputs.push_back( i );
                commandInputs.push_back( 0 );
                SendCommandsToXplorer();
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onBuildNewFlyButton( wxCommandEvent& WXUNUSED( event ) )
{}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onAcceptNewFlyName( wxCommandEvent& WXUNUSED( event ) )
{
    //This will be used once user defined names are implemented

    //we can handle this now with the new schema. All we have to do is send back
    //a string name for each view point. ---biv
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onCancelNewFlyName( wxCommandEvent& WXUNUSED( event ) )
{
    //This will be used once user defined names are implemented

    //we can handle this now with the new schema. All we have to do is send back
    //a string name for each view point. ---biv
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onActiveFlySel( wxCommandEvent& WXUNUSED( event ) )
{}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onAddVPtoFlySel( wxCommandEvent& WXUNUSED( event ) )
{
    if( _numView_LocsGlobal > 0 )
    {
        if( flyThroughList.size() > 0 )
        {
            for( unsigned int i = 0; i < flyThroughList.size(); i++ )
            {
                if( _activeflySel->GetValue() == _flythroughName[i] )
                {
                    for( unsigned int j = 0; j < _numStoredLocations; j++ )
                    {
                        if( _addvptoflySel->GetValue() == _locationName[j] )
                        {
                            dataValueName = "ADD_NEW_POINT_TO_FLYTHROUGH";
                            commandInputs.push_back( i );
                            commandInputs.push_back( j );
                            SendCommandsToXplorer();

                        }
                    }
                }
            }
        }
    }

}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onInsertVPinFlySel( wxCommandEvent& WXUNUSED( event ) )
{}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onRemoveVPfromFlySel( wxCommandEvent& WXUNUSED( event ) )
{}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onDeleteFlySel( wxCommandEvent& WXUNUSED( event ) )
{}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onStartActiveFly( wxCommandEvent& WXUNUSED( event ) )
{
    dataValueName = "RUN_ACTIVE_FLYTHROUGH";
    commandInputs.push_back( 0 );
    commandInputs.push_back( 0 );
    SendCommandsToXplorer();
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onStopFly( wxCommandEvent& WXUNUSED( event ) )
{
    dataValueName = "STOP_ACTIVE_FLYTHROUGH";
    commandInputs.push_back( 0 );
    commandInputs.push_back( 0 );
    SendCommandsToXplorer();
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onFlyBuilderListBox( wxCommandEvent& WXUNUSED( event ) )
{}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onSpeedChange( wxSpinEvent& WXUNUSED( event ) )
{
    dataValueName = "CHANGE_MOVEMENT_SPEED";
    commandInputs.push_back( _spinSpeedControls->GetValue() );
    commandInputs.push_back( 0 );
    SendCommandsToXplorer();
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_rebuildPage()
{
    if( _movetovwptSel )
        _movetovwptSel->Clear();

    _movetovwptSel->Insert( wxT( "Select a View Point" ), 0 );
    if( _movetovwptSel )
    {
        for( unsigned int i = 0; i < _numStoredLocations; i++ )
        {
            _movetovwptSel->Insert( _locationName[ i ] , i );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::_resetSelections()
{
    if( _removevwptSel )
    {
        _removevwptSel->SetSelection( 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ViewLocPane::SendCommandsToXplorer()
{

    //This assumes that the command name was set by the callback
    //as well as the DataValuePairs
    CommandPtr veCommand( new Command() );

    ///This is a hack to get around sending of only 1 command name and not using event handlers in the original
    ///code.
    ///This will have to be re-written to handle commands properly on the xplorer side -- biv
    if( !commandInputs.empty() )
    {
        _dataValuePairList.clear();

        _commandName = "ViewLoc_Data";
        // Create the command and data value pairs
        DataValuePairPtr dataValuePair( new DataValuePair() );
        dataValuePair->SetData( dataValueName, commandInputs );
        _dataValuePairList.push_back( dataValuePair );
    }

    veCommand->SetCommandName( _commandName );

    for( size_t i = 0; i < _dataValuePairList.size(); i++ )
    {
        veCommand->AddDataValuePair( _dataValuePairList.at( i ) );
    }

    try
    {
        // CORBA releases the allocated memory so we do not have to
        CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
    }
    catch ( ... )
    {
        wxMessageBox( _( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect." ),
                      _( "Communication Failure" ), wxOK | wxICON_INFORMATION );
    }
    commandInputs.clear();
    _dataValuePairList.clear();
    _commandName = " ";
    _resetSelections();
}
////////////////////////////////////////////////////////////////////////////////
