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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Utilities/CORBAServiceList.h"
#include "VE_Conductor/Framework/ViewLocPane.h"
#include "VE_Conductor/Framework/Frame.h"
#include "VE_Conductor/Framework/App.h"
#include "VE_Open/XML/DOMDocumentManager.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"

#include <iostream>
#include <string>
#include <sstream>

BEGIN_EVENT_TABLE(ViewLocPane, wxDialog)
   EVT_BUTTON(VIEWLOC_LOAD_BUTTON,ViewLocPane::_onLoad)
   EVT_BUTTON(REMOVE_VIEW_PT_BUTTON,ViewLocPane::_onRemoveVP)
   EVT_BUTTON(VIEWLOC_ACCEPTNEWVPNAME_BUTTON,ViewLocPane::_onAcceptNewVPName)
   EVT_BUTTON(VIEWLOC_CANCELNEWVPNAME_BUTTON,ViewLocPane::_onCancelNewVPName)
   //EVT_COMBOBOX(VIEWLOC_REMOVEVP_COMBOBOX,ViewLocPane::_onRemoveVP)
   EVT_COMBOBOX(VIEWLOC_MOVETOVP_COMBOBOX,ViewLocPane::_onMoveToVP)
   EVT_BUTTON(VIEWLOC_NEWFLY_BUTTON,ViewLocPane::_onBuildNewFlyButton)
   EVT_BUTTON(VIEWLOC_ACCEPTNEWFLYNAME_BUTTON,ViewLocPane::_onAcceptNewFlyName)
   EVT_BUTTON(VIEWLOC_CANCELNEWFLYNAME_BUTTON,ViewLocPane::_onCancelNewFlyName)
   EVT_COMBOBOX(VIEWLOC_ACTIVEFLYSEL_COMBOBOX,ViewLocPane::_onActiveFlySel)
   EVT_COMBOBOX(VIEWLOC_ADDVPTOFLYSEL_COMBOBOX,ViewLocPane::_onAddVPtoFlySel)
   EVT_COMBOBOX(VIEWLOC_INSERTVPINFLYSEL_COMBOBOX,ViewLocPane::_onInsertVPinFlySel)
   EVT_COMBOBOX(VIEWLOC_REMOVEVPFROMFLYSEL_COMBOBOX,ViewLocPane::_onRemoveVPfromFlySel)
   EVT_COMBOBOX(VIEWLOC_DELETEFLYSEL_COMBOBOX,ViewLocPane::_onDeleteFlySel)
   EVT_BUTTON(VIEWLOC_RUNFLY_BUTTON,ViewLocPane::_onStartActiveFly)
   EVT_BUTTON(VIEWLOC_LOAD_FILE,ViewLocPane::_onLoadStoredPointsFile)
   EVT_BUTTON(VIEWLOC_SAVE_FILE,ViewLocPane::_onSaveStoredPointsFile)
   EVT_BUTTON(VIEWLOC_STOPFLY_BUTTON,ViewLocPane::_onStopFly)
   EVT_LISTBOX(VIEWLOC_FLYBUILDER_LISTBOX,ViewLocPane::_onFlyBuilderListBox)
   EVT_COMMAND_SCROLL(VIEWLOC_SPEED_CONTROL_SLIDER, ViewLocPane::_onSpeedChange )
END_EVENT_TABLE()
///////////////
//Constructor//
///////////////
ViewLocPane::ViewLocPane(  )
:wxDialog(NULL, -1, _("Viewing Locations Pane"),
         wxDefaultPosition, wxDefaultSize, 
		  (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP)
{
   _numStoredLocations = 0;
   _numStoredFlythroughs = 0;
   _vwptsInActiveFly = 0;
   _numViewLocLocal = 0;
   _vwptsInActiveFlyLocal= 0;
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
   
   //wxSize displaySize = ::wxGetDisplaySize();
   //wxRect dialogPosition( displaySize.GetWidth() - 575, displaySize.GetHeight() - 550, 575, 550 );
   //this->SetSize( dialogPosition );

   _buildPage();
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
   Centre();

}
/////////////////////////////////
ViewLocPane::~ViewLocPane( void )
{
   ;
}
/////////////////////////////////////////////////////////////////
void ViewLocPane::_onLoadStoredPointsFile(wxCommandEvent& event)
{
   ///this is Waaaaaaaaaaaaaaay hacked... --biv
   wxFileDialog dialog(this,
                       _T("Open file"), 
                       _T(""), 
                       _T(""),
                       _T("View Location files (*.vel;*.dat)|*.vel;*.dat;"),
                       wxOPEN|wxFILE_MUST_EXIST, 
                       wxDefaultPosition);
   
   if (dialog.ShowModal() == wxID_OK)
   {
      wxFileName viewPtsFilename( dialog.GetPath() );
      viewPtsFilename.MakeRelativeTo( ::wxGetCwd(), wxPATH_NATIVE );
      wxString relativeViewLocationsPath( wxString("./",wxConvUTF8) + viewPtsFilename.GetFullPath() );

      VE_XML::DataValuePair* velFileName = new VE_XML::DataValuePair();
      velFileName->SetData("View Locations file", ConvertUnicode( relativeViewLocationsPath.c_str() ));
      _dataValuePairList.push_back(velFileName);

      _commandName = "QC_LOAD_STORED_POINTS";
      SendCommandsToXplorer();
      ///hack!!! ping xplorer since it was just updated to populate the GUI!!!
      ///This is no good!!!---biv
      _updateWithcfdQuatCamHandler();
   }
}
/////////////////////////////////////////////////////////////////
void ViewLocPane::_onSaveStoredPointsFile(wxCommandEvent& event)
{
   wxFileName velFileName;
   do
   {
      wxTextEntryDialog viewLocationsFileDlg(this, 
                                       _("Enter the prefix for *.vel filename:"),
                                       _("Save VEL file as..."),
                                       _("locations"),wxOK|wxCANCEL);

      if ( viewLocationsFileDlg.ShowModal() == wxID_OK )
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
   while ( velFileName.FileExists() );
   
   if ( velFileName.HasName() ) 
   {
      _dataValuePairList.clear();
      VE_XML::DataValuePair* velFile = new VE_XML::DataValuePair();
      velFile->SetData("View Points file", ConvertUnicode( velFileName.GetFullPath( wxPATH_NATIVE ).c_str() ) );
      _dataValuePairList.push_back(velFile);

      _commandName = "VL_SAVE_STORED_POINTS";
      SendCommandsToXplorer();
   }
}
//////////////////////////////
//build the viewing locations tab       //
//////////////////////////////
void ViewLocPane::_buildPage()
{
   

//*******Setting up the widgets for making and naming a new view point
   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
  
   wxString choices[] = { _("Choose a View Point" )};
   //scrollWindow = new wxScrolledWindow( this, -1, wxDefaultPosition, wxDefaultSize, wxHSCROLL | wxVSCROLL);
   int nUnitX=20;
   int nUnitY=10;
   int nPixX = 5;
   int nPixY = 10;
   //scrollWindow->SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );

   wxStaticBox* _allVPCtrlBox = new wxStaticBox(this, -1, _("View Point Controls"), wxDefaultPosition,wxDefaultSize,wxCAPTION); 

	//wxStaticBox* _newVPNameCtrlBox = new wxStaticBox(this, -1, _("Name the new View Point"), wxDefaultPosition,wxDefaultSize,wxCAPTION);

   wxButton* loadViewLocationButton = new wxButton(this,VIEWLOC_LOAD_FILE, _("Load View Location File") );

   wxButton* _addnewviewptButton = new wxButton(this, VIEWLOC_LOAD_BUTTON, wxT("Add New View Pt"));
   
   //wxTextCtrl* _newvwptNameCtrl = new wxTextCtrl(this, -1, wxT("Enter Name for new pt"));

   //wxButton* _newvwptNameOKButton = new wxButton(this, VIEWLOC_ACCEPTNEWVPNAME_BUTTON, wxT("OK"));

   //wxButton* _newvwptNameCancelButton = new wxButton(this, VIEWLOC_CANCELNEWVPNAME_BUTTON, wxT("Cancel"));

   //wxBoxSizer* _newVPNameButtonsSizer = new wxBoxSizer(wxHORIZONTAL);
   //_newVPNameButtonsSizer->Add(_newvwptNameOKButton,1,wxALIGN_LEFT|wxEXPAND);
   //_newVPNameButtonsSizer->Add(_newvwptNameCancelButton,1,wxALIGN_RIGHT|wxEXPAND);

   //wxStaticBoxSizer* _newVPNameCtrlGroup = new wxStaticBoxSizer(_newVPNameCtrlBox, wxVERTICAL);
   //_newVPNameCtrlGroup->Add(_newvwptNameCtrl,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //_newVPNameCtrlGroup->Add(_newVPNameButtonsSizer,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
//***************************************************************************

//********Finishing off the view points controls
   wxStaticText* _removevwptLabel = new wxStaticText(this, -1, wxT("Delete View Points "));
   wxButton* _removeViewPointButton = new wxButton(this,REMOVE_VIEW_PT_BUTTON,wxT("Delete"));
   wxStaticText* _movetovwptLabel = new wxStaticText(this, -1, wxT("Move to a View Point "));

   _movetovwptSel = new wxComboBox(this, VIEWLOC_MOVETOVP_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                 wxDefaultSize,1, 
                                 choices, wxCB_READONLY);

   wxBoxSizer* _newVPControlsSizer = new wxBoxSizer(wxVERTICAL);
   _newVPControlsSizer->Add(loadViewLocationButton,1,wxALIGN_CENTER_HORIZONTAL);
   _newVPControlsSizer->Add(_addnewviewptButton,1,wxALIGN_CENTER_HORIZONTAL);
   _newVPControlsSizer->Add(_removevwptLabel,2,wxALIGN_LEFT);
   _newVPControlsSizer->Add(_removeViewPointButton,1,wxALIGN_CENTER_HORIZONTAL);
   _newVPControlsSizer->Add(_movetovwptLabel,2,wxALIGN_LEFT);
   _newVPControlsSizer->Add(_movetovwptSel,1,wxALIGN_CENTER_HORIZONTAL);

   wxStaticBoxSizer* _allVPCtrlsGroup = new wxStaticBoxSizer(_allVPCtrlBox, wxVERTICAL);
   _allVPCtrlsGroup->Add(_newVPControlsSizer,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

//*******Throw in the Speed Control Slider
   wxStaticBox* _speedCtrlBox = new wxStaticBox(this, -1, _("Movement Speed Control"), wxDefaultPosition,wxDefaultSize,wxCAPTION); 
   wxStaticText* _speedctrlLabel = new wxStaticText(this, -1, wxT("Approximate Linear Speed in feet/second"));

   _speedCtrlSlider = new wxSlider(this, VIEWLOC_SPEED_CONTROL_SLIDER,10,0,100,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_LABELS );

   wxStaticBoxSizer* _speedCtrlGroup = new wxStaticBoxSizer(_speedCtrlBox, wxVERTICAL);
   _speedCtrlGroup->Add(_speedctrlLabel,1,wxALIGN_CENTER_HORIZONTAL);
   _speedCtrlGroup->Add(_speedCtrlSlider,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxBoxSizer* _allLeftSide = new wxBoxSizer(wxVERTICAL);
   _allLeftSide->Add(_allVPCtrlsGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 
   _allLeftSide->Add(_speedCtrlGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxButton* _runactiveflyButton = new wxButton(this, VIEWLOC_RUNFLY_BUTTON, wxT("Start Active Flythrough"));

   wxButton* _stopactiveflyButton = new wxButton(this, VIEWLOC_STOPFLY_BUTTON, wxT("Stop Flythrough"));

   wxBoxSizer* _runStopFlyButtonsSizer = new wxBoxSizer(wxHORIZONTAL);
   _runStopFlyButtonsSizer->Add(_runactiveflyButton,1,wxALIGN_LEFT);
   _runStopFlyButtonsSizer->Add(_stopactiveflyButton,1,wxALIGN_RIGHT);

   wxBoxSizer* _allFlythroughCtrls = new wxBoxSizer(wxHORIZONTAL);
   _allFlythroughCtrls->Add(_runStopFlyButtonsSizer,1,wxALIGN_CENTER_HORIZONTAL);

   wxStaticBox* _allFlyCtrlBox = new wxStaticBox(this, -1, _("Flythrough Controls"), wxDefaultPosition,wxDefaultSize,wxCAPTION); 
   wxStaticBoxSizer* _allFlyCtrlsGroup = new wxStaticBoxSizer(_allFlyCtrlBox, wxHORIZONTAL);
   _allFlyCtrlsGroup->Add(_allFlythroughCtrls,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _allLeftSide->Add(_allFlyCtrlsGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
//********Now Put the entire page together
   //the main group
   wxBoxSizer* viewlocPanelGroup = new wxBoxSizer(wxHORIZONTAL);

   //add the rows to the main panel
   viewlocPanelGroup->Add(_allLeftSide,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 
   mainSizer->Add( viewlocPanelGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
 
   SetAutoLayout(true);
   SetSizer(mainSizer);

   //Complete Hack needed to get the page to refresh properly
   // Hack because Refresh and SetSize(GetSize() ) don't work on win32 platform
   static bool test = false;
   int flag = 0;
   if ( test )
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
   temp.SetHeight( temp.GetHeight()+flag );
   temp.SetWidth( temp.GetWidth()+flag );
   SetSize( temp );

   _rebuildPage();
	_resetSelections();
   //_activeflySel->SetSelection( 0 );

//   _newVPNameCtrlBox->Enable( false );
   //_newvwptNameCtrl->Enable( false );
   //_newvwptNameOKButton->Enable( false );
   //_newvwptNameCancelButton->Enable( false );

/*   _newFlyNameCtrlBox->Enable( false );
   _newflythroughNameCtrl->Enable( false );
   _newflythroughNameOKButton->Enable( false );
   _newflythroughNameCancelButton->Enable( false );
*/
}

void ViewLocPane::_rebuildNameArrays( void )
{
   //This will get called every time there's a change so all
   //dynamic memory allocations have to be cleaned up

   _locationName.Clear();
   _flythroughName.Clear();
   //Now the wxString arrays can be filled
   if( _numStoredLocations > 0 )
   {

      for( unsigned int i=0; i<_numStoredLocations; i++)
      {
         std::ostringstream vwptstream;
         vwptstream << "View Location " << i ;
         _locationName.Add( wxString(vwptstream.str().c_str(), wxConvUTF8) );
      }
   }
   else
   {
      _numStoredLocations = 1;
      _locationName.Add(wxT("No Stored Locations"));
   }

   if( !flyThroughList.empty() )
   {
      _numStoredFlythroughs = flyThroughList.size();
      //_flythroughName = new wxArrayString[ _numStoredFlythroughs ];

      for( unsigned int i=0; i<_numStoredFlythroughs; i++)
      {
         std::ostringstream flynamestream;
         flynamestream << "Flythrough " << i ;
         _flythroughName.Add( wxString(flynamestream.str().c_str(), wxConvUTF8) );
      }
 
   }
   else
   {
      _numStoredFlythroughs = 1;
      //_flythroughName = new wxString[1];
      _flythroughName.Add( _("No Flythroughs Built") );
      _vwptsInActiveFly = 1;
      //_activeFlyNames = new wxString[ _vwptsInActiveFly ];
      _activeFlyNames.Add( _("No Flythroughs Built") );
   }
   
}
///////////////////////////////////////////////////////////
void ViewLocPane::_setUpActiveFlyThroughNames( int index )
{
   _activeFlyNames.Clear();

   _vwptsInActiveFly = flyThroughList.at( index ).size();

   for ( unsigned int i=0; i<_vwptsInActiveFly; i++ )
   {
      std::ostringstream activeflynamestream;
      activeflynamestream << "View Location " << flyThroughList.at( index ).at( i ) ;
      _activeFlyNames.Add( wxString(activeflynamestream.str().c_str(),wxConvUTF8));
   }
  
}
//////////////////////////////////////////////////////
void ViewLocPane::_updateWithcfdQuatCamHandler( void )
{
   unsigned int tempindex = 0;
   /*for( unsigned int i=0;i<flyThroughList.size();i++ )
   {
	   if( _activeflySel->GetValue() == _flythroughName[i])
      {
         tempindex = i;
      }
   } */
   

   if ( !VE_Conductor::CORBAServiceList::instance()->IsConnectedToXplorer() )
   {
      return;
   }

   VjObs::obj_pd_var tempTest;
   int tempTestlocal = 0;
   int counter = 0;
   
   while ( tempTestlocal == 0 )
   {
      tempTest = VE_Conductor::CORBAServiceList::instance()->GetXplorerPointer()->getDouble1D( "getCompletionTest" );
      tempTestlocal = (int)tempTest[ 0 ];       
      wxMilliSleep( 50 );
      ++counter;
      if ( counter == 6 )
      {
         return;
      }
   }
   
   _numStoredLocations = VE_Conductor::CORBAServiceList::instance()->GetXplorerPointer()->getIsoValue();
   VjObs::double2DArray_var  flyThroughArray;
   flyThroughArray = VE_Conductor::CORBAServiceList::instance()->GetXplorerPointer()->getDouble2D( "getFlythroughData" );
 
   flyThroughList.clear();
   for (CORBA::ULong j=0; j<flyThroughArray->length(); j++ )
   {
      std::vector<int> tempPts;
      for (CORBA::ULong k=0; k<flyThroughArray[j].length(); k++ )
      {
         tempPts.push_back( (int)flyThroughArray[j][k] );
      }    
      flyThroughList.push_back(tempPts);
      tempPts.clear();
   }

   _numView_LocsGlobal = _numStoredLocations;
 
   _rebuildNameArrays();
   if ( flyThroughList.size() > tempindex && flyThroughList.size() != 0 )
   {
      _setUpActiveFlyThroughNames( tempindex );
   }
   else if ( flyThroughList.size() != 0 )
   {
      tempindex = 0;
      _setUpActiveFlyThroughNames( tempindex );
   }
   else
   {
      tempindex = 0;
   }
 
   _rebuildPage();
   //_activeflySel->SetValue( _flythroughName[ tempindex ] );
   _resetSelections();

}
//////////////////
//event handling//
///////////////////

//////////////////////////////////////////////////
void ViewLocPane::_onLoad(wxCommandEvent& WXUNUSED(event))
{
   dataValueName = "LOAD_NEW_VIEWPT";
   commandInputs.push_back( 0 );
   commandInputs.push_back( 0 );
   SendCommandsToXplorer();

   _updateWithcfdQuatCamHandler();
   _resetSelections();
}
//////////////////////////////////////////////////////////////////////
void ViewLocPane::_onAcceptNewVPName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented

    //we can handle this now with the new schema. All we have to do is send back
   //a string name for each view point. ---biv
}
/////////////////////////////////////////////////////////////////////
void ViewLocPane::_onCancelNewVPName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented

    //we can handle this now with the new schema. All we have to do is send back
   //a string name for each view point. ---biv
}

void ViewLocPane::_onRemoveVP(wxCommandEvent& WXUNUSED(event))
{
   if ( _numView_LocsGlobal > 0 )
   {
      wxSingleChoiceDialog viewPointSelector(this, _T("Select View Point to Delete."), 
                                          _T("Remove View Point"),
                                           _locationName);
  
      viewPointSelector.SetSize(GetRect());
      unsigned int selectionIndex = 0;
      if (viewPointSelector.ShowModal() == wxID_OK)
      {
         for(size_t i = 0; i < _numStoredLocations; i++)
         {
            if(!_locationName[i].Cmp(viewPointSelector.GetStringSelection()) )
            {
               selectionIndex = i;
               break;
            }
         } 
         dataValueName = "REMOVE_SELECTED_VIEWPT";
         commandInputs.push_back( selectionIndex );
         SendCommandsToXplorer();
         _updateWithcfdQuatCamHandler();
      }
   }
}

void ViewLocPane::_onMoveToVP(wxCommandEvent& WXUNUSED(event))
{
   //if ( _numView_LocsGlobal > 0 )
   {
      for( unsigned int i=0;i<_numStoredLocations;i++ )
	   {
		   if( _movetovwptSel->GetValue() == _locationName[i])
         {
            dataValueName = "MOVE_TO_SELECTED_LOCATION";
            commandInputs.push_back( i );
            commandInputs.push_back( 0 );
            SendCommandsToXplorer();
         }
	   }
      //_resetSelections();
   }
}
///////////////////////////////////////////////////////////////////////
void ViewLocPane::_onBuildNewFlyButton(wxCommandEvent& WXUNUSED(event))
{
   /*if ( _numView_LocsGlobal > 0 )
   {
      dataValueName = "ADD_NEW_FLYTHROUGH";
      commandInputs.push_back( 0 );
      commandInputs.push_back( 0 );
      SendCommandsToXplorer();

      _updateWithcfdQuatCamHandler();
      _setUpActiveFlyThroughNames( flyThroughList.size() - 1 );
      _rebuildPage();
      _resetSelections();
      _activeflySel->SetSelection( flyThroughList.size() - 1 );
   }*/
}
//////////////////////////////////////////////////////////////////////
void ViewLocPane::_onAcceptNewFlyName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented

   //we can handle this now with the new schema. All we have to do is send back
   //a string name for each view point. ---biv
}
//////////////////////////////////////////////////////////////////////
void ViewLocPane::_onCancelNewFlyName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented

    //we can handle this now with the new schema. All we have to do is send back
   //a string name for each view point. ---biv
}
///////////////////////////////////////////////////////////////////
void ViewLocPane::_onActiveFlySel(wxCommandEvent& WXUNUSED(event))
{
   /*if ( _flythroughName[ 0 ] != wxT("No Flythroughs Built") )
   {
      int tempindex = 0;
      for( unsigned int i=0; i<flyThroughList.size(); i++ )
	   {
		   if( _activeflySel->GetValue() == _flythroughName[ i ])
         {
            _setUpActiveFlyThroughNames( i );
            tempindex = i;
         }
	   }
      _rebuildPage();
      _activeflySel->SetValue( _flythroughName[ tempindex ] );
      _resetSelections();
   }*/
}
///////////////////////////////////////////////////////////////////
void ViewLocPane::_onAddVPtoFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( _numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _activeflySel->GetValue() == _flythroughName[i])
            {
               for ( unsigned int j=0; j<_numStoredLocations; j++ )
               {
                  if( _addvptoflySel->GetValue() == _locationName[j])
                  {
                     dataValueName = "ADD_NEW_POINT_TO_FLYTHROUGH";
                     commandInputs.push_back( i );
                     commandInputs.push_back( j );
                     SendCommandsToXplorer();
                     _updateWithcfdQuatCamHandler();
                  }
               }
            }
	      }
      }
      //_resetSelections();
   }
   
}
//////////////////////////////////////////////////////////////////////
void ViewLocPane::_onInsertVPinFlySel(wxCommandEvent& WXUNUSED(event))
{
   /*if ( _numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _activeflySel->GetValue() == _flythroughName[i])
            {
               for ( unsigned int j=0; j<flyThroughList.at( i ).size(); j++ )
               {
                  if( _flybuilderListBox->IsSelected( j ) )
                  {
                     for ( unsigned int k=0; k<_numStoredLocations; k++ )
                     {
                        if( _insertvpinflySel->GetValue() == _locationName[k])
                        {
                           dataValueName = "INSERT_NEW_POINT_IN_FLYTHROUGH";
                           commandInputs.push_back( i );
                           commandInputs.push_back( j );
                           commandInputs.push_back( k );
                           SendCommandsToXplorer();
                           _updateWithcfdQuatCamHandler();
                        }
                     }
                  }
               }
            }
	      }
      }
      //_resetSelections();
   }*/
}
////////////////////////////////////////////////////////////////////////
void ViewLocPane::_onRemoveVPfromFlySel(wxCommandEvent& WXUNUSED(event))
{
  /* if ( _numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _activeflySel->GetValue() == _flythroughName[i])
            {
               for ( unsigned int j=0; j<flyThroughList.at( i ).size(); j++ )
               {
                  if( _removevpfromflySel->GetValue() == _activeFlyNames[j])
                  {
                     dataValueName = "REMOVE_POINT_FROM_FLYTHROUGH";
                     commandInputs.push_back( i );
                     commandInputs.push_back( j );
                     SendCommandsToXplorer();
                     _updateWithcfdQuatCamHandler();
                  }
               }
            }
	      }
      }
      //_resetSelections();
   }*/

}
///////////////////////////////////////////////////////////////////
void ViewLocPane::_onDeleteFlySel(wxCommandEvent& WXUNUSED(event))
{
   /*if ( flyThroughList.size() > 0 )
   {
      for( unsigned int i=0; i<flyThroughList.size(); i++ )
	   {
		   if( _deleteflySel->GetValue() == _flythroughName[i])
         {
            dataValueName = "DELETE_ENTIRE_FLYTHROUGH";
            commandInputs.push_back( i );
            commandInputs.push_back( 0 );
            SendCommandsToXplorer();
            _updateWithcfdQuatCamHandler();
         }
	   }
      //_resetSelections();
   }*/

}
////////////////////////////////////////////////////////////////////
void ViewLocPane::_onStartActiveFly(wxCommandEvent& WXUNUSED(event))
{
   /*if ( _numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( ( _activeflySel->GetValue() == _flythroughName[i] ) && ( flyThroughList.at( i ).size() > 1 ) )
            {
               dataValueName = "RUN_ACTIVE_FLYTHROUGH";
               commandInputs.push_back( i );
               commandInputs.push_back( 0 );
               SendCommandsToXplorer();
            }
	      }
      }
   }*/
   dataValueName = "RUN_ACTIVE_FLYTHROUGH";
   commandInputs.push_back( 0 );
   commandInputs.push_back( 0 );
   SendCommandsToXplorer();
}
/////////////////////////////////////////////////////////////
void ViewLocPane::_onStopFly(wxCommandEvent& WXUNUSED(event))
{
   dataValueName = "STOP_ACTIVE_FLYTHROUGH";
   commandInputs.push_back( 0 );
   commandInputs.push_back( 0 );
   SendCommandsToXplorer();
}
///////////////////////////////////////////////////////////////////////
void ViewLocPane::_onFlyBuilderListBox(wxCommandEvent& WXUNUSED(event))
{
   
}
///////////////////////////////////////////////////////////////////
void ViewLocPane::_onSpeedChange( wxScrollEvent& WXUNUSED(event) )
{
   dataValueName = "CHANGE_MOVEMENT_SPEED";
   commandInputs.push_back( _speedCtrlSlider->GetValue() );
   commandInputs.push_back( 0 );
   SendCommandsToXplorer();
}
///////////////////////////////////////
void ViewLocPane::_rebuildPage( void )
{ 
   //if(_removevwptSel)
   //   _removevwptSel->Clear();
   if(_movetovwptSel)
      _movetovwptSel->Clear();
   /*_addvptoflySel->Clear();
   _insertvpinflySel->Clear();
   _activeflySel->Clear();
   _deleteflySel->Clear();
   _removevpfromflySel->Clear();
   _flybuilderListBox->Clear();

   _removevwptSel->Insert( wxT("Select a View Point"), 0 );
   
   _addvptoflySel->Insert( wxT("Select a View Point"), 0 );
   _insertvpinflySel->Insert( wxT("Select a View Point"), 0 );
   _deleteflySel->Insert( wxT("Select a Flythrough"), 0 );   
   _removevpfromflySel->Insert( ("Select a View Point"), 0 ); 
*/
   _movetovwptSel->Insert( wxT("Select a View Point"), 0 );
   if( _movetovwptSel)
   {
      for ( unsigned int i=0; i<_numStoredLocations; i++ )
      {  
         //_removevwptSel->Insert( _locationName[ i ],i );
         _movetovwptSel->Insert( _locationName[ i ] ,i);
      //_addvptoflySel->Insert( _locationName[ i ], ( i + 1 ) );
      //_insertvpinflySel->Insert( _locationName[ i ], ( i + 1 ) );
      }
   }

   /*for ( unsigned int i=0; i<_numStoredFlythroughs; i++ )
   {
      _activeflySel->Insert( _flythroughName[ i ], ( i ) );
      _deleteflySel->Insert( _flythroughName[ i ], ( i + 1 ) );
   }

   for ( unsigned int i=0; i<_vwptsInActiveFly; i++ )
   {
      _removevpfromflySel->Insert( _activeFlyNames[ i ], ( i + 1 ) );
      _flybuilderListBox->Insert( _activeFlyNames[ i ], ( i ) );
   }*/


}
///////////////////////////////////////////
void ViewLocPane::_resetSelections( void )
{
   if(_removevwptSel)
      _removevwptSel->SetSelection( 0 );
   //if(_movetovwptSel)
   //   _movetovwptSel->SetSelection( 0 );
   /*_addvptoflySel->SetSelection( 0 );
   _insertvpinflySel->SetSelection( 0 );
   _deleteflySel->SetSelection( 0 );   
   _removevpfromflySel->SetSelection( 0 ); 
*/
}
///////////////////////////////////////////////////////
void ViewLocPane::SetCommInstance( VjObs_ptr veEngine )
{
   //xplorerPtr = VjObs::_duplicate( veEngine );
}
////////////////////////////////////////////////
void ViewLocPane::SendCommandsToXplorer( void )
{

   //This assumes that the command name was set by the callback
   //as well as the DataValuePairs
   VE_XML::Command* veCommand = new VE_XML::Command();

   ///This is a hack to get around sending of only 1 command name and not using event handlers in the original
   ///code.
   ///This will have to be re-written to handle commands properly on the xplorer side -- biv
   if(!commandInputs.empty())
   {
      _dataValuePairList.clear();

      _commandName = "ViewLoc_Data";
      // Create the command and data value pairs
      VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair( std::string("LONG") );
      dataValuePair->SetData( dataValueName, commandInputs );
      _dataValuePairList.push_back( dataValuePair );
   }
 
   veCommand->SetCommandName(_commandName);

   for(size_t i =0; i < _dataValuePairList.size(); i++)
   {
      veCommand->AddDataValuePair(_dataValuePairList.at(i));
   }

   try
   {
      // CORBA releases the allocated memory so we do not have to
      VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   }
   catch ( ... )
   {
      wxMessageBox( _("Send data to VE-Xplorer failed. Probably need to disconnect and reconnect."), 
                     _("Communication Failure"), wxOK | wxICON_INFORMATION );
   }
   //Clean up memory
   delete veCommand;
   commandInputs.clear();
   _dataValuePairList.clear();
   _commandName = " ";
   _resetSelections();
}
