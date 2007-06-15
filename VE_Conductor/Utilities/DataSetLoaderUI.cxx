/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#include "wx/wxprec.h"


////@begin includes
#include <wx/sizer.h>
#include <wx/scrolwin.h>
#include <wx/statbox.h>
#include <wx/listbox.h>
#include <wx/bitmap.h>
#include <wx/icon.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/combobox.h>
#include <wx/filedlg.h>
#include <wx/filename.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/string.h>
////@end includes

#include "VE_Installer/installer/installerImages/ve_icon32x32.xpm"
#include "VE_Conductor/Utilities/DataSetLoaderUI.h"
#include "VE_Conductor/Utilities/TransformUI.h"

#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Model/Model.h"

#include <iostream>

////@begin XPM images
////@end XPM images

/*!
 * DataSetLoaderUI type definition
 */

//IMPLEMENT_DYNAMIC_CLASS( DataSetLoaderUI, wxDialog )

/*!
 * DataSetLoaderUI event table definition
 */

BEGIN_EVENT_TABLE( DataSetLoaderUI, wxDialog )

////@begin DataSetLoaderUI event table entries
   EVT_BUTTON( ID_BUTTON, DataSetLoaderUI::OnButtonClick )
   EVT_BUTTON( ID_BUTTON3, DataSetLoaderUI::OnButton4Click )
   EVT_BUTTON( ID_BUTTON4, DataSetLoaderUI::OnButton4Click )
   EVT_BUTTON( ID_BUTTON6, DataSetLoaderUI::OnButton6Click )
   EVT_BUTTON( ID_BUTTON2, DataSetLoaderUI::OnButton2Click )
   EVT_BUTTON( ID_ADD_DATASET, DataSetLoaderUI::OnInformationPacketAdd )
   EVT_LISTBOX_DCLICK( ID_LISTBOX, DataSetLoaderUI::OnListboxSelected )
   EVT_COMBOBOX( ID_COMBOBOX, DataSetLoaderUI::OnInformationPacketChange ) // this will refresh all the widgets for the given dataset
   //EVT_TEXT( ID_COMBOBOX, DataSetLoaderUI::OnInformationPacketChangeName ) // change the name of a given data set, will also need to change the name seen by xplorer
   //EVT_TEXT_ENTER( ID_COMBOBOX, DataSetLoaderUI::OnInformationPacketAdd ) // add a name to the list once enter is pushed, also add new information block
////@end DataSetLoaderUI event table entries

END_EVENT_TABLE()

/*!
 * DataSetLoaderUI constructors
 */

DataSetLoaderUI::DataSetLoaderUI( )
{
   paramBlock = 0;
}

DataSetLoaderUI::DataSetLoaderUI( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style, VE_XML::VE_Model::Model* veModel )
{
   Create(parent, id, caption, pos, size, style, veModel );
}

/*!
 * DataSetLoaderUI creator
 */

bool DataSetLoaderUI::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style, VE_XML::VE_Model::Model* veModel )
{
////@begin DataSetLoaderUI member initialisation
   this->veModel = veModel;
   paramBlock = 0;
   lastAddition = -1;
   dataSetList = NULL;
   dataSetTextEntry = NULL;
   dataSetOpenButton = NULL;
   preComputDirTextEntry = NULL;
   preComputeOpenButton = NULL;
   surfaceDataText = NULL;
   surfaceDataOpenButton = NULL;
   transformButton = NULL;
   scalarButton = NULL;
   itemListBox24 = 0;
   itemTextCtrl21 = 0;
   itemButton22 = 0;
   itemStaticBoxSizer6Static = 0;
   itemStaticBoxSizer9Static = 0;
   itemStaticBoxSizer12Static = 0;
   itemStaticBoxSizer15Static = 0;
   itemStaticBoxSizer19Static = 0;
////@end DataSetLoaderUI member initialisation

////@begin DataSetLoaderUI creation
   //SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
   wxDialog::Create( parent, id, caption, pos, size, style );

   CreateControls();
   
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
   Centre();
   InitializeWidgets();
   EnableUI( false );
   SetAutoLayout( true );
   Refresh();
   wxSize temp = GetSize();
   temp.SetHeight( temp.GetHeight() +1);
   temp.SetWidth( temp.GetWidth()+1 );
   SetSize( temp );
   this->SetIcon( ve_icon32x32_xpm );
////@end DataSetLoaderUI creation
   return true;
}

/*!
 * Control creation for DataSetLoaderUI
 */

void DataSetLoaderUI::CreateControls()
{    
////@begin DataSetLoaderUI content construction
    DataSetLoaderUI* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    //wxScrolledWindow* itemScrolledWindow3 = new wxScrolledWindow( itemDialog1, ID_SCROLLEDWINDOW, wxDefaultPosition, wxDefaultSize, wxHSCROLL|wxVSCROLL );
    //itemBoxSizer2->Add(itemScrolledWindow3, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
    //itemScrolledWindow3->SetScrollbars(20, 10, 5, 10);

    //wxStaticBox* itemStaticBoxSizer4Static = new wxStaticBox(itemScrolledWindow3, wxID_ANY, _("Steady State Data"));
    wxStaticBox* itemStaticBoxSizer4Static = new wxStaticBox( itemDialog1, wxID_ANY, _("Steady State Data"));
    wxStaticBoxSizer* itemStaticBoxSizer4 = new wxStaticBoxSizer(itemStaticBoxSizer4Static, wxVERTICAL);
    //itemScrolledWindow3->SetSizer(itemStaticBoxSizer4);
    itemBoxSizer2->Add( itemStaticBoxSizer4, 1, wxALL|wxEXPAND, 5);

   ///////////////////////////////////////////////////////
    wxString* dataSetListStrings = NULL;
    //dataSetList = new wxComboBox( itemScrolledWindow3, ID_COMBOBOX, _("Type New DataSet Name Here"), wxDefaultPosition, wxSize(250, -1), 0, dataSetListStrings, wxCB_DROPDOWN );
    dataSetList = new wxComboBox( itemDialog1, ID_COMBOBOX, _(""), wxDefaultPosition, wxDefaultSize, 0, dataSetListStrings, wxCB_DROPDOWN );
    //dataSetList->SetStringSelection(_("Type New DataSet Name Here"));
    //dataSetList->Append(_("Unselect"));
    //dataSetList->Append(_("Add Dataset"));
    //dataSetList->Append(_("Edit Dataset"));
    //dataSetList->Append(_("Delete Dataset"));
    dataSetList->SetHelpText(_("Text Entry"));
    if (ShowToolTips())
        dataSetList->SetToolTip(_("Text Entry"));
    itemStaticBoxSizer4->Add(dataSetList, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
    dataSetList->Raise();
   ///////////////////////////////////////////////////
    wxBoxSizer* dataButtonsSizer = new wxBoxSizer(wxHORIZONTAL);

    itemStaticBoxSizer4->Add(dataButtonsSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    wxButton* loadButton = new wxButton( itemDialog1, ID_ADD_DATASET, _("Add DataSet"), wxDefaultPosition, wxDefaultSize, 0 );
    dataButtonsSizer->Add(loadButton);

    wxButton* deleteButton = new wxButton( itemDialog1, ID_DELETE_DATASET, _("Delete DataSet"), wxDefaultPosition, wxDefaultSize, 0 );
    dataButtonsSizer->Add(deleteButton);

   ///////////////////////////////////////////////////////
    itemStaticBoxSizer6Static = new wxStaticBox(itemDialog1, wxID_ANY, 
                                                _("DataSet Filename"));
    wxStaticBoxSizer* itemStaticBoxSizer6 = new wxStaticBoxSizer(itemStaticBoxSizer6Static, wxHORIZONTAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer6, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);

    dataSetTextEntry = new wxTextCtrl( itemDialog1, ID_TEXTCTRL, 
                                 _("Enter Filename Here-->"), wxDefaultPosition, 
                                 wxDefaultSize, wxTE_READONLY );
    dataSetTextEntry->SetHelpText(_("Text Entry"));
    if (ShowToolTips())
        dataSetTextEntry->SetToolTip(_("Text Entry"));
    itemStaticBoxSizer6->Add(dataSetTextEntry, 1, wxALIGN_CENTER_VERTICAL|wxALL|wxEXPAND, 5);

    dataSetOpenButton = new wxButton( itemDialog1, ID_BUTTON, 
                                       _("Open"), wxDefaultPosition, 
                                       wxDefaultSize, 0 );
    dataSetOpenButton->SetHelpText(_("Load Data"));
    if (ShowToolTips())
        dataSetOpenButton->SetToolTip(_("Load Data"));
    itemStaticBoxSizer6->Add(dataSetOpenButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
   ///////////////////////////////////////////////////////
    itemStaticBoxSizer9Static = new wxStaticBox(itemDialog1, wxID_ANY, _("Precomputed Data Directory"));
    wxStaticBoxSizer* itemStaticBoxSizer9 = new wxStaticBoxSizer(itemStaticBoxSizer9Static, wxHORIZONTAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer9, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);

    preComputDirTextEntry = new wxTextCtrl( itemDialog1, ID_TEXTCTRL2, _("Enter Dir Here-->"), wxDefaultPosition, wxDefaultSize, wxTE_READONLY );
    preComputDirTextEntry->SetHelpText(_("Text Entry"));
    if (ShowToolTips())
        preComputDirTextEntry->SetToolTip(_("Text Entry"));
    itemStaticBoxSizer9->Add(preComputDirTextEntry, 1, wxALIGN_CENTER_VERTICAL|wxALL|wxEXPAND, 5);

    preComputeOpenButton = new wxButton( itemDialog1, ID_BUTTON3, _("Open"), wxDefaultPosition, wxDefaultSize, 0 );
    if (ShowToolTips())
        preComputeOpenButton->SetToolTip(_("Precomputed Data Dir"));
    itemStaticBoxSizer9->Add(preComputeOpenButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
    preComputDirTextEntry->Raise();
   ///////////////////////////////////////////////////////
    itemStaticBoxSizer12Static = new wxStaticBox(itemDialog1, wxID_ANY, _("Surface Data Directory"));
    wxStaticBoxSizer* itemStaticBoxSizer12 = new wxStaticBoxSizer(itemStaticBoxSizer12Static, wxHORIZONTAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer12, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);

    surfaceDataText = new wxTextCtrl( itemDialog1, ID_TEXTCTRL3, 
                                       _("Enter Dir Here-->"), wxDefaultPosition, 
                                       wxDefaultSize, wxTE_READONLY );
    surfaceDataText->SetHelpText(_("Text Entry"));
    if (ShowToolTips())
        surfaceDataText->SetToolTip(_("Text Entry"));
    itemStaticBoxSizer12->Add(surfaceDataText, 1, wxALIGN_CENTER_VERTICAL|wxALL|wxEXPAND, 5);

    surfaceDataOpenButton = new wxButton( itemDialog1, ID_BUTTON4, 
                                             _("Open"), wxDefaultPosition, 
                                             wxDefaultSize, 0 );
    surfaceDataOpenButton->SetHelpText(_("Surface Data Dir"));
    if (ShowToolTips())
        surfaceDataOpenButton->SetToolTip(_("Surface Data Dir"));
    itemStaticBoxSizer12->Add(surfaceDataOpenButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
    surfaceDataText->Raise();
   ///////////////////////////////////////////////////////
    itemStaticBoxSizer15Static = new wxStaticBox(itemDialog1, wxID_ANY, _("DataSet Attributes"));
    wxStaticBoxSizer* itemStaticBoxSizer15 = new wxStaticBoxSizer(itemStaticBoxSizer15Static, wxHORIZONTAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer15, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);

    transformButton = new wxButton( itemDialog1, ID_BUTTON6, _("Transform"), wxDefaultPosition, wxDefaultSize, 0 );
    transformButton->SetHelpText(_("Transform GUI"));
    if (ShowToolTips())
        transformButton->SetToolTip(_("Transform GUI"));
    itemStaticBoxSizer15->Add(transformButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    itemStaticBoxSizer15->Add(65, 5, 0, wxALIGN_CENTER_VERTICAL|wxALL|wxEXPAND, 5);

    scalarButton = new wxButton( itemDialog1, ID_BUTTON5, _("Scalar"), wxDefaultPosition, wxDefaultSize, 0 );
    scalarButton->SetHelpText(_("Future Use"));
    if (ShowToolTips())
        scalarButton->SetToolTip(_("Future Use"));
    scalarButton->Enable(false);
    itemStaticBoxSizer15->Add(scalarButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
    transformButton->Raise();
   ///////////////////////////////////////////////////////
    itemStaticBoxSizer19Static = new wxStaticBox(itemDialog1, wxID_ANY, _("Volumetric Data Directories"));
    wxStaticBoxSizer* itemStaticBoxSizer19 = new wxStaticBoxSizer(itemStaticBoxSizer19Static, wxVERTICAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer19, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);

    wxBoxSizer* itemBoxSizer20 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer19->Add(itemBoxSizer20, 0, wxALIGN_LEFT|wxEXPAND, 5);

    itemTextCtrl21 = new wxTextCtrl( itemDialog1, ID_TEXTCTRL1, _T("Enter Dir Here-->"), wxDefaultPosition, wxDefaultSize, wxTE_READONLY );
    itemBoxSizer20->Add(itemTextCtrl21, 1, wxALIGN_CENTER_VERTICAL|wxALL|wxEXPAND, 5);

    itemButton22 = new wxButton( itemDialog1, ID_BUTTON2, _("Open"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer20->Add(itemButton22, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxScrolledWindow* itemScrolledWindow23 = new wxScrolledWindow( itemDialog1, ID_SCROLLEDWINDOW1, wxDefaultPosition, wxDefaultSize, wxHSCROLL|wxVSCROLL );
    itemStaticBoxSizer19->Add(itemScrolledWindow23, 1, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
    itemScrolledWindow23->SetScrollbars(1, 1, 0, 0);

    wxString* itemListBox24Strings = NULL;
    itemListBox24 = new wxListBox( itemScrolledWindow23, ID_LISTBOX, wxDefaultPosition, wxDefaultSize, 0, itemListBox24Strings, wxLB_SINGLE );
    itemTextCtrl21->Raise();
   ///////////////////////////////////////////////////////
    wxStdDialogButtonSizer* itemStdDialogButtonSizer25 = new wxStdDialogButtonSizer;

    itemBoxSizer2->Add(itemStdDialogButtonSizer25, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    wxButton* itemButton26 = new wxButton( itemDialog1, wxID_OK, _("Load"), wxDefaultPosition, wxDefaultSize, 0 );
    //itemButton26->SetDefault();
    itemStdDialogButtonSizer25->AddButton(itemButton26);

    wxButton* itemButton27 = new wxButton( itemDialog1, wxID_CANCEL, _("&Cancel"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStdDialogButtonSizer25->AddButton(itemButton27);

    itemStdDialogButtonSizer25->Realize();

////@end DataSetLoaderUI content construction
}
///////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::InitializeWidgets( void )
{
   if ( veModel )
   {
      size_t numParamBlocks = veModel->GetNumberOfInformationPackets();

      for ( size_t i = 0; i < numParamBlocks; ++i )
      {
		  dataSetList->Append( wxString( veModel->GetInformationPacket( i )->GetName().c_str(), wxConvUTF8 ) );
      }
   }
}
///////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::SetTextCtrls( void )
{
   if ( paramBlock )
   {
      // clear the listbox before we enter the loop so that 
      // we can add the appropriate entries if need be
      itemListBox24->Clear();
      dataSetTextEntry->SetValue( _("Enter Filename Here-->") );
      surfaceDataText->SetValue( _("Enter Dir Here-->") );
      preComputDirTextEntry->SetValue( _("Enter Dir Here-->") );
      itemTextCtrl21->SetValue( _("Enter Dir Here-->") );

      size_t numProperties = paramBlock->GetNumberOfProperties();
      for ( size_t i = 0; i < numProperties; ++i )
      {
         VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( i );
         if ( tempDVP->GetDataName() == "VTK_TEXTURE_DIR_PATH" )
         {
            //clear...then append
            itemListBox24->Append( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
            itemTextCtrl21->SetValue( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
         }
         else if ( tempDVP->GetDataName() == "VTK_DATA_FILE" )
         {
            //clear...then append
            dataSetTextEntry->SetValue( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
         }
         else if ( tempDVP->GetDataName() == "VTK_SURFACE_DIR_PATH" )
         {
            //clear...then append
            surfaceDataText->SetValue( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
         }
         else if ( tempDVP->GetDataName() == "VTK_PRECOMPUTED_DIR_PATH" )
         {
            //clear...then append
            preComputDirTextEntry->SetValue( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
         }
      }
   }
}

/*!
 * Should we show tooltips?
 */

bool DataSetLoaderUI::ShowToolTips()
{
    return true;
}

/*!
 * Get bitmap resources
 */

wxBitmap DataSetLoaderUI::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin DataSetLoaderUI bitmap retrieval
    wxUnusedVar(name);
    return wxNullBitmap;
////@end DataSetLoaderUI bitmap retrieval
}

/*!
 * Get icon resources
 */

wxIcon DataSetLoaderUI::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin DataSetLoaderUI icon retrieval
    wxUnusedVar(name);
    return wxNullIcon;
////@end DataSetLoaderUI icon retrieval
}
/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON
 */

void DataSetLoaderUI::OnButtonClick( wxCommandEvent& WXUNUSED(event) )
{
   //Load a vtk file
////@begin wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON in DataSetLoaderUI.
    // Before editing this code, remove the block markers.   
   wxPoint pos(0,0);
   wxFileDialog dialog(this,
                        _T("Open file"), 
                        _T(""), 
                        _T(""),
                        _T("VTK DataSet Files (*.vtk;*.vtu;*.vts;*.vti)|*.vtk;*.vtu;*.vts;*.vti;|StarCD Parameter File (*.param)|*.param;|EnSight(*.ens;*.case)|*.ens;*.case;|MFIX (*.mfix)|*.mfix;|Fluent (*.cas)|*.cas;|AVS (*.avs)|*.avs;|Dicom (*.dcm)|*.dcm;|All Files (*.*)|*.*"),
                        wxOPEN|wxFILE_MUST_EXIST,
                        wxDefaultPosition); 

   if ( dialog.ShowModal() == wxID_OK ) 
   {
      wxFileName datasetFilename( dialog.GetPath() );
      datasetFilename.MakeRelativeTo( ::wxGetCwd() );
      wxString relativeDataSetPath( datasetFilename.GetFullPath() );
      relativeDataSetPath.Replace( _("\\"), _("/"), true );
      dataSetTextEntry->SetValue( relativeDataSetPath );
      VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( "VTK_DATA_FILE" );
      if ( !tempDVP )
      {
         tempDVP = paramBlock->GetProperty( -1 );
      }
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( relativeDataSetPath.c_str() ) ) );
      tempDVP->SetData( "VTK_DATA_FILE", tempStr );
   }
////@end wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON in DataSetLoaderUI. 
}


/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON4
 */

void DataSetLoaderUI::OnButton4Click( wxCommandEvent& event )
{
   //Launch the surface data dir 
   //Launch the precomputed data dir 
////@begin wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON4 in DataSetLoaderUI.
   wxPoint pos(0,0);
   wxFileDialog dialog(this,
                        _T("Open file"), 
                        _T(""), 
                        _T(""),
                        _T("VTK Surface Files (*.vtk;*.vtp)|*.vtk;*.vtp;"),
                        wxOPEN|wxFILE_MUST_EXIST,
                        wxDefaultPosition); 

   if ( dialog.ShowModal() == wxID_OK ) 
   {
      wxFileName surfaceDir( dialog.GetPath() );
      surfaceDir.MakeRelativeTo( ::wxGetCwd() );
      if ( event.GetId() == ID_BUTTON4 )
      {
         wxString relativeSurfaceDirPath( surfaceDir.GetPath() );
         relativeSurfaceDirPath.Replace( _("\\"), _("/"), true );
         surfaceDataText->SetValue( relativeSurfaceDirPath );
         VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( "VTK_SURFACE_DIR_PATH" );
         if ( !tempDVP )
         {
            tempDVP = paramBlock->GetProperty( -1 );
         }
         std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( relativeSurfaceDirPath.c_str() ) ) );
         tempDVP->SetData( "VTK_SURFACE_DIR_PATH", tempStr );
      }
      else if ( event.GetId() == ID_BUTTON3 )
      {
         wxString relativePrecomputedDirPath( surfaceDir.GetPath() );
         relativePrecomputedDirPath.Replace( _("\\"), _("/"), true );
         preComputDirTextEntry->SetValue( relativePrecomputedDirPath );
         VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" );
         if ( !tempDVP )
         {
            tempDVP = paramBlock->GetProperty( -1 );
         }
         std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( relativePrecomputedDirPath.c_str() ) ) );
         tempDVP->SetData( "VTK_PRECOMPUTED_DIR_PATH", tempStr );
         preComputDirTextEntry->SetValue( relativePrecomputedDirPath );
      }
   }
////@end wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON4 in DataSetLoaderUI. 
}

/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON6
 */

void DataSetLoaderUI::OnButton6Click( wxCommandEvent& WXUNUSED(event) )
{
   // Launch the transform UI
////@begin wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON6 in DataSetLoaderUI.
    // Before editing this code, remove the block markers.
   wxDialog transformDialog( this, 
                              ::wxNewId(), 
                              _("Transform Input Window"),
                              wxDefaultPosition, 
                              wxDefaultSize, 
                              wxCAPTION|wxCLOSE_BOX|wxSYSTEM_MENU|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX|wxMAXIMIZE_BOX|wxMINIMIZE_BOX );

   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
   //wxBoxSizer* notebookSizer = new wxBoxSizer(wxVERTICAL);
   //wxBoxSizer* bottomRow = new wxBoxSizer(wxHORIZONTAL);
   VE_Conductor::GUI_Utilities::TransformUI* transformPanel = new VE_Conductor::GUI_Utilities::TransformUI( &transformDialog, _("Transform Input"), paramBlock->GetTransform());

   if ( paramBlock )
   {
      mainSizer->Add( transformPanel, -1, wxEXPAND|wxALIGN_CENTER_HORIZONTAL );

   }
   else
   {
      mainSizer->Add( new VE_Conductor::GUI_Utilities::TransformUI( &transformDialog, _("Transform Input"), 0 ), 
                  -1, wxEXPAND|wxALIGN_CENTER_HORIZONTAL );
   }

   mainSizer->Add( transformDialog.CreateStdDialogButtonSizer( wxOK|wxCANCEL ), -1, wxEXPAND|wxALIGN_CENTER_HORIZONTAL );
   //set this flag and let wx handle alignment  
   transformDialog.SetAutoLayout(true);

   //assign the group to the panel              
   transformDialog.SetSizer(mainSizer);
   mainSizer->Fit( &transformDialog ); 

   //set parameterblock unique (GUID) id for transform GUI
   transformPanel->SetParamBlockID( paramBlock->GetID() );
   transformPanel->SetParamBlockTransform( paramBlock->GetTransform() );

   if ( transformDialog.ShowModal() == wxID_OK )
   {
      ;
   }
////@end wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON6 in DataSetLoaderUI. 
}

/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON2
 */

void DataSetLoaderUI::OnButton2Click( wxCommandEvent& WXUNUSED(event) )
{
   //Load data for the texturebased vis
////@begin wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON2 in DataSetLoaderUI.
    // Before editing this code, remove the block markers.
   wxString cwd( ::wxGetCwd() );
   int answer = 0;
   do
   {
      wxPoint pos(0,0);
      wxFileDialog dialog(this,
                           _T("Open file"), 
                           _T(""), 
                           _T(""),
                           _T("VTK Texture Files (*.vti)|*.vti;"),
                           wxOPEN|wxFILE_MUST_EXIST|wxCHANGE_DIR,
                           wxDefaultPosition); 

      if ( dialog.ShowModal() == wxID_OK ) 
      {
         wxFileName textureDir( dialog.GetPath() );
         textureDir.MakeRelativeTo( cwd );
         wxString relativeTextureDirPath( textureDir.GetPath() );
         relativeTextureDirPath.Replace( _("\\"), _("/"), true );
         itemTextCtrl21->SetValue( relativeTextureDirPath );
         textureDirs.insert( relativeTextureDirPath );
      }
      wxMessageDialog promptDlg( this, 
                        _("Are you done selecting texture files?"), 
                        _("Texture Chooser"), 
                        wxYES_NO|wxNO_DEFAULT|wxICON_QUESTION, 
                        wxDefaultPosition);
      answer = promptDlg.ShowModal();
   }
   while ( answer == wxID_NO );
   wxFileName::SetCwd( cwd );
   
   std::set< wxString >::iterator iter;
   for ( iter = textureDirs.begin(); iter != textureDirs.end(); ++iter )
   {
      VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( -1 );
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( (*iter).c_str() ) ) );
      tempDVP->SetData( "VTK_TEXTURE_DIR_PATH", tempStr );
      wxString* dirString = new wxString( (*iter) );
      itemListBox24->InsertItems( 1, dirString, 0 );
   }
////@end wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON2 in DataSetLoaderUI. 
}

/*!
 * wxEVT_COMMAND_LISTBOX_SELECTED event handler for ID_LISTBOX
 */
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnListboxSelected( wxCommandEvent& WXUNUSED(event) )
{
   // When the list box is selected
////@begin wxEVT_COMMAND_LISTBOX_SELECTED event handler for ID_LISTBOX in DataSetLoaderUI.
   size_t numProperties = paramBlock->GetNumberOfProperties();
   for ( size_t i = 0; i < numProperties; ++i )
   {
      VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( i );
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( itemListBox24->GetStringSelection().c_str() ) ) );
      if ( ( tempDVP->GetDataName() == "VTK_TEXTURE_DIR_PATH" ) && 
            ( tempDVP->GetDataString() == tempStr ) 
         )
      {
         paramBlock->RemoveProperty( i );
         itemListBox24->Delete( itemListBox24->GetSelection() );
         return;
      }
   }
////@end wxEVT_COMMAND_LISTBOX_SELECTED event handler for ID_LISTBOX in DataSetLoaderUI. 
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnInformationPacketChange( wxCommandEvent& WXUNUSED(event) )
{
   /// wxEVT_COMMAND_COMBOBOX_SELECTED event handler for ID_LISTBOX
   // When we change the combobox the newly selected item
   // should be queried and all other widgets should be update with
   // appropriate info

   wxString selection = dataSetList->GetStringSelection();
std::cout<<wxConvCurrent->cWX2MB( selection )<<std::endl;
   if ( selection == wxString( _("Add Dataset") ) )
   {
      lastAddition = dataSetList->Append( _("Type new data block name here") );
      dataSetList->SetStringSelection( _("Type new data block name here") );
      paramBlock = 0;
      EnableUI( false );
      return;
   }

   if ( selection == wxString( _("Unselect") ) )
   {
      paramBlock = 0;
      // disable all other guis
      EnableUI( false );
      return;
   }

   size_t numParamBlocks = veModel->GetNumberOfInformationPackets();
std::cout<<"NUMBER OF PACKETS: "<<numParamBlocks<<std::endl;
   for ( size_t i = 0; i < numParamBlocks; ++i )
   {
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( selection.c_str() ) ) );
      if ( veModel->GetInformationPacket( i )->GetName() == tempStr )
      {
         paramBlock = veModel->GetInformationPacket( i );
         EnableUI( true );
         SetTextCtrls();
         break;
      }
   }
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnInformationPacketAdd( wxCommandEvent& WXUNUSED(event) )
{
    /// wxEVT_COMMAND_TEXT_ENTER event handler for ID_LISTBOX
   // When enter is pushed on the combox and new entry is specifiy and
   // the appropriate widgets should be updated
   wxTextEntryDialog newDataSetName(this, 
                                wxString( _("New Dataset") ),
                                        wxString( _("Enter name for new Dataset:") ),
                                        wxString( _("Dataset") ),wxOK);

   newDataSetName.CentreOnParent();
   newDataSetName.ShowModal();
   std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( newDataSetName.GetValue().GetData() ) ) );
   if(DatasetExists( tempStr ) )
   {
      wxMessageBox( _("Data with this name is already loaded."), 
                          newDataSetName.GetValue(), wxOK | wxICON_INFORMATION );
                              return;
   }
   else
   {
      _availableDatasets.Add(newDataSetName.GetValue());
      dataSetList->Append(newDataSetName.GetValue());
      dataSetList->SetStringSelection(newDataSetName.GetValue());
      
      paramBlock = veModel->GetInformationPacket( -1 );
      tempStr = ( static_cast< const char* >( wxConvCurrent->cWX2MB( newDataSetName.GetValue() ) ) );
      paramBlock->SetName( tempStr );
      paramBlock->SetBlockId( ::wxNewId() );
      EnableUI( true );
   }
  
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnInformationPacketChangeName( wxCommandEvent& WXUNUSED(event) )
{
    /// wxEVT_COMMAND_TEXT_UPDATED event handler for ID_LISTBOX
   // If any text is changed with the name of a information packet then
   // then the information packet should change as well in veModel.
   if ( paramBlock )
   {
      int selection = dataSetList->GetSelection();
      dataSetList->SetString( selection, dataSetList->GetValue() );
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( dataSetList->GetValue().c_str() ) ) );
      paramBlock->SetName( tempStr );
      //std::cout << "OnInformationPacketChangeName " << paramBlock->GetName() << std::endl;
   }
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::EnableUI( bool flag )
{
   if(dataSetList->GetCount())
   {
      dataSetList->Enable(true);
   }
   else
   {
      dataSetList->Enable(false);
   }
   dataSetOpenButton->Enable( flag );
   preComputeOpenButton->Enable( flag );
   surfaceDataOpenButton->Enable( flag );
   transformButton->Enable( flag );
   itemButton22->Enable( flag );
}
/////////////////////////////////////////////////////////////
bool DataSetLoaderUI::DatasetExists(std::string name)
{
   for(size_t i = 0; i < _availableDatasets.GetCount(); i++)
   {
      if( wxString( name.c_str(), wxConvUTF8 ) == _availableDatasets[i])
         return true;
   }
   return false;
}
/////////////////////////////////////////////////////////////
std::string DataSetLoaderUI::GetActiveDataSetName()
{
   return paramBlock->GetName();
}
/////////////////////////////////////////////////////////////
VE_XML::ParameterBlock* DataSetLoaderUI::GetParamBlock()
{
   return paramBlock;
}
/////////////////////////////////////////////////////////////
VE_XML::VE_Model::Model* DataSetLoaderUI::GetModel()
{
   return veModel;
}
