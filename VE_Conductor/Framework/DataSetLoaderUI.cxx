/////////////////////////////////////////////////////////////////////////////
// Name:        DataSetLoaderUI.cxx
// Purpose:     
// Author:      
// Modified by: 
// Created:     11/04/2006 20:56:53
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

// Generated by DialogBlocks (unregistered), 11/04/2006 20:56:53

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma implementation "DataSetLoaderUI.h"
#endif

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
//#include "wx/wx.h"
#endif

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
////@end includes

#include "VE_Conductor/Framework/DataSetLoaderUI.h"
#include "VE_Conductor/Framework/TransformUI.h"

#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Model/Model.h"

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
   EVT_LISTBOX( ID_LISTBOX, DataSetLoaderUI::OnListboxSelected )
   EVT_COMBOBOX( ID_INFORMATION_PACKET_LIST, DataSetLoaderUI::OnInformationPacketChange ) // this will refresh all the widgets for the given dataset
   EVT_TEXT( ID_INFORMATION_PACKET_CHANGE_NAME, DataSetLoaderUI::OnInformationPacketChangeName ) // change the name of a given data set, will also need to change the name seen by xplorer
   EVT_TEXT_ENTER( ID_INFORMATION_PACKET_ADD_NAME, DataSetLoaderUI::OnInformationPacketAdd ) // add a name to the list once enter is pushed, also add new information block
////@end DataSetLoaderUI event table entries

END_EVENT_TABLE()

/*!
 * DataSetLoaderUI constructors
 */

DataSetLoaderUI::DataSetLoaderUI( )
{
   paramBlock = 0;
}

DataSetLoaderUI::DataSetLoaderUI( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style, VE_Model::Model* veModel )
{
   Create(parent, id, caption, pos, size, style, veModel );
}

/*!
 * DataSetLoaderUI creator
 */

bool DataSetLoaderUI::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style, VE_Model::Model* veModel )
{
////@begin DataSetLoaderUI member initialisation
   this->veModel = veModel;
   dataSetList = NULL;
   dataSetTextEntry = NULL;
   dataSetOpenButton = NULL;
   preComputDirTextEntry = NULL;
   preComputeOpenButton = NULL;
   surfaceDataText = NULL;
   surfaceDataOpenButton = NULL;
   transformButton = NULL;
   scalarButton = NULL;
////@end DataSetLoaderUI member initialisation

////@begin DataSetLoaderUI creation
   SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
   wxDialog::Create( parent, id, caption, pos, size, style );

   CreateControls();
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
   Centre();
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

    wxScrolledWindow* itemScrolledWindow3 = new wxScrolledWindow( itemDialog1, ID_SCROLLEDWINDOW, wxDefaultPosition, wxSize(300, 500), wxHSCROLL|wxVSCROLL );
    itemBoxSizer2->Add(itemScrolledWindow3, 0, wxALIGN_LEFT|wxALL, 5);
    itemScrolledWindow3->SetScrollbars(1, 1, 0, 0);

    wxStaticBox* itemStaticBoxSizer4Static = new wxStaticBox(itemScrolledWindow3, wxID_ANY, _("Steady State Data"));
    wxStaticBoxSizer* itemStaticBoxSizer4 = new wxStaticBoxSizer(itemStaticBoxSizer4Static, wxVERTICAL);
    itemScrolledWindow3->SetSizer(itemStaticBoxSizer4);

   ///////////////////////////////////////////////////////
    wxString* dataSetListStrings = NULL;
    dataSetList = new wxComboBox( itemScrolledWindow3, ID_COMBOBOX, _("Type New DataSet Name Here"), wxDefaultPosition, wxSize(250, -1), 0, dataSetListStrings, wxCB_DROPDOWN );
    dataSetList->SetStringSelection(_("Type New DataSet Name Here"));
    dataSetList->SetHelpText(_("Text Entry"));
    if (ShowToolTips())
        dataSetList->SetToolTip(_("Text Entry"));
    itemStaticBoxSizer4->Add(dataSetList, 0, wxALIGN_LEFT|wxALL, 5);

   ///////////////////////////////////////////////////////
    wxStaticBox* itemStaticBoxSizer6Static = new wxStaticBox(itemScrolledWindow3, wxID_ANY, _("DataSet Filename"));
    wxStaticBoxSizer* itemStaticBoxSizer6 = new wxStaticBoxSizer(itemStaticBoxSizer6Static, wxHORIZONTAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer6, 0, wxALIGN_LEFT|wxALL, 5);

    dataSetTextEntry = new wxTextCtrl( itemScrolledWindow3, ID_TEXTCTRL, _("Enter Filename Here"), wxDefaultPosition, wxSize(150, -1), 0 );
    dataSetTextEntry->SetHelpText(_("Text Entry"));
    if (ShowToolTips())
        dataSetTextEntry->SetToolTip(_("Text Entry"));
    itemStaticBoxSizer6->Add(dataSetTextEntry, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    dataSetOpenButton = new wxButton( itemScrolledWindow3, ID_BUTTON, _("Open"), wxDefaultPosition, wxDefaultSize, 0 );
    dataSetOpenButton->SetHelpText(_("Load Data"));
    if (ShowToolTips())
        dataSetOpenButton->SetToolTip(_("Load Data"));
    itemStaticBoxSizer6->Add(dataSetOpenButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

   ///////////////////////////////////////////////////////
    wxStaticBox* itemStaticBoxSizer9Static = new wxStaticBox(itemScrolledWindow3, wxID_ANY, _("Precomputed Data Directory"));
    wxStaticBoxSizer* itemStaticBoxSizer9 = new wxStaticBoxSizer(itemStaticBoxSizer9Static, wxHORIZONTAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer9, 0, wxALIGN_LEFT|wxALL, 5);

    preComputDirTextEntry = new wxTextCtrl( itemScrolledWindow3, ID_TEXTCTRL2, _("Enter Dir Here"), wxDefaultPosition, wxSize(150, -1), 0 );
    preComputDirTextEntry->SetHelpText(_("Text Entry"));
    if (ShowToolTips())
        preComputDirTextEntry->SetToolTip(_("Text Entry"));
    itemStaticBoxSizer9->Add(preComputDirTextEntry, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    preComputeOpenButton = new wxButton( itemScrolledWindow3, ID_BUTTON3, _("Open"), wxDefaultPosition, wxDefaultSize, 0 );
    if (ShowToolTips())
        preComputeOpenButton->SetToolTip(_("Precomputed Data Dir"));
    itemStaticBoxSizer9->Add(preComputeOpenButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

   ///////////////////////////////////////////////////////
    wxStaticBox* itemStaticBoxSizer12Static = new wxStaticBox(itemScrolledWindow3, wxID_ANY, _("Surface Data Directory"));
    wxStaticBoxSizer* itemStaticBoxSizer12 = new wxStaticBoxSizer(itemStaticBoxSizer12Static, wxHORIZONTAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer12, 0, wxALIGN_LEFT|wxALL, 5);

    surfaceDataText = new wxTextCtrl( itemScrolledWindow3, ID_TEXTCTRL3, _("Enter Dir Here"), wxDefaultPosition, wxSize(150, -1), 0 );
    surfaceDataText->SetHelpText(_("Text Entry"));
    if (ShowToolTips())
        surfaceDataText->SetToolTip(_("Text Entry"));
    itemStaticBoxSizer12->Add(surfaceDataText, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    surfaceDataOpenButton = new wxButton( itemScrolledWindow3, ID_BUTTON4, _("Open"), wxDefaultPosition, wxDefaultSize, 0 );
    surfaceDataOpenButton->SetHelpText(_("Surface Data Dir"));
    if (ShowToolTips())
        surfaceDataOpenButton->SetToolTip(_("Surface Data Dir"));
    itemStaticBoxSizer12->Add(surfaceDataOpenButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

   ///////////////////////////////////////////////////////
    wxStaticBox* itemStaticBoxSizer15Static = new wxStaticBox(itemScrolledWindow3, wxID_ANY, _("DataSet Attributes"));
    wxStaticBoxSizer* itemStaticBoxSizer15 = new wxStaticBoxSizer(itemStaticBoxSizer15Static, wxHORIZONTAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer15, 0, wxALIGN_LEFT|wxALL, 5);

    transformButton = new wxButton( itemScrolledWindow3, ID_BUTTON6, _("Transform"), wxDefaultPosition, wxDefaultSize, 0 );
    transformButton->SetHelpText(_("Transform GUI"));
    if (ShowToolTips())
        transformButton->SetToolTip(_("Transform GUI"));
    itemStaticBoxSizer15->Add(transformButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    itemStaticBoxSizer15->Add(65, 5, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    scalarButton = new wxButton( itemScrolledWindow3, ID_BUTTON5, _("Scalar"), wxDefaultPosition, wxDefaultSize, 0 );
    scalarButton->SetHelpText(_("Future Use"));
    if (ShowToolTips())
        scalarButton->SetToolTip(_("Future Use"));
    scalarButton->Enable(false);
    itemStaticBoxSizer15->Add(scalarButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

   ///////////////////////////////////////////////////////
    wxStaticBox* itemStaticBoxSizer19Static = new wxStaticBox(itemScrolledWindow3, wxID_ANY, _("Volumetric Data Directories"));
    wxStaticBoxSizer* itemStaticBoxSizer19 = new wxStaticBoxSizer(itemStaticBoxSizer19Static, wxVERTICAL);
    itemStaticBoxSizer4->Add(itemStaticBoxSizer19, 0, wxALIGN_LEFT|wxALL, 5);

    wxBoxSizer* itemBoxSizer20 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer19->Add(itemBoxSizer20, 0, wxALIGN_LEFT, 5);

    wxTextCtrl* itemTextCtrl21 = new wxTextCtrl( itemScrolledWindow3, ID_TEXTCTRL1, _T(""), wxDefaultPosition, wxSize(150, -1), 0 );
    itemBoxSizer20->Add(itemTextCtrl21, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* itemButton22 = new wxButton( itemScrolledWindow3, ID_BUTTON2, _("Open"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer20->Add(itemButton22, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxScrolledWindow* itemScrolledWindow23 = new wxScrolledWindow( itemScrolledWindow3, ID_SCROLLEDWINDOW1, wxDefaultPosition, wxSize(-1, 40), wxHSCROLL|wxVSCROLL );
    itemStaticBoxSizer19->Add(itemScrolledWindow23, 0, wxALIGN_LEFT|wxALL, 5);
    itemScrolledWindow23->SetScrollbars(1, 1, 0, 0);

    wxString* itemListBox24Strings = NULL;
    wxListBox* itemListBox24 = new wxListBox( itemScrolledWindow23, ID_LISTBOX, wxDefaultPosition, wxSize(225, -1), 0, itemListBox24Strings, wxLB_SINGLE );

   ///////////////////////////////////////////////////////
    wxStdDialogButtonSizer* itemStdDialogButtonSizer25 = new wxStdDialogButtonSizer;

    itemBoxSizer2->Add(itemStdDialogButtonSizer25, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    wxButton* itemButton26 = new wxButton( itemDialog1, wxID_OK, _("Load"), wxDefaultPosition, wxDefaultSize, 0 );
    itemButton26->SetDefault();
    itemStdDialogButtonSizer25->AddButton(itemButton26);

    wxButton* itemButton27 = new wxButton( itemDialog1, wxID_CANCEL, _("&Cancel"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStdDialogButtonSizer25->AddButton(itemButton27);

    itemStdDialogButtonSizer25->Realize();

////@end DataSetLoaderUI content construction
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
                        _T("VTK DataSet Files (*.vtk)|*.vtk;|VTK XML Unstructured Files (*.vtu)|*.vtu;|VTK XML Structured Files (*.vts)|*.vts;|VTK XML Image Files (*.vti)|*.vti;"),
                        wxOPEN|wxFILE_MUST_EXIST,
                        wxDefaultPosition); 

   if ( dialog.ShowModal() == wxID_OK ) 
   {
      wxFileName datasetFilename( dialog.GetPath() );
      datasetFilename.MakeRelativeTo( ::wxGetCwd(), wxPATH_NATIVE );
      wxString relativeDataSetPath( wxString("./") + datasetFilename.GetFullPath() );
      dataSetTextEntry->SetValue( relativeDataSetPath );
      VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( "VTK_DATA_FILE" );
      tempDVP->SetData( "VTK_DATA_FILE", relativeDataSetPath.c_str() );
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
      surfaceDir.MakeRelativeTo( ::wxGetCwd(), wxPATH_NATIVE );
      if ( event.GetId() == ID_BUTTON4 )
      {
         wxString relativeSurfaceDirPath( wxString("./") + surfaceDir.GetPath() );
         surfaceDataText->SetValue( relativeSurfaceDirPath );
         VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( "VTK_SURFACE_DIR_PATH" );
         tempDVP->SetData( "VTK_SURFACE_DIR_PATH", relativeSurfaceDirPath.c_str() );
      }
      else if ( event.GetId() == ID_BUTTON3 )
      {
         wxString relativePrecomputedDirPath( wxString("./") + surfaceDir.GetPath() );
         preComputDirTextEntry->SetValue( relativePrecomputedDirPath );
         VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" );
         tempDVP->SetData( "VTK_PRECOMPUTED_DIR_PATH", relativePrecomputedDirPath.c_str() );
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

   if ( paramBlock )
   {
      mainSizer->Add( new VE_Conductor::GUI_Utilities::TransformUI( &transformDialog, _("Transform Input"), paramBlock->GetTransform() ), 
                  -1, wxEXPAND|wxALIGN_CENTER_HORIZONTAL );
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
   if ( transformDialog.ShowModal() == wxID_OK )
   {
      ;
   }
////@end wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON6 in DataSetLoaderUI. 
}

/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON2
 */

void DataSetLoaderUI::OnButton2Click( wxCommandEvent& event )
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
         textureDir.MakeRelativeTo( cwd, wxPATH_NATIVE );
         wxString relativeTextureDirPath( textureDir.GetPath() );
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
      VE_XML::DataValuePair* tempDVP = paramBlock->GetProperty( "VTK_TEXTURE_DIR_PATH" );
      tempDVP->SetData( "VTK_SURFACE_DIR_PATH", (*iter).c_str() );
   }
////@end wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON2 in DataSetLoaderUI. 
}

/*!
 * wxEVT_COMMAND_LISTBOX_SELECTED event handler for ID_LISTBOX
 */
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnListboxSelected( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_LISTBOX_SELECTED event handler for ID_LISTBOX in DataSetLoaderUI.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_LISTBOX_SELECTED event handler for ID_LISTBOX in DataSetLoaderUI. 
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnInformationPacketChange( wxCommandEvent& event )
{
   /// wxEVT_COMMAND_COMBOBOX_SELECTED event handler for ID_LISTBOX
   // When we change the combox the newly slected item
   // should be queried and all other widgets should be update with
   // appropriate info
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnInformationPacketAdd( wxCommandEvent& event )
{
    /// wxEVT_COMMAND_TEXT_ENTER event handler for ID_LISTBOX
   // When enter is pushed on the combox and new entry is specifiy and
   // the appropriate widgets should be updated
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnInformationPacketChangeName( wxCommandEvent& event )
{
    /// wxEVT_COMMAND_TEXT_UPDATED event handler for ID_LISTBOX
   // If any text is changed with the name of a information packet then
   // then the information packet should change as well in veModel.
}
//////////////////////////////////////////////////////////////////////////////


