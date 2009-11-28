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
#include <wx/log.h>
#include <wx/checkbox.h>

#include <ves/conductor/util/DataSetLoaderUI.h>
#include <ves/conductor/util/TransformUI.h>

#include <ves/conductor/util/DataSetDataArrayChoiceDialog.h>

#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/OneDStringArray.h>

#include <ves/xplorer/util/cfdVTKFileHandler.h>

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
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( DataSetLoaderUI, wxDialog )

    ////@begin DataSetLoaderUI event table entries
    EVT_BUTTON( ID_BUTTON, DataSetLoaderUI::OnLoadFile )
    EVT_BUTTON( ID_BUTTON3, DataSetLoaderUI::OnLoadSurfaceFile )
    EVT_BUTTON( ID_BUTTON4, DataSetLoaderUI::OnLoadSurfaceFile )
    EVT_BUTTON( ID_BUTTON6, DataSetLoaderUI::OnTransformDataset )
    EVT_BUTTON( ID_BUTTON2, DataSetLoaderUI::OnLoadTextureFile )
    EVT_BUTTON( ID_ADD_DATASET, DataSetLoaderUI::OnInformationPacketAdd )
    EVT_BUTTON( ID_DELETE_DATASET, DataSetLoaderUI::OnDeleteDataset )
    EVT_LISTBOX_DCLICK( ID_LISTBOX, DataSetLoaderUI::OnListboxSelected )
    EVT_COMBOBOX( ID_COMBOBOX, DataSetLoaderUI::OnInformationPacketChange ) // this will refresh all the widgets for the given dataset
    //EVT_TEXT( ID_COMBOBOX, DataSetLoaderUI::OnInformationPacketChangeName ) // change the name of a given data set, will also need to change the name seen by xplorer
    //EVT_TEXT_ENTER( ID_COMBOBOX, DataSetLoaderUI::OnInformationPacketAdd ) // add a name to the list once enter is pushed, also add new information block
    EVT_CHECKBOX( ID_CREATE_SURFACE_WRAP, DataSetLoaderUI::OnSurfaceWrapCheckBox )
END_EVENT_TABLE()

/*!
 * DataSetLoaderUI constructors
 */

DataSetLoaderUI::DataSetLoaderUI( )
{
}

DataSetLoaderUI::DataSetLoaderUI( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style, ves::open::xml::model::ModelPtr veModel )
{
    Create( parent, id, caption, pos, size, style, veModel );
}

/*!
 * DataSetLoaderUI creator
 */

bool DataSetLoaderUI::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style, ves::open::xml::model::ModelPtr veModel )
{
////@begin DataSetLoaderUI member initialisation
    m_veModel = veModel;
    lastAddition = -1;
    dataSetList = NULL;
    dataSetTextEntry = NULL;
    dataSetOpenButton = NULL;
    preComputDirTextEntry = NULL;
    preComputeOpenButton = NULL;
    //surfaceDataText = NULL;
    //surfaceDataOpenButton = NULL;
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
    m_surfaceWrapChkBox = 0;
////@end DataSetLoaderUI member initialisation

////@begin DataSetLoaderUI creation
    //SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();

    GetSizer()->Fit( this );
    GetSizer()->SetSizeHints( this );
    Centre();
    InitializeWidgets();
    EnableUI( false );
    SetAutoLayout( true );
    Refresh();
    wxSize temp = GetSize();
    temp.SetHeight( temp.GetHeight() + 1 );
    temp.SetWidth( temp.GetWidth() + 1 );
    SetSize( temp );
    ///this->SetIcon( ve_icon32x32_xpm );
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

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer( wxVERTICAL );
    itemDialog1->SetSizer( itemBoxSizer2 );

    //wxScrolledWindow* itemScrolledWindow3 = new wxScrolledWindow( itemDialog1, ID_SCROLLEDWINDOW, wxDefaultPosition, wxDefaultSize, wxHSCROLL|wxVSCROLL );
    //itemBoxSizer2->Add(itemScrolledWindow3, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
    //itemScrolledWindow3->SetScrollbars(20, 10, 5, 10);

    //wxStaticBox* itemStaticBoxSizer4Static = new wxStaticBox(itemScrolledWindow3, wxID_ANY, _("Steady State Data"));
    wxStaticBox* itemStaticBoxSizer4Static = new wxStaticBox( itemDialog1, wxID_ANY, _( "Steady State Data" ) );
    wxStaticBoxSizer* itemStaticBoxSizer4 = new wxStaticBoxSizer( itemStaticBoxSizer4Static, wxVERTICAL );
    //itemScrolledWindow3->SetSizer(itemStaticBoxSizer4);
    itemBoxSizer2->Add( itemStaticBoxSizer4, 1, wxALL | wxEXPAND, 5 );

    ///////////////////////////////////////////////////////
    wxString* dataSetListStrings = NULL;
    //dataSetList = new wxComboBox( itemScrolledWindow3, ID_COMBOBOX, _("Type New DataSet Name Here"), wxDefaultPosition, wxSize(250, -1), 0, dataSetListStrings, wxCB_DROPDOWN );
    dataSetList = new wxComboBox( itemDialog1, ID_COMBOBOX, _( "" ), wxDefaultPosition, wxDefaultSize, 0, dataSetListStrings, wxCB_DROPDOWN );

    dataSetList->SetHelpText( _( "Text Entry" ) );
    if( ShowToolTips() )
        dataSetList->SetToolTip( _( "Text Entry" ) );
    itemStaticBoxSizer4->Add( dataSetList, 0, wxALIGN_LEFT | wxALL | wxEXPAND, 5 );
    dataSetList->Raise();
    ///////////////////////////////////////////////////
    wxBoxSizer* dataButtonsSizer = new wxBoxSizer( wxHORIZONTAL );

    itemStaticBoxSizer4->Add( dataButtonsSizer, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );
    wxButton* loadButton = new wxButton( itemDialog1, ID_ADD_DATASET, _( "Add DataSet" ), wxDefaultPosition, wxDefaultSize, 0 );
    dataButtonsSizer->Add( loadButton );

    wxButton* deleteButton = new wxButton( itemDialog1, ID_DELETE_DATASET, _( "Delete DataSet" ), wxDefaultPosition, wxDefaultSize, 0 );
    dataButtonsSizer->Add( deleteButton );

    ///////////////////////////////////////////////////////
    itemStaticBoxSizer6Static = new wxStaticBox( itemDialog1, wxID_ANY,
                                                 _( "DataSet Filename" ) );
    wxStaticBoxSizer* itemStaticBoxSizer6 = new wxStaticBoxSizer( itemStaticBoxSizer6Static, wxHORIZONTAL );
    itemStaticBoxSizer4->Add( itemStaticBoxSizer6, 0, wxALIGN_LEFT | wxALL | wxEXPAND, 5 );

    dataSetTextEntry = new wxTextCtrl( itemDialog1, ID_TEXTCTRL,
                                       _( "Enter Filename Here-->" ), wxDefaultPosition,
                                       wxDefaultSize, wxTE_READONLY );
    dataSetTextEntry->SetHelpText( _( "Text Entry" ) );
    if( ShowToolTips() )
        dataSetTextEntry->SetToolTip( _( "Text Entry" ) );
    itemStaticBoxSizer6->Add( dataSetTextEntry, 1, wxALIGN_CENTER_VERTICAL | wxALL | wxEXPAND, 5 );

    dataSetOpenButton = new wxButton( itemDialog1, ID_BUTTON,
                                      _( "Open" ), wxDefaultPosition,
                                      wxDefaultSize, 0 );
    dataSetOpenButton->SetHelpText( _( "Load Data" ) );
    if( ShowToolTips() )
        dataSetOpenButton->SetToolTip( _( "Load Data" ) );
    itemStaticBoxSizer6->Add( dataSetOpenButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
    ///////////////////////////////////////////////////////
    itemStaticBoxSizer9Static = new wxStaticBox( itemDialog1, wxID_ANY, _( "Precomputed Data Directory" ) );
    wxStaticBoxSizer* itemStaticBoxSizer9 = new wxStaticBoxSizer( itemStaticBoxSizer9Static, wxHORIZONTAL );
    itemStaticBoxSizer4->Add( itemStaticBoxSizer9, 0, wxALIGN_LEFT | wxALL | wxEXPAND, 5 );

    preComputDirTextEntry = new wxTextCtrl( itemDialog1, ID_TEXTCTRL2, _( "Enter Dir Here-->" ), wxDefaultPosition, wxDefaultSize, wxTE_READONLY );
    preComputDirTextEntry->SetHelpText( _( "Text Entry" ) );
    if( ShowToolTips() )
        preComputDirTextEntry->SetToolTip( _( "Text Entry" ) );
    itemStaticBoxSizer9->Add( preComputDirTextEntry, 1, wxALIGN_CENTER_VERTICAL | wxALL | wxEXPAND, 5 );

    preComputeOpenButton = new wxButton( itemDialog1, ID_BUTTON3, _( "Open" ), wxDefaultPosition, wxDefaultSize, 0 );
    if( ShowToolTips() )
        preComputeOpenButton->SetToolTip( _( "Precomputed Data Dir" ) );
    itemStaticBoxSizer9->Add( preComputeOpenButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
    preComputDirTextEntry->Raise();
    ///////////////////////////////////////////////////////
    //At this time the precomputed surfaces does not appear to work
    //precomputed surface appear to be loaded as a normal dataset
    //thus there is no more per data set association - 03-10-2008 -mccdo
    /*itemStaticBoxSizer12Static = new wxStaticBox( itemDialog1, wxID_ANY, _( "Surface Data Directory" ) );
    wxStaticBoxSizer* itemStaticBoxSizer12 = new wxStaticBoxSizer( itemStaticBoxSizer12Static, wxHORIZONTAL );
    itemStaticBoxSizer4->Add( itemStaticBoxSizer12, 0, wxALIGN_LEFT | wxALL | wxEXPAND, 5 );

    surfaceDataText = new wxTextCtrl( itemDialog1, ID_TEXTCTRL3,
                                      _( "Enter Dir Here-->" ), wxDefaultPosition,
                                      wxDefaultSize, wxTE_READONLY );
    surfaceDataText->SetHelpText( _( "Text Entry" ) );
    if( ShowToolTips() )
        surfaceDataText->SetToolTip( _( "Text Entry" ) );
    itemStaticBoxSizer12->Add( surfaceDataText, 1, wxALIGN_CENTER_VERTICAL | wxALL | wxEXPAND, 5 );

    surfaceDataOpenButton = new wxButton( itemDialog1, ID_BUTTON4,
                                          _( "Open" ), wxDefaultPosition,
                                          wxDefaultSize, 0 );
    surfaceDataOpenButton->SetHelpText( _( "Surface Data Dir" ) );
    if( ShowToolTips() )
        surfaceDataOpenButton->SetToolTip( _( "Surface Data Dir" ) );
    itemStaticBoxSizer12->Add( surfaceDataOpenButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
    surfaceDataText->Raise();*/
    ///////////////////////////////////////////////////////
    itemStaticBoxSizer15Static = new wxStaticBox( itemDialog1, wxID_ANY, _( "DataSet Attributes" ) );
    wxStaticBoxSizer* itemStaticBoxSizer15 = new wxStaticBoxSizer( itemStaticBoxSizer15Static, wxHORIZONTAL );
    itemStaticBoxSizer4->Add( itemStaticBoxSizer15, 0, wxALIGN_LEFT | wxALL | wxEXPAND, 5 );

    transformButton = new wxButton( itemDialog1, ID_BUTTON6, _( "Transform" ), wxDefaultPosition, wxDefaultSize, 0 );
    transformButton->SetHelpText( _( "Transform GUI" ) );
    if( ShowToolTips() )
        transformButton->SetToolTip( _( "Transform GUI" ) );
    itemStaticBoxSizer15->Add( transformButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    itemStaticBoxSizer15->Add( 65, 5, 0, wxALIGN_CENTER_VERTICAL | wxALL | wxEXPAND, 5 );

    scalarButton = new wxButton( itemDialog1, ID_BUTTON5, _( "Scalar" ), wxDefaultPosition, wxDefaultSize, 0 );
    scalarButton->SetHelpText( _( "Future Use" ) );
    if( ShowToolTips() )
        scalarButton->SetToolTip( _( "Future Use" ) );
    scalarButton->Enable( false );
    itemStaticBoxSizer15->Add( scalarButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
    
    m_surfaceWrapChkBox = new wxCheckBox( itemDialog1, ID_CREATE_SURFACE_WRAP, _T( "Create Surface Wrap" ), wxDefaultPosition, wxDefaultSize, 0 );
    m_surfaceWrapChkBox->SetValue( false );
    m_surfaceWrapChkBox->SetHelpText( _( "Create a surface from the 3D dataset" ) );
    itemStaticBoxSizer15->Add( m_surfaceWrapChkBox, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
    
    transformButton->Raise();
    ///////////////////////////////////////////////////////
    itemStaticBoxSizer19Static = new wxStaticBox( itemDialog1, wxID_ANY, _( "Volumetric Data Directories" ) );
    wxStaticBoxSizer* itemStaticBoxSizer19 = new wxStaticBoxSizer( itemStaticBoxSizer19Static, wxVERTICAL );
    itemStaticBoxSizer4->Add( itemStaticBoxSizer19, 0, wxALIGN_LEFT | wxALL | wxEXPAND, 5 );

    wxBoxSizer* itemBoxSizer20 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer19->Add( itemBoxSizer20, 0, wxALIGN_LEFT | wxEXPAND, 5 );

    itemTextCtrl21 = new wxTextCtrl( itemDialog1, ID_TEXTCTRL1, _T( "Enter Dir Here-->" ), wxDefaultPosition, wxDefaultSize, wxTE_READONLY );
    itemBoxSizer20->Add( itemTextCtrl21, 1, wxALIGN_CENTER_VERTICAL | wxALL | wxEXPAND, 5 );

    itemButton22 = new wxButton( itemDialog1, ID_BUTTON2, _( "Open" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer20->Add( itemButton22, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    wxScrolledWindow* itemScrolledWindow23 = new wxScrolledWindow( itemDialog1, ID_SCROLLEDWINDOW1, wxDefaultPosition, wxDefaultSize, wxHSCROLL | wxVSCROLL );
    itemStaticBoxSizer19->Add( itemScrolledWindow23, 1, wxALIGN_LEFT | wxALL | wxEXPAND, 5 );
    itemScrolledWindow23->SetScrollbars( 1, 1, 0, 0 );

    wxString* itemListBox24Strings = NULL;
    itemListBox24 = new wxListBox( itemScrolledWindow23, ID_LISTBOX, wxDefaultPosition, wxDefaultSize, 0, itemListBox24Strings, wxLB_SINGLE );
    itemTextCtrl21->Raise();
    ///////////////////////////////////////////////////////
    wxStdDialogButtonSizer* itemStdDialogButtonSizer25 = new wxStdDialogButtonSizer;

    itemBoxSizer2->Add( itemStdDialogButtonSizer25, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );
    wxButton* itemButton26 = new wxButton( itemDialog1, wxID_OK, _( "OK" ), wxDefaultPosition, wxDefaultSize, 0 );
    //itemButton26->SetDefault();
    itemStdDialogButtonSizer25->AddButton( itemButton26 );

    wxButton* itemButton27 = new wxButton( itemDialog1, wxID_CANCEL, _( "&Cancel" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStdDialogButtonSizer25->AddButton( itemButton27 );

    itemStdDialogButtonSizer25->Realize();

////@end DataSetLoaderUI content construction
}
///////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::InitializeWidgets( void )
{
    if( !m_veModel )
    {
        return;
    }
    size_t nummParamBlocks = m_veModel->GetNumberOfInformationPackets();
    //Clear so that we can use this function in delete
    dataSetList->Clear();
    for( size_t i = 0; i < nummParamBlocks; ++i )
    {
        dataSetList->Append( wxString( m_veModel->GetInformationPacket( i )->GetName().c_str(), wxConvUTF8 ) );
    }
}
///////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::SetTextCtrls( void )
{
    if( mParamBlock )
    {
        // clear the listbox before we enter the loop so that
        // we can add the appropriate entries if need be
        itemListBox24->Clear();
        dataSetTextEntry->SetValue( _( "Enter Filename Here-->" ) );
        //surfaceDataText->SetValue( _( "Enter Dir Here-->" ) );
        preComputDirTextEntry->SetValue( _( "Enter Dir Here-->" ) );
        itemTextCtrl21->SetValue( _( "Enter Dir Here-->" ) );

        size_t numProperties = mParamBlock->GetNumberOfProperties();
        for( size_t i = 0; i < numProperties; ++i )
        {
            ves::open::xml::DataValuePairPtr tempDVP = mParamBlock->GetProperty( i );
            if( tempDVP->GetDataName() == "VTK_TEXTURE_DIR_PATH" )
            {
                //clear...then append
                itemListBox24->Append( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
                itemTextCtrl21->SetValue( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( tempDVP->GetDataName() == "VTK_DATA_FILE" )
            {
                //clear...then append
                dataSetTextEntry->SetValue( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( tempDVP->GetDataName() == "VTK_SURFACE_DIR_PATH" )
            {
                //clear...then append
                //surfaceDataText->SetValue( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( tempDVP->GetDataName() == "VTK_PRECOMPUTED_DIR_PATH" )
            {
                //clear...then append
                preComputDirTextEntry->SetValue( wxString( tempDVP->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( tempDVP->GetDataName() == "Create Surface Wrap" )
            {
                //clear...then append
                unsigned int tempData = 0;
                tempDVP->GetData( tempData );
                m_surfaceWrapChkBox->SetValue( tempData );
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
    wxUnusedVar( name );
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
    wxUnusedVar( name );
    return wxNullIcon;
////@end DataSetLoaderUI icon retrieval
}
/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON
 */

void DataSetLoaderUI::OnLoadFile( wxCommandEvent& WXUNUSED( event ) )
{
    //Load a vtk file
////@begin wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON in DataSetLoaderUI.
    // Before editing this code, remove the block markers.
    wxPoint pos( 0, 0 );
    wxFileDialog dialog( this,
                         _T( "Open Data Set File" ),
                         ::wxGetCwd(),
                         _T( "" ),
                         _T( "VTK DataSet Files (*.vtk;*.vtu;*.vts;*.vti;*.vtm;*.vtp;*.vtr;)|*.vtk;*.vtu;*.vts;*.vti;*.vtm;*.vtp;*.vtr;|StarCD Parameter File (*.param)|*.param;|EnSight(*.ens;*.case)|*.ens;*.case;|MFIX (*.mfix)|*.mfix;|Fluent (*.cas)|*.cas;|AVS (*.avs)|*.avs;|Dicom (*.dcm)|*.dcm;|All Files (*.*)|*.*" ),
                         wxOPEN | wxFILE_MUST_EXIST | wxFD_PREVIEW,
                         wxDefaultPosition );
    dialog.CentreOnParent();

    if( dialog.ShowModal() == wxID_OK )
    {
        wxFileName datasetFilename( dialog.GetPath() );
        datasetFilename.MakeRelativeTo( ::wxGetCwd() );
        wxString relativeDataSetPath( datasetFilename.GetFullPath() );
        relativeDataSetPath.Replace( _( "\\" ), _( "/" ), true );
        dataSetTextEntry->SetValue( relativeDataSetPath );
        std::string tempStr;
        {
            ves::open::xml::DataValuePairPtr tempDVP = 
                mParamBlock->GetProperty( "VTK_DATA_FILE" );
            if( !tempDVP )
            {
                tempDVP = mParamBlock->GetProperty( -1 );
            }
            tempStr = static_cast< const char* >( 
                wxConvCurrent->cWX2MB( relativeDataSetPath.c_str() ) );
            tempDVP->SetData( "VTK_DATA_FILE", tempStr );
        }

        ves::xplorer::util::cfdVTKFileHandler tempHandler;
        std::vector< std::string > dataArrayList = 
            tempHandler.GetDataSetArraysFromFile( tempStr );
        
        if( !dataArrayList.empty() )
        {
            //open dialog to choose scalars to load
            DataSetDataArrayChoiceDialog choiceDialog( this );
            choiceDialog.SetDataArrays( dataArrayList );
            if( choiceDialog.ShowModal() == wxID_OK )
            {
                dataArrayList = choiceDialog.GetUserActiveArrays();
                ves::open::xml::DataValuePairPtr arraysDVP = 
                    mParamBlock->GetProperty( "VTK_ACTIVE_DATA_ARRAYS" );
                if( !arraysDVP )
                {
                    arraysDVP = mParamBlock->GetProperty( -1 );
                }
                ves::open::xml::OneDStringArrayPtr 
                    stringArray( new ves::open::xml::OneDStringArray() );
                stringArray->SetArray( dataArrayList );
                arraysDVP->SetData( "VTK_ACTIVE_DATA_ARRAYS", stringArray );
            }
        }
        ves::open::xml::DataValuePairSharedPtr 
            dataValuePair( new ves::open::xml::DataValuePair() );
        dataValuePair->SetData( "CREATE_NEW_DATASETS",
            ves::open::xml::model::ModelPtr( 
            new ves::open::xml::model::Model( *m_veModel ) ) );
        SendCommandToXplorer( dataValuePair );
    }
}


/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON4
 */

void DataSetLoaderUI::OnLoadSurfaceFile( wxCommandEvent& event )
{
    //Launch the surface data dir
    //Launch the precomputed data dir
    wxPoint pos( 0, 0 );
    wxFileDialog dialog( this,
                         _T( "Open VTK PolyData File" ),
                         ::wxGetCwd(),
                         _T( "" ),
                         _T( "VTK Surface Files (*.vtk;*.vtp)|*.vtk;*.vtp;" ),
                         wxOPEN | wxFILE_MUST_EXIST | wxFD_PREVIEW,
                         wxDefaultPosition );
    dialog.CentreOnParent();

    if( dialog.ShowModal() == wxID_OK )
    {
        wxFileName surfaceDir( dialog.GetPath() );
        surfaceDir.MakeRelativeTo( ::wxGetCwd() );
        if( event.GetId() == ID_BUTTON4 )
        {
            /*wxString relativeSurfaceDirPath( surfaceDir.GetPath() );
            relativeSurfaceDirPath.Replace( _( "\\" ), _( "/" ), true );
            surfaceDataText->SetValue( relativeSurfaceDirPath );
            ves::open::xml::DataValuePairPtr tempDVP = mParamBlock->GetProperty( "VTK_SURFACE_DIR_PATH" );
            if( !tempDVP )
            {
                tempDVP = mParamBlock->GetProperty( -1 );
            }
            std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( relativeSurfaceDirPath.c_str() ) ) );
            tempDVP->SetData( "VTK_SURFACE_DIR_PATH", tempStr );

            ves::open::xml::DataValuePairSharedPtr dataValuePair(
                  new ves::open::xml::DataValuePair() );
            dataValuePair->SetData( "ADD_SURFACE_DATA_DIR", tempDVP );
            SendCommandToXplorer( dataValuePair );*/
        }
        else if( event.GetId() == ID_BUTTON3 )
        {
            wxString relativePrecomputedDirPath( surfaceDir.GetPath() );
            relativePrecomputedDirPath.Replace( _( "\\" ), _( "/" ), true );
            preComputDirTextEntry->SetValue( relativePrecomputedDirPath );
            ves::open::xml::DataValuePairPtr tempDVP = mParamBlock->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" );
            if( !tempDVP )
            {
                tempDVP = mParamBlock->GetProperty( -1 );
            }
            std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( relativePrecomputedDirPath.c_str() ) ) );
            tempDVP->SetData( "VTK_PRECOMPUTED_DIR_PATH", tempStr );
            preComputDirTextEntry->SetValue( relativePrecomputedDirPath );

            ves::open::xml::DataValuePairSharedPtr dataValuePair(
               new ves::open::xml::DataValuePair() );
            dataValuePair->SetData( "ADD_PRECOMPUTED_DATA_DIR", tempDVP );
            SendCommandToXplorer( dataValuePair );
        }
    }
////@end wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON4 in DataSetLoaderUI.
}

/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON6
 */

void DataSetLoaderUI::OnTransformDataset( wxCommandEvent& WXUNUSED( event ) )
{
    // Launch the transform UI
////@begin wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON6 in DataSetLoaderUI.
    // Before editing this code, remove the block markers.
    wxDialog transformDialog( this,
                              ::wxNewId(),
                              _( "Transform Input Window" ),
                              wxDefaultPosition,
                              wxDefaultSize,
                              wxCAPTION | wxCLOSE_BOX | wxSYSTEM_MENU | wxRESIZE_BORDER | wxSYSTEM_MENU | wxCLOSE_BOX | wxMAXIMIZE_BOX | wxMINIMIZE_BOX );

    wxBoxSizer* mainSizer = new wxBoxSizer( wxVERTICAL );
    //wxBoxSizer* notebookSizer = new wxBoxSizer(wxVERTICAL);
    //wxBoxSizer* bottomRow = new wxBoxSizer(wxHORIZONTAL);
    ves::conductor::util::TransformUI* transformPanel = new ves::conductor::util::TransformUI( &transformDialog, _( "Transform Input" ), mParamBlock->GetTransform() );

    if( mParamBlock )
    {
        mainSizer->Add( transformPanel, -1, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );
    }
    else
    {
        mainSizer->Add( new TransformUI( &transformDialog, _( "Transform Input" ),
                        ves::open::xml::TransformPtr() ),
                        -1, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );
    }

    mainSizer->Add( transformDialog.CreateStdDialogButtonSizer( wxOK | wxCANCEL ), -1, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );
    //set this flag and let wx handle alignment
    transformDialog.SetAutoLayout( true );

    //assign the group to the panel
    transformDialog.SetSizer( mainSizer );
    mainSizer->Fit( &transformDialog );

    //set parameterblock unique (GUID) id for transform GUI
    if( mParamBlock->GetProperty( "VTK_DATA_FILE" ) )
    {
        transformPanel->SetParamBlockID( 
            mParamBlock->GetProperty( "VTK_DATA_FILE" )->GetDataString() );
    }
    else
    {
        transformPanel->SetParamBlockID( "NULL" );
            //mParamBlock->GetID() );
    }
    transformPanel->SetParamBlockTransform( mParamBlock->GetTransform() );

    if( transformDialog.ShowModal() == wxID_OK )
    {
        ;
    }
}

/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON2
 */

void DataSetLoaderUI::OnLoadTextureFile( wxCommandEvent& WXUNUSED( event ) )
{
    //Load data for the texturebased vis
////@begin wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON2 in DataSetLoaderUI.
    // Before editing this code, remove the block markers.
    wxString cwd( ::wxGetCwd() );
    int answer = 0;
    do
    {
        wxPoint pos( 0, 0 );
        wxFileDialog dialog( this,
                             _T( "Open VTK Image File" ),
                             ::wxGetCwd(),
                             _T( "" ),
                             _T( "VTK Texture Files (*.vti)|*.vti;" ),
                             wxOPEN | wxFILE_MUST_EXIST | wxFD_PREVIEW,
                             wxDefaultPosition );
        dialog.CentreOnParent();
        
        if( dialog.ShowModal() == wxID_OK )
        {
            wxFileName textureDir( dialog.GetPath() );
            textureDir.MakeRelativeTo( cwd );
            wxString relativeTextureDirPath( textureDir.GetPath() );
            relativeTextureDirPath.Replace( _( "\\" ), _( "/" ), true );
            itemTextCtrl21->SetValue( relativeTextureDirPath );
            textureDirs.insert( relativeTextureDirPath );
        }
        wxMessageDialog promptDlg( this,
                                   _( "Are you done selecting texture files?" ),
                                   _( "Texture Chooser" ),
                                   wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION,
                                   wxDefaultPosition );
        answer = promptDlg.ShowModal();
    }
    while( answer == wxID_NO );
    wxFileName::SetCwd( cwd );

    std::set< wxString >::iterator iter;
    for( iter = textureDirs.begin(); iter != textureDirs.end(); ++iter )
    {
        ves::open::xml::DataValuePairPtr tempDVP = mParamBlock->GetProperty( -1 );
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB(( *iter ).c_str() ) ) );
        tempDVP->SetData( "VTK_TEXTURE_DIR_PATH", tempStr );
        wxString* dirString = new wxString(( *iter ) );
        itemListBox24->InsertItems( 1, dirString, 0 );
        //Send data to xplorer
        ves::open::xml::DataValuePairSharedPtr dataValuePair(
         new ves::open::xml::DataValuePair() );
        dataValuePair->SetData( "ADD_TEXTURE_DATA_DIR", tempDVP );
        SendCommandToXplorer( dataValuePair );
    }
}

/*!
 * wxEVT_COMMAND_LISTBOX_SELECTED event handler for ID_LISTBOX
 */
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnListboxSelected( wxCommandEvent& WXUNUSED( event ) )
{
    // When the list box is selected
    size_t numProperties = mParamBlock->GetNumberOfProperties();
    for( size_t i = 0; i < numProperties; ++i )
    {
        ves::open::xml::DataValuePairPtr tempDVP = mParamBlock->GetProperty( i );
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( itemListBox24->GetStringSelection().c_str() ) ) );
        if( ( tempDVP->GetDataName() == "VTK_TEXTURE_DIR_PATH" ) &&
                ( tempDVP->GetDataString() == tempStr ) )
        {
            mParamBlock->RemoveProperty( i );
            itemListBox24->Delete( itemListBox24->GetSelection() );
            return;
        }
    }
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnInformationPacketChange( wxCommandEvent& WXUNUSED( event ) )
{
    /// wxEVT_COMMAND_COMBOBOX_SELECTED event handler for ID_LISTBOX
    // When we change the combobox the newly selected item
    // should be queried and all other widgets should be update with
    // appropriate info

    wxString selection = dataSetList->GetStringSelection();

    size_t numParamBlocks = m_veModel->GetNumberOfInformationPackets();
    for( size_t i = 0; i < numParamBlocks; ++i )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( selection.c_str() ) ) );
        if( m_veModel->GetInformationPacket( i )->GetName() == tempStr )
        {
            mParamBlock = m_veModel->GetInformationPacket( i );
            EnableUI( true );
            SetTextCtrls();
            break;
        }
    }
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnInformationPacketAdd( wxCommandEvent& WXUNUSED( event ) )
{
    /// wxEVT_COMMAND_TEXT_ENTER event handler for ID_LISTBOX
    // When enter is pushed on the combox and new entry is specifiy and
    // the appropriate widgets should be updated
    wxTextEntryDialog newDataSetName( this,
                                      wxString( _( "New Dataset" ) ),
                                      wxString( _( "Enter name for new Dataset:" ) ),
                                      wxString( _( "Dataset" ) ), 
                                      wxOK | wxCANCEL | wxCENTRE );

    newDataSetName.CentreOnParent();

    if( newDataSetName.ShowModal() == wxID_CANCEL )
    {
        return;
    }
    
    
    if( dataSetList->FindString( newDataSetName.GetValue() ) != wxNOT_FOUND )
    {
        wxMessageBox( _( "Data with this name is already loaded." ),
                      newDataSetName.GetValue(), wxOK | wxICON_INFORMATION );
        return;
    }
    else
    {
        dataSetList->Append( newDataSetName.GetValue() );
        dataSetList->SetStringSelection( newDataSetName.GetValue() );

        mParamBlock = m_veModel->GetInformationPacket( -1 );
        std::string tempStr;
        tempStr = ( static_cast< const char* >( wxConvCurrent->cWX2MB( newDataSetName.GetValue() ) ) );
        
        mParamBlock->SetName( tempStr );
        EnableUI( true );
        SetTextCtrls();
    }
}
//////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnDeleteDataset( wxCommandEvent& WXUNUSED( event ) )
{
    if( dataSetList->GetCount() == 0 )
    {
        return;
    }

    wxString selection = dataSetList->GetStringSelection();

    std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( selection.c_str() ) ) );
    
    ves::open::xml::DataValuePairSharedPtr dataFileDVP = mParamBlock->GetProperty( "VTK_DATA_FILE" );

    if( !dataFileDVP )
    {
        ::wxLogMessage(  wxString( "This dataset has no VTK dataset.", wxConvUTF8 ) );
        return;
    }
    std::string tempDataSetName =
        mParamBlock->GetProperty( "VTK_DATA_FILE" )->GetDataString();

    ves::open::xml::DataValuePairSharedPtr dataValuePair(
        new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "DELETE_DATASET", tempDataSetName );

    SendCommandToXplorer( dataValuePair );
    //Rebuild GUI
    m_veModel->RemoveInformationPacket( tempStr );
    mParamBlock = ves::open::xml::ParameterBlockPtr();

    InitializeWidgets();
    dataSetList->SetSelection( 0 );
    wxCommandEvent event;
    OnInformationPacketChange( event );
    Refresh();
}
////////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnInformationPacketChangeName( wxCommandEvent& WXUNUSED( event ) )
{
    /// wxEVT_COMMAND_TEXT_UPDATED event handler for ID_LISTBOX
    // If any text is changed with the name of a information packet then
    // then the information packet should change as well in veModel.
    if( mParamBlock )
    {
        int selection = dataSetList->GetSelection();
        dataSetList->SetString( selection, dataSetList->GetValue() );
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( dataSetList->GetValue().c_str() ) ) );
        mParamBlock->SetName( tempStr );
        //std::cout << "OnInformationPacketChangeName " << mParamBlock->GetName() << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::EnableUI( bool flag )
{
    if( dataSetList->GetCount() )
    {
        dataSetList->Enable( true );
    }
    else
    {
        dataSetList->Enable( false );
    }
    dataSetOpenButton->Enable( flag );
    preComputeOpenButton->Enable( flag );
    //surfaceDataOpenButton->Enable( flag );
    transformButton->Enable( flag );
    itemButton22->Enable( flag );
}
////////////////////////////////////////////////////////////////////////////////
std::string DataSetLoaderUI::GetActiveDataSetName()
{
    return mParamBlock->GetName();
}
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::ParameterBlockPtr DataSetLoaderUI::GetParamBlock()
{
    return mParamBlock;
}
////////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::SendCommandToXplorer(
    ves::open::xml::DataValuePairSharedPtr tempObject )
{
    if( !mParamBlock )
    {
        return;
    }
    
    //Now send the data to xplorer
    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();

    // Create the command and data value pairs
    ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    veCommand->SetCommandName( std::string( "UPDATE_MODEL_DATASETS" ) );
    veCommand->AddDataValuePair( tempObject );
    //Add the active dataset name to the command
    ves::open::xml::DataValuePairSharedPtr dataSetName(
        new ves::open::xml::DataValuePair() );
    if( mParamBlock->GetProperty( "VTK_DATA_FILE" ) )
    {
        dataSetName->SetData( "VTK_DATASET_NAME",
                              mParamBlock->GetProperty( "VTK_DATA_FILE" )->GetDataString() );
    }
    else
    {
        dataSetName->SetData( "VTK_DATASET_NAME", "NULL" );
    }
    veCommand->AddDataValuePair( dataSetName );
    //Now send the command
    CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
}
////////////////////////////////////////////////////////////////////////////////
void DataSetLoaderUI::OnSurfaceWrapCheckBox( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::DataValuePairPtr surfaceDVP = 
        mParamBlock->GetProperty( "Create Surface Wrap" );
    if( !surfaceDVP )
    {
        ves::open::xml::DataValuePairSharedPtr dataValuePair(
            new ves::open::xml::DataValuePair() );
        mParamBlock->AddProperty( dataValuePair );
        surfaceDVP = dataValuePair;
    }
    unsigned int chkValue = m_surfaceWrapChkBox->GetValue();
    surfaceDVP->SetData( "Create Surface Wrap", chkValue );

    ves::open::xml::DataValuePairSharedPtr dataValuePair(
        new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "Update Surface Wrap", surfaceDVP );

    SendCommandToXplorer( dataValuePair );
}
////////////////////////////////////////////////////////////////////////////////
