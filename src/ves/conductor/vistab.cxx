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
#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/conductor/util/TBToolBar.h>
#include <ves/conductor/util/UI_TransientDialog.h>
#include <ves/conductor/vectors.h>
#include <ves/conductor/contours.h>
#include <ves/conductor/streamlines.h>
#include <ves/conductor/isosurfaces.h>
#include <ves/conductor/polydata.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/conductor/util/spinctld.h>

#include <ves/conductor/vistab.h>
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/icon.h>
#include <wx/statbmp.h>
#include <wx/combobox.h>
#include <wx/listbox.h>
#include <wx/msgdlg.h>
#include <wx/app.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/checkbox.h>
#include <wx/intl.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/filename.h>

#include <iostream>
#include <string>
////@begin XPM images
#include <ves/conductor/xpm/new_vector.xpm>
#include <ves/conductor/xpm/contour.xpm>
#include <ves/conductor/xpm/streamlines.xpm>
#include <ves/conductor/xpm/isosurface.xpm>
#include <ves/conductor/xpm/scalartb.xpm>
////@end XPM images

using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( Vistab, wxDialog )
   EVT_TOOL     ( CONTOUR_BUTTON,       Vistab::_onContour )
   EVT_TOOL     ( VECTOR_BUTTON,        Vistab::_onVector )
   EVT_TOOL     ( STREAMLINE_BUTTON,    Vistab::_onStreamline )
   EVT_TOOL     ( ISOSURFACE_BUTTON,    Vistab::_onIsosurface )
   EVT_TOOL     ( TEXTURE_BASED_BUTTON, Vistab::_onTextureBased )
   EVT_TOOL     ( POLYDATA_BUTTON,      Vistab::_onPolydata )
   EVT_COMBOBOX ( ID_COMBOBOX,          Vistab::_OnSelectDataset )
   EVT_LISTBOX  ( ID_LISTBOX,           Vistab::_OnSelectScalar )
   EVT_LISTBOX  ( ID_LISTBOX1,          Vistab::_OnSelectVector )
   EVT_BUTTON   ( ID_CLEAR_ALL_BUTTON,  Vistab::OnClearAll )
   EVT_BUTTON   ( CLOSE_BUTTON,         Vistab::_onClose )
   EVT_COMMAND_SCROLL( MIN_SPINCTRL,    Vistab::_onMinSpinCtrl )
   EVT_COMMAND_SCROLL( MAX_SPINCTRL,    Vistab::_onMaxSpinCtrl )
   EVT_COMMAND_SCROLL( MIN_MAX_SLIDERS, Vistab::_onMinMaxSlider )
   EVT_COMMAND_SCROLL( MIN_SLIDER,      Vistab::_onMinSlider )
   EVT_COMMAND_SCROLL( MAX_SLIDER,      Vistab::_onMaxSlider )
   EVT_TEXT_ENTER( MIN_SPINCTRL,        Vistab::UpdateMinSlider )
   EVT_TEXT_ENTER( MAX_SPINCTRL,        Vistab::UpdateMaxSlider )
   EVT_TEXT_ENTER( ID_DATA_UPDATE_AXES, Vistab::UpdateAxesLabels )
   EVT_CHECKBOX( ID_DATA_BBOX_CB,       Vistab::UpdateBoundingBox)
   EVT_CHECKBOX( ID_DATA_WIREFRAME_CB,  Vistab::UpdateWireFrame)
   EVT_CHECKBOX( ID_DATA_AXES_CB,       Vistab::UpdateAxes)
   EVT_CHECKBOX( ID_DATA_SCALAR_BAR,    Vistab::UpdateScalarBar)
END_EVENT_TABLE()

//////////////////////////////////////////
//Constructor                           //
//////////////////////////////////////////
Vistab::Vistab(VjObs::Model_var activeModel )
{
   _activeModel = 0;
   _scalarSelection = 0;    
   _vectorSelection = 0;    
   _nDatasetsInActiveModel = 0;
   _datasetSelection = 0;
   _tbTools = 0;
   scalarContour = 0;
   vectorContour = 0;
   isosurface = 0;
   streamline = 0;
   polydata = 0;

   //scalarSelect = false;
   //vectorSelect = false;
   //scalarValue = 0;
   _availableSolutions["MESH_SCALARS"].Add( _("") ); 
   _availableSolutions["MESH_VECTORS"].Add( _("") ); 
   _availableSolutions["TEXTURE_SCALARS"].Add( _("") );  
   _availableSolutions["TEXTURE_VECTORS"].Add( _("") );
   _commandName = "VISUALIZATION_TAB";
    m_vistabButtonMap["Scalar Contour"] = CONTOUR_BUTTON;
    m_vistabButtonMap["Vector Contour"] = VECTOR_BUTTON;
    m_vistabButtonMap["Streamlines"] = STREAMLINE_BUTTON; 
    m_vistabButtonMap["Isosurface"] = ISOSURFACE_BUTTON; 
    m_vistabButtonMap["TBET"] = TEXTURE_BASED_BUTTON; 
    m_vistabButtonMap["Polydata"] = POLYDATA_BUTTON; 
   SetActiveModel(activeModel);
   _activeScalarName = ConvertUnicode( _scalarSelection->GetStringSelection() );
   _activeVectorName = ConvertUnicode( _vectorSelection->GetStringSelection() );
   _activeScalarRange = _originalScalarRanges[_activeScalarName];
   

    
   //_vistabPosition = dynamic_cast<AppFrame*>(wxTheApp->GetTopWindow())->GetAppropriateSubDialogSize();
}
///////////////////////////////////////////////////////////////////
//Constructor                                                    //
///////////////////////////////////////////////////////////////////
Vistab::Vistab(VjObs::Model_var activeModel,
               wxWindow* parent, wxWindowID id,
               const wxString& caption,
               const wxPoint& pos, const wxSize& size, long style )
{
   _activeModel = 0;
   _scalarSelection = 0;    
   _vectorSelection = 0;    
   _nDatasetsInActiveModel = 0;
   _datasetSelection = 0;
   isosurface = 0;
   _tbTools = 0;
   scalarContour = 0;
   vectorContour = 0;
   streamline = 0;
   polydata = 0;

   //scalarSelect = false;
   //vectorSelect = false;
   //scalarValue = 0;
   _availableSolutions["MESH_SCALARS"].Add( _("") ); 
   _availableSolutions["MESH_VECTORS"].Add( _("") ); 
   _availableSolutions["TEXTURE_SCALARS"].Add( _("") );  
   _availableSolutions["TEXTURE_VECTORS"].Add( _("") ); 

    m_vistabButtonMap["Scalar Contour"] = CONTOUR_BUTTON;
    m_vistabButtonMap["Vector Contour"] = VECTOR_BUTTON;
    m_vistabButtonMap["Streamlines"] = STREAMLINE_BUTTON; 
    m_vistabButtonMap["Isosurface"] = ISOSURFACE_BUTTON; 
    m_vistabButtonMap["TBET"] = TEXTURE_BASED_BUTTON; 
    m_vistabButtonMap["Polydata"] = POLYDATA_BUTTON; 

   SetActiveModel(activeModel);
   Create(parent, id, caption, pos, wxDefaultSize, style);
   _commandName = "VISUALIZATION_TAB";
   
   if(_nDatasetsInActiveModel)
   {
      _setActiveDataset(0);
   }
   _activeScalarName = ConvertUnicode( _scalarSelection->GetStringSelection() );
   _activeVectorName = ConvertUnicode( _vectorSelection->GetStringSelection() );
   _activeScalarRange = _originalScalarRanges[_activeScalarName];

   
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool Vistab::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
  itemToolBar3 = 0;
  itemComboBox11 = 0;
  itemComboBox12 = 0;
  itemListBox13 = 0; 
  itemListBox15 = 0;
  _minSpinner = 0;
  _maxSpinner = 0;

  scalarRange = 0;
  SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
  wxDialog::Create( parent, id, caption, pos, wxDefaultSize, style );

  CreateControls();
  return true;
}
/////////////////
Vistab::~Vistab()
{
   ClearBaseInformation();
   if(isosurface)
   {
      isosurface->Destroy();
      isosurface = 0;
   }
   if(_tbTools)
   {
      _tbTools->Destroy();
      _tbTools = 0;
   }
   if(scalarContour)
   {
      scalarContour->Destroy();
      scalarContour = 0;
   }
    if(vectorContour)
   {
      vectorContour->Destroy();
      vectorContour = 0;
   }
   if(streamline)
   {
      streamline->Destroy();
      streamline = 0;
   }
   if(polydata)
   {
      polydata->Destroy();
      polydata = 0;
   }
}
/////////////////////////////
void Vistab::CreateControls()
{    
////@begin Vistab content construction
    // Generated by DialogBlocks, Wed 26 Apr 2006 18:52:50 CDT 

    Vistab* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    itemToolBar3=new wxToolBar(itemDialog1,ID_TOOLBAR,wxDefaultPosition,wxDefaultSize,wxTB_FLAT|wxTB_HORIZONTAL/*| wxTB_TEXT*/ );
    itemToolBar3->SetToolBitmapSize(wxSize(32,32));

    wxBitmap itemtool4Bitmap(contour_xpm);
    wxBitmap itemtool4BitmapDisabled;
    itemToolBar3->AddTool(CONTOUR_BUTTON, _T("Scalars"),
                          itemtool4Bitmap, itemtool4BitmapDisabled,
                          wxITEM_NORMAL/*wxITEM_RADIO*/, _T("Scalar Contours"), wxEmptyString);

    wxBitmap itemtool5Bitmap(new_vector_xpm);
    wxBitmap itemtool5BitmapDisabled;
    itemToolBar3->AddTool(VECTOR_BUTTON, _T("Vectors"), 
                          itemtool5Bitmap, itemtool5BitmapDisabled,
                          wxITEM_NORMAL/*wxITEM_RADIO*/, _T("Vectors"), wxEmptyString);

    wxBitmap itemtool6Bitmap(streamlines_xpm);
    wxBitmap itemtool6BitmapDisabled;
    itemToolBar3->AddTool(STREAMLINE_BUTTON, _T("Streamlines"), itemtool6Bitmap, itemtool6BitmapDisabled, wxITEM_NORMAL/*wxITEM_RADIO*/, _T("Streamlines"), wxEmptyString);

    wxBitmap itemtool7Bitmap(isosurface_xpm);
    wxBitmap itemtool7BitmapDisabled;
    itemToolBar3->AddTool(ISOSURFACE_BUTTON, _T("Isosurfaces"), itemtool7Bitmap, itemtool7BitmapDisabled, wxITEM_NORMAL/*wxITEM_RADIO*/, _T("Isosurfaces"), wxEmptyString);

    wxBitmap itemtool8Bitmap(scalartb_xpm);
    wxBitmap itemtool8BitmapDisabled;
    itemToolBar3->AddTool(TEXTURE_BASED_BUTTON, _T("Texture-Based"), itemtool8Bitmap, itemtool8BitmapDisabled, wxITEM_NORMAL/*wxITEM_RADIO*/, _T("Texture Based"), wxEmptyString);

    wxBitmap itemtool9Bitmap(isosurface_xpm);
    wxBitmap itemtool9BitmapDisabled;
    itemToolBar3->AddTool(POLYDATA_BUTTON, _T("Polydata"), itemtool9Bitmap, itemtool8BitmapDisabled, wxITEM_NORMAL/*wxITEM_RADIO*/, _T("Polydata"), wxEmptyString);

    itemToolBar3->Realize();
    itemBoxSizer2->Add(itemToolBar3, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    //itemToolBar3->ToggleTool(CONTOUR_BUTTON, false);

    {
       //Setup the dataset section of the gui
       wxStaticBox* dataSetStaticBox = new wxStaticBox(itemDialog1, wxID_ANY, _T("Data Set Chooser"));
       wxStaticBoxSizer* dataSetSBSizer = new wxStaticBoxSizer( dataSetStaticBox, wxVERTICAL);
       itemBoxSizer2->Add( dataSetSBSizer, 0, wxGROW|wxALL, 5);
       
       _datasetSelection = new wxComboBox( itemDialog1, ID_COMBOBOX, _T(""), wxDefaultPosition, wxDefaultSize, _availableDatasets, wxCB_DROPDOWN );
       
       if (ShowToolTips())
          _datasetSelection->SetToolTip(_T("Data Sets"));
       dataSetSBSizer->Add(_datasetSelection, 1, wxALIGN_CENTER_VERTICAL|wxALL|wxEXPAND, 5);
       
       //setup checkboxes for datasets
       wxBoxSizer* itemBoxSizer10 = new wxBoxSizer(wxHORIZONTAL);
       dataSetSBSizer->Add( itemBoxSizer10, 0, wxGROW|wxALL, 5);
       
       wireFrameCB = new wxCheckBox( itemDialog1, ID_DATA_WIREFRAME_CB, 
            _("Surface Wrap"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );
       itemBoxSizer10->Add( wireFrameCB, 1, wxALIGN_CENTER_VERTICAL);
       bboxCB = new wxCheckBox( itemDialog1, ID_DATA_BBOX_CB, 
            _("Bounding Box"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );
       itemBoxSizer10->Add( bboxCB, 1, wxALIGN_CENTER_VERTICAL );

       scalarBarCB = new wxCheckBox( itemDialog1, ID_DATA_SCALAR_BAR, 
                                     _("Scalar Bar"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );
       itemBoxSizer10->Add( scalarBarCB, 1, wxALIGN_CENTER_VERTICAL);
       //wxBoxSizer* axesBS = new wxBoxSizer(wxHORIZONTAL);
       //dataSetSBSizer->Add( axesBS, 0, wxGROW|wxALL, 5);
       axesCB = new wxCheckBox( itemDialog1, ID_DATA_AXES_CB, 
            _("Axes"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );
       itemBoxSizer10->Add( axesCB, 1, wxALIGN_CENTER_VERTICAL);
       //updateAxes = new wxButton( itemDialog1, ID_DATA_UPDATE_AXES, _T("Axes"), wxDefaultPosition, wxDefaultSize, 0 );
       //itemBoxSizer10->Add( updateAxes, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

       // add text input for axes
       wxBoxSizer* axesTextBS = new wxBoxSizer(wxHORIZONTAL);
       dataSetSBSizer->Add( axesTextBS, 0, wxGROW);
       xAxisEntry = new wxTextCtrl( itemDialog1, ID_DATA_UPDATE_AXES, 
                                    _("X Axis"), wxDefaultPosition, 
                                    wxDefaultSize, wxHSCROLL|wxTE_PROCESS_ENTER );
       xAxisEntry->Disable();
       axesTextBS->Add( xAxisEntry, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);
       yAxisEntry = new wxTextCtrl( itemDialog1, ID_DATA_UPDATE_AXES, 
                                    _("Y Axis"), wxDefaultPosition, 
                                    wxDefaultSize, wxHSCROLL|wxTE_PROCESS_ENTER );
       yAxisEntry->Disable();
       axesTextBS->Add( yAxisEntry, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);
       zAxisEntry = new wxTextCtrl( itemDialog1, ID_DATA_UPDATE_AXES, 
                                    _("Z Axis"), wxDefaultPosition, 
                                    wxDefaultSize, wxHSCROLL|wxTE_PROCESS_ENTER );
       axesTextBS->Add( zAxisEntry, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);
       zAxisEntry->Disable();
    }

    //Setup the scalars section
    wxBoxSizer* itemBoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
    itemBoxSizer2->Add(itemBoxSizer11, 0, wxGROW|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer12Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Scalars"));
    wxStaticBoxSizer* itemStaticBoxSizer12 = new wxStaticBoxSizer(itemStaticBoxSizer12Static, wxHORIZONTAL);
    itemBoxSizer11->Add(itemStaticBoxSizer12, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    _scalarSelection = new wxListBox( itemDialog1, ID_LISTBOX, wxDefaultPosition, wxSize(125, 75), _availableSolutions["MESH_SCALARS"] , wxLB_SINGLE|wxLB_NEEDED_SB );
    _scalarSelection->SetSelection(0);    
    itemStaticBoxSizer12->Add(_scalarSelection, 1, wxGROW|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer14Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Vectors"));
    wxStaticBoxSizer* itemStaticBoxSizer14 = new wxStaticBoxSizer(itemStaticBoxSizer14Static, wxHORIZONTAL);
    itemBoxSizer11->Add(itemStaticBoxSizer14, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    _vectorSelection = new wxListBox( itemDialog1, ID_LISTBOX1, wxDefaultPosition, wxSize(125, 75), _availableSolutions["MESH_VECTORS"], wxLB_SINGLE|wxLB_NEEDED_SB );
    _vectorSelection->SetSelection(0);
    itemStaticBoxSizer14->Add(_vectorSelection, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxStaticBox* scalarBoundsStatic = new wxStaticBox(itemDialog1, wxID_ANY, _T("Scalar Range"));   

    wxStaticBoxSizer* scalarBoundsSizer = new wxStaticBoxSizer( scalarBoundsStatic, wxVERTICAL );

    wxBoxSizer* minSizer = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* maxSizer = new wxBoxSizer(wxHORIZONTAL);  

    _minSpinner = new wxSpinCtrlDbl( *itemDialog1, MIN_SPINCTRL, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 100, 0, 0.1, -1, wxEmptyString );
    _minSlider = new wxSlider( itemDialog1, MIN_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );

    _maxSpinner = new wxSpinCtrlDbl( *itemDialog1, MAX_SPINCTRL, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 100, 100, 0.1, -1, wxEmptyString );
    _maxSlider = new wxSlider( itemDialog1, MAX_SLIDER, 100, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
   
    wxStaticText* _min = new wxStaticText( itemDialog1, wxID_STATIC, _T("Min"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT ); 
    wxStaticText* _max = new wxStaticText( itemDialog1, wxID_STATIC, _T("Max"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT ); 

    minSizer->Add( _minSpinner, 0, wxALIGN_LEFT|wxTOP|wxLEFT|wxRIGHT, 5 );
    minSizer->Add( _minSlider, 1, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxBOTTOM|wxEXPAND, 5 );

    maxSizer->Add( _maxSpinner, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5 );
    maxSizer->Add( _maxSlider, 1, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxBOTTOM|wxEXPAND, 5 );

    scalarBoundsSizer->Add( _min,0,wxALL|wxGROW );
    scalarBoundsSizer->Add( minSizer,0,wxALL|wxGROW );
    scalarBoundsSizer->Add( _max,0,wxALL|wxGROW );
    scalarBoundsSizer->Add( maxSizer,0,wxALL|wxGROW );

    itemBoxSizer2->Add(scalarBoundsSizer, 0, wxALIGN_CENTER|wxGROW|wxALL, 5);
   
    scalarBoundsStatic->Enable(false);
    //Last row buttons - advanced, clear all, ...
    wxBoxSizer* lastRowButtons = new wxBoxSizer( wxHORIZONTAL );
    
    //wxButton* itemButton20 = new wxButton( itemDialog1, ID_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    //lastRowButtons->Add( itemButton20, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    clearAllButton = new wxButton( itemDialog1, ID_CLEAR_ALL_BUTTON, _T("Clear All"), wxDefaultPosition, wxDefaultSize, 0 );
    lastRowButtons->Add( clearAllButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
//    wxButton* _closeButton = new wxButton( itemDialog1, CLOSE_BUTTON, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
    lastRowButtons->Add(_closeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
    itemBoxSizer2->Add( lastRowButtons, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );
    SetAutoLayout(true);
    
    itemBoxSizer2->Fit(this);
///@end Vistab content construction
}
///////////////////////////
bool Vistab::ShowToolTips()
{
    return true;
}
//////////////////////////////////////////////////////////
wxBitmap Vistab::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin Vistab bitmap retrieval
    return wxNullBitmap;
////@end Vistab bitmap retrieval
}
/*!
 * Get icon resources
 */
//////////////////////////////////////////////////////
wxIcon Vistab::GetIconResource( const wxString& name )
{
    // Icon retrieval
    //wxUnusedVar(name);
    return wxNullIcon;
}
/////////////////////////////////////////////////
void Vistab::ResetAllDatasetDependentCheckBoxes()
{
   if(wireFrameCB)
   {
      wireFrameCB->SetValue(false);
   }
   if(bboxCB)
   {
      bboxCB->SetValue(false);
   }
   if(axesCB)
   {
      axesCB->SetValue(false);
   }
   if(scalarBarCB)
   {
      scalarBarCB->SetValue(false);
   }
}
////////////////////////////////////////////////////////////
void Vistab::_onContour( wxCommandEvent& WXUNUSED(event) )
{
//   InitialScalarVector();
   if( _activeScalarName.empty() )
   {
      wxMessageBox( _("Select a scalar"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      return;
   }
   if(!scalarContour)
   {
      scalarContour = new Contours(this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_CONTOURS_TITLE,
                  SYMBOL_CONTOURS_POSITION,
                  SYMBOL_CONTOURS_SIZE, 
                  SYMBOL_CONTOURS_STYLE );
   }

   scalarContour->SetSize( this->GetRect() );
   scalarContour->ShowModal();
}
/////////////////////////////////////////////////////////
void Vistab::_onVector( wxCommandEvent& WXUNUSED(event) )
{
   if( _activeVectorName.empty() || _activeVectorName == "None" )
   {
      wxMessageBox( _("Select a vector"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      return;
   }
   else if(!vectorContour)
   {
      vectorContour = new Contours (this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_CONTOURS_TITLE,
                  SYMBOL_CONTOURS_POSITION,
                  SYMBOL_CONTOURS_SIZE, 
                  SYMBOL_CONTOURS_STYLE,"VECTOR" );
   }

   vectorContour->SetSize( this->GetRect() );
   vectorContour->ShowModal();
}
////////////////////////////////////////////////////////////
void Vistab::_onStreamline( wxCommandEvent& WXUNUSED(event) )
{
   if( _activeScalarName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      return;
   }
   else if(!streamline)
   {
      streamline = new Streamlines ( this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_STREAMLINES_TITLE,
                  SYMBOL_STREAMLINES_POSITION,
                  SYMBOL_STREAMLINES_SIZE, 
                  SYMBOL_STREAMLINES_STYLE );
   }
   streamline->SetSize( this->GetRect() );
   
   if ( streamline->ShowModal() == wxID_OK )
   {
      ves::open::xml::Command* veCommand = new ves::open::xml::Command();
      veCommand->SetCommandName( std::string("Display Seed Points") );
	  ves::open::xml::DataValuePair* seedPointDVP = new ves::open::xml::DataValuePair();
      seedPointDVP->SetData("OnOff",static_cast<unsigned int>(0));
      veCommand->AddDataValuePair(seedPointDVP);

      ves::open::xml::DataValuePair* activeDataset = new ves::open::xml::DataValuePair;
      activeDataset->SetData("Active Dataset",GetActiveDatasetName());
      veCommand->AddDataValuePair(activeDataset);

	  ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
      delete veCommand;   
   }
}
////////////////////////////////////////////////////////////
void Vistab::_onIsosurface( wxCommandEvent& WXUNUSED(event) )
{
   if( _activeScalarName.empty() )
   {
      wxMessageBox( _("Select a scalar"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      return;
   }
   else if(!isosurface)
   {
      isosurface = new Isosurfaces ( this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_ISOSURFACES_TITLE,
                  SYMBOL_ISOSURFACES_POSITION,
                  SYMBOL_ISOSURFACES_SIZE, 
                  SYMBOL_ISOSURFACES_STYLE );

	  isosurface->SetActiveScalar(_activeScalarName);
   }

   isosurface->SetSize( this->GetRect() );
   _activeScalarRange = _originalScalarRanges[_activeScalarName];
   isosurface->SetScalarRange( _activeScalarName, _activeScalarRange );
   isosurface->SetAvailableScalars( _availableSolutions["MESH_SCALARS"] );
   isosurface->SetScalarList( _originalScalarRanges );
//   isosurface->InitializeScalarData( _activeScalarName );
   isosurface->ShowModal();
   //_activeScalarName.erase();
   
}
////////////////////////////////////////////////////////////
void Vistab::_onTextureBased( wxCommandEvent& WXUNUSED(event) )
{
   if(!_tbTools)
   {
      _tbTools = new TextureBasedToolBar (this,-1);
      _tbTools->CentreOnParent();
   }

   _tbTools->SetSize(this->GetSize().x, this->GetSize().y, -1, -1, wxSIZE_USE_EXISTING);
   _tbTools->SetSubDialogSize( this->GetSize() );
   _tbTools->SetVectors(_availableSolutions["TEXTURE_VECTORS"]);
   _tbTools->SetScalars(_availableSolutions["TEXTURE_SCALARS"]);

   if(_tbTools->ActivateTextureVisualization())
   {
      _tbTools->CentreOnParent();
      _tbTools->Show();
   }
}
////////////////////////////////////////////////////////////
void Vistab::_onPolydata( wxCommandEvent& WXUNUSED(event) )
{
   if( _activeScalarName.empty() )
   {
      wxMessageBox( _("Select a scalar"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      return;
   }
   else if(!polydata)
   {
      polydata = new Polydata ( this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_POLYDATA_TITLE,
                  SYMBOL_POLYDATA_POSITION,
                  SYMBOL_POLYDATA_SIZE, 
                  SYMBOL_POLYDATA_STYLE );
   } 
   polydata->SetSize(this->GetRect());
   polydata->SetAvailableScalars(_availableSolutions["MESH_SCALARS"]);
   polydata->SetActiveScalar(_activeScalarName);
   polydata->ShowModal();
}
///////////////////////////////////////////////////////
void Vistab::SetActiveModel(VjObs::Model_var activeModel)
{
   _activeModel = activeModel;
   _updateModelInformation(_activeModel);
   if(_datasetSelection)
   {
      _datasetSelection->Clear();
      for(size_t i = 0; i < _availableDatasets.GetCount(); i++)
	  {
         _datasetSelection->Append(_availableDatasets[i]);
	  }
     //set the active dataset to the initial dataset in the model
      _setActiveDataset(0); 
   }
}
///////////////////////////////////////////////
void Vistab::SetActiveDataset(std::string name)
{
   //is this incorrect to assume we have a model? -- biv
   if(_activeModel)
   {
      //loop over available datasets
      for(unsigned int i = 0; i < _activeModel->dataVector.length(); i++)
      {
         if(wxString(_activeModel->dataVector[i].datasetname,wxConvUTF8) == wxString(name.c_str(), wxConvUTF8))
         {
            _setActiveDataset(i);
            return;
         }
      }
   }
}
//////////////////////////////////////////////////////////////
void Vistab::_updateModelInformation(VjObs::Model_var newModel)
{
   //number of datasets
   _nDatasetsInActiveModel =  newModel->dataVector.length();   
   if(_nDatasetsInActiveModel > 0 )
   {
      //get all the dataset names in this model
      _availableDatasets.Clear();
      for(unsigned int i = 0; i < _nDatasetsInActiveModel; i++)
      {
         _availableDatasets.Add(wxString(newModel->dataVector[i].datasetname, wxConvUTF8));
      }
   }
}
//////////////////////////////////////////////////
void Vistab::_setActiveDataset(unsigned int index)
{
   //std::cout<<"setActiveDataset"<<std::endl;
   //is this incorrect to assume we have a model? -- biv
   if(_activeModel)
   {
      CORBA::ULong i = index;
      if(i < _nDatasetsInActiveModel)
      {
         _activeDataset = _activeModel->dataVector[i];
         _activeDataSetName = _activeDataset.datasetname;
         //if(_datasetSelection)
         {
            _datasetSelection->SetSelection(i);
         }
         //update the available scalar, vector and texture data
         _updateDatasetInformation(_activeDataset);
      }
   }
}
/////////////////////////////////////////////////
void Vistab::SetTextureData(wxArrayString textureData,
                            std::string type)
{
   if(type != "TEXTURE_SCALARS"&& 
      type != "TEXTURE_VECTORS")
   {
      std::cout<<"Invalid type: "<<type<<std::endl;
      return;
   }
   std::map<std::string, wxArrayString >::iterator currentSolution;
   currentSolution = _availableSolutions.find( type );
   if(currentSolution != _availableSolutions.end())
   {
      currentSolution->second.Clear();
      for(size_t i = 0; i < textureData.Count(); i++)
      {
         currentSolution->second.Add(textureData[i]);
      }
   }
}
////////////////////////////////////////////////////////////////////
void Vistab::_updateDatasetInformation(VjObs::Dataset datasetInfo )
{
   _nScalarsInActiveDataset = datasetInfo.scalarVector.length();
   if(_nScalarsInActiveDataset)
   {
      _updateAvailableScalarMeshSolutions(datasetInfo.scalarVector);
	  SetButtonStatus("All Scalar Operations",true);
   }
   else
   {
	   ///It there are no scalars, vectors probably won't work
	   ///either...not sure of the logic on the xplorer side though
       SetButtonStatus("All Scalar Operations",false);
   }

   _nVectorsInActiveDataset = datasetInfo.vectornames.length();
   if(_nVectorsInActiveDataset)
   {
      _updateAvailableSolutions("MESH_VECTORS",datasetInfo.vectornames);
	  SetButtonStatus("All Vector Operations",true);
   }
   else
   {
	   SetButtonStatus("All Vector Operations",false);
   }
}
///////////////////////////////////////////////////////////////////////////
//We need this extra method because the data info is stored differenly   //
//for scalars                                                            // 
///////////////////////////////////////////////////////////////////////////
void Vistab::_updateAvailableScalarMeshSolutions(VjObs::Scalars newScalars)
{
   ///clear out the scalar ranges from before
   _originalScalarRanges.clear();

   //std::cout<<"updateAvailableScalarMeshSolutions"<<std::endl;
   std::map<std::string, wxArrayString >::iterator currentSolution;
   currentSolution = _availableSolutions.find( "MESH_SCALARS" );
   if(currentSolution != _availableSolutions.end())
   {
      currentSolution->second.Clear();
      for(size_t i = 0; i < newScalars.length(); i++)
      {
         _originalScalarRanges[std::string(newScalars[i].scalarnames)].push_back(newScalars[i].scalarrange[0]);
         _originalScalarRanges[std::string(newScalars[i].scalarnames)].push_back(newScalars[i].scalarrange[1]);

         currentSolution->second.Add(wxString(newScalars[i].scalarnames, wxConvUTF8));
      }
      _updateComboBoxNames("MESH_SCALARS",currentSolution->second);      
   }
   else
   {
      std::cout<<"MESH_SCALAR data not available in current dataset: "<<_activeDataset.datasetname<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////////
void Vistab::_updateAvailableSolutions(std::string dataType,
                                       VjObs::scalar_p names)
{
   if(dataType != "MESH_VECTORS")
   {
      std::cout<<dataType<<" not available in current dataset: "<<_activeDataset.datasetname<<std::endl;
      return;
   }
   //std::cout<<"updateAvailableSolutions"<<std::endl;
   std::map<std::string, wxArrayString >::iterator currentSolution;
   currentSolution = _availableSolutions.find( dataType );
   _none = new wxString("None",wxConvUTF8);
   if(currentSolution != _availableSolutions.end())
   {
      currentSolution->second.Clear();
      for(size_t i = 0; i < names.length(); i++)
      {
         currentSolution->second.Add(wxString(names[i], wxConvUTF8));
      }
//      _none = "None";
      _updateComboBoxNames("MESH_VECTORS",currentSolution->second);
      _vectorSelection->InsertItems(1,_none,0); 
   }
   else
   {
      std::cout<<dataType<<" not available in current dataset: "<<_activeDataset.datasetname<<std::endl;
   }
}
///////////////////////////////////////////////////////////
void Vistab::_updateComboBoxNames(std::string dataType,
                                 wxArrayString listOfNames)
{
   //std::cout<<"updateComboBoxNames"<<std::endl;
   wxListBox* activeComboBox = 0; 
   if(dataType == "MESH_SCALARS" ||
      dataType == "TEXTURE_SCALARS")
   {
      //std::cout<<"scalar combo box"<<std::endl;
      activeComboBox = _scalarSelection;
   }
   else if(dataType == "MESH_VECTORS" ||
         dataType == "TEXTURE_VECTORS")
   {
      //std::cout<<"vector list box"<<std::endl;
      activeComboBox = _vectorSelection;
   }
   else
   {
      std::cout<<"Invalid data type: "<<dataType<<std::endl;
   }
   if(activeComboBox)
   {
      activeComboBox->Clear();
      for(size_t i = 0; i < listOfNames.GetCount(); i++)
      {
         activeComboBox->Insert(listOfNames[i],i);
      }
   }

   //if( _activeScalarName.empty() )
   {
      _scalarSelection->SetSelection(0);
      _activeScalarName = ConvertUnicode( _scalarSelection->GetStringSelection() );
   }
   /*else
   {
      _scalarSelection->SetSelection(scalarValue);
      _activeScalarName = ConvertUnicode( _scalarSelection->GetStringSelection() );
   }*/

   //scalarSelect = false;
   //vectorSelect = false;
}
////////////////////////////////////////////
DualSlider* Vistab::GetScalarRangeControls()
{
   return scalarRange;
}
////////////////////////////////////////////////////
void Vistab::_OnSelectDataset(wxCommandEvent& WXUNUSED(event))
{
   //_activeDataSetName = ConvertUnicode( _datasetSelection->GetValue() );
   // SetActiveDataset(_activeDataSetName);
   _setActiveDataset( _datasetSelection->GetCurrentSelection() );
   UpdateSpinControls();
}
///////////////////////////////////////////////////
void Vistab::_OnSelectScalar(wxCommandEvent& WXUNUSED(event))
{ 
   _activeScalarName = ConvertUnicode( _scalarSelection->GetStringSelection() );
   _activeScalarRange = _originalScalarRanges[_activeScalarName];

   if( !_activeScalarName.empty() )
   {
      double minBoundRange = ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 0.99;
      double maxBoundRange = ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 0.01;
      _minSpinner->SetRange( _activeScalarRange.at(0), minBoundRange );   
      _minSpinner->SetValue( _activeScalarRange.at(0) );
      _maxSpinner->SetRange( maxBoundRange, _activeScalarRange.at(1) );
      _maxSpinner->SetValue( _activeScalarRange.at(1) );

      if( _activeScalarRange.at(1) == _activeScalarRange.at(0) )
      {
         _minSpinner->Enable(false);
         _maxSpinner->Enable(false);
         _minSlider->Enable(false);
         _maxSlider->Enable(false);
      }
      else
      {
         _minSpinner->Enable(true);
         _maxSpinner->Enable(true);
         _minSlider->Enable(true);
         _maxSlider->Enable(true);
      }

      _minSlider->SetValue( 0 );
      _maxSlider->SetValue( 100 );
      //scalarValue = _scalarSelection->GetSelection();
      
      //scalarSelect = true;
   }
}
///////////////////////////////////////////////////
void Vistab::_OnSelectVector(wxCommandEvent& WXUNUSED(event))
{
   _activeVectorName = ConvertUnicode( _vectorSelection->GetStringSelection() );

   if( _nScalarsInActiveDataset == 0 )
   {
      wxMessageBox( _("Scalar must be present"),_("Dataset Warning"), 
                      wxOK | wxICON_INFORMATION );
      return;
   }

   //not sure why we do this...
   //_scalarSelection->Select(0);
   _activeScalarName = ConvertUnicode( _scalarSelection->GetStringSelection() );
   _activeScalarRange = _originalScalarRanges[_activeScalarName];

   double minBoundRange = ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 0.99;
   double maxBoundRange = ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 0.01;

   _minSpinner->SetRange( _activeScalarRange.at(0), minBoundRange );   
   _minSpinner->SetValue( _activeScalarRange.at(0) );
   _maxSpinner->SetRange( maxBoundRange, _activeScalarRange.at(1) );
   _maxSpinner->SetValue( _activeScalarRange.at(1) );

   if( _activeScalarRange.at(1) == _activeScalarRange.at(0) )
   {
      _minSpinner->Enable(false);
      _maxSpinner->Enable(false);
      _minSlider->Enable(false);
      _maxSlider->Enable(false);
   }
   else
   {
      _minSpinner->Enable(true);
      _maxSpinner->Enable(true);
      _minSlider->Enable(true);
      _maxSlider->Enable(true);
   }
   _minSlider->SetValue( 0 );
   _maxSlider->SetValue( 100 );

   //vectorSelect = true;
}
/*
///////////////////////////////////////////////////
void Vistab::_OnMinSlider(wxSpinEvent& WXUNUSED(event))
{

}
///////////////////////////////////////////////////
void Vistab::_OnMaxSlider(wxSpinEvent& WXUNUSED(event))
{

}
*/
//////////////////////////////////////////
std::string Vistab::GetActiveScalarName()
{
   return _activeScalarName;
}
//////////////////////////////////////////
std::string Vistab::GetActiveVectorName()
{
   return _activeVectorName;
}
//////////////////////////////////////////
std::string Vistab::GetActiveDatasetName()
{
   return _activeDataSetName;
}
////////////////////////////////////
void Vistab::ClearBaseInformation()
{
   _vistabBaseInformation.clear();
   _commandName.clear();
}
////////////////////////////////////////
void Vistab::ClearSpecificInformation()
{
   _vistabSpecificInformation.clear();
   _commandName.clear();
}
///////////////////////////////////////////
void Vistab::_updateBaseInformation()
{
   ClearBaseInformation();
   ///This is the default. Other dialogs actions will set the command name to the specific value if they are launched
   _commandName = "VISUALIZATION_SETTINGS";

   ves::open::xml::DataValuePair* activeScalar = new ves::open::xml::DataValuePair();
   activeScalar->SetDataType("STRING");
   activeScalar->SetDataName(std::string("Active Scalar"));
   activeScalar->SetDataString(_activeScalarName);

   _vistabBaseInformation.push_back(activeScalar);
   
   ves::open::xml::DataValuePair* activeVector = new ves::open::xml::DataValuePair();
   activeVector->SetDataType("STRING");
   activeVector->SetDataName(std::string("Active Vector"));
   activeVector->SetDataString(_activeVectorName);

   _vistabBaseInformation.push_back(activeVector);

   ves::open::xml::DataValuePair* activeDataset= new ves::open::xml::DataValuePair();
   activeDataset->SetDataType("STRING");
   activeDataset->SetDataName(std::string("Active Dataset"));
   activeDataset->SetDataString(_activeDataSetName);

   _vistabBaseInformation.push_back(activeDataset);

   ves::open::xml::DataValuePair* scalarMin = new ves::open::xml::DataValuePair();
//   minimumValue = _activeScalarRange.at(0) 
//                    + (scalarRange->GetMinSliderValue()/100.0)*(_activeScalarRange.at(1) - _activeScalarRange.at(0));
   minimumValue = _activeScalarRange.at(0) 
                    + ((double)_minSlider->GetValue()/100.0)*(_activeScalarRange.at(1) - _activeScalarRange.at(0));

   scalarMin->SetData("Scalar Min",minimumValue);

   _vistabBaseInformation.push_back(scalarMin);

   ves::open::xml::DataValuePair* scalarMax = new ves::open::xml::DataValuePair();
//   maximumValue = _activeScalarRange.at(0) 
//                    + (scalarRange->GetMaxSliderValue()/100.0)*(_activeScalarRange.at(1) - _activeScalarRange.at(0));
   maximumValue = _activeScalarRange.at(1) - ( (_activeScalarRange.at(1) - _activeScalarRange.at(0)) 
                  * (100 - (double)_maxSlider->GetValue()) / 100.0);
   scalarMax->SetData("Scalar Max",maximumValue);
   
   _vistabBaseInformation.push_back(scalarMax);

   //Store the axes display value
   /*VE_XML::DataValuePair* axes= new VE_XML::DataValuePair();
   axes->SetData( std::string("Show Axes"), static_cast< unsigned int >( axesCB->GetValue() ) );
   _vistabBaseInformation.push_back( axes );*/

   //Store the axes display value
   ves::open::xml::DataValuePair* bbox = new ves::open::xml::DataValuePair();
   bbox->SetData( std::string("Show Bounding Box"), static_cast< unsigned int >( bboxCB->GetValue() ) );
   _vistabBaseInformation.push_back( bbox );

   //Store the axes display value
   ves::open::xml::DataValuePair* wireMesh= new ves::open::xml::DataValuePair();
   wireMesh->SetData( std::string("Show Wire Mesh"), static_cast< unsigned int >( wireFrameCB->GetValue() ) );
   _vistabBaseInformation.push_back( wireMesh );

   //set scalar bar state
   ves::open::xml::DataValuePair* scalarBarDVP = new ves::open::xml::DataValuePair();
   scalarBarDVP->SetData( "Scalar Bar State", static_cast< unsigned int >( scalarBarCB->GetValue() ) );
   _vistabBaseInformation.push_back( scalarBarDVP );
}
////////////////////////////////////////////////////////////////////////////
void Vistab::OnClearAll( wxCommandEvent& WXUNUSED(event) )
{
   ves::open::xml::DataValuePair* dataValuePair = new ves::open::xml::DataValuePair(  std::string("UNSIGNED INT") );
   dataValuePair->SetDataName( "CLEAR_ALL" );
   dataValuePair->SetDataValue( static_cast< unsigned int >( 1 ) );
   ves::open::xml::Command* veCommand = new ves::open::xml::Command();
   veCommand->SetCommandName( std::string("CLEAR_VIS_OBJECTS") );
   veCommand->AddDataValuePair( dataValuePair );

   bool connected = ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   delete veCommand;
}
////////////////////////////////////////////////////////////////////////////
void Vistab::SendUpdatedSettingsToXplorer(ves::open::xml::Command* subDialogCommand)
{
   _updateBaseInformation();
   ves::open::xml::Command* newCommand = new ves::open::xml::Command();

   for(size_t i =0; i < _vistabBaseInformation.size(); i++)
   {
      newCommand->AddDataValuePair(_vistabBaseInformation.at(i));
   }
   if(subDialogCommand)
   {
      ves::open::xml::DataValuePair* subDialogSettings = new ves::open::xml::DataValuePair();
      subDialogSettings->SetData("Sub-Dialog Settings",subDialogCommand);
      newCommand->AddDataValuePair(subDialogSettings);
   }

   newCommand->SetCommandName(_commandName);

   bool connected = ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( newCommand );

   delete newCommand;
   newCommand = 0;
}
///////////////////////////////////////////////////////////////////////////
void Vistab::_onMinSpinCtrl( wxScrollEvent& WXUNUSED(event) )
{
//   double range = _activeScalarRange.at(1) - _activeScalarRange.at(0);
   double minValue = 0;

   if( _activeScalarName.empty() && _activeVectorName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _minSpinner->SetValue(0);
      return;
   }

   minValue = ( ( _minSpinner->GetValue() - _activeScalarRange.at(0) ) 
               / ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 100);

   if( minValue == 100 )
   {  
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
   }
   else if( _maxSlider->GetValue() <= (int)minValue )
   {
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
      _maxSpinner->SetValue( _activeScalarRange.at(1) - ( ( _activeScalarRange.at(1) - _activeScalarRange.at(0) )
                               * ( 100 - (double)_maxSlider->GetValue() ) / 100 ) );
   }
   else
   {
      _minSlider->SetValue( (int)minValue );  
   }
} 
///////////////////////////////////////////////////////////////////////////
void Vistab::_onMaxSpinCtrl( wxScrollEvent& WXUNUSED(event) )
{
//   double range = _activeScalarRange.at(1) - _activeScalarRange.at(0);
   double maxValue = 100;

   if( _activeScalarName.empty() && _activeVectorName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _maxSpinner->SetValue(100);
      return;
   }

   maxValue = ( ( _activeScalarRange.at(1) - _activeScalarRange.at(0) 
               - ( _activeScalarRange.at(1) - _maxSpinner->GetValue() ) ) 
               / ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 100);

   if( maxValue == 0 )
   {  
      _minSlider->SetValue( (int)maxValue+1 );
      _maxSlider->SetValue( (int)maxValue );     
   }
   else if( _minSlider->GetValue() >= (int)maxValue )
   {
      _minSlider->SetValue( (int)maxValue-1 );
      _maxSlider->SetValue( (int)maxValue );   
      _minSpinner->SetValue( ( _activeScalarRange.at(1) - _activeScalarRange.at(0) )
                              * (double)_minSlider->GetValue() / 100 + _activeScalarRange.at(0) );
   } 
   else
   {  
      _maxSlider->SetValue( (int)maxValue );   
   } 
} 
//////////////////////////////////////////////////////////////////////////
void Vistab::_onMinMaxSlider( wxScrollEvent& WXUNUSED(event) )
{
   double range = _activeScalarRange.at(1) - _activeScalarRange.at(0);

   _minSpinner->SetValue( ( range - (double)scalarRange->GetMinSliderValue() ) / 100 + _activeScalarRange.at(0) );
   _maxSpinner->SetValue( ( _activeScalarRange.at(1) - ( range - ( range - (double)scalarRange->GetMaxSliderValue() ) ) ) / 100);
}
//////////////////////////////////////////////////////////////////////////
void Vistab::_onMinSlider( wxScrollEvent& WXUNUSED(event) )
{
   if( _activeScalarName.empty() && _activeVectorName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _minSlider->SetValue(0);
      return;
   }

   double range = _activeScalarRange.at(1) - _activeScalarRange.at(0);

   if( _minSlider->GetValue() >= _maxSlider->GetValue() ) // && _minSlider->GetValue() < 100 )
   {
      _ensureSliders(MIN_SLIDER);
   }

   _minSpinner->SetValue( range * (double)_minSlider->GetValue() / 100  + _activeScalarRange.at(0) );
   _maxSpinner->SetValue( _activeScalarRange.at(1) - ( range * ( 100 - (double)_maxSlider->GetValue() ) / 100 ) ); 
}
//////////////////////////////////////////////////////////////////////////
void Vistab::_onMaxSlider( wxScrollEvent& WXUNUSED(event) )
{
   if( _activeScalarName.empty() && _activeVectorName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _maxSlider->SetValue(100);
      return;
   }

   double range = _activeScalarRange.at(1) - _activeScalarRange.at(0);

   if( _maxSlider->GetValue() <= _minSlider->GetValue() ) //&& _maxSlider->GetValue() > 0 )
   {
      _ensureSliders(MAX_SLIDER);
   }

   _minSpinner->SetValue( ( range * (double)_minSlider->GetValue() ) / 100 + _activeScalarRange.at(0) );
   _maxSpinner->SetValue( _activeScalarRange.at(1) - ( range * ( 100 - (double)_maxSlider->GetValue() ) / 100 ) );
}
//////////////////////////////////////////////////////////////////////////
bool Vistab::_ensureSliders(int activeSliderID)
{
   int minValue = _minSlider->GetValue();
   int maxValue = _maxSlider->GetValue();

   //maintain the value on the min/max sliders.
   if(minValue > maxValue - static_cast<int>(1))
   {
      if(minValue == 100)
      {
         _minSlider->SetValue(100 - 1);
      }
      else if(maxValue == 0)
      {
         _maxSlider->SetValue(0 + 1);
      }

      if(activeSliderID == MIN_SLIDER)
      {
         _maxSlider->SetValue(_minSlider->GetValue() + 1);
         return true;
      }
      else if(activeSliderID == MAX_SLIDER)
      {
         _minSlider->SetValue(_maxSlider->GetValue() - 1);
         return true;
      }
   }
   return false;
}
////////////////////////////////////////////////////////////////////////
void Vistab::_onClose( wxCommandEvent& WXUNUSED(event) )
{
//   vistab->EndModal(-1);
//   vistab->OnOK(EVT_BUTTON);
//   vistab->Show(false);
   //scalarSelect = false;
   //vectorSelect = false;
//   _activeDataSetName.erase();
}
////////////////////////////////////////////////////////////////////////
void Vistab::UpdateBoundingBox( wxCommandEvent& WXUNUSED(event) )
{
   ves::open::xml::DataValuePair* dataValuePair = new ves::open::xml::DataValuePair();
   dataValuePair->SetData( "Bounding Box State", static_cast< unsigned int >( bboxCB->GetValue() ) );
   ves::open::xml::Command* veCommand = new ves::open::xml::Command();
   veCommand->SetCommandName( std::string("Change Bounding Box State") );
   veCommand->AddDataValuePair( dataValuePair );
   
   bool connected = ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   delete veCommand;
}
////////////////////////////////////////////////////////////////////////
void Vistab::UpdateWireFrame( wxCommandEvent& WXUNUSED(event) )
{
   ves::open::xml::DataValuePair* dataValuePair = new ves::open::xml::DataValuePair();
   dataValuePair->SetData( "Wire Frame State", static_cast< unsigned int >( wireFrameCB->GetValue() ) );
   ves::open::xml::Command* veCommand = new ves::open::xml::Command();
   veCommand->SetCommandName( std::string("Change Wire Frame State") );
   veCommand->AddDataValuePair( dataValuePair );
   
   bool connected = ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   delete veCommand;
}
////////////////////////////////////////////////////////////////////////
void Vistab::UpdateAxes( wxCommandEvent& WXUNUSED(event) )
{
   ves::open::xml::DataValuePair* dataValuePair = new ves::open::xml::DataValuePair();
   dataValuePair->SetData( "Axes State", static_cast< unsigned int >( axesCB->GetValue() ) );
   ves::open::xml::Command* veCommand = new ves::open::xml::Command();
   veCommand->SetCommandName( std::string("Change Axes State") );
   veCommand->AddDataValuePair( dataValuePair );
   
   bool connected = ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   delete veCommand;
   
   if ( axesCB->IsChecked() )
   {
      xAxisEntry->Enable();
      yAxisEntry->Enable();
      zAxisEntry->Enable();
   }
   else
   {
      xAxisEntry->Disable();
      yAxisEntry->Disable();
      zAxisEntry->Disable();
   }
}
////////////////////////////////////////////////////////////////////////
void Vistab::UpdateAxesLabels( wxCommandEvent& event )
{
   std::string activeAxesLAbel = "Axes Labels";
   std::vector< std::string > labels;
   labels.push_back( ConvertUnicode( xAxisEntry->GetValue().c_str() ) );
   labels.push_back( ConvertUnicode( yAxisEntry->GetValue().c_str() ) );
   labels.push_back( ConvertUnicode( zAxisEntry->GetValue().c_str() ) );
   
   ves::open::xml::DataValuePair* dataValuePair = new ves::open::xml::DataValuePair();
   dataValuePair->SetData( activeAxesLAbel, labels );
   ves::open::xml::Command* veCommand = new ves::open::xml::Command();
   veCommand->SetCommandName( std::string("Change Axes Labels") );
   veCommand->AddDataValuePair( dataValuePair );
   
   bool connected = ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   delete veCommand;
}
////////////////////////////////////////////////////////////////////////
void Vistab::UpdateScalarBar( wxCommandEvent& event )
{
   ves::open::xml::DataValuePair* dataValuePair = new ves::open::xml::DataValuePair();
   dataValuePair->SetData( "Scalar Bar State", static_cast< unsigned int >( scalarBarCB->GetValue() ) );
   ves::open::xml::Command* veCommand = new ves::open::xml::Command();
   veCommand->SetCommandName( std::string("Change Scalar Bar State") );
   veCommand->AddDataValuePair( dataValuePair );
   
   bool connected = CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   delete veCommand;
}
////////////////////////////////////////////////////////////////////////
void Vistab::UpdateMinSlider( wxCommandEvent& event )
{
//   double range = _activeScalarRange.at(1) - _activeScalarRange.at(0);
   double minValue = 0;

   if( _activeScalarName.empty() && _activeVectorName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"), _("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _minSpinner->SetValue(0);
      return;
   }

   minValue = ( ( _minSpinner->GetValue() - _activeScalarRange.at(0) ) 
               / ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 100);

   if( minValue == 100 )
   {  
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
   }
   else if( _maxSlider->GetValue() <= (int)minValue )
   {
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
      _maxSpinner->SetValue( _activeScalarRange.at(1) - ( ( _activeScalarRange.at(1) - _activeScalarRange.at(0) )
                               * ( 100 - (double)_maxSlider->GetValue() ) / 100 ) );
   }
   else
   {
      _minSlider->SetValue( (int)minValue );  
   }
} 
////////////////////////////////////////////////////////////////////////
void Vistab::UpdateMaxSlider( wxCommandEvent& event )
{
   double maxValue = 100;

   if( _activeScalarName.empty() && _activeVectorName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"), _("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _maxSpinner->SetValue(100);
      return;
   }

   maxValue = ( ( _activeScalarRange.at(1) - _activeScalarRange.at(0) 
               - ( _activeScalarRange.at(1) - _maxSpinner->GetValue() ) ) 
               / ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 100);

   if( maxValue == 0 )
   {  
      _minSlider->SetValue( (int)maxValue+1 );
      _maxSlider->SetValue( (int)maxValue );     
   }
   else if( _minSlider->GetValue() >= (int)maxValue )
   {
      _minSlider->SetValue( (int)maxValue-1 );
      _maxSlider->SetValue( (int)maxValue );   
      _minSpinner->SetValue( ( _activeScalarRange.at(1) - _activeScalarRange.at(0) )
                              * (double)_minSlider->GetValue() / 100 + _activeScalarRange.at(0) );
   } 
   else
   {  
      _maxSlider->SetValue( (int)maxValue );   
   } 
}
////////////////////////////////////////////////////////////////////////
void Vistab::InitialScalarVector()
{
   if( !_scalarSelection->IsEmpty() )
   {
      _scalarSelection->SetSelection(0);
      _activeScalarName = ConvertUnicode( _scalarSelection->GetStringSelection() );
      //_scalarSelection->GetStringSelection();
      //_activeScalarName.assign(GetActiveScalarName());

      //_scalarSelection->UpdateWindowUI();
      //_scalarSelection->SetString(0);
      //std::cout<<"THIS WORKS: "<<_activeScalarName<<std::endl;
   }
}
////////////////////////////////////////////////////////////////
void Vistab::UpdateSpinControls()
{
   _activeScalarName = ConvertUnicode( _scalarSelection->GetStringSelection() );
   _activeScalarRange = _originalScalarRanges[_activeScalarName];
   
   _minSpinner->SetRange( _activeScalarRange.at(0), _activeScalarRange.at(1) );
   _maxSpinner->SetRange( _activeScalarRange.at(0), _activeScalarRange.at(1) );
   _minSpinner->SetValue( _activeScalarRange.at(0) );
   _maxSpinner->SetValue( _activeScalarRange.at(1) );

   if( _activeScalarRange.at(1) == _activeScalarRange.at(0) )
   {
		_minSpinner->Enable(false);
		_maxSpinner->Enable(false);
		return;
   }

   _minSpinner->Enable(true);
   _maxSpinner->Enable(true);
   _minSlider->Enable(true);
   _maxSlider->Enable(true);
   _minSlider->SetValue(0);
   _maxSlider->SetValue(100);
   //double minValue = 0;
   //double maxValue = 100;

   if( _activeScalarName.empty() && _activeVectorName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _minSpinner->SetValue(0);
	  _maxSpinner->SetValue(100);
      return;
   }
}
////////////////////////////////////////////////////////////////
void Vistab::SetButtonStatus(std::string buttonName, bool onOff)
{
    std::map<std::string,VISTAB_IDS>::iterator buttonIterator;
    //check for "All"
    if(buttonName == "All")
    {
        for(buttonIterator = m_vistabButtonMap.begin();
            buttonIterator != m_vistabButtonMap.end();
            ++buttonIterator)
        {
            itemToolBar3->EnableTool(buttonIterator->second,onOff);             
        }
    }
	else if(buttonName == "All Scalar Operations")
    {
		itemToolBar3->EnableTool(m_vistabButtonMap["Scalar Contour"],onOff);
		itemToolBar3->EnableTool(m_vistabButtonMap["Isosurface"],onOff);
        ///Is polydata really scalar dependent?
		itemToolBar3->EnableTool(m_vistabButtonMap["Polydata"],onOff);
    }
	else if(buttonName == "All Vector Operations")
    {
		itemToolBar3->EnableTool(m_vistabButtonMap["Vector Contour"],onOff);
		itemToolBar3->EnableTool(m_vistabButtonMap["Streamlines"],onOff);
    }
    else
    {
        buttonIterator = m_vistabButtonMap.find(buttonName);
        if(buttonIterator != m_vistabButtonMap.end())
        {
            itemToolBar3->EnableTool(buttonIterator->second,onOff);             
        }
        else
        {
             wxMessageBox( _("Invalid button specified!"),_("Button failure!"),
                     wxOK | wxICON_ERROR );

        }
    }
}
