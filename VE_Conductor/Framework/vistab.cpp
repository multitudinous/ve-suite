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
 * File:          $RCSfile: vistab.cpp vistab.cppvistab.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/TBToolBar.h"
#include "VE_Conductor/Framework/App.h"
#include "VE_Conductor/Framework/Frame.h"
#include "VE_Conductor/Framework/CORBAServiceList.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include "VE_Builder/Utilities/gui/spinctld.h"
////@end includes

#include "VE_Conductor/Framework/vistab.h"
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

#include <iostream>
#include <string>
////@begin XPM images
#include "VE_Conductor/xpm/new_vector.xpm"
#include "VE_Conductor/xpm/contour.xpm"
#include "VE_Conductor/xpm/streamlines.xpm"
#include "VE_Conductor/xpm/isosurface.xpm"
#include "VE_Conductor/xpm/scalartb.xpm"
////@end XPM images

BEGIN_EVENT_TABLE( Vistab, wxDialog )
////@begin Vistab event table entries
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
////@end Vistab event table entries
END_EVENT_TABLE()
using namespace VE_Conductor::GUI_Utilities;

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
   xplorerPtr = 0;
   polydata = 0;

   scalarSelect = false;
   vectorSelect = false;

   _availableSolutions["MESH_SCALARS"].Add(""); 
   _availableSolutions["MESH_VECTORS"].Add(""); 
   _availableSolutions["TEXTURE_SCALARS"].Add("");  
   _availableSolutions["TEXTURE_VECTORS"].Add("");
   _commandName = "VISUALIZATION_TAB";
   SetActiveModel(activeModel);
   _activeScalarName = _scalarSelection->GetStringSelection();
   _activeVectorName = _vectorSelection->GetStringSelection();
   _activeScalarRange = _originalScalarRanges[_activeScalarName];

   _vistabPosition = dynamic_cast<AppFrame*>(wxTheApp->GetTopWindow())->GetAppropriateSubDialogSize();
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
   xplorerPtr = 0;
   isosurface =  0;
   _tbTools = 0;
   scalarContour = 0;
   vectorContour = 0;
   streamline = 0;
   polydata = 0;

   scalarSelect = false;
   vectorSelect = false;
   
   _availableSolutions["MESH_SCALARS"].Add(""); 
   _availableSolutions["MESH_VECTORS"].Add(""); 
   _availableSolutions["TEXTURE_SCALARS"].Add("");  
   _availableSolutions["TEXTURE_VECTORS"].Add(""); 

   SetActiveModel(activeModel);
   Create(parent, id, caption, pos, size, style);
   _commandName = "VISUALIZATION_TAB";
   _activeScalarName = _scalarSelection->GetStringSelection();
   _activeVectorName = _vectorSelection->GetStringSelection();
   _activeScalarRange = _originalScalarRanges[_activeScalarName];
   if(_nDatasetsInActiveModel)
   {
      _setActiveDataset(0);
   }
   _vistabPosition = dynamic_cast<AppFrame*>(wxTheApp->GetTopWindow())->GetAppropriateSubDialogSize();
   SetSize( _vistabPosition );
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool Vistab::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
////@begin Vistab member initialisation
   itemToolBar3 = 0;
   itemComboBox11 = 0;
   itemComboBox12 = 0;
   itemListBox13 = 0; 
   itemListBox15 = 0;
   _minSpinner = 0;
   _maxSpinner = 0;
   /*vector = 0;
   contour = 0;
   streamline = 0;
   isosurface = 0;
   _tbTools = 0;*/
   scalarRange = 0;
////@end Vistab member initialisation

////@begin Vistab creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
//    GetSizer()->SetDimension(427, 20, 427, 400);
////@end Vistab creation
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

    itemToolBar3 = new wxToolBar( itemDialog1, ID_TOOLBAR, wxDefaultPosition, wxDefaultSize, wxTB_HORIZONTAL| wxTB_TEXT );
    itemToolBar3->SetToolBitmapSize(wxSize(45, 30));

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
    itemBoxSizer2->Add(itemToolBar3, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    //itemToolBar3->ToggleTool(CONTOUR_BUTTON, false);

    wxBoxSizer* itemBoxSizer10 = new wxBoxSizer(wxHORIZONTAL);
    itemBoxSizer2->Add(itemBoxSizer10, 0, wxGROW|wxALL, 5);

    _datasetSelection = new wxComboBox( itemDialog1, ID_COMBOBOX, _T(""), wxDefaultPosition, wxDefaultSize, _availableDatasets, wxCB_DROPDOWN );
    
    if (ShowToolTips())
        _datasetSelection->SetToolTip(_T("Data Sets"));
    itemBoxSizer10->Add(_datasetSelection, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    /*wxString itemComboBox12Strings[] = {
        _T("DataSet1"),
        _T("DataSet2"),
        _T("DataSet3")
    };
    itemComboBox12 = new wxComboBox( itemDialog1, ID_COMBOBOX1, _T(""), wxDefaultPosition, wxDefaultSize, 3, itemComboBox12Strings, wxCB_DROPDOWN );

    itemBoxSizer10->Add(itemComboBox12, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);*/

    wxBoxSizer* itemBoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
    itemBoxSizer2->Add(itemBoxSizer11, 0, wxGROW|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer12Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Scalars"));
    wxStaticBoxSizer* itemStaticBoxSizer12 = new wxStaticBoxSizer(itemStaticBoxSizer12Static, wxHORIZONTAL);
    itemBoxSizer11->Add(itemStaticBoxSizer12, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    _scalarSelection = new wxListBox( itemDialog1, ID_LISTBOX, wxDefaultPosition, wxSize(125, -1), _availableSolutions["MESH_SCALARS"] , wxLB_SINGLE );
    _scalarSelection->SetSelection(0);    
    itemStaticBoxSizer12->Add(_scalarSelection, 1, wxGROW|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer14Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Vectors"));
    wxStaticBoxSizer* itemStaticBoxSizer14 = new wxStaticBoxSizer(itemStaticBoxSizer14Static, wxHORIZONTAL);
    itemBoxSizer11->Add(itemStaticBoxSizer14, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    _vectorSelection = new wxListBox( itemDialog1, ID_LISTBOX1, wxDefaultPosition, wxSize(125, -1), _availableSolutions["MESH_VECTORS"], wxLB_SINGLE );
    _vectorSelection->SetSelection(0);
    itemStaticBoxSizer14->Add(_vectorSelection, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxStaticBox* scalarBoundsStatic = new wxStaticBox(itemDialog1, wxID_ANY, _T("Scalar Range"));
    wxStaticBoxSizer* scalarBoundsSizer = new wxStaticBoxSizer( scalarBoundsStatic, wxHORIZONTAL );
//    wxBoxSizer* scalarBoundsSizer = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* spinnerSizer = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer* scalarSizer = new wxBoxSizer(wxVERTICAL);

    wxStaticText* _space1 = new wxStaticText( itemDialog1, wxID_STATIC, _T(""), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE ); 
    wxStaticText* _min = new wxStaticText( itemDialog1, wxID_STATIC, _T("Min"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE ); 
    _minSpinner = new wxSpinCtrlDbl( *this, MIN_SPINCTRL, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 100, 0, 0.1, -1, wxEmptyString );
    wxStaticText* _space2 = new wxStaticText( itemDialog1, wxID_STATIC, _T(""), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE ); 
    wxStaticText* _space3 = new wxStaticText( itemDialog1, wxID_STATIC, _T(""), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
    wxStaticText* _max = new wxStaticText( itemDialog1, wxID_STATIC, _T("Max"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
    _maxSpinner = new wxSpinCtrlDbl( *this, MAX_SPINCTRL, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 100, 100, 0.1, -1, wxEmptyString );

    spinnerSizer->Add(_space1, 0, wxALIGN_LEFT|wxALL, 5);
    spinnerSizer->Add(_min, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5);   
    spinnerSizer->Add(_minSpinner, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxBOTTOM, 5);
    spinnerSizer->Add(_space2, 0, wxALIGN_LEFT|wxALL, 5);
    spinnerSizer->Add(_space3, 0, wxALIGN_LEFT, 5);
    spinnerSizer->Add(_max, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5);    
    spinnerSizer->Add(_maxSpinner, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxBOTTOM, 5);

//    scalarRange = new DualSlider(this,MIN_MAX_SLIDERS,1,0,100,0,100,wxDefaultPosition,wxDefaultSize,
//                             wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,wxString(""));
//    scalarSizer->Add(scalarRange,1,wxALIGN_CENTER|wxEXPAND);

    _minSlider = new wxSlider( itemDialog1, MIN_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    _maxSlider = new wxSlider( itemDialog1, MAX_SLIDER, 100, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    scalarSizer->Add(_minSlider,1,wxALIGN_CENTER|wxEXPAND);
    scalarSizer->Add(_maxSlider,1,wxALIGN_CENTER|wxEXPAND);


    scalarBoundsSizer->Add(spinnerSizer,1,wxALIGN_CENTER|wxEXPAND);
    scalarBoundsSizer->Add(scalarSizer,3,wxALIGN_CENTER|wxEXPAND);
    
    itemBoxSizer2->Add(scalarBoundsSizer, 3, wxALIGN_CENTER|wxEXPAND|wxALL, 5);
   
    //Last row buttons - advanced, clear all, ...
    wxBoxSizer* lastRowButtons = new wxBoxSizer( wxHORIZONTAL );
    itemBoxSizer2->Add( lastRowButtons, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );

    wxButton* itemButton20 = new wxButton( itemDialog1, ID_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    lastRowButtons->Add( itemButton20, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    clearAllButton = new wxButton( itemDialog1, ID_CLEAR_ALL_BUTTON, _T("Clear All"), wxDefaultPosition, wxDefaultSize, 0 );
    lastRowButtons->Add( clearAllButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

   wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
//    wxButton* _closeButton = new wxButton( itemDialog1, CLOSE_BUTTON, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
    lastRowButtons->Add(_closeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);


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
//////////////////////////////////////////////////
void Vistab::SetCommInstance( VjObs_ptr veEngine )
{
   xplorerPtr = VjObs::_duplicate( veEngine );
}
////////////////////////////////////////////////////////////
void Vistab::_onContour( wxCommandEvent& WXUNUSED(event) )
{
   if(!scalarContour)
   {
      scalarContour = new Contours(this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_CONTOURS_TITLE,
                  SYMBOL_CONTOURS_POSITION,
                  SYMBOL_CONTOURS_SIZE, 
                  SYMBOL_CONTOURS_STYLE );
      scalarContour->SetSize(_vistabPosition);
   }
   
   if(scalarSelect || vectorSelect)
   {
      scalarContour->ShowModal();
   }
   else
   {
      wxMessageBox( "Select a scalar or vector","Dataset Failure", 
                     wxOK | wxICON_INFORMATION );
   }
   //itemToolBar3->ToggleTool(CONTOUR_BUTTON, false);
}
/////////////////////////////////////////////////////////
void Vistab::_onVector( wxCommandEvent& WXUNUSED(event) )
{
   if(!vectorContour)
   {
      vectorContour = new Contours (this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_CONTOURS_TITLE,
                  SYMBOL_CONTOURS_POSITION,
                  SYMBOL_CONTOURS_SIZE, 
                  SYMBOL_CONTOURS_STYLE,"VECTOR" );
      vectorContour->SetSize(_vistabPosition);
   }

   if(scalarSelect || vectorSelect)
   {
      vectorContour->ShowModal();
   }
   else
   {
      wxMessageBox( "Select a scalar or vector","Dataset Failure", 
                     wxOK | wxICON_INFORMATION );
   }
   //itemToolBar3->ToggleTool(VECTOR_BUTTON, false);
}
////////////////////////////////////////////////////////////
void Vistab::_onStreamline( wxCommandEvent& WXUNUSED(event) )
{
   if(!streamline)
   {
      streamline = new Streamlines ( this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_STREAMLINES_TITLE,
                  SYMBOL_STREAMLINES_POSITION,
                  SYMBOL_STREAMLINES_SIZE, 
                  SYMBOL_STREAMLINES_STYLE );
      streamline->SetSize(_vistabPosition);
   }

   if(scalarSelect || vectorSelect)
   {
      streamline->ShowModal();
   }
   else
   {
      wxMessageBox( "Select a scalar or vector","Dataset Failure", 
                     wxOK | wxICON_INFORMATION );
   }
   //itemToolBar3->ToggleTool(STREAMLINE_BUTTON, false);
}
////////////////////////////////////////////////////////////
void Vistab::_onIsosurface( wxCommandEvent& WXUNUSED(event) )
{
   if(!isosurface)
   {
      isosurface = new Isosurfaces ( this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_ISOSURFACES_TITLE,
                  SYMBOL_ISOSURFACES_POSITION,
                  SYMBOL_ISOSURFACES_SIZE, 
                  SYMBOL_ISOSURFACES_STYLE );
      isosurface->SetSize(_vistabPosition);
   }
   
   if(scalarSelect || vectorSelect)
   {
      isosurface->SetAvailableScalars(_availableSolutions["MESH_SCALARS"]);
      isosurface->SetActiveScalar(_activeScalarName);
      isosurface->ShowModal();
   }
   else
   {
      wxMessageBox( "Select a scalar or vector","Dataset Failure", 
                     wxOK | wxICON_INFORMATION );
   }

   //itemToolBar3->ToggleTool(ISOSURFACE_BUTTON, false);
}
////////////////////////////////////////////////////////////
void Vistab::_onTextureBased( wxCommandEvent& WXUNUSED(event) )
{
   if(!_tbTools)
   {
   
      _tbTools = new TextureBasedToolBar (this,-1);
      _tbTools->SetSize(_vistabPosition.x, _vistabPosition.y, -1, -1, wxSIZE_USE_EXISTING);
   }
   _tbTools->SetVjObsPtr(xplorerPtr);
   _tbTools->SetVectors(_availableSolutions["TEXTURE_VECTORS"]);
   _tbTools->SetScalars(_availableSolutions["TEXTURE_SCALARS"]);
   if(_tbTools->ActivateTextureVisualization())
   {
      _tbTools->ShowModal();
   }
}
////////////////////////////////////////////////////////////
void Vistab::_onPolydata( wxCommandEvent& WXUNUSED(event) )
{
   if(!polydata)
   {
      polydata = new Polydata ( this,                
                  /*SYMBOL_CONTOURS_IDNAME*/-1, 
                  SYMBOL_POLYDATA_TITLE,
                  SYMBOL_POLYDATA_POSITION,
                  SYMBOL_POLYDATA_SIZE, 
                  SYMBOL_POLYDATA_STYLE );
      polydata->SetSize(_vistabPosition);
   }
   if(scalarSelect || vectorSelect)
   {
      polydata->SetAvailableScalars(_availableSolutions["MESH_SCALARS"]);
      polydata->SetActiveScalar(_activeScalarName);
      polydata->ShowModal();
   }
   else
   {
      wxMessageBox( "Select a scalar or vector","Dataset Failure", 
                     wxOK | wxICON_INFORMATION );
   }   
}
///////////////////////////////////////////////////////
void Vistab::SetActiveModel(VjObs::Model_var activeModel)
{
   _activeModel = activeModel;
   _updateModelInformation(_activeModel);
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
         if(wxString(_activeModel->dataVector[i].datasetname) == wxString(name.c_str()))
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
   std::cout<<"nDatasetsInActiveModel: "<< _nDatasetsInActiveModel<<std::endl;
      //get all the dataset names in this model
      _availableDatasets.Clear();
      for(unsigned int i = 0; i < _nDatasetsInActiveModel; i++)
      {
         _availableDatasets.Add(wxString(newModel->dataVector[i].datasetname));
      }

      //set the active dataset to the initial dataset in the model
      _setActiveDataset(0); 
   }
}
//////////////////////////////////////////////////
void Vistab::_setActiveDataset(unsigned int index)
{
   std::cout<<"setActiveDataset"<<std::endl;
   //is this incorrect to assume we have a model? -- biv
   if(_activeModel)
   {
      CORBA::ULong i = index;
      if(i < _nDatasetsInActiveModel)
      {
         _activeDataset = _activeModel->dataVector[i];
         _activeDataSetName = _activeDataset.datasetname;
         if(_datasetSelection)
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
   std::cout<<"udpateDatasetInformation"<<std::endl;
   _nScalarsInActiveDataset = datasetInfo.scalarVector.length();
   if(_nScalarsInActiveDataset)
   {
      _updateAvailableScalarMeshSolutions(datasetInfo.scalarVector);
   }

   _nVectorsInActiveDataset = datasetInfo.vectornames.length();
   if(_nVectorsInActiveDataset)
   {
      _updateAvailableSolutions("MESH_VECTORS",datasetInfo.vectornames);
   }
}
////////////////////////////////////////////////////////////////////////
//We need this extra method because the data info is stored differenly//
//for scalars                                                         // 
////////////////////////////////////////////////////////////////////////
void Vistab::_updateAvailableScalarMeshSolutions(VjObs::Scalars newScalars)
{
   ///clear out the scalar ranges from before
   _originalScalarRanges.clear();

   std::cout<<"udpateAvailableScalarMeshSolutions"<<std::endl;
   std::map<std::string, wxArrayString >::iterator currentSolution;
   currentSolution = _availableSolutions.find( "MESH_SCALARS" );
   if(currentSolution != _availableSolutions.end())
   {
      currentSolution->second.Clear();
      for(size_t i = 0; i < newScalars.length(); i++)
      {
         _originalScalarRanges[std::string(newScalars[i].scalarnames)].push_back(newScalars[i].scalarrange[0]);
         _originalScalarRanges[std::string(newScalars[i].scalarnames)].push_back(newScalars[i].scalarrange[1]);

         currentSolution->second.Add(wxString(newScalars[i].scalarnames));
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
   std::cout<<"udpateAvailableSolutions"<<std::endl;
   std::map<std::string, wxArrayString >::iterator currentSolution;
   currentSolution = _availableSolutions.find( dataType );
   if(currentSolution != _availableSolutions.end())
   {
      currentSolution->second.Clear();
      for(size_t i = 0; i < names.length(); i++)
      {
         currentSolution->second.Add(wxString(names[i]));
      }
      _updateComboBoxNames("MESH_VECTORS",currentSolution->second);
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
   std::cout<<"updateComboBoxNames"<<std::endl;
   wxListBox* activeComboBox = 0; 
   if(dataType == "MESH_SCALARS" ||
      dataType == "TEXTURE_SCALARS")
   {
      std::cout<<"scalar combo box"<<std::endl;
      activeComboBox = _scalarSelection;
   }
   else if(dataType == "MESH_VECTORS" ||
         dataType == "TEXTURE_VECTORS")
   {
      std::cout<<"vector list box"<<std::endl;
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

   scalarSelect = false;
   vectorSelect = false;
}
////////////////////////////////////////////
DualSlider* Vistab::GetScalarRangeControls()
{
   return scalarRange;
}
////////////////////////////////////////////////////
void Vistab::_OnSelectDataset(wxCommandEvent& WXUNUSED(event))
{
   _activeDataSetName = _datasetSelection->GetValue();
   SetActiveDataset(_activeDataSetName);
}
///////////////////////////////////////////////////
void Vistab::_OnSelectScalar(wxCommandEvent& WXUNUSED(event))
{
   _activeScalarName = _scalarSelection->GetStringSelection();
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
//   scalarRange->SetMinimumSliderValue( 0 );
//   scalarRange->SetMaximumSliderValue( 100 );
   _minSlider->SetValue( 0 );
   _maxSlider->SetValue( 100 );

   scalarSelect = true;
}
///////////////////////////////////////////////////
void Vistab::_OnSelectVector(wxCommandEvent& WXUNUSED(event))
{
   _activeVectorName = _vectorSelection->GetStringSelection();
   vectorSelect = true;
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

   VE_XML::DataValuePair* activeScalar = new VE_XML::DataValuePair();
   activeScalar->SetDataType("STRING");
   activeScalar->SetDataName(std::string("Active Scalar"));
   activeScalar->SetDataString(_activeScalarName);

   _vistabBaseInformation.push_back(activeScalar);
   
   VE_XML::DataValuePair* activeVector = new VE_XML::DataValuePair();
   activeVector->SetDataType("STRING");
   activeVector->SetDataName(std::string("Active Vector"));
   activeVector->SetDataString(_activeVectorName);

   _vistabBaseInformation.push_back(activeVector);

   VE_XML::DataValuePair* activeDataset= new VE_XML::DataValuePair();
   activeDataset->SetDataType("STRING");
   activeDataset->SetDataName(std::string("Active Dataset"));
   activeDataset->SetDataString(_activeDataSetName);

   _vistabBaseInformation.push_back(activeDataset);

   VE_XML::DataValuePair* scalarMin = new VE_XML::DataValuePair();
//   minimumValue = _activeScalarRange.at(0) 
//                    + (scalarRange->GetMinSliderValue()/100.0)*(_activeScalarRange.at(1) - _activeScalarRange.at(0));
   minimumValue = _activeScalarRange.at(0) 
                    + ((double)_minSlider->GetValue()/100.0)*(_activeScalarRange.at(1) - _activeScalarRange.at(0));
   scalarMin->SetData("Scalar Min",minimumValue);

   _vistabBaseInformation.push_back(scalarMin);

   VE_XML::DataValuePair* scalarMax = new VE_XML::DataValuePair();
//   maximumValue = _activeScalarRange.at(0) 
//                    + (scalarRange->GetMaxSliderValue()/100.0)*(_activeScalarRange.at(1) - _activeScalarRange.at(0));
   maximumValue = _activeScalarRange.at(1) - ( (_activeScalarRange.at(1) - _activeScalarRange.at(0)) 
                  * (100 - (double)_maxSlider->GetValue()) / 100.0);
   scalarMax->SetData("Scalar Max",maximumValue);
   
   _vistabBaseInformation.push_back(scalarMax);
}
////////////////////////////////////////////////////////////////////////////
void Vistab::OnClearAll( wxCommandEvent& WXUNUSED(event) )
{
   VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair(  std::string("UNSIGNED INT") );
   dataValuePair->SetDataName( "CLEAR_ALL" );
   dataValuePair->SetDataValue( static_cast< unsigned int >( 1 ) );
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("CLEAR_VIS_OBJECTS") );
   veCommand->AddDataValuePair( dataValuePair );

   bool connected = dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->SendCommandStringToXplorer( veCommand );
   delete veCommand;
}
////////////////////////////////////////////////////////////////////////////
void Vistab::SendUpdatedSettingsToXplorer(VE_XML::Command* subDialogCommand)
{
   _updateBaseInformation();
   VE_XML::Command* newCommand = new VE_XML::Command();

   for(size_t i =0; i < _vistabBaseInformation.size(); i++)
   {
      newCommand->AddDataValuePair(_vistabBaseInformation.at(i));
   }
   if(subDialogCommand)
   {
      VE_XML::DataValuePair* subDialogSettings = new VE_XML::DataValuePair();
      subDialogSettings->SetData("Sub-Dialog Settings",subDialogCommand);
      newCommand->AddDataValuePair(subDialogSettings);
   }

   newCommand->SetCommandName(_commandName);

   bool connected = dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->SendCommandStringToXplorer( newCommand );

   delete newCommand;
   newCommand = 0;
}
///////////////////////////////////////////////////////////////////////////
void Vistab::_onMinSpinCtrl( wxScrollEvent& WXUNUSED(event) )
{
//   double range = _activeScalarRange.at(1) - _activeScalarRange.at(0);
   double minValue = 0;

   minValue = ( ( _minSpinner->GetValue() - _activeScalarRange.at(0) ) / ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 100);

   if( minValue == 100 )
   {
//      scalarRange->SetMaximumSliderValue( (int)minValue+1 );    
//      scalarRange->SetMinimumSliderValue( (int)minValue );   
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
   }
//   else if( scalarRange->GetMaxSliderValue() <= (int)minValue )
   else if( _maxSlider->GetValue() <= (int)minValue )
   {
//      scalarRange->SetMaximumSliderValue( (int)minValue+1 );
//      scalarRange->SetMinimumSliderValue( (int)minValue );
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
      _maxSpinner->SetValue( _activeScalarRange.at(1) - ( ( _activeScalarRange.at(1) - _activeScalarRange.at(0) )
                               * ( 100 - (double)_maxSlider->GetValue() ) / 100 ) );
   }
   else
   {
//      scalarRange->SetMinimumSliderValue( (int)minValue );
      _minSlider->SetValue( (int)minValue );  
   }

} 
///////////////////////////////////////////////////////////////////////////
void Vistab::_onMaxSpinCtrl( wxScrollEvent& WXUNUSED(event) )
{
//   double range = _activeScalarRange.at(1) - _activeScalarRange.at(0);
   double maxValue = 100;

   maxValue = ( ( _activeScalarRange.at(1) - _activeScalarRange.at(0) 
               - ( _activeScalarRange.at(1) - _maxSpinner->GetValue() ) ) 
               / ( _activeScalarRange.at(1) - _activeScalarRange.at(0) ) * 100);

   if( maxValue == 0 )
   {
//      scalarRange->SetMaximumSliderValue( (int)maxValue+1 );    
//      scalarRange->SetMinimumSliderValue( (int)maxValue );  
      _minSlider->SetValue( (int)maxValue+1 );
      _maxSlider->SetValue( (int)maxValue );     
   }
//   else if( scalarRange->GetMinSliderValue() >= (int)maxValue )
   else if( _minSlider->GetValue() >= (int)maxValue )
   {
//      scalarRange->SetMinimumSliderValue( (int)maxValue-1 );
//      scalarRange->SetMaximumSliderValue( (int)maxValue );
      _minSlider->SetValue( (int)maxValue-1 );
      _maxSlider->SetValue( (int)maxValue );   
      _minSpinner->SetValue( ( _activeScalarRange.at(1) - _activeScalarRange.at(0) )
                              * (double)_minSlider->GetValue() / 100 + _activeScalarRange.at(0) );
   } 
   else
   {  
//      scalarRange->SetMaximumSliderValue( (int)maxValue );
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
void Vistab::_onClose( wxCommandEvent& event )
{
//   vistab->EndModal(-1);
//   vistab->OnOK(EVT_BUTTON);
//   vistab->Show(false);
   scalarSelect = false;
   vectorSelect = false;
}
