/////////////////////////////////////////////////////////////////////////////
// Name:        advancedvectors.cpp
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Thu 20 Apr 2006 22:00:30 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////



////@begin includes
////@end includes

#include "VE_Conductor/Framework/advancedvectors.h"

#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/checkbox.h>
#include <wx/icon.h>


BEGIN_EVENT_TABLE( AdvancedVectors, wxDialog )
////@begin AdvancedVectors event table entries
   EVT_SLIDER       ( VECTOR_MAX_SLIDER,        AdvancedVectors::_onVectorMax )
   EVT_SLIDER       ( VECTOR_MIN_SLIDER,        AdvancedVectors::_onVectorMin )
   EVT_SLIDER       ( VECTOR_SCALE_SLIDER,      AdvancedVectors::_onVectorScale )
   EVT_SLIDER       ( VECTOR_RATIO_SLIDER,      AdvancedVectors::_onVectorRatio )
   EVT_CHECKBOX     ( SCALAR_BY_VECTOR_CHK,     AdvancedVectors::_onScalarByVectorMag )
////@end AdvancedVectors event table entries
END_EVENT_TABLE()


using namespace VE_Conductor::GUI_Utilities;
//////////////////////////////////////////////////////////////////
AdvancedVectors::AdvancedVectors(wxWindow* parent, wxWindowID id, 
                             const wxString& caption, 
                             const wxPoint& pos, 
                             const wxSize& size, long style )
{
    Create(parent, id, caption, pos, size, style);
    wxSize displaySize = ::wxGetDisplaySize();
    wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
    this->SetSize( dialogPosition );
}
//////////////////////////////////////////////////////////////
bool AdvancedVectors::Create(wxWindow* parent, wxWindowID id, 
                          const wxString& caption, const wxPoint& pos, 
                          const wxSize& size, long style )
{
   _vectorScaleSlider = 0;
   _vectorRatioSlider = 0;
   _scaleByMagCheck = 0;

   SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
   wxDialog::Create( parent, id, caption, pos, size, style );

   CreateControls();
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
   Centre();
   return true;
}
//////////////////////////////////////
void AdvancedVectors::CreateControls()
{    
   AdvancedVectors* itemDialog1 = this;

   wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
   itemDialog1->SetSizer(itemBoxSizer2);

   wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Advanced Vector Controls"));
   wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
   itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

   wxBoxSizer* vectorSizer = new wxBoxSizer(wxHORIZONTAL);
   vectorRange = new DualSlider(this,-1,1,0,100,0,100,wxDefaultPosition,wxDefaultSize,
                             wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,wxString("Vector Threshold"));
   vectorSizer->Add(vectorRange,1,wxALIGN_CENTER|wxEXPAND);
   itemStaticBoxSizer3->Add(vectorSizer, 5, wxALIGN_CENTER|wxEXPAND|wxALL, 5);

   wxStaticText* itemStaticText9 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Vector Scale"), wxDefaultPosition, wxDefaultSize, 0 );
   itemStaticBoxSizer3->Add(itemStaticText9, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

   _vectorScaleSlider = new wxSlider( itemDialog1, VECTOR_SCALE_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
   itemStaticBoxSizer3->Add(_vectorScaleSlider, 0, wxGROW|wxALL, 5);

   wxBoxSizer* itemBoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
   itemStaticBoxSizer3->Add(itemBoxSizer11, 1, wxGROW|wxALL, 5);

   wxStaticText* itemStaticText12 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Decrease Size"), wxDefaultPosition, wxDefaultSize, 0 );
   itemBoxSizer11->Add(itemStaticText12, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

   wxStaticText* itemStaticText13 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Increase Size"), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
   itemBoxSizer11->Add(itemStaticText13, 1, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

   wxStaticText* itemStaticText14 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Vector Ratio"), wxDefaultPosition, wxDefaultSize, 0 );
   itemStaticBoxSizer3->Add(itemStaticText14, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

   _vectorRatioSlider = new wxSlider( itemDialog1, VECTOR_RATIO_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
   itemStaticBoxSizer3->Add(_vectorRatioSlider, 0, wxGROW|wxALL, 5);

   wxBoxSizer* itemBoxSizer16 = new wxBoxSizer(wxHORIZONTAL);
   itemStaticBoxSizer3->Add(itemBoxSizer16, 1, wxGROW|wxALL, 5);

   wxStaticText* itemStaticText17 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Dense"), wxDefaultPosition, wxDefaultSize, 0 );
   itemBoxSizer16->Add(itemStaticText17, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

   wxStaticText* itemStaticText18 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Sparse"), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
   itemBoxSizer16->Add(itemStaticText18, 1, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

   _scaleByMagCheck = new wxCheckBox( itemDialog1, SCALAR_BY_VECTOR_CHK, _T("Scalar by Vector Magnitude"), wxDefaultPosition, wxDefaultSize, 0 );
   _scaleByMagCheck->SetValue(false);
   itemStaticBoxSizer3->Add(_scaleByMagCheck, 0, wxALIGN_LEFT|wxALL, 5);
}
///////////////////////////////////////////////////////////////////
void AdvancedVectors::SetVectorThreshold(std::vector<double> range)
{
   if(vectorRange)
   {
      vectorRange->SetMinimumSliderValue(static_cast<int>(range.at(0)*100));
      vectorRange->SetMaximumSliderValue(static_cast<int>(range.at(1)*100));
   }
}
///////////////////////////////////////////////////////////////////
void AdvancedVectors::GetVectorThreshold(std::vector<double>& range)
{
   if(vectorRange)
   {
      range.clear();
      range.push_back(static_cast<double>(vectorRange->GetMinSliderValue()/100.0));
      range.push_back(static_cast<double>(vectorRange->GetMaxSliderValue()/100.0));
   }
}
//////////////////////////////////////////////////
void AdvancedVectors::SetVectorScale(double value)
{
   if(_vectorScaleSlider)
   {
      _vectorScaleSlider->SetValue(static_cast<int>(value*100));
   }
}
//////////////////////////////////////////////////
void AdvancedVectors::SetVectorRatio(double value)
{
   if(_vectorRatioSlider)
   {
      _vectorRatioSlider->SetValue(static_cast<int>(value*100));
   }
}
///////////////////////////////////////////////////
void AdvancedVectors::SetScaleByMagFlag(bool value)
{
   if(_scaleByMagCheck)
   {
      _scaleByMagCheck->SetValue(value);
   }
}
////////////////////////////////////////
double AdvancedVectors::GetVectorScale()
{
   return static_cast<double>(_vectorScaleSlider->GetValue()/100.0);
}
////////////////////////////////////////
double AdvancedVectors::GetVectorRatio()
{
   return static_cast<double>(_vectorRatioSlider->GetValue()/100.0);
}
/////////////////////////////////////////
bool AdvancedVectors::GetScaleByMagFlag()
{
   return _scaleByMagCheck->GetValue();
}
////////////////////////////////////
bool AdvancedVectors::ShowToolTips()
{
    return true;
}
////////////////////////////////////////////////////////////////////
wxBitmap AdvancedVectors::GetBitmapResource( const wxString& name )
{
   wxUnusedVar(name);
   return wxNullBitmap;
}
////////////////////////////////////////////////////////////////
wxIcon AdvancedVectors::GetIconResource( const wxString& name )
{
   wxUnusedVar(name);
   return wxNullIcon;
}
///////////////////////////////////////////////////////////
void AdvancedVectors::_onVectorMax( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER in AdvancedVectors.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER in AdvancedVectors. 
}
///////////////////////////////////////////////////////////
void AdvancedVectors::_onVectorMin( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER1 in AdvancedVectors.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER1 in AdvancedVectors. 
}
/////////////////////////////////////////////////////////////
void AdvancedVectors::_onVectorScale( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER2 in AdvancedVectors.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER2 in AdvancedVectors. 
}
/////////////////////////////////////////////////////////////
void AdvancedVectors::_onVectorRatio( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER3 in AdvancedVectors.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER3 in AdvancedVectors. 
}
///////////////////////////////////////////////////////////////////
void AdvancedVectors::_onScalarByVectorMag( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX in AdvancedVectors.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX in AdvancedVectors. 
}
