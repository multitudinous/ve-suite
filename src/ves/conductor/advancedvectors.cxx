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

#include <ves/conductor/advancedvectors.h>

#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/checkbox.h>
#include <wx/icon.h>

#include <wx/bmpbuttn.h>
#include <wx/stattext.h>
#include <wx/statbox.h>



using namespace VE_Conductor::GUI_Utilities;
//////////////////////////////////////////////////////////////////
AdvancedVectors::AdvancedVectors(wxWindow* parent, wxWindowID id, 
                             const wxString& caption, 
                             const wxPoint& pos, 
                             const wxSize& size, long style )
{
   Create(parent, id, caption, pos, size, style);
   wxSize displaySize = ::wxGetDisplaySize();
   int tempH = displaySize.GetHeight()-480;
   wxRect dialogPosition( displaySize.GetWidth()-427, displaySize.GetHeight()-tempH, 427, tempH );
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
   SetAutoLayout( true );
   Refresh();
   wxSize temp = GetSize();
   temp.SetHeight( temp.GetHeight() +1);
   temp.SetWidth( temp.GetWidth()+1 );
   SetSize( temp );
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
                             wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,_("Vector Threshold"));
   vectorSizer->Add(vectorRange,1,wxALIGN_CENTER|wxEXPAND);
   itemStaticBoxSizer3->Add(vectorSizer, 5, wxALIGN_CENTER|wxEXPAND|wxALL, 5);
   //set the default values for the dual slider
   vectorRange->SetMinimumSliderValue( 0 );
   vectorRange->SetMaximumSliderValue( 100 );

   wxStaticText* itemStaticText9 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Vector Scale"), wxDefaultPosition, wxDefaultSize, 0 );
   itemStaticBoxSizer3->Add(itemStaticText9, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

   _vectorScaleSlider = new wxSlider( itemDialog1, VECTOR_SCALE_SLIDER, 1, 1, 200, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
   itemStaticBoxSizer3->Add(_vectorScaleSlider, 0, wxGROW|wxALL, 5);

   wxBoxSizer* itemBoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
   itemStaticBoxSizer3->Add(itemBoxSizer11, 1, wxGROW|wxALL, 5);

   wxStaticText* itemStaticText12 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Decrease Size"), wxDefaultPosition, wxDefaultSize, 0 );
   itemBoxSizer11->Add(itemStaticText12, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

   wxStaticText* itemStaticText13 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Increase Size"), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
   itemBoxSizer11->Add(itemStaticText13, 1, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

   wxStaticText* itemStaticText14 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Vector Ratio"), wxDefaultPosition, wxDefaultSize, 0 );
   itemStaticBoxSizer3->Add(itemStaticText14, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

   _vectorRatioSlider = new wxSlider( itemDialog1, VECTOR_RATIO_SLIDER, 1, 1, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
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

   wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
   itemStaticBoxSizer3->Add(_closeButton, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

}
///////////////////////////////////////////////////////////////////
void AdvancedVectors::SetVectorThreshold(std::vector<double> range)
{
   if(vectorRange)
   {
      vectorRange->SetMinimumSliderValue(static_cast<int>(range.at(0)));
      vectorRange->SetMaximumSliderValue(static_cast<int>(range.at(1)));
   }
}
///////////////////////////////////////////////////////////////////
void AdvancedVectors::GetVectorThreshold(std::vector<double>& range)
{
   if(vectorRange)
   {
      range.clear();
      range.push_back(static_cast<double>(vectorRange->GetMinSliderValue()));
      range.push_back(static_cast<double>(vectorRange->GetMaxSliderValue()));
   }
}
//////////////////////////////////////////////////
void AdvancedVectors::SetVectorScale(double value)
{
   if(_vectorScaleSlider)
   {
      _vectorScaleSlider->SetValue(static_cast<int>(value));
   }
}
//////////////////////////////////////////////////
void AdvancedVectors::SetVectorRatio(double value)
{
   if(_vectorRatioSlider)
   {
      _vectorRatioSlider->SetValue(static_cast<int>(value));
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
   return static_cast<double>(_vectorScaleSlider->GetValue());
}
////////////////////////////////////////
double AdvancedVectors::GetVectorRatio()
{
   return static_cast<double>(_vectorRatioSlider->GetValue());
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
