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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <wx/listbox.h>
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/icon.h>
#include <wx/bitmap.h>
#include <wx/statbox.h>
#include <wx/msgdlg.h>

#include "advancedisosurface.h"

IMPLEMENT_DYNAMIC_CLASS( AdvancedIsosurface, wxDialog )

BEGIN_EVENT_TABLE( AdvancedIsosurface, wxDialog )
   EVT_LISTBOX( SELECT_SCALAR,			AdvancedIsosurface::OnScalarSelection )
   EVT_COMMAND_SCROLL( MIN_SPINCTRL,    AdvancedIsosurface::OnMinSpinCtrl )
   EVT_COMMAND_SCROLL( MAX_SPINCTRL,    AdvancedIsosurface::OnMaxSpinCtrl )
   EVT_COMMAND_SCROLL( MIN_SLIDER,      AdvancedIsosurface::OnMinSlider )
   EVT_COMMAND_SCROLL( MAX_SLIDER,      AdvancedIsosurface::OnMaxSlider )
   EVT_TEXT_ENTER	 ( MIN_SPINCTRL,    AdvancedIsosurface::UpdateMinSlider )
   EVT_TEXT_ENTER	 ( MAX_SPINCTRL,    AdvancedIsosurface::UpdateMaxSlider )
END_EVENT_TABLE()

AdvancedIsosurface::AdvancedIsosurface( )
{

}

AdvancedIsosurface::AdvancedIsosurface( wxWindow* parent,
									    wxWindowID id,
										const wxString& caption,
										const wxPoint& pos,
										const wxSize& size,
										long style )
{
	_availableSolutions["MESH_SCALARS"].Add( _("") ); 

    Create(parent, id, caption, pos, size, style);

	_colorScalarName = ConvertUnicode( scalarSelection->GetStringSelection() );
    _colorScalarRange = _colorScalarRanges[_colorScalarName];
}

bool AdvancedIsosurface::Create( wxWindow* parent,
								 wxWindowID id,
								 const wxString& caption,
								 const wxPoint& pos, 
								 const wxSize& size,
								 long style )
{
    SetExtraStyle(wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );


	scalarSelection = 0;
	_minSpinner = 0;
	_maxSpinner = 0;
	_minSlider = 0;
	_maxSlider = 0;

    CreateControls();
    if (GetSizer())
    {
        GetSizer()->SetSizeHints(this);
    }
    Centre();

    return true;
}

void AdvancedIsosurface::CreateControls()
{    
    AdvancedIsosurface* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _("Color by Scalar"));
    wxStaticBoxSizer* selectScalarSizer = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);

    wxStaticText* itemStaticText4 = new wxStaticText( itemDialog1, wxID_STATIC, _("Select Scalar to Color Isosurface by"), wxDefaultPosition, wxDefaultSize, 0 );
    selectScalarSizer->Add(itemStaticText4, 0, wxALIGN_LEFT|wxALL|wxADJUST_MINSIZE, 5);

//	_vectorSelection = new wxListBox( itemDialog1, ID_LISTBOX1, wxDefaultPosition, wxSize(125, 75), _availableSolutions["MESH_VECTORS"], wxLB_SINGLE|wxLB_NEEDED_SB );
    scalarSelection = new wxListBox( itemDialog1, SELECT_SCALAR, wxDefaultPosition, wxDefaultSize, _availableSolutions["MESH_SCALARS"], wxLB_SINGLE|wxLB_NEEDED_SB );
    scalarSelection->SetSelection(0);
	selectScalarSizer->Add(scalarSelection, 0, wxALIGN_CENTER_HORIZONTAL|wxALL|wxEXPAND, 5);

    itemBoxSizer2->Add(selectScalarSizer, 0, wxALIGN_CENTER|wxGROW|wxALL, 5);

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

    wxBoxSizer* buttonSizer = new wxBoxSizer(wxHORIZONTAL);

    wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
    buttonSizer->Add(_closeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

	itemBoxSizer2->Add(buttonSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
}

bool AdvancedIsosurface::ShowToolTips()
{
    return true;
}

wxBitmap AdvancedIsosurface::GetBitmapResource( const wxString& name )
{
    wxUnusedVar(name);
    return wxNullBitmap;
}

wxIcon AdvancedIsosurface::GetIconResource( const wxString& name )
{
    wxUnusedVar(name);
    return wxNullIcon;
}
//////////////////////////////////////////////////////////////////
void AdvancedIsosurface::OnScalarSelection(wxCommandEvent& WXUNUSED(event))
{
	SetScalarRange();
}
//////////////////////////////////////////////////////////////////
void AdvancedIsosurface::OnMinSpinCtrl(wxScrollEvent& WXUNUSED(event))
{
   double minValue = 0;

   if( _colorScalarName.empty() )
   {
      wxMessageBox( _("Select a scalar"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _minSpinner->SetValue(0);
      return;
   }

   minValue = ( ( _minSpinner->GetValue() - _colorScalarRange.at(0) ) 
               / ( _colorScalarRange.at(1) - _colorScalarRange.at(0) ) * 100);

   if( minValue == 100 )
   {  
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
   }
   else if( _maxSlider->GetValue() <= (int)minValue )
   {
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
      _maxSpinner->SetValue( _colorScalarRange.at(1) - ( ( _colorScalarRange.at(1) - _colorScalarRange.at(0) )
                               * ( 100 - (double)_maxSlider->GetValue() ) / 100 ) );
   }
   else
   {
      _minSlider->SetValue( (int)minValue );  
   }
}
//////////////////////////////////////////////////////////////////
void AdvancedIsosurface::OnMaxSpinCtrl(wxScrollEvent& WXUNUSED(event))
{
   double maxValue = 100;

   if( _colorScalarName.empty() )
   {
      wxMessageBox( _("Select a scalar"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _maxSpinner->SetValue(100);
      return;
   }

   maxValue = ( ( _colorScalarRange.at(1) - _colorScalarRange.at(0) 
               - ( _colorScalarRange.at(1) - _maxSpinner->GetValue() ) ) 
               / ( _colorScalarRange.at(1) - _colorScalarRange.at(0) ) * 100);

   if( maxValue == 0 )
   {  
      _minSlider->SetValue( (int)maxValue+1 );
      _maxSlider->SetValue( (int)maxValue );     
   }
   else if( _minSlider->GetValue() >= (int)maxValue )
   {
      _minSlider->SetValue( (int)maxValue-1 );
      _maxSlider->SetValue( (int)maxValue );   
      _minSpinner->SetValue( ( _colorScalarRange.at(1) - _colorScalarRange.at(0) )
                              * (double)_minSlider->GetValue() / 100 + _colorScalarRange.at(0) );
   } 
   else
   {  
      _maxSlider->SetValue( (int)maxValue );   
   }
}
//////////////////////////////////////////////////////////////////
void AdvancedIsosurface::OnMinSlider(wxScrollEvent& WXUNUSED(event))
{
   if( _colorScalarName.empty())
   {
      wxMessageBox( _("Select a scalar"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _minSlider->SetValue(0);
      return;
   }

   double range = _colorScalarRange.at(1) - _colorScalarRange.at(0);

   if( _minSlider->GetValue() >= _maxSlider->GetValue() ) // && _minSlider->GetValue() < 100 )
   {
      _ensureSliders(MIN_SLIDER);
   }

   _minSpinner->SetValue( range * (double)_minSlider->GetValue() / 100  + _colorScalarRange.at(0) );
   _maxSpinner->SetValue( _colorScalarRange.at(1) - ( range * ( 100 - (double)_maxSlider->GetValue() ) / 100 ) ); 
}
//////////////////////////////////////////////////////////////////
void AdvancedIsosurface::OnMaxSlider(wxScrollEvent& WXUNUSED(event))
{
   if( _colorScalarName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"),_("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _maxSlider->SetValue(100);
      return;
   }

   double range = _colorScalarRange.at(1) - _colorScalarRange.at(0);

   if( _maxSlider->GetValue() <= _minSlider->GetValue() ) //&& _maxSlider->GetValue() > 0 )
   {
      _ensureSliders(MAX_SLIDER);
   }

   _minSpinner->SetValue( ( range * (double)_minSlider->GetValue() ) / 100 + _colorScalarRange.at(0) );
   _maxSpinner->SetValue( _colorScalarRange.at(1) - ( range * ( 100 - (double)_maxSlider->GetValue() ) / 100 ) );
}
bool AdvancedIsosurface::_ensureSliders(int activeSliderID)
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
void AdvancedIsosurface::UpdateMinSlider( wxCommandEvent& event )
{
//   double range = _activeScalarRange.at(1) - _activeScalarRange.at(0);
   double minValue = 0;

   if( _colorScalarName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"), _("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _minSpinner->SetValue(0);
      return;
   }

   minValue = ( ( _minSpinner->GetValue() - _colorScalarRange.at(0) ) 
               / ( _colorScalarRange.at(1) - _colorScalarRange.at(0) ) * 100);

   if( minValue == 100 )
   {  
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
   }
   else if( _maxSlider->GetValue() <= (int)minValue )
   {
      _minSlider->SetValue( (int)minValue );
      _maxSlider->SetValue( (int)minValue+1 );   
      _maxSpinner->SetValue( _colorScalarRange.at(1) - ( ( _colorScalarRange.at(1) - _colorScalarRange.at(0) )
                               * ( 100 - (double)_maxSlider->GetValue() ) / 100 ) );
   }
   else
   {
      _minSlider->SetValue( (int)minValue );  
   }
} 
////////////////////////////////////////////////////////////////////////
void AdvancedIsosurface::UpdateMaxSlider( wxCommandEvent& event )
{
   double maxValue = 100;

   if( _colorScalarName.empty() )
   {
      wxMessageBox( _("Select a scalar or vector"), _("Dataset Failure"), 
                     wxOK | wxICON_INFORMATION );
      _maxSpinner->SetValue(100);
      return;
   }

   maxValue = ( ( _colorScalarRange.at(1) - _colorScalarRange.at(0) 
               - ( _colorScalarRange.at(1) - _maxSpinner->GetValue() ) ) 
               / ( _colorScalarRange.at(1) - _colorScalarRange.at(0) ) * 100);

   if( maxValue == 0 )
   {  
      _minSlider->SetValue( (int)maxValue+1 );
      _maxSlider->SetValue( (int)maxValue );     
   }
   else if( _minSlider->GetValue() >= (int)maxValue )
   {
      _minSlider->SetValue( (int)maxValue-1 );
      _maxSlider->SetValue( (int)maxValue );   
      _minSpinner->SetValue( ( _colorScalarRange.at(1) - _colorScalarRange.at(0) )
                              * (double)_minSlider->GetValue() / 100 + _colorScalarRange.at(0) );
   } 
   else
   {  
      _maxSlider->SetValue( (int)maxValue );   
   } 
}
//////////////////////////////////////////////////////////////////
void AdvancedIsosurface::PopulateList( wxArrayString _scalarlist )
{
	scalarSelection->InsertItems( _scalarlist,0 );

	for( size_t i=0; i<_scalarlist.GetCount(); i++ )
	{
		if( wxString( _activeScalar.c_str(), wxConvUTF8 ) == _scalarlist[i] )
		{
			scalarSelection->SetSelection(i);
		}
		SetScalarRange();
	}
}
////////////////////////////////////////////////////////////
void AdvancedIsosurface::SetScalarList( std::map<std::string,std::vector<double> > colorScalarRanges )
{
	_colorScalarRanges = colorScalarRanges;

}
////////////////////////////////////////////////////////////
void AdvancedIsosurface::SetActiveScalar( std::string activeScalar )
{
	_activeScalar = activeScalar;
}
///////////////////////////////////////////////////////////
void AdvancedIsosurface::SetScalarRange( )
{
   _colorScalarName = ConvertUnicode( scalarSelection->GetStringSelection() );
   _colorScalarRange = _colorScalarRanges[_colorScalarName];

   if( !_colorScalarName.empty() )
   {
      double minBoundRange = ( _colorScalarRange.at(1) - _colorScalarRange.at(0) ) * 0.99;
      double maxBoundRange = ( _colorScalarRange.at(1) - _colorScalarRange.at(0) ) * 0.01;
      _minSpinner->SetRange( _colorScalarRange.at(0), minBoundRange );   
      _minSpinner->SetValue( _colorScalarRange.at(0) );
      _maxSpinner->SetRange( maxBoundRange, _colorScalarRange.at(1) );
      _maxSpinner->SetValue( _colorScalarRange.at(1) );

      if( _colorScalarRange.at(1) == _colorScalarRange.at(0) )
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
   }
}
////////////////////////////////////////////////////////////
double AdvancedIsosurface::GetMinScalarValue()
{
	return  _minSpinner->GetValue();
}
////////////////////////////////////////////////////////////
double AdvancedIsosurface::GetMaxScalarValue()
{
	return _maxSpinner->GetValue();
}
////////////////////////////////////////////////////////////
std::string AdvancedIsosurface::GetScalarName()
{
	return ConvertUnicode( scalarSelection->GetStringSelection() );
}
