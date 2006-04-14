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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Utilities/DualSlider.h"
#include <wx/slider.h>
#include <iostream>
#include <cmath>
#include <wx/sizer.h>
#include <wx/statbox.h>
using namespace VE_Conductor::GUI_Utilities;
BEGIN_EVENT_TABLE(DualSlider,wxPanel)
   EVT_COMMAND_SCROLL(MIN_SLIDER,DualSlider::_onSlider)
   EVT_COMMAND_SCROLL(MAX_SLIDER,DualSlider::_onSlider)
END_EVENT_TABLE()
////////////////////////////////////////////////////////
DualSlider::DualSlider( wxWindow* parent, wxWindowID id,
                        unsigned int buffer,
                        int min, int max,
                        int minSliderValue, int maxSliderValue,
                        const wxPoint& pos, 
                        const wxSize& size,
                        long style,
                        const wxString& name)
:wxPanel(parent,id,pos,size,wxTAB_TRAVERSAL,name)
{
   wxBoxSizer* dualSliderPanelSizer = new wxBoxSizer(wxVERTICAL);
   wxStaticBox* dualSliderGroup = new wxStaticBox(this, -1, name);
   wxStaticBoxSizer* dualSliderSizer = new wxStaticBoxSizer(dualSliderGroup, wxVERTICAL);

   _buffer = buffer;
   _range[0] = min;
   _range[1] = max;

   if(abs(_range[1] - _range[0]) < _buffer)
   {
      std::cout<<"ERROR!!"<<std::endl;
      std::cout<<"Range: "<<_range[1]<<"-"<<_range[0]<<"="<<abs(_range[1]-_range[0])<<std::endl;
      std::cout<<"DualSlider buffer: "<<_buffer<<std::endl;
   }

   _minSlider = new wxSlider(this, MIN_SLIDER, minSliderValue ,_range[0], _range[1], wxDefaultPosition,
                             wxDefaultSize, style, wxDefaultValidator,"minSlider");

   _maxSlider = new wxSlider(this, MAX_SLIDER, maxSliderValue ,_range[0], _range[1], wxDefaultPosition,
                             wxDefaultSize, style, wxDefaultValidator,"maxSlider");
   _ensureSliders(MIN_SLIDER);
   dualSliderSizer->Add(_minSlider,1,wxALIGN_CENTER|wxEXPAND);
   dualSliderSizer->Add(_maxSlider,1,wxALIGN_CENTER|wxEXPAND);

   dualSliderPanelSizer->Add(dualSliderSizer,1,wxEXPAND|wxALIGN_CENTER);
   SetAutoLayout(true);
   SetSizer(dualSliderPanelSizer);
}
/////////////////////////
DualSlider::~DualSlider()
{
   if(_minSlider)
   {
      _minSlider->Destroy(); 
      _minSlider = 0;
   }
   if(_maxSlider)
   {
      _maxSlider->Destroy(); 
      _maxSlider = 0;
   }
}
//////////////////////////////////////////////////
void DualSlider::SetSliderRange(int min, int max)
{
   _range[0] = min;
   _range[1] = max;
   _minSlider->SetRange(_range[0],_range[1]);
   _maxSlider->SetRange(_range[0],_range[1]);
}
///////////////////////////////////////////
void DualSlider::SetSliderBuffer(int buffer)
{
   if(buffer >= 1)
   {
      _buffer = buffer;
   }   
}
///////////////////////////////////
int DualSlider::GetMinSliderValue()
{
   return _minSlider->GetValue();
}
///////////////////////////////////
int DualSlider::GetMaxSliderValue()
{
   return _maxSlider->GetValue();
}
/////////////////////////////////
int DualSlider::GetSliderBuffer()
{
   return _buffer;
}
//////////////////////////////////
int DualSlider::GetSliderMaximum()
{
   return _range[1];
}
//////////////////////////////////
int DualSlider::GetSliderMinimum()
{
   return _range[0];
}
///////////////////////////////////////////////////
void DualSlider::_ensureSliders(int activeSliderID)
{
   int minValue = _minSlider->GetValue();
   int maxValue = _maxSlider->GetValue();

   //maintain the value on the min/max sliders.
   if(minValue > maxValue - static_cast<int>(_buffer))
   {
      if(minValue == _range[1])
      {
         _minSlider->SetValue(_range[1] - _buffer);
      }
      else if(maxValue == _range[0])
      {
         _maxSlider->SetValue(_range[0] + _buffer);
      }

      if(activeSliderID == MIN_SLIDER)
      {
         _maxSlider->SetValue(_minSlider->GetValue() + _buffer);
      }
      else if(activeSliderID == MAX_SLIDER)
      {
         _minSlider->SetValue(_maxSlider->GetValue() - _buffer);
      }
   }
}
////////////////////////////////////////////////
void DualSlider::_onSlider(wxScrollEvent& event)
{
   _ensureSliders(event.GetId());

   wxSlider* activeSlider = 0;
   if(event.GetId() == MIN_SLIDER)
   {
      activeSlider = _minSlider;
   }
   else if(event.GetId() == MAX_SLIDER)
   {
      activeSlider = _maxSlider;
   }

   //need to set up some user definable callbacks for each slider
}
