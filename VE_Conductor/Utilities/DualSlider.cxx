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
#include "VE_Conductor/GUIPlugin/DualSlider.h"
#include <wx/slider.h>
#include <iostream>
#include <cmath>
#include <wx/sizer.h>
#include <wx/statbox.h>
using namespace VE_Conductor::GUI_Utilities;
BEGIN_EVENT_TABLE(DualSlider,wxPanel)
   EVT_COMMAND_SCROLL_THUMBTRACK(MIN_SLIDER,DualSlider::_onSlider)
   EVT_COMMAND_SCROLL_THUMBTRACK(MAX_SLIDER,DualSlider::_onSlider)
   EVT_COMMAND_SCROLL_THUMBRELEASE(MIN_SLIDER,DualSlider::_onStop)
   EVT_COMMAND_SCROLL_THUMBRELEASE(MAX_SLIDER,DualSlider::_onStop)
END_EVENT_TABLE()
////////////////////////////////////////////////////////////////////////
DualSlider::DualSlider( wxWindow* parent, wxWindowID id,
                        unsigned int buffer,
                        int minRange, int maxRange,
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
   _range[0] = minRange;
   _range[1] = maxRange;

   if(abs(_range[1] - _range[0]) < static_cast<int> (_buffer)) //check
   {
      std::cout<<"ERROR!!"<<std::endl;
      std::cout<<"Range: "<<_range[1]<<"-"<<_range[0]<<"="<<abs(_range[1]-_range[0])<<std::endl;
      std::cout<<"DualSlider buffer: "<<_buffer<<std::endl;
   }

   _minSlider = new wxSlider(this, MIN_SLIDER, minSliderValue ,_range[0], _range[1], wxDefaultPosition,
                             wxDefaultSize, style, wxDefaultValidator,_("minSlider") );

   _maxSlider = new wxSlider(this, MAX_SLIDER, maxSliderValue ,_range[0], _range[1], wxDefaultPosition,
                             wxDefaultSize, style, wxDefaultValidator,_("maxSlider") );
   _ensureSliders(MIN_SLIDER);
   dualSliderSizer->Add(_minSlider,1,wxALIGN_CENTER|wxEXPAND);
   dualSliderSizer->Add(_maxSlider,1,wxALIGN_CENTER|wxEXPAND);

   dualSliderPanelSizer->Add(dualSliderSizer,1,wxEXPAND|wxALIGN_CENTER);
   _minSlider->Raise();
   _maxSlider->Raise();
   
   SetAutoLayout(true);
   SetSizer(dualSliderPanelSizer);
   dualSliderPanelSizer->Fit(this);
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

   for ( std::map<int ,SliderCallback*>::iterator itr = _callbacks.begin();
                                       itr != _callbacks.end(); itr++ )
   {
      if(itr->second)
      {
         delete itr->second;
         itr->second = 0;
      }
   }
   _callbacks.clear();
}
/////////////////////////////////////////////////////////////
void DualSlider::SetMinimumSliderValue(int value)
{
   _minSlider->SetValue(value);
}
//////////////////////////////////////////////////////////////
void DualSlider::SetMaximumSliderValue(int value)
{
   _maxSlider->SetValue(value);
}
///////////////////////////////////////////////////////
void DualSlider::SetSliderRange(int minValue, 
                             int maxValue)
{
   _range[0] = minValue;
   _range[1] = maxValue;
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
bool DualSlider::_ensureSliders(int activeSliderID)
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
         return true;
      }
      else if(activeSliderID == MAX_SLIDER)
      {
         _minSlider->SetValue(_maxSlider->GetValue() - _buffer);
         return true;
      }
   }
   return false;
}
////////////////////////////////////////////////////////////////////////////////////////
void DualSlider::SetStopSliderUpdateCallback(SliderCallback* bothSCbk)
{
   _callbacks[STOP] = bothSCbk;
}
////////////////////////////////////////////////////////////////////////////////////////
void DualSlider::SetBothSliderUpdateCallback(SliderCallback* bothSCbk)
{
   _callbacks[BOTH_SLIDERS] = bothSCbk;
}
/////////////////////////////////////////////////////////////////////////////////////////
void DualSlider::SetMinSliderCallback(DualSlider::SliderCallback* minCbk)
{
   _callbacks[MIN_SLIDER] = minCbk;
}
/////////////////////////////////////////////////////////////////////////
void DualSlider::SetMaxSliderCallback(DualSlider::SliderCallback* maxCbk)
{
   _callbacks[MAX_SLIDER] = maxCbk;
}
////////////////////////////////////////////////////////////
void DualSlider::_onSlider(wxScrollEvent& event)
{
   int callbackID = event.GetId();
   if(_ensureSliders(callbackID))
   { 
      callbackID = BOTH_SLIDERS;
   }
   try
   {
     std::map<int,DualSlider::SliderCallback*>::iterator activeCallback;
      activeCallback = _callbacks.find(callbackID);
      if(activeCallback != _callbacks.end())
      {
         activeCallback->second->SetDualSlider(this);
         activeCallback->second->SliderOperation();   
      }
   }
   catch(...)
   {
      std::cout<<"Callback not found: "<<callbackID<<std::endl;
   }
}
////////////////////////////////////////////////////////////
void DualSlider::_onStop(wxScrollEvent& event)
{
   int callbackID = event.GetId();
   try
   {
      std::map<int,DualSlider::SliderCallback*>::iterator activeCallback;
      activeCallback = _callbacks.find(STOP);
      if(activeCallback != _callbacks.end())
      {
         activeCallback->second->SetDualSlider(this);
         activeCallback->second->SliderOperation();   
      }
   }
   catch(...)
   {
      std::cout<<"Callback not found: "<<callbackID<<std::endl;
   }
}
