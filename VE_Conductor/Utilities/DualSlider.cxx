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
using namespace VE_Conductor::GUI_Utilities;
////////////////////////
DualSlider::DualSlider()
:wxControl()
{
   _buffer = 1;
   _minSlider = 0;
   _maxSlider = 0;
   _range[0] = 0;
   _range[1] = 100;
}
////////////////////////////////////////////////////////
DualSlider::DualSlider( wxWindow* parent, wxWindowID id,
                        int min, int max,
                        const wxPoint& pos, 
                        const wxSize& size,
                        long style,
                        const wxString& name)
{
   Create(parent,id,min,max,pos,size,style,name);
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
//////////////////////////////////////////////////////////////////////////
bool DualSlider::Create(wxWindow* parent, wxWindowID id,
                        int min,int max,
                        const wxPoint& pos, const wxSize& size, long style,
                        const wxString& name )
{
   if(!wxControl::Create(parent,id,pos,size,style|wxNO_BORDER,wxDefaultValidator,name))
   {
      return false; 
   }
   wxControl::SetLabel(name);
   wxControl::SetBackgroundColour(parent->GetBackgroundColour());
   wxControl::SetForegroundColour(parent->GetBackgroundColour());

   int width = size.GetWidth(), height = size.GetHeight();

   wxSize best_size( DoGetBestSize() );
   if (width  == -1) width  = best_size.GetWidth();
   if (height == -1) height = best_size.GetHeight();

   _range[0] = min;
   _range[1] = max;
   _minSlider = new wxSlider(this,id,_range[0] ,_range[0], _range[1]-1, wxPoint(0,height/2),
                             wxSize(-1,height/2), wxSL_HORIZONTAL, wxDefaultValidator,"minSlider");
   _maxSlider = new wxSlider(this,id,_range[1] ,_range[0]+1, _range[1], wxPoint(0,0),
                             wxSize(-1,height/2), wxSL_HORIZONTAL, wxDefaultValidator,"maxSlider");

   DoSetSize(pos.x,pos.y,width,height);
   SetBestSize(wxSize(width,height));
   return true;
}
//////////////////////////////////////////////////
void DualSlider::SetSliderRange(int min, int max)
{
   _range[0] = min;
   _range[1] = max;
   _minSlider->SetRange(_range[0],_range[1]-1);
   _maxSlider->SetRange(_range[0]+1,_range[1]);
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
