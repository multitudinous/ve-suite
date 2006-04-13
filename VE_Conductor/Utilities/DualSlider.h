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

#ifndef VE_DUAL_SLIDER_H
#define VE_DUAL_SLIDER_H
#include "VE_Installer/include/VEConfig.h"
/*!\file DualSlider.h
  DualSlider API
  */
/*!\class VE_Conductor::GUI_Utilities::DualSlider
 * widget containing two wxSliders(min/max). The sliders are free to move
 * EXCEPT that the min can never be higher than the max and the max
 * can never be lower than the min.
 */
#include <wx/control.h>
#include <wx/slider.h>
namespace VE_Conductor
{
namespace GUI_Utilities
{
class VE_CONDUCTOR_UTILS_EXPORTS DualSlider: public wxControl
{
public:
   ///Constructor
   DualSlider();

   ///Constructor
   ///\param parent The parent window.
   ///\param id wxWindowID number
   ///\param minValue Minimum value.
   ///\param maxValue Maximum value.
   ///\param point Initial location of control
   ///\param size Initial size of control
   ///\param style Style for the sliders.
   DualSlider(wxWindow* parent, wxWindowID id,
              int minValue, int maxValue,
              const wxPoint& point = wxDefaultPosition,
              const wxSize& size = wxDefaultSize, long style = wxSL_HORIZONTAL,
              const wxString& name = "minMaxDualSlider" );
   ///Destructor
   virtual ~DualSlider();

   ///Set the range on the min slider
   ///\param min The minimum value
   ///\param max The maximum value
   void SetSliderRange(int min,int max);

   ///Set the minimum allowable distance between slider values.
   ///\param buffer The buffer value.
   void SetSliderBuffer(int buffer);

   ///Create the DualSlider
   ///\param parent The parent window.
   ///\param id wxWindowID number
   ///\param minValue Minimum value.
   ///\param maxValue Maximum value.
   ///\param point Initial location of control
   ///\param size Initial size of control
   ///\param style Style for the sliders.
   bool Create(wxWindow* parent, wxWindowID id,
               int min, int max,
               const wxPoint& pos, const wxSize& size, long style,
               const wxString& name);

   ///Get the slider buffer value
   int GetSliderBuffer();

   ///Get the min slider maximum value.
   int GetSliderMaximum();

   ///Get the min slider minimum value.
   int GetSliderMinimum();

   ///Get the value of the minimum slider
   int GetMinSliderValue();
   ///Get the value of the maximum slider
   int GetMaxSliderValue();
protected:
   int _range[2];///<Slider value bounds.
   unsigned int _buffer;///<Set the minimum space between sliders
   wxSlider* _minSlider;///<Minimum slider.\m Displayed on the top of the pair
   wxSlider* _maxSlider;///<Maximum slider.\m Displayed on the bottom of the pair.
};
}
}
#endif// VE_DUAL_SLIDER_H
