/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#ifndef DUAL_SLIDER_H
#define DUAL_SLIDER_H
#include <ves/VEConfig.h>
/*!\file DualSlider.h
  DualSlider API
  */
/*!\class ves::conductor::util::DualSlider
 * widget containing two wxSliders(min/max). The sliders are free to move
 * EXCEPT that the min can never be higher than the max and the max
 * can never be lower than the min.
 */
#include <wx/panel.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>

#include <map>

namespace ves
{
namespace conductor
{
namespace util
{
class wxSpinCtrlDbl;

class VE_CONDUCTOR_UTILS_EXPORTS DualSlider: public wxPanel
{
public:
    enum DUAL_SLIDER_IDS
    {
        BOTH_SLIDERS,
        MAX_SLIDER,
        MIN_SLIDER,
        STOP
    };
    ///Constructor
    ///\param parent The parent window.
    ///\param id wxWindowID number
    ///\param buffer Minimum allowable distance between slider values.
    ///\param minValue Minimum value.
    ///\param maxValue Maximum value.
    ///\param minSliderValue Initial Minimum Slider value.
    ///\param maxSliderValue Initial Maximum Slider value.
    ///\param point Initial location of control
    ///\param size Initial size of control
    ///\param style Style for the sliders.
    DualSlider( wxWindow* parent, wxWindowID id,
                unsigned int buffer,
                int minValue, int maxValue,
                int minSliderValue, int maxSliderValue,
                const wxPoint& point = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = wxSL_HORIZONTAL,
                const wxString& name = wxT( "minMaxDualSlider" ) );
    ///Destructor
    virtual ~DualSlider();

    ///Set the range on the min slider
    ///\param min The minimum value
    ///\param max The maximum value
    void SetSliderRange( int minValue, int maxValue );

    ///Set the min slider
    ///\param min The minimum value
    void SetMinimumSliderValue( int value );

    ///Set the max slider
    ///\param max The maximum value
    void SetMaximumSliderValue( int value );

    ///Set the minimum allowable distance between slider values.
    ///\param buffer The buffer value.
    void SetSliderBuffer( int buffer );

    ///Get the slider buffer value
    int GetSliderBuffer();

    ///Get the slider maximum value.
    double GetSliderMaximum();

    ///Get the slider minimum value.
    double GetSliderMinimum();

    ///Get the value of the minimum slider
    double GetMinSliderValue();
    ///Get the value of the maximum slider
    double GetMaxSliderValue();

    /*!\class DualSlider::SliderCallback
     *Class that allows the user to do operations based on slider events
     */
    class SliderCallback
    {
    public:
        ///Constructor
        SliderCallback()
        {
            _dualSlider = 0;
        }
        ///Destructor
        virtual ~SliderCallback()
        {}
        void SetDualSlider( DualSlider* ds )
        {
            _dualSlider = ds;
        }
        ///Do operations based on slider movement
        virtual void SliderOperation() = 0;
    protected:
        DualSlider* _dualSlider;
    };

    ///Set the callback for the maxSlider
    ///\param minSCbk The callback for the min slider
    void SetMinSliderCallback( SliderCallback* minSCbk );

    ///Set the callback for the maxSlider
    ///\param maxSCbk The callback for the max slider
    void SetMaxSliderCallback( SliderCallback* maxSCbk );

    ///Callback that allows user to react to both sliders moving
    ///\param maxSCbk The callback for the max slider
    void SetBothSliderUpdateCallback( SliderCallback* bothSCbk );

    ///Callback that allows user to react to sliders stopping
    ///\param maxSCbk The callback for the max slider
    void SetStopSliderUpdateCallback( SliderCallback* bothSCbk );
protected:
    ///Handle events on the sliders
    ///\param event wxScollEvent
    void _onSlider( wxScrollEvent& event );

    ///Handle stop events on the sliders
    ///\param event wxScollEvent
    void _onStop( wxScrollEvent& event );

    ///Handle events for spinners
    void UpdateSlider( wxCommandEvent& event );

    ///Handle events for spinners
    void UpdateSpinners( wxSpinEvent& event );

    ///Ensure that sliders don't cross over.
    ///\param activeSliderID The slider on the dial that's moving
    bool _ensureSliders( int activeSliderID );

    ///Ensure that sliders don't cross over.
    ///\param callbackID The slider on the dial that's moving
    void UpdateSpinnerValues( int callbackID );

    int _range[2];///<Slider value bounds.
    unsigned int _buffer;///<Set the minimum space between sliders
    wxSlider* _minSlider;///<Minimum slider.\m Displayed on the top of the pair
    wxSlider* _maxSlider;///<Maximum slider.\m Displayed on the bottom of the pair.

    ///Min double spinner
    wxSpinCtrlDbl* m_minSpinner;
    ///Max double spinner
    wxSpinCtrlDbl* m_maxSpinner;

    std::map<int, SliderCallback*> _callbacks;///<Map for the slider callbacks.

    DECLARE_EVENT_TABLE()
};
}
}
}
#endif// DUAL_SLIDER_H
