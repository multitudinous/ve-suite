/*************** <auto-copyright.pl BEGIN do not edit this line> **************
*
* osgWorks is (C) Copyright 2009-2011 by Kenneth Mark Bryden
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License version 2.1 as published by the Free Software Foundation.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Library General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the
* Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* Boston, MA 02111-1307, USA.
*
*************** <auto-copyright.pl END do not edit this line> ***************/

#include <ves/xplorer/device/MxInputAdapterGadgeteerGamePad.h>

#include <ostream>
#include <iostream>

#include <gmtl/Math.h>


namespace osgwMx
{
////////////////////////////////////////////////////////////////////////////////

#define MIN_AXIS_RANGE -10000
#define MAX_AXIS_RANGE 10000

////////////////////////////////////////////////////////////////////////////////
MxInputAdapterGadgeteerGamePad::MxInputAdapterGadgeteerGamePad() 
{
    rangeValueAttenuate[ ROTATE_X_AXIS_IDX ] = 0.05f;
    rangeValueAttenuate[ ROTATE_Y_AXIS_IDX ] = 0.05f;
    rangeValueAttenuate[ MOVE_X_AXIS_IDX ] = 0.05f;
    rangeValueAttenuate[ MOVE_Y_AXIS_IDX ] = 0.05f;
}
////////////////////////////////////////////////////////////////////////////////
MxInputAdapterGadgeteerGamePad::~MxInputAdapterGadgeteerGamePad()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MxInputAdapterGadgeteerGamePad::ExtractButtons( float value )
{
    /*int cnt, maxBtns = BUTTON_COUNT;
    if (BUTTON_COUNT > 128)       // don't read more than is available in DIJOYSTATE2.
        maxBtns = 128;
    for (cnt = 0; cnt < maxBtns; cnt++)
    {
        if (devState.rgbButtons[cnt])
            buttons[cnt] = true;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void MxInputAdapterGadgeteerGamePad::ExtractPOV( float value )

{
    /*DWORD pov = devState.rgdwPOV[0] & 0xffff;          // get POV value
    if (pov != 0xffff)                                 // if current POV not centered
    {
        if ((pov >= 30000) || (pov <= 6000))            // forward
        {
            dpad.up = true;
            if ((pov >= 4500) && (pov <= 6000))
                dpad.right = true;
            else if ((pov >= 30000) && (pov <= 31500))
                dpad.left = true;
        }
        else if ((pov > 6000) && (pov < 12000))         // right
            dpad.right = true;
        else if ((pov >= 12000) && (pov <= 24000))      // backward
        {
            dpad.down = true;
            if (pov <= 13500)
                dpad.right = true;
            else if (pov >= 22500)
                dpad.left = true;
        }
        else if ((pov > 24000) && (pov < 30000))        // left
            dpad.left = true;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void MxInputAdapterGadgeteerGamePad::ExtractAxis( unsigned int axis, float value )
{
    rangeValues[ axis ] = GetNormalizedAxisValue( value );
    if( gmtl::Math::abs( rangeValues[ axis ] ) < rangeValueAttenuate[ axis ])
    {
        rangeValues[ axis ] = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
double MxInputAdapterGadgeteerGamePad::GetNormalizedAxisValue( float av )
{
    static double maxAxis = (double)MAX_AXIS_RANGE;

    return (double)av / maxAxis;
}
////////////////////////////////////////////////////////////////////////////////
} // osgwMx
