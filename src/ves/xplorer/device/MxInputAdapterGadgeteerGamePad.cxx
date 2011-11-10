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
/*
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

}
////////////////////////////////////////////////////////////////////////////////
void MxInputAdapterGadgeteerGamePad::ExtractPOV( float value )

{

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
*/