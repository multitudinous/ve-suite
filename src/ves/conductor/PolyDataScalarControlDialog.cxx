/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/conductor/PolyDataScalarControlDialog.h>
#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/vistab.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <wx/msgdlg.h>

#include <sstream>
#include <string>

using namespace ves::conductor;

////////////////////////////////////////////////////////////////////////////////
PolyDataScalarControlDialog::PolyDataScalarControlDialog( wxWindow* parent )
:
ScalarControlDialog( parent )
{
    m_scalarRange = dynamic_cast<Vistab*>( parent->GetParent() )->GetActiveScalarRange();
}
////////////////////////////////////////////////////////////////////////////////
void PolyDataScalarControlDialog::OnMinTextInput( wxCommandEvent& event )
{
    double minValue = 0;
    double minTexVal = 0.0;
    if( !m_minTextCtrl->GetValue().ToDouble( &minTexVal ) )
    {
        return;
    }

    minValue = (( minTexVal - m_scalarRange.first )
                / ( m_scalarRange.second - m_scalarRange.first ) * 100 );
    
    if( minValue == 100 )
    {
        m_minSlider->SetValue(( int )minValue );
        m_maxSlider->SetValue(( int )minValue + 1 );
    }
    else if( m_maxSlider->GetValue() <= ( int )minValue )
    {
        m_minSlider->SetValue(( int )minValue );
        m_maxSlider->SetValue(( int )minValue + 1 );

        std::ostringstream tempInput;
        tempInput << m_scalarRange.second - (( m_scalarRange.second - m_scalarRange.first )
                                             *( 100 - ( double )m_maxSlider->GetValue() ) / 100 );
        m_maxTextControl->SetValue( wxString( tempInput.str().c_str(), wxConvUTF8 ) );
    }
    else
    {
        m_minSlider->SetValue(( int )minValue );
    }

    SendData( VISTAB_MIN_SLIDER );
}
////////////////////////////////////////////////////////////////////////////////
void PolyDataScalarControlDialog::OnMinSlider( wxScrollEvent& event )
{
    double range = m_scalarRange.second - m_scalarRange.first;
    
    if( m_minSlider->GetValue() >= m_maxSlider->GetValue() ) // && m_minSlider->GetValue() < 100 )
    {
        EnsureSliders( VISTAB_MIN_SLIDER );
    }
    
    std::ostringstream tempInput;
    tempInput << ( range *( double )m_minSlider->GetValue() ) / 100 + m_scalarRange.first;
    m_minTextCtrl->SetValue( wxString( tempInput.str().c_str(), wxConvUTF8 ) );
    
    std::ostringstream tempInputMax;
    tempInputMax << m_scalarRange.second - ( range *( 100 - ( double )m_maxSlider->GetValue() ) / 100 );
    m_maxTextControl->SetValue( wxString( tempInputMax.str().c_str(), wxConvUTF8 ) );
    
    SendData( VISTAB_MIN_SLIDER );
}
////////////////////////////////////////////////////////////////////////////////
void PolyDataScalarControlDialog::OnMaxTextInput( wxCommandEvent& event )
{
    double maxValue = 100;
    double maxTexVal = 0.0;
    if( !m_maxTextControl->GetValue().ToDouble( &maxTexVal ) )
    {
        return;
    }
    
    maxValue = (( m_scalarRange.second - m_scalarRange.first
                 - ( m_scalarRange.second - maxTexVal ) )
                / ( m_scalarRange.second - m_scalarRange.first ) * 100 );
    
    if( maxValue == 0 )
    {
        m_minSlider->SetValue(( int )maxValue + 1 );
        m_maxSlider->SetValue(( int )maxValue );
    }
    else if( m_minSlider->GetValue() >= ( int )maxValue )
    {
        m_minSlider->SetValue(( int )maxValue - 1 );
        m_maxSlider->SetValue(( int )maxValue );
        std::ostringstream tempInput;
        tempInput << ( m_scalarRange.second - m_scalarRange.first )
            *( double )m_minSlider->GetValue() / 100 + m_scalarRange.first;
        m_minTextCtrl->SetValue( wxString( tempInput.str().c_str(), wxConvUTF8 ) );
    }
    else
    {
        m_maxSlider->SetValue(( int )maxValue );
    }
    
    SendData( VISTAB_MAX_SLIDER );
}
////////////////////////////////////////////////////////////////////////////////
void PolyDataScalarControlDialog::OnMaxSlider( wxScrollEvent& event )
{
    double range = m_scalarRange.second - m_scalarRange.first;
    
    if( m_maxSlider->GetValue() <= m_minSlider->GetValue() ) //&& m_maxSlider->GetValue() > 0 )
    {
        EnsureSliders( VISTAB_MAX_SLIDER );
    }
    
    std::ostringstream tempInput;
    tempInput << ( range *( double )m_minSlider->GetValue() ) / 100 + m_scalarRange.first;
    m_minTextCtrl->SetValue( wxString( tempInput.str().c_str(), wxConvUTF8 ) );
    
    std::ostringstream tempInputMax;
    tempInputMax << m_scalarRange.second - ( range *( 100 - ( double )m_maxSlider->GetValue() ) / 100 );
    m_maxTextControl->SetValue( wxString( tempInputMax.str().c_str(), wxConvUTF8 ) );
    
    SendData( VISTAB_MAX_SLIDER );
}
////////////////////////////////////////////////////////////////////////////////
bool PolyDataScalarControlDialog::EnsureSliders( int activeSliderID )
{
    int minValue = m_minSlider->GetValue();
    int maxValue = m_maxSlider->GetValue();
    
    //maintain the value on the min/max sliders.
    if( minValue > maxValue - static_cast<int>( 1 ) )
    {
        if( minValue == 100 )
        {
            m_minSlider->SetValue( 100 - 1 );
        }
        else if( maxValue == 0 )
        {
            m_maxSlider->SetValue( 0 + 1 );
        }
        
        if( activeSliderID == VISTAB_MIN_SLIDER )
        {
            m_maxSlider->SetValue( m_minSlider->GetValue() + 1 );
            return true;
        }
        else if( activeSliderID == VISTAB_MAX_SLIDER )
        {
            m_minSlider->SetValue( m_maxSlider->GetValue() - 1 );
            return true;
        }
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void PolyDataScalarControlDialog::SendData( int activeSliderID )
{
    double minMaxVal;
    std::string minMaxTag;
    
    if( activeSliderID == VISTAB_MIN_SLIDER )
    {
        m_minTextCtrl->GetValue().ToDouble( &minMaxVal );
        minMaxTag = "minValue";
    }
    else if( activeSliderID == VISTAB_MAX_SLIDER )
    {
        m_maxTextControl->GetValue().ToDouble( &minMaxVal );
        minMaxTag = "maxValue";
    }

    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );
    newCommand->SetCommandName( "LIVE_POLYDATA_UPDATE" );
    
    ves::open::xml::DataValuePairPtr 
        warpSurface( new ves::open::xml::DataValuePair() );
    warpSurface->SetData( minMaxTag, minMaxVal );
    newCommand->AddDataValuePair( warpSurface );

    ves::conductor::util::CORBAServiceList::instance()->
        SendCommandStringToXplorer( newCommand );
}
////////////////////////////////////////////////////////////////////////////////
