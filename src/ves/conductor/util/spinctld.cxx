/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#ifndef WX_PRECOMP
#include <wx/valtext.h>     // for wxTextValidator
#include <wx/textctrl.h>
#endif // WX_PRECOMP

#include <iostream>

#include <ves/conductor/util/spinctld.h>
using namespace ves::conductor::util;
#include <math.h>

#if wxMINOR_VERSION > 4
#include <wx/math.h>
#else
#if defined(__VISUALC__) || defined(__BORLANDC__) || defined(__WATCOMC__)
#include <float.h>
#define wxFinite(x) _finite(x)
#elif defined(__GNUG__)||defined(__GNUWIN32__)||defined(__DJGPP__)|| \
      defined(__SGI_CC__)||defined(__SUNCC__)||defined(__XLC__)|| \
          defined(__HPUX__)||defined(__MWERKS__)
#define wxFinite(x) finite(x)
#else
#define wxFinite(x) ((x) == (x))
#endif
#endif

// NOTES : if the textctrl is focused and the program is ending, a killfocus
//         event is sent in MSW, this is why m_textCtrl is set to NULL in it's
//         destructor and there's so many checks for it not being NULL

//----------------------------------------------------------------------------
// wxSpinCtrlDbl
//----------------------------------------------------------------------------

// the textctrl used for the wxSpinCtrlDbl, needed for keypresses
namespace ves
{
namespace conductor
{
namespace util
{
class wxSpinCtrlDblTextCtrl : public wxTextCtrl
{
public:
    wxSpinCtrlDblTextCtrl( wxWindow *parent, wxWindowID id,
                           const wxString &value = wxEmptyString,
                           const wxPoint &pos = wxDefaultPosition,
                           const wxSize &size = wxDefaultSize,
                           const wxString &name = wxTextCtrlNameStr );

    // MSW sends extra kill focus event
    virtual ~wxSpinCtrlDblTextCtrl()
    {
        if( m_parent ) m_parent->m_textCtrl = NULL;
        m_parent = NULL;
    }

    wxSpinCtrlDbl *m_parent;

    void OnChar( wxKeyEvent &event );         // pass chars to wxSpinCtrlDbl
    void OnKillFocus( wxFocusEvent &event );  // sync the spin to textctrl

private:
    DECLARE_EVENT_TABLE()
};
}
}
}

BEGIN_EVENT_TABLE( wxSpinCtrlDblTextCtrl, wxTextCtrl )
    //  EVT_TEXT_ENTER( wxID_ANY, wxSpinCtrlDblTextCtrl::OnTextEnter ) // get them from spinctrldbl
    //  EVT_TEXT( wxID_ANY, wxSpinCtrlDblTextCtrl::OnTextUpdate )      // get them from spinctrldbl
    EVT_CHAR( wxSpinCtrlDblTextCtrl::OnChar )
    EVT_KILL_FOCUS( wxSpinCtrlDblTextCtrl::OnKillFocus )
END_EVENT_TABLE()

wxSpinCtrlDblTextCtrl::wxSpinCtrlDblTextCtrl( wxWindow *parent, wxWindowID id,
                                              const wxString &value,
                                              const wxPoint &pos, const wxSize &size,
                                              const wxString &name )
        : wxTextCtrl( parent, id, value, pos, size,
                      wxTE_NOHIDESEL | wxTE_PROCESS_ENTER,
                      wxDefaultValidator, name )
{
    m_parent = ( wxSpinCtrlDbl* )parent;

    wxTextValidator valid( wxFILTER_INCLUDE_CHAR_LIST );
    wxArrayString list;

    wxString valid_chars( wxT( " 0123456789+-.eE" ) );
    size_t len = valid_chars.Length();
    for( size_t i = 0; i < len; i++ )
        list.Add( wxString( valid_chars.GetChar( i ) ) );

    valid.SetIncludes( list );
    SetValidator( valid );
}

void wxSpinCtrlDblTextCtrl::OnChar( wxKeyEvent &event )
{
    if( m_parent ) m_parent->OnChar( event );
}

void wxSpinCtrlDblTextCtrl::OnKillFocus( wxFocusEvent &event )
{
    if( m_parent ) m_parent->SyncSpinToText( TRUE );
    event.Skip();
}

//----------------------------------------------------------------------------
// wxSpinCtrlDbl
//----------------------------------------------------------------------------

//IMPLEMENT_DYNAMIC_CLASS( wxSpinCtrlDbl, wxControl )

BEGIN_EVENT_TABLE( wxSpinCtrlDbl, wxControl )
    EVT_SPIN( wxID_ANY, wxSpinCtrlDbl::OnSpin)
    EVT_SPIN_UP( wxID_ANY, wxSpinCtrlDbl::OnSpinUp )
    EVT_SPIN_DOWN( wxID_ANY, wxSpinCtrlDbl::OnSpinDown )
    EVT_TEXT_ENTER( wxID_ANY, wxSpinCtrlDbl::OnTextEnter )
    EVT_SET_FOCUS( wxSpinCtrlDbl::OnFocus )
    EVT_KILL_FOCUS( wxSpinCtrlDbl::OnKillFocus )
END_EVENT_TABLE()

void wxSpinCtrlDbl::Init()
{
    m_min = 0;
    m_max = 100;
    m_value = 0;
    m_default_value = 0;
    m_increment = 1;
    m_digits = wxSPINCTRLDBL_AUTODIGITS;
    m_snap_ticks = FALSE;
    m_spinButton = NULL;
    m_textCtrl = NULL;
}

bool wxSpinCtrlDbl::Create( wxWindow *parent, wxWindowID id,
                            const wxString& value,
                            const wxPoint& pos, const wxSize& size, long style,
                            double min, double max,
                            double initial,
                            double increment, int digits,
                            const wxString& name )
{
    if( !wxControl::Create( parent, id, pos, size, style | wxNO_BORDER, wxDefaultValidator, name ) )
        return FALSE;

    wxControl::SetLabel( name );
    wxControl::SetBackgroundColour( parent->GetBackgroundColour() );
    wxControl::SetForegroundColour( parent->GetForegroundColour() );

    int width = size.GetWidth(), height = size.GetHeight();

    wxSize best_size( DoGetBestSize() );
    if( width  == -1 ) width  = best_size.GetWidth();
    if( height == -1 ) height = best_size.GetHeight();

    m_spinButton = new wxSpinButton( this, id, wxPoint( 0, 0 ), wxSize( -1, height ),
                                     wxSP_ARROW_KEYS | wxSP_VERTICAL );
    m_textCtrl = new wxSpinCtrlDblTextCtrl( this, id, value, wxPoint( 0, 0 ),
                                            wxSize( width - m_spinButton->GetSize().GetWidth(), height ) );

    DoSetSize( pos.x, pos.y, width, height );
    SetInitialSize( wxSize( width, height ) );

    m_min = min;
    m_max = max;
    m_value = initial;
    m_default_value = initial;
    m_increment = increment;
    SetDigits( digits );
    m_spinButton->SetRange( int( m_min ), int( m_max ) );
    m_spinButton->SetValue( int( m_value ) );
    // set the value here without generating an event (also don't use virutal
    //  SetValue functions)
    if( !value.IsEmpty() )
        m_textCtrl->SetValue( value );
    else
        m_textCtrl->SetValue( wxString::Format( m_textFormat.c_str(), initial ) );

    return TRUE;
}

wxSpinCtrlDbl::~wxSpinCtrlDbl()
{
    if( m_textCtrl ) // null this since MSW sends KILL_FOCUS on deletion
    {
        m_textCtrl->m_parent = NULL;
        m_textCtrl = NULL;
    }
}

#define wxSPINCTRLDBL_SPIN_WIDTH  15
#define wxSPINCTRLDBL_SPIN_HEIGHT 22

void wxSpinCtrlDbl::DoSetSize( int x, int y, int width, int height, int sizeFlags )
{
    //wxPrintf(wxT("DoSetSize %d, %d %d %d %d %d\n"), GetId(), x, y, width, height, sizeFlags);

    wxSize bestSize( DoGetBestSize() );
    if( width < 0 )  width  = bestSize.GetWidth();
    if( height < 0 ) height = bestSize.GetHeight();

    wxWindow::DoSetSize( x, y, width, height, sizeFlags );

    int spinwidth  = wxSPINCTRLDBL_SPIN_WIDTH;
    int spinheight = wxSPINCTRLDBL_SPIN_HEIGHT;
    if( m_spinButton )
        m_spinButton->GetSize( &spinwidth, &spinheight );

#ifdef __WIN95__   // humm... these used to be different
    if( m_textCtrl )   m_textCtrl->SetSize( 0, 0, width - spinwidth, height );
    if( m_spinButton ) m_spinButton->SetSize( width - spinwidth - 2, 0, -1, height );
    //m_textCtrl->SetSize( -3, -3, width - spinwidth, height );   // old wxWin < 2.3.2
    //m_spinButton->SetSize( width-spinwidth-4, -3, -1, height-1 );
#else
    if( m_textCtrl )   m_textCtrl->SetSize( 0, 0, width - spinwidth, height );
    if( m_spinButton ) m_spinButton->SetSize( width - spinwidth, 0, -1, height );
#endif
}

static wxSize s_spinctrl_bestSize( -999, -999 );

double   wxSpinCtrlDbl::GetValue() const
{
    return m_value;
}

wxSize wxSpinCtrlDbl::DoGetBestSize() const
{
    //wxPrintf(wxT("GetBestSize %d\n"), GetId());
    if( s_spinctrl_bestSize.x == -999 )
    {
        wxSpinCtrl spin(( wxWindow* )this, wxID_ANY );
        s_spinctrl_bestSize = spin.GetBestSize();
        // oops something went wrong, set to reasonable value
        if( s_spinctrl_bestSize.GetWidth()  < 20 )
            s_spinctrl_bestSize.SetWidth( 95 );
        if( s_spinctrl_bestSize.GetHeight() < 10 )
            s_spinctrl_bestSize.SetHeight( wxSPINCTRLDBL_SPIN_HEIGHT );
    }

    return s_spinctrl_bestSize;
}

void wxSpinCtrlDbl::DoSendEvent()
{
    wxCommandEvent event( wxEVT_COMMAND_SPINCTRL_UPDATED, GetId() );
    event.SetEventObject( this );
    event.SetInt(( int )( m_value + 0.5 ) );
    if( m_textCtrl ) event.SetString( m_textCtrl->GetValue() );
    GetEventHandler()->ProcessEvent( event );
}

void wxSpinCtrlDbl::OnSpin( wxSpinEvent& event )
{
    event.Skip();
}

void wxSpinCtrlDbl::OnSpinUp( wxSpinEvent &WXUNUSED( event ) )
{
    if( m_textCtrl && m_textCtrl->IsModified() )
        SyncSpinToText( FALSE );

    if( InRange( m_value + m_increment ) )
    {
        m_value += m_increment;
        SetValue( m_value );
        DoSendEvent();
    }
}

void wxSpinCtrlDbl::OnSpinDown( wxSpinEvent &WXUNUSED( event ) )
{
    if( m_textCtrl && m_textCtrl->IsModified() )
        SyncSpinToText( FALSE );

    if( InRange( m_value - m_increment ) )
    {
        m_value -= m_increment;
        SetValue( m_value );
        DoSendEvent();
    }
}

void wxSpinCtrlDbl::OnTextEnter( wxCommandEvent &event )
{
    SyncSpinToText( TRUE );
    event.Skip();
}

void wxSpinCtrlDbl::OnChar( wxKeyEvent &event )
{
    double modifier = 1.0;
    if( event.m_shiftDown ) modifier  = 2.0;
    if( event.m_controlDown ) modifier *= 10.0;
    if( event.m_altDown ) modifier *= 100.0;

    switch ( event.GetKeyCode() )
    {
        case WXK_UP :
        {
            if( m_textCtrl && m_textCtrl->IsModified() ) SyncSpinToText( FALSE );
            SetValue( m_value + m_increment * modifier );
            DoSendEvent();
            break;
        }
        case WXK_DOWN :
        {
            if( m_textCtrl && m_textCtrl->IsModified() ) SyncSpinToText( FALSE );
            SetValue( m_value - m_increment * modifier );
            DoSendEvent();
            break;
        }
        case WXK_PAGEUP :  // pg-up
        {
            if( m_textCtrl && m_textCtrl->IsModified() ) SyncSpinToText( FALSE );
            SetValue( m_value + m_increment * 10.0 * modifier );
            DoSendEvent();
            break;
        }
        case WXK_PAGEDOWN :  // pg-up
        {
            if( m_textCtrl && m_textCtrl->IsModified() ) SyncSpinToText( FALSE );
            SetValue( m_value - m_increment * 10.0 * modifier );
            DoSendEvent();
            break;
        }
        case WXK_SPACE :
        {
            SetValue( m_value );
            event.Skip( FALSE );
            break;
        }
        case WXK_ESCAPE :
        {
            SetDefaultValue();
            DoSendEvent();
            break;
        }
        case WXK_TAB :
        {
            wxNavigationKeyEvent new_event;
            new_event.SetEventObject( GetParent() );
            new_event.SetDirection( !event.ShiftDown() );
            // CTRL-TAB changes the (parent) window, i.e. switch notebook page
            new_event.SetWindowChange( event.ControlDown() );
            new_event.SetCurrentFocus( this );
            GetParent()->GetEventHandler()->ProcessEvent( new_event );
            break;
        }
        default :
            event.Skip();
            break;
    }
}

void wxSpinCtrlDbl::SetValue( double value )
{
    // this check is probably too much to ask
    if( !m_textCtrl || !InRange( value ) )
        return;

    if( m_snap_ticks && ( m_increment != 0 ) )
    {
        double snap_value = ( value - m_default_value ) / m_increment;

        if( wxFinite( snap_value ) ) // FIXME what to do about a failure?
        {
            if( snap_value - floor( snap_value ) < ceil( snap_value ) - snap_value )
                value = m_default_value + floor( snap_value ) * m_increment;
            else
                value = m_default_value + ceil( snap_value ) * m_increment;
        }
    }

    wxString str( wxString::Format( m_textFormat.c_str(), value ) );
    if (( value != m_value ) || ( str != m_textCtrl->GetValue() ) )
    {
        m_textCtrl->ChangeValue( str );
        m_textCtrl->DiscardEdits();
        m_value = value;
        str.ToDouble( &m_value );    // wysiwyg for textctrl
        m_spinButton->SetValue( int( m_value ) );
    }
}

void wxSpinCtrlDbl::SetValue( const wxString& text, bool force )
{
    if( !m_textCtrl ) return;

    double value;
    if( text.ToDouble( &value ) )
        SetValue( value );
    else if( force )
    {
        m_textCtrl->ChangeValue( text );
        m_textCtrl->DiscardEdits();
    }
}

void wxSpinCtrlDbl::SetRange( double min_val, double max_val )
{
    //wxCHECK_RET(max_val > min_val, wxT("invalid spinctrl range"));
    m_min = min_val;
    m_max = max_val;
    m_spinButton->SetRange( int( m_min ), int( m_max ) );

    if( HasRange() )
    {
        if( m_value > m_max )
            SetValue( m_max );
        else if( m_value < m_min )
            SetValue( m_min );
    }
}

void wxSpinCtrlDbl::SetIncrement( double increment )
{
    m_increment = increment;
    SetValue( m_value );
}

void wxSpinCtrlDbl::SetDigits( int digits, formatType fmt )
{
    wxCHECK_RET( digits >= -1, wxT( "invalid spinctrl format" ) );

    if (( digits == wxSPINCTRLDBL_AUTODIGITS ) && ( fmt != lg_fmt ) )
    {
        wxString wxstr;
        int lastplace = -1, extra_digits = 0;
        if( fmt == le_fmt )
        {
            wxstr.Printf( wxT( "%le" ), m_increment );
            wxstr.LowerCase();
            lastplace = wxstr.Find( wxT( 'e' ) ) - 2;
            long places;
            if( wxstr.AfterFirst( wxT( 'e' ) ).ToLong( &places ) )
                extra_digits = int( labs( places ) );
        }
        else if( fmt == lf_fmt )
        {
            wxstr.Printf( wxT( "%lf" ), m_increment );
            lastplace = wxstr.Len() - 1;
        }

        int decimalplace = wxstr.Find( wxT( '.' ) );

        int i = 0;

        for( i = lastplace; i > decimalplace; i-- )
        {
            if( wxstr.GetChar( i ) != wxT( '0' ) )
            {
                m_digits = extra_digits + i - decimalplace;
                switch ( fmt )
                {
                    case le_fmt :
                        m_textFormat.Printf( wxT( "%%.%dle" ), m_digits );
                        break;
                    case lf_fmt :
                    default     :
                        m_textFormat.Printf( wxT( "%%.%dlg" ), m_digits );
                        break;
                }

                SetValue( m_value );
                return;
            }
        }

        m_digits = 0;  // no digits, I guess
    }
    else
        m_digits = digits;

    switch ( fmt )
    {
        case le_fmt :
            m_textFormat.Printf( wxT( "%%.%dle" ), m_digits );
            break;
        case lg_fmt :
        {
            if( m_digits == -1 )
                m_textFormat.Printf( wxT( "%%lg" ) );
            else
                m_textFormat.Printf( wxT( "%%.%dlg" ), m_digits );
            break;
        }
        case lf_fmt :
        default     :
            m_textFormat.Printf( wxT( "%%.%dlf" ), m_digits );
            break;
    }

    SetValue( m_value );
}

void wxSpinCtrlDbl::SetFormat( const wxString& format )
{
    wxString wxstr;
    if( wxstr.Printf( format.c_str(), 123456.123456 ) > 0 )
        m_textFormat = format;

    SetValue( m_value );
}

void wxSpinCtrlDbl::SetDefaultValue( double default_value )
{
    if( InRange( default_value ) )
    {
        m_default_value = default_value;
        SetDefaultValue();
    }
}

void wxSpinCtrlDbl::SetSnapToTicks( bool forceTicks )
{
    if( m_snap_ticks != forceTicks )
    {
        m_snap_ticks = forceTicks;
        SetValue( m_value );
    }
}

void wxSpinCtrlDbl::OnFocus( wxFocusEvent &event )
{
    if( m_textCtrl )
        m_textCtrl->SetFocus(); // this is to pass TAB navigation

    event.Skip();
}

void wxSpinCtrlDbl::OnKillFocus( wxFocusEvent &event )
{
    SyncSpinToText( TRUE );
    event.Skip();
}

void wxSpinCtrlDbl::SyncSpinToText( bool send_event )
{
    double txt_value;
    if( m_textCtrl && m_textCtrl->GetValue().ToDouble( &txt_value ) )
    {
        if( InRange( txt_value ) && ( m_value != txt_value ) )
        {
            SetValue( txt_value );
            if( send_event ) DoSendEvent();
        }
    }
}

bool wxSpinCtrlDbl::SetFont( const wxFont &font )
{
    if( !m_textCtrl ) return FALSE;
    return m_textCtrl->SetFont( font );
}
wxFont wxSpinCtrlDbl::GetFont() const
{
    if( !m_textCtrl ) return GetFont();
    return m_textCtrl->GetFont();
}

wxSpinCtrlDbl::wxSpinCtrlDbl() : wxControl()
{
    Init();
}

// Native constructor - note &parent, this is to avoid ambiguity
wxSpinCtrlDbl::wxSpinCtrlDbl( wxWindow &parent, wxWindowID id,
                              const wxString &value,
                              const wxPoint& pos,
                              const wxSize& size,
                              long style,
                              double min, double max,
                              double initial,
                              double increment, int digits,
                              const wxString& name )
{
    Init();
    Create( &parent, id, value, pos, size, style,
            min, max, initial, increment, digits, name );
}

// wxSpinCtrl compatibility, call SetIncrement(increment,digits) after
wxSpinCtrlDbl::wxSpinCtrlDbl( wxWindow *parent, wxWindowID id ,
                              const wxString &value,
                              const wxPoint& pos ,
                              const wxSize& size,
                              long style,
                              int min, int max,
                              int initial,
                              const wxString& name )
{
    Init();
    Create( parent, id, value, pos, size, style,
            ( double )min, ( double )max, ( double )initial, 1.0, -1, name );
}
