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
#ifndef ADVANCED_STREAMLINES_H
#define ADVANCED_STREAMLINES_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

namespace ves
{
namespace open
{
namespace xml
{
class Command;
}
}
}

// --- wxWidgets Includes --- //
#include <wx/dialog.h>

class wxSlider;
class wxCheckBox;
class wxButton;

// --- C/C++ Libraries --- //
#include <vector>

#define SYMBOL_ADVANCEDSTREAMLINES_STYLE wxCAPTION | wxRESIZE_BORDER | wxSYSTEM_MENU | wxCLOSE_BOX
#define SYMBOL_ADVANCEDSTREAMLINES_TITLE _T( "Advanced Streamlines" )
#define SYMBOL_ADVANCEDSTREAMLINES_IDNAME 10000
#define SYMBOL_ADVANCEDSTREAMLINES_SIZE wxSize( 400, 300 )
#define SYMBOL_ADVANCEDSTREAMLINES_POSITION wxDefaultPosition

namespace ves
{
namespace conductor
{

namespace util
{
class UI_TransientDialog;
}
/*!\file advancedstreamlines.h
 *advancedstreamlines API
 */

/*!\class AdvancedStreamlines
 *
 */
class VE_GUIPLUGINS_EXPORTS AdvancedStreamlines: public wxDialog
{
public:
    /// Constructors
    AdvancedStreamlines();
    AdvancedStreamlines( wxWindow* parent,
                         wxWindowID id = SYMBOL_ADVANCEDSTREAMLINES_IDNAME,
                         const wxString& caption = SYMBOL_ADVANCEDSTREAMLINES_TITLE,
                         const wxPoint& pos = SYMBOL_ADVANCEDSTREAMLINES_POSITION,
                         const wxSize& size = SYMBOL_ADVANCEDSTREAMLINES_SIZE,
                         long style = SYMBOL_ADVANCEDSTREAMLINES_STYLE );

    void SendCommandsToXplorer();
    /// Creation
    bool Create( wxWindow* parent,
                 wxWindowID id = SYMBOL_ADVANCEDSTREAMLINES_IDNAME,
                 const wxString& caption = SYMBOL_ADVANCEDSTREAMLINES_TITLE,
                 const wxPoint& pos = SYMBOL_ADVANCEDSTREAMLINES_POSITION,
                 const wxSize& size = SYMBOL_ADVANCEDSTREAMLINES_SIZE,
                 long style = SYMBOL_ADVANCEDSTREAMLINES_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

    ///Set the propagation slider value
    ///\param value The size
    void SetPropagationSize( double value );

    ///Set the integration step size.
    ///\param value The size
    void SetIntegrationStepSize( double value );
    ///Set the sphere/arrow/particle(?) size
    ///\param value The size
    void SetSphereArrowParticleSize( double value );

    ///Set the stream arrow flag
    ///\param value The stream arrow flag
    void SetStreamArrow( bool value );

    ///Set the use last seed pt flag
    ///\param value The use last seed pt flag
    void SetUseLastSeedPt( bool value );

    ///Set the line diameter
    ///\param value The size
    void SetLineDiameter( double value );

    ///Set the glow strength
    ///\param value The strength
    void SetGlowStrength( double value );

    ///Set the animated particle flag
    void SetAnimatedParticle( bool animateParticle );
    
    ///Live command for setting streamline size
    void _OnLineDiameter( wxCommandEvent& WXUNUSED( event ) );

    ///Live command for setting streamline glow strength
    void _OnGlowStrength( wxCommandEvent& WXUNUSED( event ) );

    ///Call particle tracking gui
    void _OnParticleTracking( wxCommandEvent& WXUNUSED( event ) );

    ///Get the propagation step size.
    double GetPropagationSize();

    ///Get the integration step size
    double GetIntegrationStepSize();

    ///Get the Sphere/Arrow/Particle(?) size
    double GetSphereArrowParticleSize();

    ///Get the line diameter.
    double GetLineDiameter();

    ///Get the glow strength.
    ///\return The glow strength
    double GetGlowStrength();

    ///Get the stream arrow.
    bool GetStreamArrow();
    
    ///Render animated particles or not
    ///\return True or false to render animated particles
    bool GetAnimatedParticle();

    ///Get the use last seed point flag
    bool GetUseLastSeedPoint();

    ///Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    ///Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );

    ///Should we show tooltips?
    static bool ShowToolTips();

    wxWindow* _parentLocal;

protected:

    wxSlider* _propagationSlider;
    wxSlider* _integrationSlider;
    wxSlider* _sphereArrowParticleSlider;
    wxSlider* _diameterSlider;
    wxSlider* _glowSlider;
    wxCheckBox* _lastSeedPtCheck;
    wxCheckBox* _streamArrowCheck;
    wxCheckBox* m_particleCheck;

    ves::conductor::util::UI_TransientDialog* particleControls;
    AdvancedStreamlines* _particleParent;

    DECLARE_EVENT_TABLE()
};
}
}

#endif // end ADVANCED_STREAMLINES_H
