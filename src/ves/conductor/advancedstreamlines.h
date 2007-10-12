/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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

#ifndef _ADVANCEDSTREAMLINES_H_
#define _ADVANCEDSTREAMLINES_H_
/*!\file advancedstreamlines.h
*advancedstreamlines API
*/
/*!\class AdvancedStreamlines
* 
*/
#include <vector>
#include <wx/dialog.h>


#include <ves/VEConfig.h>
////@end includes

/*!
 * Forward declarations
 */

////@begin forward declarations
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

//namespace VE_UI
//{
   class UI_TransientDialog;
//}
class wxSlider;
class wxCheckBox;
class wxButton;
////@end forward declarations

/*!
 * Control identifiers
 */

////@begin control identifiers
#define ID_DIALOG 10000
#define SYMBOL_ADVANCEDSTREAMLINES_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ADVANCEDSTREAMLINES_TITLE _T("Advanced Streamlines")
#define SYMBOL_ADVANCEDSTREAMLINES_IDNAME ID_DIALOG
#define SYMBOL_ADVANCEDSTREAMLINES_SIZE wxSize(400, 300)
#define SYMBOL_ADVANCEDSTREAMLINES_POSITION wxDefaultPosition
/*#define ID_SLIDER 10001
#define ID_SLIDER1 10002
#define ID_SLIDER2 10003
#define ID_SLIDER3 10004
#define ID_SLIDER4 10005
#define ID_CHECKBOX 10006
#define ID_CHECKBOX1 10007
#define ID_BUTTON 10008*/

////@end control identifiers

namespace ves
{
namespace conductor
{
class VE_GUIPLUGINS_EXPORTS AdvancedStreamlines: public wxDialog
{    
    DECLARE_EVENT_TABLE()
public:
    /// Constructors
    AdvancedStreamlines( );
    AdvancedStreamlines( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDSTREAMLINES_IDNAME, const wxString& caption = SYMBOL_ADVANCEDSTREAMLINES_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDSTREAMLINES_POSITION, const wxSize& size = SYMBOL_ADVANCEDSTREAMLINES_SIZE, long style = SYMBOL_ADVANCEDSTREAMLINES_STYLE );

   enum ADVANCED_STREAMLINE_IDS
   {
      PARTICLE_TRACKING_BUTTON,
      USE_SEED_POINT_CHK,
      PROPOGATION_SLIDER,
      INTEGRATION_STEP_SLIDER,
      ARROWS_CHK,
      SPHERE_SIZE_SLIDER,
      LINE_DIAMETER_SLIDER,
      PARTICLE_TRACKING,
      PARTICLE_TRACKING_DIALOG
   };

    void SendCommandsToXplorer( void );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDSTREAMLINES_IDNAME, const wxString& caption = SYMBOL_ADVANCEDSTREAMLINES_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDSTREAMLINES_POSITION, const wxSize& size = SYMBOL_ADVANCEDSTREAMLINES_SIZE, long style = SYMBOL_ADVANCEDSTREAMLINES_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

   ///Set the propagation slider value
   ///\param value The size
   void SetPropagationSize(double value);

   ///Set the integration step size.
   ///\param value The size
   void SetIntegrationStepSize(double value);
   ///Set the sphere/arrow/particle(?) size
   ///\param value The size
   void SetSphereArrowParticleSize(double value);

   ///Set the stream arrow flag
   ///\param value The stream arrow flag
   void SetStreamArrow(bool value);

   ///Set the use last seed pt flag
   ///\param value The use last seed pt flag
   void SetUseLastSeedPt(bool value);

   ///Set the line diameter
   ///\param value The size
   void SetLineDiameter(double value);

   ///Call particle tracking gui
   void _OnParticleTracking( wxCommandEvent& WXUNUSED(event) ); 

   ///Get the propagation step size.
   double GetPropagationSize();
   ///Get the integration step size
   double GetIntegrationStepSize();
   ///Get the Sphere/Arrow/Particle(?) size
   double GetSphereArrowParticleSize();

   ///Get the line diameter.
   double GetLineDiameter();

   ///Get the stream arrow.
   bool GetStreamArrow();

   ///Get the use last seed point flag
   bool GetUseLastSeedPoint();


   /// Retrieves bitmap resources
   wxBitmap GetBitmapResource( const wxString& name );

   /// Retrieves icon resources
   wxIcon GetIconResource( const wxString& name );

   /// Should we show tooltips?
   static bool ShowToolTips();

   wxWindow* _parentLocal;

protected:

   wxSlider* _propagationSlider;  
   wxSlider* _integrationSlider; 
   wxSlider* _sphereArrowParticleSlider; 
   wxSlider* _diameterSlider; 
   wxCheckBox* _lastSeedPtCheck;
   wxCheckBox* _streamArrowCheck;

   UI_TransientDialog* particleControls;
   AdvancedStreamlines* _particleParent;
};
}
}
#endif
    // _ADVANCEDSTREAMLINES_H_
