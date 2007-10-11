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

#ifndef _ADVANCEDCONTOURS_H_
#define _ADVANCEDCONTOURS_H_
/*!\file advancedcontours.h
*advancedcontours API
*/
/*!\class AdvancedContours
* 
*/
#include <vector>
#include <string>

#include <wx/dialog.h>

#include <ves/VEConfig.h>

////@end includes

/*!
 * Forward declarations
 */

////@begin forward declarations
namespace VE_XML
{
   class Command;
}

class wxSlider;
class wxCheckBox;
class wxRadioBox;
////@end forward declarations

/*!
 * Control identifiers
 */

////@begin control identifiers
#define ID_DIALOG 10000
#define SYMBOL_ADVANCEDCONTOURS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ADVANCEDCONTOURS_TITLE _T("AdvancedContours")
#define SYMBOL_ADVANCEDCONTOURS_IDNAME ID_DIALOG
#define SYMBOL_ADVANCEDCONTOURS_SIZE wxSize(400, 300)
#define SYMBOL_ADVANCEDCONTOURS_POSITION wxDefaultPosition
#define ID_SLIDER 10001
#define ID_SLIDER1 10002
#define ID_SLIDER2 10003

class VE_GUIPLUGINS_EXPORTS AdvancedContours: public wxDialog
{    

public:
    /// Constructors
    AdvancedContours( );
    AdvancedContours( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDCONTOURS_IDNAME, 
                      const wxString& caption = SYMBOL_ADVANCEDCONTOURS_TITLE, 
                      const wxPoint& pos = SYMBOL_ADVANCEDCONTOURS_POSITION, 
                      const wxSize& size = SYMBOL_ADVANCEDCONTOURS_SIZE, 
                      long style = SYMBOL_ADVANCEDCONTOURS_STYLE );

   enum ADVANCED_CONTOUR_IDS
   {
      OPACITY_SLIDER,
      WARPED_SCALE_SLIDER,
      LOD_SLIDER,
      CONTOUR_TYPE_RBOX,
      WARP_OPTION_CHK
   };

    void SendCommandsToXplorer( void );
    /// Creation
    bool Create( wxWindow* parent, 
       wxWindowID id = SYMBOL_ADVANCEDCONTOURS_IDNAME,
       const wxString& caption = SYMBOL_ADVANCEDCONTOURS_TITLE,
       const wxPoint& pos = SYMBOL_ADVANCEDCONTOURS_POSITION, 
       const wxSize& size = SYMBOL_ADVANCEDCONTOURS_SIZE, 
       long style = SYMBOL_ADVANCEDCONTOURS_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

    ///The opacity setting.
    ///\param opacity The opacity setting
    void SetOpacity(double opacity);
    ///The warped scale.
    ///\param warpScale The scale for warping on the contours
    void SetWarpedScale(double warpScale);
    ///The Level of Detail
    ///\param LOD The LOD setting
    void SetLOD(double LOD);

    ///Set the contour type
    ///\param contourType The contour type.
    void SetContourType(std::string contourType);

    ///Set the warp option
    ///\param warpOption true/false for warped contour
    void SetWarpOption(bool warpOption);

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );

    /// Should we show tooltips?
    static bool ShowToolTips();

    ///The opacity setting.
    double GetOpacity();
    ///The warped scale.
    double GetWarpedScale();
    ///The Level of Detail
    double GetLOD();

    ///Get the contour type.
    std::string GetContourType();

    ///Get the value for the warped contour control
    bool GetWarpOption();

protected:
   
   wxSlider* _opacitySlider;///<Opacity slider.
   wxSlider* _warpedScaleSlider;///<Warped scale slider.
   wxSlider* _LODSlider;///<Level Of Detail slider.
   wxRadioBox* _contourTypeRBox;///<Contour radio box dialog.
   wxCheckBox* _warpOptionCBox;///<Warp contour option.
   
   std::string ConvertUnicode( const wxChar* data )
   {
         std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
         return tempStr;
   }
};

#endif
    // _ADVANCEDCONTOURS_H_
